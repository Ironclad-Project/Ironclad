--  devices-drive_cache.adb: Drive-independent cache.
--  Copyright (C) 2025 streaksu
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

package body Devices.Drive_Cache with SPARK_Mode => Off is
   procedure Init
      (Drive_Arg : System.Address;
       Read      : System.Address;
       Write     : System.Address;
       Registry  : out Cache_Registry)
   is
   begin
      Registry :=
         (Drive_Arg  => Drive_Arg,
          Read_Proc  => Read,
          Write_Proc => Write,
          Mutex      => Lib.Synchronization.Unlocked_Mutex,
          Caches     => [others => (Is_Used => False, others => <>)],
          Next_Evict => 1);
   end Init;

   procedure Read
      (Registry  : aliased in out Cache_Registry;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Dev_Status)
   is
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
      Succ : Boolean;
   begin
      Succ := True;
      Lib.Synchronization.Seize (Registry.Mutex);
      while Progress < Data'Length loop
         Current_LBA := (Offset + Unsigned_64 (Progress)) /
            Unsigned_64 (Sector_Size);

         Get_Cache_Index
          (Registry => Registry,
           LBA      => Current_LBA,
           Idx      => Cache_Idx,
           Success  => Succ);
         if not Succ then
            Succ := True;
            goto Cleanup;
         end if;

         Copy_Count   := Data'Length - Progress;
         Cache_Offset := Natural ((Offset + Unsigned_64 (Progress)) mod
                                  Unsigned_64 (Sector_Size));
         if Copy_Count > Sector_Size - Cache_Offset then
            Copy_Count := Sector_Size - Cache_Offset;
         end if;
         Data (Data'First + Progress .. Data'First + Progress + Copy_Count - 1)
            := Registry.Caches (Cache_Idx).Data (Cache_Offset + 1 ..
                                          Cache_Offset + Copy_Count);
         Progress := Progress + Copy_Count;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Registry.Mutex);
      Ret_Count := Progress;
      Success   := (if Succ then Dev_Success else Dev_IO_Failure);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Registry.Mutex);
         Data      := [others => 0];
         Success   := Dev_IO_Failure;
         Ret_Count := 0;
   end Read;

   procedure Write
      (Registry  : aliased in out Cache_Registry;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Dev_Status)
   is
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
      Succ : Boolean;
   begin
      Succ := True;
      Lib.Synchronization.Seize (Registry.Mutex);
      while Progress < Data'Length loop
         Current_LBA := (Offset + Unsigned_64 (Progress)) /
            Unsigned_64 (Sector_Size);

         Get_Cache_Index
          (Registry => Registry,
           LBA      => Current_LBA,
           Idx      => Cache_Idx,
           Success  => Succ);
         if not Succ then
            Succ := Progress /= 0;
            goto Cleanup;
         end if;

         Copy_Count   := Data'Length - Progress;
         Cache_Offset := Natural ((Offset + Unsigned_64 (Progress)) mod
                                  Unsigned_64 (Sector_Size));
         if Copy_Count > Sector_Size - Cache_Offset then
            Copy_Count := Sector_Size - Cache_Offset;
         end if;
         Registry.Caches (Cache_Idx).Data (Cache_Offset + 1 ..
                                    Cache_Offset + Copy_Count) :=
            Data (Data'First + Progress ..
                  Data'First + Progress + Copy_Count - 1);
         Registry.Caches (Cache_Idx).Is_Dirty := True;
         Progress := Progress + Copy_Count;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Registry.Mutex);
      Ret_Count := Progress;
      Success   := (if Succ then Dev_Success else Dev_IO_Failure);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Registry.Mutex);
         Success   := Dev_IO_Failure;
         Ret_Count := 0;
   end Write;

   procedure Sync
      (Registry : aliased in out Cache_Registry;
       Success  : out Boolean)
   is
      procedure Write_Sector
         (Drive       : System.Address;
          LBA         : Unsigned_64;
          Data_Buffer : Sector_Data;
          Success     : out Boolean)
      with Import, Address => Registry.Write_Proc;
   begin
      Success := True;
      Lib.Synchronization.Seize (Registry.Mutex);
      for Cache of Registry.Caches loop
         if Cache.Is_Used and Cache.Is_Dirty then
            Write_Sector (Registry.Drive_Arg, Cache.LBA_Offset, Cache.Data,
               Success);
            if not Success then
               goto Cleanup;
            end if;
            Cache.Is_Dirty := False;
         end if;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Registry.Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Registry.Mutex);
         Success := False;
   end Sync;

   procedure Sync_Range
      (Registry : aliased in out Cache_Registry;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       Success  : out Boolean)
   is
      procedure Write_Sector
         (Drive       : System.Address;
          LBA         : Unsigned_64;
          Data_Buffer : Sector_Data;
          Success     : out Boolean)
      with Import, Address => Registry.Write_Proc;

      First_LBA : Unsigned_64;
      Last_LBA  : Unsigned_64;
   begin
      Lib.Synchronization.Seize (Registry.Mutex);

      First_LBA := Offset / Unsigned_64 (Sector_Size);
      Last_LBA  := (Offset + Count) / Unsigned_64 (Sector_Size);
      Success   := True;

      for Cache of Registry.Caches loop
         if Cache.Is_Used                 and
            Cache.Is_Dirty                and
            Cache.LBA_Offset >= First_LBA and
            Cache.LBA_Offset <= Last_LBA
         then
            Write_Sector (Registry.Drive_Arg, Cache.LBA_Offset, Cache.Data,
               Success);
            if not Success then
               goto Cleanup;
            end if;
            Cache.Is_Dirty := False;
         end if;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Registry.Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Registry.Mutex);
         Success := False;
   end Sync_Range;
   ----------------------------------------------------------------------------
   procedure Get_Cache_Index
      (Registry : aliased in out Cache_Registry;
       LBA      : Unsigned_64;
       Idx      : out Natural;
       Success  : out Boolean)
   is
      procedure Read_Sector
         (Drive       : System.Address;
          LBA         : Unsigned_64;
          Data_Buffer : out Sector_Data;
          Success     : out Boolean)
         with Import, Address => Registry.Read_Proc;
      procedure Write_Sector
         (Drive       : System.Address;
          LBA         : Unsigned_64;
          Data_Buffer : Sector_Data;
          Success     : out Boolean)
      with Import, Address => Registry.Write_Proc;
   begin
      Idx := 0;

      for I in Registry.Caches'Range loop
         if Registry.Caches (I).Is_Used and
            Registry.Caches (I).LBA_Offset = LBA
         then
            Idx     := I;
            Success := True;
            return;
         elsif not Registry.Caches (I).Is_Used and Idx = 0 then
            Idx := I;
         end if;
      end loop;

      if Idx = 0 then
         Idx := Registry.Next_Evict;

         if Registry.Caches (Registry.Next_Evict).Is_Dirty then
            Write_Sector
               (Drive       => Registry.Drive_Arg,
                LBA         => Registry.Caches (Idx).LBA_Offset,
                Data_Buffer => Registry.Caches (Idx).Data,
                Success     => Success);
            if not Success then
               return;
            end if;
         end if;

         if Registry.Next_Evict = Registry.Caches'Last then
            Registry.Next_Evict := Registry.Caches'First;
         else
            Registry.Next_Evict := Registry.Next_Evict + 1;
         end if;
      end if;

      Registry.Caches (Idx) :=
         (Is_Used    => True,
          LBA_Offset => LBA,
          Is_Dirty   => False,
          Data       => <>);

      Read_Sector
         (Drive       => Registry.Drive_Arg,
          LBA         => Registry.Caches (Idx).LBA_Offset,
          Data_Buffer => Registry.Caches (Idx).Data,
          Success     => Success);
   exception
      when Constraint_Error =>
         Idx     := 0;
         Success := False;
   end Get_Cache_Index;
end Devices.Drive_Cache;
