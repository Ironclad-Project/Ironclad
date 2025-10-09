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

with Alignment;

package body Devices.Drive_Cache is
   procedure Init
      (Drive_Arg : System.Address;
       Read      : not null Read_Sector_Acc;
       Write     : not null Write_Sector_Acc;
       Registry  : aliased out Cache_Registry)
   is
   begin
      Registry.Drive_Arg  := Drive_Arg;
      Registry.Read_Proc  := Read;
      Registry.Write_Proc := Write;
      for C of Registry.Caches loop
         C.Mutex   := Synchronization.Unlocked_Mutex;
         C.Is_Used := False;
      end loop;
   end Init;

   procedure Read
      (Registry  : aliased in out Cache_Registry;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Dev_Status)
   is
      Progress, Copy_Count, Cache_Offset : Natural := 0;
      Cache_Idx, Current_LBA : Unsigned_64;
      Succ : Boolean;
   begin
      Succ := True;
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
         Synchronization.Release (Registry.Caches (Cache_Idx).Mutex);
         Progress := Progress + Copy_Count;
      end loop;

   <<Cleanup>>
      Ret_Count := Progress;
      Success   := (if Succ then Dev_Success else Dev_IO_Failure);
   exception
      when Constraint_Error =>
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
      Progress, Copy_Count, Cache_Offset : Natural := 0;
      Cache_Idx, Current_LBA : Unsigned_64;
      Succ : Boolean;
   begin
      Succ := True;
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
         Synchronization.Release (Registry.Caches (Cache_Idx).Mutex);
         Progress := Progress + Copy_Count;
      end loop;

   <<Cleanup>>
      Ret_Count := Progress;
      Success   := (if Succ then Dev_Success else Dev_IO_Failure);
   exception
      when Constraint_Error =>
         Success   := Dev_IO_Failure;
         Ret_Count := 0;
   end Write;

   procedure Sync
      (Registry : aliased in out Cache_Registry;
       Success  : out Boolean)
   is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
   begin
      Success := True;
      for Cache of Registry.Caches loop
         Synchronization.Seize (Cache.Mutex);
         if Cache.Is_Used and Cache.Is_Dirty then
            Registry.Write_Proc
               (Registry.Drive_Arg, Cache.LBA_Offset, Cache.Data, Success);
            Cache.Is_Dirty := False;
         end if;
         Synchronization.Release (Cache.Mutex);
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Sync;

   procedure Sync_Range
      (Registry : aliased in out Cache_Registry;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       Success  : out Boolean)
   is
      package Align is new Alignment (Unsigned_64);
      First_LBA, LBAs, Idx, Current_LBA : Unsigned_64;
   begin
      Success := True;
      First_LBA := Offset / Unsigned_64 (Sector_Size);
      LBAs := Align.Divide_Round_Up (Count, Unsigned_64 (Sector_Size)) + 1;

      for I in 1 .. LBAs loop
         Current_LBA := First_LBA + I - 1;
         Idx := Get_Cache_Index (Current_LBA);
         Synchronization.Seize (Registry.Caches (Idx).Mutex);
         if Registry.Caches (Idx).Is_Used and
            Registry.Caches (Idx).Is_Dirty and
            Registry.Caches (Idx).LBA_Offset = Current_LBA
         then
            Registry.Write_Proc
               (Registry.Drive_Arg,
                Registry.Caches (Idx).LBA_Offset,
                Registry.Caches (Idx).Data,
                Success);
            Registry.Caches (Idx).Is_Dirty := False;
         end if;
         Synchronization.Release (Registry.Caches (Idx).Mutex);
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Sync_Range;
   ----------------------------------------------------------------------------
   procedure Get_Cache_Index
      (Registry : aliased in out Cache_Registry;
       LBA      : Unsigned_64;
       Idx      : out Unsigned_64;
       Success  : out Boolean)
   is
   begin
      Idx := Get_Cache_Index (LBA);

      --  If the cache is used, we can just use it if its us, or evict it.
      Synchronization.Seize (Registry.Caches (Idx).Mutex);
      if Registry.Caches (Idx).Is_Used then
         if Registry.Caches (Idx).LBA_Offset = LBA then
            Success := True;
            return;
         elsif Registry.Caches (Idx).Is_Dirty then
            Registry.Write_Proc
               (Drive       => Registry.Drive_Arg,
                LBA         => Registry.Caches (Idx).LBA_Offset,
                Data_Buffer => Registry.Caches (Idx).Data,
                Success     => Success);
            if not Success then
               Synchronization.Release (Registry.Caches (Idx).Mutex);
               return;
            end if;
         end if;
      end if;

      --  Set the found index as not dirty and used, and read into it.
      Registry.Caches (Idx).Is_Used := True;
      Registry.Caches (Idx).LBA_Offset := LBA;
      Registry.Caches (Idx).Is_Dirty := False;
      Registry.Read_Proc
         (Drive       => Registry.Drive_Arg,
          LBA         => Registry.Caches (Idx).LBA_Offset,
          Data_Buffer => Registry.Caches (Idx).Data,
          Success     => Success);
   exception
      when Constraint_Error =>
         Idx     := 0;
         Success := False;
   end Get_Cache_Index;

   function Get_Cache_Index (LBA : Unsigned_64) return Unsigned_64 is
   begin
      return (LBA rem Max_Caching_Len) + 1;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Cache_Index;
end Devices.Drive_Cache;
