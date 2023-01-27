--  vfs.adb: FS and register dispatching.
--  Copyright (C) 2021 streaksu
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

with VFS.USTAR;
with System; use System;
with Lib.Synchronization;
with Ada.Unchecked_Deallocation;
with Lib.Alignment;

package body VFS with SPARK_Mode => Off is
   procedure Free_Sector_Cache is new Ada.Unchecked_Deallocation
      (Sector_Cache, Sector_Cache_Acc);
   type Sector_Cache_Arr is array (Unsigned_64 range <>) of Sector_Cache_Acc;
   Path_Buffer_Length : constant :=  100;
   Cache_Array_Length : constant := 8000;
   type Mount_Container is record
      Is_Cached   : Boolean;
      Mounted_Dev : Device_Handle;
      Mounted_FS  : FS_Type;
      FS_Data     : System.Address;
      Path_Length : Natural;
      Path_Buffer : String (1 .. Path_Buffer_Length);
      Cache_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
      Cache       : Sector_Cache_Arr (1 .. Cache_Array_Length);
      Last_Evict  : Unsigned_64;
   end record;
   type Mount_Container_Arr is array (1 .. 5) of Mount_Container;
   Mounts       : access Mount_Container_Arr;
   Mounts_Mutex : aliased Lib.Synchronization.Binary_Semaphore;

   package Ali is new Lib.Alignment (Unsigned_64);

   procedure Init is
   begin
      Mounts       := new Mount_Container_Arr;
      Mounts_Mutex := Lib.Synchronization.Unlocked_Semaphore;
      for Mount of Mounts.all loop
         Mount.Mounted_Dev := Error_Handle;
      end loop;
   end Init;

   function Mount (Name, Path : String; FS : FS_Type) return Boolean is
      Dev     : constant Device_Handle := Devices.Fetch (Name);
      FS_Data : System.Address         := System.Null_Address;
      Free_I  : Natural                := 0;
   begin
      if not Is_Absolute (Path) or Path'Length > Path_Buffer_Length or
         Dev = Error_Handle
      then
         return False;
      end if;

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev = Dev then
            goto Return_End;
         elsif Mounts (I).Mounted_Dev = Error_Handle then
            Free_I := I;
         end if;
      end loop;
      if Free_I = 0 then
         goto Return_End;
      end if;

      --  We need an initialized mount for probing the FS, thats why the
      --  awkward split initialization.
      Mounts (Free_I).Is_Cached := Devices.Is_Block_Device (Dev);
      Mounts (Free_I).Mounted_Dev                    := Dev;
      Mounts (Free_I).Path_Length                    := Path'Length;
      Mounts (Free_I).Path_Buffer (1 .. Path'Length) := Path;
      Mounts (Free_I).Cache                          := (others => null);
      Mounts (Free_I).Cache_Mutex := Lib.Synchronization.Unlocked_Semaphore;
      Mounts (Free_I).Last_Evict  := 1;
      case FS is
         when FS_USTAR =>
            FS_Data := VFS.USTAR.Probe (Free_I);
            if FS_Data /= System.Null_Address then
               Mounts (Free_I).Mounted_FS := FS_USTAR;
               Mounts (Free_I).FS_Data    := FS_Data;
            else
               Free_I := 0;
            end if;
      end case;

   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
      return Free_I /= 0;
   end Mount;

   procedure Unmount (Path : String) is
   begin
      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Error_Handle and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = Path
         then
            Flush_Caches (I);
            for Cache of Mounts (I).Cache loop
               if Cache /= null then
                  Free_Sector_Cache (Cache);
               end if;
            end loop;
            Mounts (I).Mounted_Dev := Error_Handle;
            goto Return_End;
         end if;
      end loop;
   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
   end Unmount;

   function Get_Mount (Path : String) return Natural is
      Returned : Natural := 0;
   begin
      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Error_Handle and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = Path
         then
            Returned := I;
            goto Return_End;
         end if;
      end loop;
   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
      return Returned;
   end Get_Mount;

   function Is_Valid (Key : Positive) return Boolean is
   begin
      return Key <= Mounts'Last and then
             Mounts (Key).Mounted_Dev /= Error_Handle;
   end Is_Valid;

   function Get_Backing_FS (Key : Positive) return FS_Type is
   begin
      return Mounts (Key).Mounted_FS;
   end Get_Backing_FS;

   function Get_Backing_FS_Data (Key : Positive) return System.Address is
   begin
      return Mounts (Key).FS_Data;
   end Get_Backing_FS_Data;

   function Get_Backing_Device (Key : Positive) return Device_Handle is
   begin
      return Mounts (Key).Mounted_Dev;
   end Get_Backing_Device;

   procedure Read
      (Key       : Positive;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      Block_Size : constant Unsigned_64 := Unsigned_64 (Devices.Get_Block_Size
         (Mounts (Key).Mounted_Dev));
      Ali_Offset : constant Unsigned_64 := Ali.Align_Down (Offset, Block_Size);
      Low_LBA    : constant Unsigned_64 := Ali_Offset / Block_Size;
      Ali_Length : constant Unsigned_64 :=
         Ali.Align_Up (Data'Length + Offset - Ali_Offset, Block_Size);
      LBA_Count : constant Unsigned_64 := Ali_Length / Block_Size;
      Caches : Sector_Cache_Arr (1 .. LBA_Count) := (others => null);
      Free_Index     : Unsigned_64;
      Sector_Index   : Unsigned_64;
      Initial_Offset : Unsigned_64;
      Searched       : Unsigned_64;
   begin
      if not Mounts (Key).Is_Cached then
         Devices.Read
            (Handle    => Mounts (Key).Mounted_Dev,
             Offset    => Offset,
             Ret_Count => Ret_Count,
             Data      => Data,
             Success   => Success);
         return;
      elsif Data'Length = 0 then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      Lib.Synchronization.Seize (Mounts (Key).Cache_Mutex);
      for I in 1 .. LBA_Count loop
         Free_Index := 0;
         Searched   := Low_LBA + I - 1;
         for J in Mounts (Key).Cache'Range loop
            if Mounts (Key).Cache (J) = null then
               if Free_Index = 0 then
                  Free_Index := J;
               end if;
            elsif Mounts (Key).Cache (J).LBA_Offset = Searched then
               Free_Index := J;
               goto Found;
            end if;
         end loop;
         if Free_Index = 0 then
            Free_Index := Mounts (Key).Last_Evict;
            if not Evict_Sector
               (Mounts (Key).Mounted_Dev,
                Mounts (Key).Cache (Free_Index),
                Searched)
            then
               Ret_Count := 0;
               Success   := False;
               return;
            end if;

            if Mounts (Key).Last_Evict = Cache_Array_Length then
               Mounts (Key).Last_Evict := 1;
            else
               Mounts (Key).Last_Evict := Mounts (Key).Last_Evict + 1;
            end if;
         else
            Mounts (Key).Cache (Free_Index) :=
               new Sector_Cache (Natural (Block_Size));
            Mounts (Key).Cache (Free_Index).LBA_Offset := Searched;
            Mounts (Key).Cache (Free_Index).Is_Dirty   := False;
            if not Read_Sector
               (Handle => Mounts (Key).Mounted_Dev,
                LBA    => Searched,
                Desto  => Mounts (Key).Cache (Free_Index).Data'Address)
            then
               Ret_Count := 0;
               Success   := False;
               return;
            end if;
         end if;
      <<Found>>
         Caches (I) := Mounts (Key).Cache (Free_Index);
      end loop;

      Free_Index     := 1;
      Sector_Index   := 1;
      Initial_Offset := Offset - Ali_Offset;
      for Byte of Data loop
         Byte :=
            Caches (Sector_Index).Data (Natural (Initial_Offset + Free_Index));
         if Free_Index >= Block_Size - Initial_Offset then
            Free_Index     := 1;
            Sector_Index   := Sector_Index + 1;
            Initial_Offset := 0;
         else
            Free_Index := Free_Index + 1;
         end if;
      end loop;
      Lib.Synchronization.Release (Mounts (Key).Cache_Mutex);
      Success   := True;
      Ret_Count := Data'Length;
   end Read;

   procedure Write
      (Key       : Positive;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
   begin
      Devices.Write
         (Handle    => Mounts (Key).Mounted_Dev,
          Offset    => Offset,
          Data      => Data,
          Ret_Count => Ret_Count,
          Success   => Success);
   end Write;

   function Evict_Sector
      (Handle  : Device_Handle;
       Sector  : Sector_Cache_Acc;
       New_LBA : Unsigned_64) return Boolean
   is
   begin
      if Sector.Is_Dirty then
         if not Write_Sector (Handle, Sector.LBA_Offset, Sector.Data'Address)
         then
            return False;
         end if;
      end if;

      Sector.LBA_Offset := New_LBA;
      Sector.Is_Dirty   := False;
      return Read_Sector (Handle, New_LBA, Sector.Data'Address);
   end Evict_Sector;

   procedure Flush_Caches (Key : Positive) is
      Discard : Boolean;
   begin
      Lib.Synchronization.Seize (Mounts (Key).Cache_Mutex);
      for Cache of Mounts (Key).Cache loop
         if Cache /= null and then Cache.Is_Dirty then
            Discard := Evict_Sector
               (Handle  => Mounts (Key).Mounted_Dev,
                Sector  => Cache,
                New_LBA => Cache.LBA_Offset);
         end if;
      end loop;
      Lib.Synchronization.Release (Mounts (Key).Cache_Mutex);
   end Flush_Caches;

   procedure Flush_Caches is
   begin
      for I in Mounts'Range loop
         if Is_Valid (I) then
            Flush_Caches (I);
         end if;
      end loop;
   end Flush_Caches;

   function Read_Sector
      (Handle : Device_Handle;
       LBA    : Unsigned_64;
       Desto  : System.Address) return Boolean
   is
      Discard    : Boolean;
      Result     : Natural;
      Block_Size : constant Natural := Devices.Get_Block_Size (Handle);
      Data       : Devices.Operation_Data (1 .. Block_Size)
         with Import, Address => Desto;
   begin
      Devices.Read
         (Handle    => Handle,
          Offset    => LBA * Unsigned_64 (Block_Size),
          Ret_Count => Result,
          Data      => Data,
          Success   => Discard);
      return Result = Block_Size;
   end Read_Sector;

   function Write_Sector
      (Handle : Device_Handle;
       LBA    : Unsigned_64;
       Data   : System.Address) return Boolean
   is
      Discard    : Boolean;
      Result     : Natural;
      Block_Size : constant Natural := Devices.Get_Block_Size (Handle);
      Data2      : Devices.Operation_Data (1 .. Block_Size)
         with Import, Address => Data;
   begin
      Devices.Write
         (Handle    => Handle,
          Offset    => LBA * Unsigned_64 (Block_Size),
          Ret_Count => Result,
          Data      => Data2,
          Success   => Discard);
      return Result = Block_Size;
   end Write_Sector;
   ----------------------------------------------------------------------------
   function Is_Absolute (Path : String) return Boolean is
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute;

   function Is_Canonical (Path : String) return Boolean is
      Previous : Character := ' ';
   begin
      if not Is_Absolute (Path) or else Path (Path'Last) = '/' then
         return False;
      end if;

      for C of Path loop
         if (Previous = '/' and C = '/') or (Previous = '.' and C = '.') then
            return False;
         end if;
         Previous := C;
      end loop;

      return True;
   end Is_Canonical;
end VFS;
