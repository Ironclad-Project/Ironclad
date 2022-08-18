--  devices-debugdev.adb: Driver for debug utilities.
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

with Arch.Debug;
with Lib.Synchronization;

package body Devices.Debug with SPARK_Mode => Off is
   function Init return Boolean is
      Stat   : VFS.File_Stat;
      Device : VFS.Resource;
   begin
      Stat := (
         Unique_Identifier => 0,
         Type_Of_File      => VFS.File_Character_Device,
         Mode              => 8#660#,
         Hard_Link_Count   => 1,
         Byte_Size         => 0,
         IO_Block_Size     => 4096,
         IO_Block_Count    => 0
      );

      Device := (
         Data       => System.Null_Address,
         Mutex      => <>,
         Stat       => Stat,
         Sync       => null,
         Read       => null,
         Write      => Write'Access,
         IO_Control => null,
         Mmap       => null,
         Munmap     => null
      );

      Lib.Synchronization.Release (Device.Mutex);
      return VFS.Register (Device, "debug");
   end Init;

   function Write
      (Data     : VFS.Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Buff : String (1 .. Natural (Count)) with Address => To_Write;
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Arch.Debug.Print (Buff);
      Lib.Synchronization.Release (Data.Mutex);
      return Count;
   end Write;
end Devices.Debug;
