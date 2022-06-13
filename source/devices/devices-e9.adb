--  devices-e9.adb: E9 driver.
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

with Arch.Wrappers;

package body Devices.E9 is
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
         Mutex      => (others => <>),
         Stat       => Stat,
         Sync       => null,
         Read       => null,
         Write      => Write'Access,
         IO_Control => null,
         Mmap       => null,
         Munmap     => null
      );

      return VFS.Register (Device, "e9debug");
   end Init;

   function Write
      (Data     : VFS.Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      Buff : array (1 .. Count) of Unsigned_8 with Address => To_Write;
   begin
      for C of Buff loop
         Arch.Wrappers.Port_Out (16#E9#, C);
      end loop;
      return Count;
   end Write;
end Devices.E9;
