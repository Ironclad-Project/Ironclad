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

with VFS.Device;
with VFS;
with Arch.Wrappers;

package body Devices.E9 is
   function Init return Boolean is
      Dev : VFS.Device.Device_Data;
   begin
      Dev.Name (1 .. 7)       := "e9debug";
      Dev.Name_Len            := 7;
      Dev.Stat.Type_Of_File   := VFS.File_Character_Device;
      Dev.Stat.Mode           := 8#660#;
      Dev.Stat.Byte_Size      := 0;
      Dev.Stat.IO_Block_Size  := 4096;
      Dev.Stat.IO_Block_Count := 0;
      Dev.Write               := Write'Access;
      return VFS.Device.Register (Dev);
   end Init;

   function Write
      (Data     : System.Address;
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
