--  devices-tty.adb: Expose a TTY device.
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

with Arch.Stivale2;
with VFS.File; use VFS.File;
with VFS.Device;
with System.Address_To_Access_Conversions;

package body Devices.TTY is
   package Conv is new System.Address_To_Access_Conversions (VFS.File.File);

   function Init return Boolean is
      Dev : VFS.Device.Device_Data;
      File_Addr : constant System.Address :=
         Conv.To_Address (Open ("/dev/ps2keyb", Access_R).all'Access);
   begin
      Dev.Name              := "ttydev1";
      Dev.Data              := File_Addr;
      Dev.Stat.Type_Of_File := VFS.File_Character_Device;
      Dev.Stat.Mode         := 8#660#;
      Dev.Sync              := null;
      Dev.Read              := Read'Access;
      Dev.Write             := Write'Access;
      Dev.IO_Control        := null;
      return VFS.Device.Register (Dev);
   end Init;

   function Read
      (Data     : System.Address;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      PS2_File_Acc : constant File_Acc := File_Acc (Conv.To_Pointer (Data));
   begin
      return Unsigned_64 (VFS.File.Read (PS2_File_Acc, Natural (Count), To_Write));
   end Read;

   function Write
      (Data     : System.Address;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      Message : String (1 .. Natural (Count)) with Address => To_Write;
   begin
      Arch.Stivale2.Print_Terminal (Message);
      return Count;
   end Write;
end Devices.TTY;
