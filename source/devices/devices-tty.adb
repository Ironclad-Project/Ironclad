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
with System.Address_To_Access_Conversions;

package body Devices.TTY is
   package Conv is new System.Address_To_Access_Conversions (VFS.File.File);

   function Init return Boolean is
      File_Addr : constant System.Address :=
         Conv.To_Address (Open ("@ps2keyb", Access_R).all'Access);
   begin
      return Register_Root ((
         Name     => "ttydev1",
         Data     => File_Addr,
         Init     => null,
         Unload   => null,
         Sync     => null,
         Create   => null,
         Open     => null,
         Close    => null,
         Read     => Read'Access,
         Write    => Write'Access,
         Get_Size => null
      ));
   end Init;

   function Read
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : System.Address;
       Count    : Positive;
       To_Write : System.Address) return Natural
   is
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);

      PS2_File_Acc : constant File_Acc := File_Acc (Conv.To_Pointer (Data));
   begin
      return VFS.File.Read (PS2_File_Acc, Count, To_Write);
   end Read;

   function Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : System.Address;
       Count    : Positive;
       To_Write : System.Address) return Natural
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
      Message : String (1 .. Count) with Address => To_Write;
   begin
      Arch.Stivale2.Print_Terminal (Message);
      return Count;
   end Write;
end Devices.TTY;
