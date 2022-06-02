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
with VFS;
with System.Address_To_Access_Conversions;

package body Devices.TTY is
   package Conv is new System.Address_To_Access_Conversions (VFS.File.File);

   function Init return Boolean is
      Dev : VFS.Device_Data;
      File_Addr : constant System.Address :=
         Conv.To_Address (Open ("/dev/ps2keyboard", Access_R).all'Access);
   begin
      Dev.Name (1 .. 6)       := "ttydev";
      Dev.Name_Len            := 6;
      Dev.Data                := File_Addr;
      Dev.Stat.Type_Of_File   := VFS.File_Character_Device;
      Dev.Stat.Mode           := 8#660#;
      Dev.Stat.Byte_Size      := 0;
      Dev.Stat.IO_Block_Size  := 4096;
      Dev.Stat.IO_Block_Count := 0;
      Dev.Read                := Read'Access;
      Dev.Write               := Write'Access;
      Dev.IO_Control          := IO_Control'Access;
      return VFS.Register (Dev);
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
      return VFS.File.Read (PS2_File_Acc, Count, To_Write);
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

   IO_Control_TIOCGWINSZ : constant := 16#5413#;
   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      type Window_Size is record
         Rows     : Unsigned_16;
         Columns  : Unsigned_16;
         X_Pixels : Unsigned_16;
         Y_Pixels : Unsigned_16;
      end record;
      pragma Unreferenced (Data);
   begin
      case Request is
         when IO_Control_TIOCGWINSZ =>
            declare
               Requested_Data : Window_Size with Address => Argument;
            begin
               --  TODO: This is hardcoded data assuming a 16x8 font.
               Requested_Data := (
                  Rows     => 25,
                  Columns  => 80,
                  X_Pixels => 80 * 8,
                  Y_Pixels => 25 * 16
               );
               return True;
            end;
         when others =>
            return False;
      end case;
   end IO_Control;
end Devices.TTY;
