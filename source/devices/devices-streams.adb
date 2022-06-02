--  devices-streams.adb: Virtual stream devices.
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

with VFS;

package body Devices.Streams is
   function Init return Boolean is
      Nulldev : VFS.Device_Data;
      Zerodev : VFS.Device_Data;
   begin
      Nulldev.Name (1 .. 4)       := "null";
      Nulldev.Name_Len            := 4;
      Nulldev.Stat.Type_Of_File   := VFS.File_Character_Device;
      Nulldev.Stat.Mode           := 8#660#;
      Nulldev.Stat.Byte_Size      := 0;
      Nulldev.Stat.IO_Block_Size  := 4096;
      Nulldev.Stat.IO_Block_Count := 0;
      Nulldev.Read                := Nulldev_Read'Access;
      Nulldev.Write               := Nulldev_Write'Access;

      Zerodev.Name (1 .. 4)       := "zero";
      Zerodev.Name_Len            := 4;
      Zerodev.Stat.Type_Of_File   := VFS.File_Character_Device;
      Zerodev.Stat.Mode           := 8#660#;
      Zerodev.Stat.Byte_Size      := 0;
      Zerodev.Stat.IO_Block_Size  := 4096;
      Zerodev.Stat.IO_Block_Count := 0;
      Zerodev.Read                := Zerodev_Read'Access;
      Zerodev.Write               := Zerodev_Write'Access;

      if not VFS.Register (Nulldev) then return False; end if;
      if not VFS.Register (Zerodev) then return False; end if;
      return True;
   end Init;
   ----------------------------------------------------------------------------
   function Nulldev_Read
      (Data   : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Count);
      pragma Unreferenced (Desto);
   begin
      --  Return that there is nothing to read, end of file.
      return 0;
   end Nulldev_Read;

   function Nulldev_Write
      (Data     : System.Address;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      pragma Unreferenced (To_Write);
   begin
      --  Return that everything was written successfully
      return Count;
   end Nulldev_Write;
   ----------------------------------------------------------------------------
   function Zerodev_Read
      (Data   : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      Result : array (1 .. Count) of Unsigned_8 with Address => Desto;
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
   begin
      for I of Result loop
         I := 0;
      end loop;
      return Count;
   end Zerodev_Read;

   function Zerodev_Write
      (Data     : System.Address;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      pragma Unreferenced (To_Write);
   begin
      --  Return that everything was written successfully
      return Count;
   end Zerodev_Write;
end Devices.Streams;
