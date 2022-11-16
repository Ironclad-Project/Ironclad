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

package body Devices.Streams with SPARK_Mode => Off is
   function Init return Boolean is
      Stat    : VFS.File_Stat;
      Nulldev : VFS.Resource;
      Zerodev : VFS.Resource;
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

      Nulldev := (
         Data       => System.Null_Address,
         Mutex      => <>,
         Stat       => Stat,
         Sync       => null,
         Read       => Nulldev_Read'Access,
         Write      => Nulldev_Write'Access,
         IO_Control => null,
         Mmap       => null,
         Munmap     => null
      );

      Zerodev := (
         Data       => System.Null_Address,
         Mutex      => <>,
         Stat       => Stat,
         Sync       => null,
         Read       => Zerodev_Read'Access,
         Write      => Zerodev_Write'Access,
         IO_Control => null,
         Mmap       => null,
         Munmap     => null
      );

      return VFS.Register (Nulldev, "null") and VFS.Register (Zerodev, "zero");
   end Init;
   ----------------------------------------------------------------------------
   function Nulldev_Read
      (Data   : VFS.Resource_Acc;
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
      (Data     : VFS.Resource_Acc;
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
      (Data   : VFS.Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      Result : array (1 .. Count) of Unsigned_8 with Import, Address => Desto;
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
   begin
      for I of Result loop
         I := 0;
      end loop;
      return Count;
   end Zerodev_Read;

   function Zerodev_Write
      (Data     : VFS.Resource_Acc;
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
