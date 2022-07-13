--  vfs-ustar.ads: USTAR FS driver.
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

with System;

package VFS.USTAR with SPARK_Mode => Off is
   --  Probe for a USTAR FS in the passed device.
   --  Return opaque FS data on success, or Null_Address on failure.
   function Probe (Dev : Resource_Acc) return System.Address;

   --  Basic file operations.
   function Open (FS : System.Address; Path : String) return System.Address;
   function Check_Permissions
      (FS        : System.Address;
       Path      : String;
       Exists    : Boolean;
       Can_Read  : Boolean;
       Can_Write : Boolean;
       Can_Exec  : Boolean) return Boolean;
   procedure Close (FS : System.Address; File_Ptr : System.Address);
   function Read
      (Data   : System.Address;
       Obj    : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64;
   function Stat
      (Data : System.Address;
       Obj  : System.Address;
       S    : out File_Stat) return Boolean;
private
   type USTAR_File is record
      Name      : String (1 .. 100);
      Name_Len  : Natural;
      Mode      : Natural;
      Start     : Unsigned_64;
      Size      : Natural;
      File_Type : Unsigned_8;
   end record;
   type USTAR_File_Acc is access USTAR_File;

   function Fetch_Header
      (FS   : System.Address;
       Path : String;
       Data : out USTAR_File) return Boolean;

   function Octal_To_Decimal (Octal : String) return Natural;
end VFS.USTAR;
