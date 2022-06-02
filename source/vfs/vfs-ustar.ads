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

package VFS.USTAR is
   --  Probe for a USTAR FS in the passed device.
   --  Return opaque FS data on success, or Null_Address on failure.
   function Probe (Dev : Device_Data) return System.Address;

   --  Basic file operations.
   function Open (FS : System.Address; Path : String) return System.Address;
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

   function Octal_To_Decimal (Octal : String) return Natural;
end VFS.USTAR;
