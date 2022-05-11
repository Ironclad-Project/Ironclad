--  vfs.ads: FS registry and dispatching library specification.
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

with Interfaces; use Interfaces;

package VFS is
   type File_Type is (
      File_Regular,
      File_Directory,
      File_Symbolic_Link,
      File_Character_Device,
      File_Block_Device
   );

   type File_Stat is record
      Unique_Identifier : Unsigned_64;
      Type_Of_File      : File_Type;
      Mode              : Unsigned_32;
      Hard_Link_Count   : Positive;
      Byte_Size         : Unsigned_64;
      IO_Block_Size     : Natural;
      IO_Block_Count    : Unsigned_64;
   end record;
end VFS;
