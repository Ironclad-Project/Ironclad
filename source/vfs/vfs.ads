--  vfs.ads: FS and register dispatching.
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
with System;
with Devices; use Devices;

package VFS with SPARK_Mode => Off is
   --  Stat structure of a file, which describes the qualities of a file.
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

   --  Initialize the internal VFS registries.
   procedure Init;

   --  Mount or unmount an FS, along with getting devices based on their FS.
   type FS_Type is (FS_USTAR);
   function Mount
      (Name : String;
       Path : String;
       FS   : FS_Type) return Boolean;
   function Get_Mount
      (Path : String;
       FS   : out FS_Type;
       Dev  : out Devices.Resource_Acc) return System.Address;
   procedure Unmount (Path : String);
   ----------------------------------------------------------------------------
   --  Check whether a path is absolute or canonical.
   function Is_Absolute  (Path : String) return Boolean;
   function Is_Canonical (Path : String) return Boolean;
end VFS;
