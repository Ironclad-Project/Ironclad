--  vfs-file.ads: File creation and management.
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
with Memory;

package VFS.File is
   type Access_Mode is (Access_R, Access_W, Access_RW);
   type String_Acc is access String;
   type File is record
      Full_Path : String_Acc;
      Dev_Data  : Resource_Acc;
      FS_Type   : VFS.FS_Type;
      FS_Data   : System.Address;
      File_Data : System.Address;
      Index     : Unsigned_64;
      Flags     : Access_Mode;
   end record;
   type File_Acc is access all File;

   --  Check if a path is absolute.
   function Is_Absolute (Path : String) return Boolean;

   --  Open a file with an absolute path, and return it, or null on failure.
   function Open (Path : String; Access_Flags : Access_Mode) return File_Acc;

   --  Check permissions for a file, in average its faster than opening a file.
   function Check_Permissions
      (Path      : String;
       Exists    : Boolean;
       Can_Read  : Boolean;
       Can_Write : Boolean;
       Can_Exec  : Boolean) return Boolean;

   --  Duplicate an opened file.
   function Duplicate (To_Duplicate : File_Acc) return File_Acc;

   --  Close an opened file.
   procedure Close (To_Close : in out File_Acc);

   --  Read from a file, and return the read count.
   function Read
      (To_Read     : File_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64;

   --  Write to a file, and return the written count.
   function Write
      (To_Write : File_Acc;
       Count    : Unsigned_64;
       Data     : System.Address) return Unsigned_64;

   --  Get the stat of the file.
   function Stat (F : File_Acc; S : out VFS.File_Stat) return Boolean;

   --  IOCTL.
   function IO_Control
      (F        : File_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;

   --  Mmap.
   function Mmap
      (F           : File_Acc;
       Address     : Memory.Virtual_Address;
       Length      : Unsigned_64;
       Map_Read    : Boolean;
       Map_Write   : Boolean;
       Map_Execute : Boolean) return Boolean;

   --  Munmap.
   function Munmap
      (F       : File_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64) return Boolean;

private

   function Resolve_File
      (Path         : String;
       Is_Dev       : out Boolean;
       Fetched_Dev  : out Resource_Acc;
       Fetched_Type : out VFS.FS_Type;
       Fetched_FS   : out System.Address) return System.Address;
end VFS.File;
