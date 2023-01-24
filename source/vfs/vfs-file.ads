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

package VFS.File with SPARK_Mode => Off is
   --  A file represents an entity in disk, held by an FS.
   --  This file abstraction is built for kernel-use and indirect userland use
   --  thru purpose-built file descriptions.
   --
   --  Emulation for /dev devices is provided, which are accessed just like
   --  any file.

   --  File type and access modes for a file to be opened with.
   type Access_Mode is (Read_Only, Write_Only, Read_Write);
   type File     is private;
   type File_Acc is access File;

   --  Open a file with an absolute path.
   --  TODO: .. and . are not supported, they are nice to have.
   --  TODO: Directories are not fully supported, they are nice to have.
   function Open
      (Path         : String;
       Access_Flags : Access_Mode;
       Follow_Links : Boolean := True) return File_Acc;

   --  Get the full path of a file.
   type String_Acc is access String;
   function Get_Path (File : File_Acc) return String_Acc;

   --  An internal position is kept for next reads to be easier.
   --  This functions get or set said position.
   --  Setting the position past of the end of file is not checked.
   function Get_Position (File : File_Acc) return Unsigned_64
      with Inline, Pre => File /= null;
   procedure Set_Position (File : File_Acc; Pos : Unsigned_64)
      with Inline, Pre => File /= null;

   --  Files have an access mode, this function fetches it for the passed file.
   function Get_Access (File : File_Acc) return Access_Mode
      with Inline, Pre => File /= null;

   --  Devices in UNIX have a unique device ID, used for representing it in
   --  stat-s. This function fetches the one the file represents or
   --  where the file is contained.
   function Get_Device_ID (File : File_Acc) return Natural
      with Inline, Pre => File /= null;

   --  Check permissions for a file. Path has the same limitations as Open.
   --  Faster and easier than Open + Stat! (its all temp so it doesnt save).
   --  Returns True if the passed permissions are supported.
   function Check_Permissions
      (Path         : String;
       Exists       : Boolean;
       Can_Read     : Boolean;
       Can_Write    : Boolean;
       Can_Exec     : Boolean;
       Follow_Links : Boolean := True) return Boolean;

   --  Increase refcount, or decrease and close an opened file.
   procedure Increase_Refcount (F : File_Acc)   with Pre => F        /= null;
   procedure Close (To_Close : in out File_Acc) with Pre => To_Close /= null;

   --  Read from a file, and return the read count.
   function Read
      (To_Read     : File_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
      with Pre => To_Read /= null;

   --  Write to a file, and return the written count.
   function Write
      (To_Write : File_Acc;
       Count    : Unsigned_64;
       Data     : System.Address) return Unsigned_64
      with Pre => To_Write /= null;

   --  Get the stat of the file.
   function Stat (F : File_Acc; S : out File_Stat) return Boolean
      with Pre => F /= null;

   --  IOCTL.
   function IO_Control
      (F        : File_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean with Pre => F /= null;

   --  Mmap.
   function Mmap
      (F           : File_Acc;
       Address     : Memory.Virtual_Address;
       Length      : Unsigned_64;
       Map_Read    : Boolean;
       Map_Write   : Boolean;
       Map_Execute : Boolean) return Boolean with Pre => F /= null;

   --  Munmap.
   function Munmap
      (F       : File_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64) return Boolean with Pre => F /= null;

private

   type File is record
      Refcount  : Natural;
      Full_Path : String_Acc;
      Dev_Data  : Device_Handle;
      FS_Type   : VFS.FS_Type;
      FS_Data   : System.Address;
      File_Data : System.Address;
      Index     : Unsigned_64;
      Flags     : Access_Mode;
   end record;

   function Resolve_File
      (Path         : String;
       Is_Device    : out Boolean;
       Fetched_Dev  : out Device_Handle;
       Fetched_Type : out FS_Type;
       Fetched_FS   : out System.Address;
       Follow_Links : Boolean) return System.Address;
end VFS.File;
