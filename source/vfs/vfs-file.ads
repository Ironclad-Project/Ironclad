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
   procedure Open
      (Path         : String;
       Access_Flags : Access_Mode;
       Result       : out File_Acc;
       Follow_Links : Boolean := True);

   --  Get the full path of a file.
   type String_Acc is access String;
   function Get_Path (File : File_Acc) return String_Acc;

   --  An internal position is kept for next reads to be easier.
   --  This functions get or set said position.
   --  Setting the position past of the end of file is not checked.
   function Get_Position (File : File_Acc) return Unsigned_64
      with Inline, Pre => File /= null;
   procedure Set_Position
      (File    : File_Acc;
       Pos     : Unsigned_64;
       Success : out Boolean)
      with Pre => File /= null;

   --  Files have an access mode, this function fetches it for the passed file.
   function Get_Access (File : File_Acc) return Access_Mode
      with Inline, Pre => File /= null;

   --  Devices in UNIX have a unique device ID, used for representing it in
   --  stat-s. This function fetches the one the file represents or
   --  where the file is contained.
   function Get_Device_ID (File : File_Acc) return Natural
      with Inline, Pre => File /= null;

   --  Increase refcount, or decrease and close an opened file.
   procedure Increase_Refcount (F : File_Acc)   with Pre => F        /= null;
   procedure Close (To_Close : in out File_Acc) with Pre => To_Close /= null;

   --  Type for file operations.
   subtype Operation_Data is Devices.Operation_Data;

   --  Read directory entries.
   --  @param To_Read   File to read.
   --  @param Entities  Where to store the read entries, as many as possible.
   --  @param Ret_Count The count of entries, even if num > Entities'Length.
   --  @param Success   True in success, False in failure.
   procedure Read_Entries
      (To_Read   : File_Acc;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out Boolean)
   with Pre => To_Read /= null;

   --  Read contents of a symbolic link.
   --  @param To_Read   File to read.
   --  @param Path      Result to store as much as possible of the path.
   --  @param Ret_Count The path length in the filesystem. 0 in failure.
   procedure Read_Symbolic_Link
      (To_Read   : File_Acc;
       Path      : out String;
       Ret_Count : out Natural)
   with Pre => To_Read /= null;

   --  Read from a file, and return the read count.
   procedure Read
      (To_Read   : File_Acc;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
      with Pre => To_Read /= null;

   --  Write to a file, and return the written count.
   procedure Write
      (To_Write  : File_Acc;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
      with Pre => To_Write /= null;

   --  Get the stat of the file.
   procedure Stat (F : File_Acc; St : out File_Stat; Success : out Boolean)
      with Pre => F /= null;

   --  Truncate the file to the passed size.
   procedure Truncate (F : File_Acc; Size : Unsigned_64; Success : out Boolean)
      with Pre => F /= null;

   --  IOCTL.
   procedure IO_Control
      (F        : File_Acc;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out Boolean)
      with Pre => F /= null;

   --  Synchronize.
   function Synchronize (F : File_Acc) return Boolean with Pre => F /= null;

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

   --  Create several kinds of files.
   procedure Create_Node
      (Path    : String;
       Typ     : File_Type;
       Mode    : File_Mode;
       Success : out Boolean);

   procedure Create_Symbolic_Link
      (Path, Target : String;
       Mode         : Unsigned_32;
       Success      : out Boolean);

   procedure Create_Hard_Link
      (Path, Target : String;
       Success      : out Boolean);

   --  Rename files.
   procedure Rename
      (Source, Target : String;
       Keep           : Boolean;
       Success        : out Boolean);

   --  Queue a file for unlinking.
   procedure Unlink (Path : String; Success : out Boolean);

private

   type File is record
      Refcount  : Natural;
      Is_Device : Boolean;
      Full_Path : String_Acc;
      Dev_Data  : Device_Handle;
      FS_Data   : FS_Handle;
      File_Data : File_Inode_Number;
      Index     : Unsigned_64;
      Flags     : Access_Mode;
   end record;

   procedure Resolve_File
      (Path         : String;
       Is_Device    : out Boolean;
       Fetched_Dev  : out Device_Handle;
       Fetched_FS   : out FS_Handle;
       Fetched_File : out File_Inode_Number;
       Success      : out Boolean;
       Follow_Links : Boolean);
end VFS.File;
