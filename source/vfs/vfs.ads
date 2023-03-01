--  vfs.ads: FS and register dispatching.
--  Copyright (C) 2023 streaksu
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
with System;     use System;
with Devices;    use Devices;

package VFS with SPARK_Mode => Off is
   --  Inodes numbers are identifiers that denotes a unique file inside an FS.
   --  These values are not consistent across filesystems, they may not be
   --  consistent across different mounts, depending on the FS.
   type File_Inode_Number is new Unsigned_64;

   --  Stat structure of a file, which describes the qualities of a file.
   type File_Timestamp is record
      Seconds_Since_Epoch    : Unsigned_64;
      Additional_Nanoseconds : Unsigned_64;
   end record;
   type File_Type is (File_Regular, File_Directory, File_Symbolic_Link,
                      File_Character_Device, File_Block_Device);
   type File_Stat is record
      Unique_Identifier : File_Inode_Number;
      Type_Of_File      : File_Type;
      Mode              : Unsigned_32;
      Hard_Link_Count   : Positive;
      Byte_Size         : Unsigned_64;
      IO_Block_Size     : Natural;
      IO_Block_Count    : Unsigned_64;
      Creation_Time     : File_Timestamp;
      Modification_Time : File_Timestamp;
      Access_Time       : File_Timestamp;
   end record;

   --  Describes an entity inside the contents of a directory.
   type Directory_Entity is record
      Inode_Number : Unsigned_64;
      Name_Buffer  : String (1 .. 60);
      Name_Len     : Natural;
      Type_Of_File : File_Type;
   end record;
   type Directory_Entities is array (Natural range <>) of Directory_Entity;
   ----------------------------------------------------------------------------
   --  Handle for interfacing with mounted FSs and FS types.
   type FS_Type   is (FS_EXT, FS_FAT32);
   type FS_Handle is private;
   Error_Handle : constant FS_Handle;

   --  Initialize the internal VFS registries.
   procedure Init;

   --  Mount the passed device name into the passed path.
   --  @param Name Name of the device (/dev/<name>).
   --  @param Path Absolute path for mounting.
   --  @param FS FS Type to mount as.
   --  @return True on success, False on failure.
   function Mount (Name, Path : String; FS : FS_Type) return Boolean;

   --  Mount the passed device name into the passed path, guessing the FS.
   --  @param Name Name of the device (/dev/<name>).
   --  @param Path Absolute path for mounting.
   --  @return True on success, False on failure.
   function Mount (Name, Path : String) return Boolean;

   --  Unmount a mount, syncing when possible.
   --  @param Path  Path of the mount to unmount.
   --  @param Force Whether to unmount even if busy.
   --  @return True on success, False if busy or non present.
   function Unmount (Path : String; Force : Boolean) return Boolean;

   --  Get a best-matching mount for the passed path.
   --  @param Path Path to search a mount for.
   --  @param Match Count of characters matched.
   --  @return Key to use to refer to the mount, or 0 if not found.
   function Get_Mount (Path : String; Match : out Natural) return FS_Handle;

   --  Get the backing FS type.
   --  @param Key Key to use to fetch the info.
   --  @return The FS type, will be a placeholder if the key is not valid.
   function Get_Backing_FS (Key : FS_Handle) return FS_Type
      with Pre => Key /= Error_Handle;

   --  Get the backing data of the FS.
   --  @param Key Key to use to fetch the info.
   --  @return The FS data, or System.Null_Address if not a valid key.
   function Get_Backing_FS_Data (Key : FS_Handle) return System.Address
      with Pre => Key /= Error_Handle;

   --  Get the backing device of a mount.
   --  @param Key Key to use to fetch the info.
   --  @return The backing device.
   function Get_Backing_Device (Key : FS_Handle) return Device_Handle
      with Pre => Key /= Error_Handle;

   --  Open a file with an absolute path inside the mount.
   --  @param Key     FS Handle to open.
   --  @param Path    Absolute path inside the mount, creation is not done.
   --  @param Ino     Found inode, if any.
   --  @param Success True if found, False if not.
   --  @return Returned opaque pointer for the passed mount, Null in failure.
   procedure Open
      (Key     : FS_Handle;
       Path    : String;
       Ino     : out File_Inode_Number;
       Success : out Boolean)
      with Pre => Key /= Error_Handle;

   --  Create a file with an absolute path inside the mount.
   --  @param Key  FS Handle to open.
   --  @param Path Absolute path inside the mount, must not exist.
   --  @param Mode Mode to use for the created file.
   --  @return True on success, False on failure.
   function Create_Regular
      (Key  : FS_Handle;
       Path : String;
       Mode : Unsigned_32) return Boolean
      with Pre => Key /= Error_Handle;

   --  Create a symlink with an absolute path inside the mount and a target.
   --  @param Key    FS Handle to open.
   --  @param Path   Absolute path inside the mount, must not exist.
   --  @param Target Target of the symlink, it is not checked in any way.
   --  @param Mode   Mode to use for the created symlink.
   --  @return True on success, False on failure.
   function Create_Symbolic_Link
      (Key          : FS_Handle;
       Path, Target : String;
       Mode         : Unsigned_32) return Boolean
      with Pre => Key /= Error_Handle;

   --  Create a directory with an absolute path inside the mount.
   --  @param Key    FS Handle to open.
   --  @param Path   Absolute path inside the mount, must not exist.
   --  @param Mode   Mode to use for the created directory.
   --  @return True on success, False on failure.
   function Create_Directory
      (Key  : FS_Handle;
       Path : String;
       Mode : Unsigned_32) return Boolean
      with Pre => Key /= Error_Handle;

   --  Delete a file by absolute path inside the mount.
   --  @param Key  FS Handle to open.
   --  @param Path Absolute path inside the mount, must exist.
   --  @return True on success, False on failure.
   function Delete (Key : FS_Handle; Path : String) return Boolean;

   --  Signal to the FS we do not need this inode anymore.
   --  @param Key FS handle to operate on.
   --  @param Ino Inode to signal to close.
   procedure Close (Key : FS_Handle; Ino : File_Inode_Number)
      with Pre => Key /= Error_Handle;

   --  Read the entries of an opened directory.
   --  @param Key       FS handle to operate on.
   --  @param Ino       Inode to operate on.
   --  @param Entities  Where to store the read entries, as many as possible.
   --  @param Ret_Count The count of entries, even if num > Entities'Length.
   --  @param Success   True in success, False in failure.
   procedure Read_Entries
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out Boolean)
      with Pre => Key /= Error_Handle;

   --  Read the entries of an opened directory.
   --  @param Key       FS handle to operate on.
   --  @param Ino       Inode to operate on.
   --  @param Entities  Where to store the read entries, as many as possible.
   --  @param Ret_Count The count of entries, even if num > Entities'Length.
   --  @param Success   True in success, False in failure.
   procedure Read_Symbolic_Link
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Path      : out String;
       Ret_Count : out Natural)
      with Pre => Key /= Error_Handle;

   --  Read from a regular file.
   --  @param Key       FS Handle to open.
   --  @param Ino       Inode to operate on.
   --  @param Offset    Offset to read from.
   --  @param Data      Place to write read data.
   --  @param Ret_Count How many items were read into Data until EOF.
   --  @param Success   True on success, False on failure.
   procedure Read
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
      with Pre => Key /= Error_Handle;

   --  Write to a regular file.
   --  @param Key       FS Handle to open.
   --  @param Ino       Inode to operate on.
   --  @param Offset    Offset to write to.
   --  @param Data      Data to write
   --  @param Ret_Count How many items were written until EOF.
   --  @param Success   True on success, False on failure.
   procedure Write
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
      with Pre => Key /= Error_Handle;

   --  Get the stat of a file.
   --  @param Key FS Handle to open.
   --  @param Ino Inode to operate on.
   --  @param S   Data to fetch.
   --  @return True on success, False on failure.
   function Stat
      (Key : FS_Handle;
       Ino : File_Inode_Number;
       S   : out File_Stat) return Boolean
      with Pre => Key /= Error_Handle;

   --  Truncate a file to size 0.
   --  @param Key      FS Handle to open.
   --  @param Ino      Inode to operate on.
   --  @param New_Size New size for the file to adopt.
   --  @return True on success, False on failure.
   function Truncate
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       New_Size : Unsigned_64) return Boolean;

   --  Do an FS-specific ioctl on the inode.
   function IO_Control
      (Key     : FS_Handle;
       Ino     : File_Inode_Number;
       Request : Unsigned_64;
       Arg     : System.Address) return Boolean
      with Pre => Key /= Error_Handle;
   ----------------------------------------------------------------------------
   --  Check whether a path is absolute.
   --  @param Path to check.
   --  @return True if absolute, False if not.
   function Is_Absolute (Path : String) return Boolean;

   --  Check whether a path is canonical, that is, whether the path is the
   --  shortest form it could be, symlinks are not checked.
   --  @param Path Path to check.
   --  @return True if canonical, False if not.
   function Is_Canonical (Path : String) return Boolean;

   --  Compound 2 components of a path, a base, and an extension.
   --  If the extension is absolute, the base will not be used.
   --  @param Base      First component to use.
   --  @param Extension Second component to use.
   --  @param Result    Where to write as much of the path as possible.
   --  @param Count     Length of the made path, if it fits, or 0 in failure.
   procedure Compound_Path
      (Base      : String;
       Extension : String;
       Result    : out String;
       Count     : out Natural);

private

   type FS_Handle is new Natural range 0 .. 5;
   Error_Handle : constant FS_Handle := 0;
end VFS;
