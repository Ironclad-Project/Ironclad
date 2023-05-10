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
with Lib.Synchronization;

package VFS is
   --  Inodes numbers are identifiers that denotes a unique file inside an FS.
   --  These values are not consistent across filesystems, they may not be
   --  consistent across different mounts, depending on the FS.
   type File_Inode_Number is new Unsigned_64;

   --  Stat structure of a file, which describes the qualities of a file.
   type File_Timestamp is record
      Seconds_Since_Epoch    : Unsigned_64;
      Additional_Nanoseconds : Unsigned_64;
   end record;
   type File_Mode is new Natural range 8#000# .. 8#7777#;
   type File_Type is (File_Regular, File_Directory, File_Symbolic_Link,
                      File_Character_Device, File_Block_Device);
   type File_Stat is record
      Unique_Identifier : File_Inode_Number;
      Type_Of_File      : File_Type;
      Mode              : File_Mode;
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
   type FS_Type   is (FS_EXT, FS_FAT, FS_QNX);
   type FS_Handle is private;
   Error_Handle       : constant FS_Handle;
   Path_Buffer_Length : constant Natural;

   --  Initialize the internal VFS registries.
   procedure Init;

   --  Mount the passed device name into the passed path, guessing the FS.
   --  @param Device_Name  Name of the device (/dev/<name>).
   --  @param Mount_Path   Absolute path for mounting.
   --  @param Do_Read_Only Force to mount read only.
   --  @return True on success, False on failure.
   function Mount
      (Device_Name  : String;
       Mount_Path   : String;
       Do_Read_Only : Boolean) return Boolean;

   --  Mount the passed device name into the passed path.
   --  @param Device_Name  Name of the device (/dev/<name>).
   --  @param Mount_Path   Absolute path for mounting.
   --  @param FS           FS Type to mount as.
   --  @param Do_Read_Only Force to mount read only.
   --  @return True on success, False on failure.
   function Mount
      (Device_Name  : String;
       Mount_Path   : String;
       FS           : FS_Type;
       Do_Read_Only : Boolean) return Boolean;

   --  Unmount a mount, syncing when possible.
   --  @param Path  Path of the mount to unmount.
   --  @param Force Whether to unmount even if busy.
   --  @return True on success, False if busy or non present.
   function Unmount (Path : String; Force : Boolean) return Boolean;

   --  Get a best-matching mount for the passed path.
   --  @param Path   Path to search a mount for.
   --  @param Match  Count of characters matched.
   --  @param Handle Key to use to refer to the mount, Error_Handle if error.
   procedure Get_Mount
      (Path   : String;
       Match  : out Natural;
       Handle : out FS_Handle);

   --  Information of a process.
   type Mountpoint_Info is record
      Inner_Type   : FS_Type;
      Source       : String (1 .. 20);
      Source_Len   : Natural;
      Location     : String (1 .. 20);
      Location_Len : Natural;
   end record;
   type Mountpoint_Info_Arr is array (Natural range <>) of Mountpoint_Info;

   --  List all mountpoints on the system.
   --  @param List  Where to write all the mount information.
   --  @param Total Total count of mountpoints, even if it is > List'Length.
   procedure List_All (List : out Mountpoint_Info_Arr; Total : out Natural);

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
   ----------------------------------------------------------------------------
   --  Status returned from file operations as result.
   type FS_Status is
      (FS_Success,       --  Success, only good value for ease of checking.
       FS_Invalid_Value, --  One of the passed values is no good.
       FS_Not_Supported, --  The operation is not supported for this FS.
       FS_RO_Failure,    --  The FS is read-only, but write access is needed.
       FS_IO_Failure);   --  The underlying device errored out.

   --  Open a file with an absolute path inside the mount.
   --  @param Key     FS Handle to open.
   --  @param Path    Absolute path inside the mount, creation is not done.
   --  @param Ino     Found inode, if any.
   --  @param Success Returned status for the operation.
   --  @return Returned opaque pointer for the passed mount, Null in failure.
   procedure Open
      (Key     : FS_Handle;
       Path    : String;
       Ino     : out File_Inode_Number;
       Success : out FS_Status)
      with Pre => Key /= Error_Handle;

   --  Create a file with an absolute path inside the mount.
   --  @param Key  FS Handle to open.
   --  @param Path Absolute path inside the mount, must not exist.
   --  @param Typ  Type of file to create.
   --  @param Mode Mode to use for the created file.
   --  @return Status for the operation.
   function Create_Node
      (Key  : FS_Handle;
       Path : String;
       Typ  : File_Type;
       Mode : File_Mode) return FS_Status
      with Pre => Key /= Error_Handle;

   --  Create a symlink with an absolute path inside the mount and a target.
   --  @param Key    FS Handle to open.
   --  @param Path   Absolute path inside the mount, must not exist.
   --  @param Target Target of the symlink, it is not checked in any way.
   --  @param Mode   Mode to use for the created symlink.
   --  @return Status for the operation.
   function Create_Symbolic_Link
      (Key          : FS_Handle;
       Path, Target : String;
       Mode         : Unsigned_32) return FS_Status
      with Pre => Key /= Error_Handle;

   --  Create a hard link with an absolute path inside the mount and a target.
   --  @param Key    FS Handle to open.
   --  @param Path   Absolute path inside the mount, must not exist.
   --  @param Target Target of the symlink, it is not checked in any way.
   --  @return Status for the operation.
   function Create_Hard_Link
      (Key          : FS_Handle;
       Path, Target : String) return FS_Status
      with Pre => Key /= Error_Handle;

   --  Rename two files.
   --  @param Key    FS Handle to open.
   --  @param Source Absolute source path inside the mount, must not exist.
   --  @param Target Target of the mode, if it exists, it will be replaced.
   --  @param Keep   Keep the source instead of plainly renaming it.
   --  @return Status for the operation.
   function Rename
      (Key    : FS_Handle;
       Source : String;
       Target : String;
       Keep   : Boolean) return FS_Status
      with Pre => Key /= Error_Handle;

   --  Queue a file for deletion inside a mount.
   --  @param Key  FS Handle to open.
   --  @param Path Absolute path inside the mount, must exist.
   --  @return Status for the operation.
   function Unlink (Key : FS_Handle; Path : String) return FS_Status;

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
   --  @param Success   Status for the operation..
   procedure Read_Entries
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
      with Pre => Key /= Error_Handle;

   --  Read the entries of an opened directory.
   --  @param Key       FS handle to operate on.
   --  @param Ino       Inode to operate on.
   --  @param Entities  Where to store the read entries, as many as possible.
   --  @param Ret_Count The count of entries, even if num > Entities'Length.
   --  @param Success   Status for the operation.
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
   --  @param Success   Status for the operation.
   procedure Read
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
      with Pre => Key /= Error_Handle;

   --  Write to a regular file.
   --  @param Key       FS Handle to open.
   --  @param Ino       Inode to operate on.
   --  @param Offset    Offset to write to.
   --  @param Data      Data to write
   --  @param Ret_Count How many items were written until EOF.
   --  @param Success   Status for the operation.
   procedure Write
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
      with Pre => Key /= Error_Handle;

   --  Get the stat of a file.
   --  @param Key      FS Handle to open.
   --  @param Ino      Inode to operate on.
   --  @param Stat_Val Data to fetch.
   --  @param Success  Status for the operation.
   procedure Stat
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       Stat_Val : out File_Stat;
       Success  : out FS_Status)
      with Pre => Key /= Error_Handle;

   --  Truncate a file to size 0.
   --  @param Key      FS Handle to open.
   --  @param Ino      Inode to operate on.
   --  @param New_Size New size for the file to adopt.
   --  @return Status for the operation.
   function Truncate
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       New_Size : Unsigned_64) return FS_Status;

   --  Do an FS-specific ioctl on the inode.
   function IO_Control
      (Key     : FS_Handle;
       Ino     : File_Inode_Number;
       Request : Unsigned_64;
       Arg     : System.Address) return FS_Status
      with Pre => Key /= Error_Handle;

   --  Synchronize all FSs mounted on the system, FSs with no implemented
   --  synchronization routines are ignored.
   --  @return True on success. False if any FSs failed for IO reasons.
   function Synchronize return Boolean;

   --  Synchronize a whole FS driver-specific caches.
   --  @param Key FS Handle to open.
   --  @return Status for the operation.
   function Synchronize (Key : FS_Handle) return FS_Status
      with Pre => Key /= Error_Handle;

   --  Synchronize the contents of a file cached by the FS driver.
   --  @param Key FS Handle to open.
   --  @param Ino Inode to operate on.
   --  @return Status for the operation.
   function Synchronize
      (Key : FS_Handle;
       Ino : File_Inode_Number) return FS_Status
      with Pre => Key /= Error_Handle;
   ----------------------------------------------------------------------------
   --  Check whether a path is absolute.
   --  @param Path to check.
   --  @return True if absolute, False if not.
   function Is_Absolute (Path : String) return Boolean;

   --  Check whether a path is canonical, that is, whether the path is the
   --  shortest form it could be, symlinks, ., and .., are not checked.
   --  @param Path Path to check.
   --  @return True if canonical, False if not.
   function Is_Canonical (Path : String) return Boolean;

   --  Compound 2 components of a path, a base, and an extension.
   --  If the extension is absolute, the base will not be used.
   --  The resulting path will be cleaned a bit in order to be made canonical.
   --  @param Base      First component to use.
   --  @param Extension Component to append to extension, if not absolute.
   --  @param Result    Where to write as much of the path as possible.
   --  @param Count     Length of the made path, if it fits, or 0 in failure.
   procedure Compound_Path
      (Base      : String;
       Extension : String;
       Result    : out String;
       Count     : out Natural);

private

   type FS_Handle is new Natural range 0 .. 5;
   Error_Handle       : constant FS_Handle := 0;
   Path_Buffer_Length : constant Natural   := 100;

   type Mount_Data is record
      Mounted_Dev : Device_Handle;
      Mounted_FS  : FS_Type;
      FS_Data     : System.Address;
      Path_Length : Natural range 0 .. Path_Buffer_Length;
      Path_Buffer : String (1 .. Path_Buffer_Length);
   end record;

   type Mount_Registry     is array (FS_Handle range 1 .. 5) of Mount_Data;
   type Mount_Registry_Acc is access Mount_Registry;

   Mounts       : Mount_Registry_Acc;
   Mounts_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
end VFS;
