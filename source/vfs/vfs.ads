--  vfs.ads: FS and register dispatching.
--  Copyright (C) 2024 streaksu
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
with Arch.MMU;
with Memory;

package VFS is
   --  Inodes numbers are identifiers that denotes a unique file inside an FS.
   --  These values are not consistent across filesystems, they may not be
   --  consistent across different mounts, depending on the FS.
   type File_Inode_Number is new Unsigned_64;

   --  Each inode has a mode. They are POSIX standard. uid, gid, and sticky
   --  bits are ignored.
   subtype File_Mode is Unsigned_32 range 8#000# .. 8#777#;

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
      Mode              : File_Mode;
      UID               : Unsigned_32;
      GID               : Unsigned_32;
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
   type Directory_Entities     is array (Natural range <>) of Directory_Entity;
   type Directory_Entities_Acc is access Directory_Entities;

   --  Maximum number of symlink recursions allowed for filesystems.
   --  When practically limited, this will be the limit the kernel will default
   --  to. When not practically limited, this will be an imposed limit in order
   --  to avoid forever loops. Recursing more than this value will be an error.
   Max_Symlink_Loop : constant Natural;
   ----------------------------------------------------------------------------
   --  Handle for interfacing with mounted FSs and FS types.
   type FS_Type   is (FS_DEV, FS_EXT, FS_FAT);
   type FS_Handle is private;
   Error_Handle       : constant FS_Handle;
   Path_Buffer_Length : constant Natural;

   --  Initialize the internal VFS registries.
   procedure Init with Post => Is_Initialized = True;

   --  Mount the passed device name into the passed path, guessing the FS.
   --  @param Device_Name  Name of the device (/dev/<name>).
   --  @param Mount_Path   Absolute path for mounting.
   --  @param Do_Read_Only Force to mount read only.
   --  @param Do_Relatime  Use relative time for access times.v
   --  @param Success      True on success, False on failure.
   procedure Mount
      (Device_Name  : String;
       Mount_Path   : String;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
      with Pre => Device_Name'Length <= Devices.Max_Name_Length and
                  Devices.Is_Initialized                        and
                  Is_Initialized;

   --  Mount the passed device name into the passed path.
   --  @param Device_Name  Name of the device (/dev/<name>).
   --  @param Mount_Path   Absolute path for mounting.
   --  @param FS           FS Type to mount as.
   --  @param Do_Read_Only Force to mount read only.
   --  @param Do_Relatime  Use relative time for access times.
   --  @param Success      True on success, False on failure.
   procedure Mount
      (Device_Name  : String;
       Mount_Path   : String;
       FS           : FS_Type;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
      with Pre => Device_Name'Length <= Devices.Max_Name_Length and
                  Devices.Is_Initialized                        and
                  Is_Initialized;

   --  Unmount a mount, syncing when possible.
   --  @param Path    Path of the mount to unmount.
   --  @param Force   Whether to unmount even if busy.
   --  @param Success True on success, False if busy or non present.
   procedure Unmount (Path : String; Force : Boolean; Success : out Boolean)
      with Pre => Is_Initialized;

   --  Get a best-matching mount for the passed path.
   --  @param Path   Path to search a mount for.
   --  @param Match  Count of characters matched.
   --  @param Handle Key to use to refer to the mount, Error_Handle if error.
   procedure Get_Mount
      (Path   : String;
       Match  : out Natural;
       Handle : out FS_Handle)
      with Pre => Is_Initialized;

   --  Array of handles.
   type Mountpoint_Arr is array (Natural range <>) of FS_Handle;

   --  List all mountpoints on the system.
   --  @param List  Where to write all the mount information.
   --  @param Total Total count of mountpoints, even if it is > List'Length.
   procedure List_All (List : out Mountpoint_Arr; Total : out Natural)
      with Pre => Devices.Is_Initialized and Is_Initialized;

   --  Get the backing FS type.
   --  @param Key Key to use to fetch the info.
   --  @return The FS type, will be a placeholder if the key is not valid.
   function Get_Backing_FS (Key : FS_Handle) return FS_Type
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the backing data of the FS.
   --  @param Key Key to use to fetch the info.
   --  @return The FS data, or System.Null_Address if not a valid key.
   function Get_Backing_FS_Data (Key : FS_Handle) return System.Address
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the backing device of a mount.
   --  @param Key Key to use to fetch the info.
   --  @return The backing device.
   function Get_Backing_Device (Key : FS_Handle) return Device_Handle
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the backing device of a mount.
   --  @param Key Key to use to fetch the info.
   --  @return The backing device.
   procedure Get_Mount_Point
      (Key    : FS_Handle;
       Name   : out String;
       Length : out Natural)
      with Pre => Is_Initialized and Key /= Error_Handle and
                  Name'Length <= Path_Buffer_Length;

   --  Remount the passed path with the desired flags.
   --  @param Key          FS key.
   --  @param Do_Read_Only Force to mount read only.
   --  @param Do_Relatime  Use relative time for access times.
   --  @param Success      True on success, False on failure.
   procedure Remount
      (Key          : FS_Handle;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
      with Pre => Is_Initialized and Key /= Error_Handle;
   ----------------------------------------------------------------------------
   --  Get the block size of the passed mounted FS. The unit symbolizes the
   --  preferred IO size of the FS.
   --  @param Key  FS Key.
   --  @param Size Block size of the FS.
   procedure Get_Block_Size (Key : FS_Handle; Size : out Unsigned_64)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the block size of the passed mounted FS. The unit symbolizes the
   --  minimum unit of allocation of the passed FS.
   --  @param Key  FS Key.
   --  @param Size Fragment size of the FS.
   procedure Get_Fragment_Size (Key : FS_Handle; Size : out Unsigned_64)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the whole size IN FRAGMENTS of the FS.
   --  @param Key  FS Key.
   --  @param Size Size of the FS IN FRAGMENTS.
   procedure Get_Size (Key : FS_Handle; Size : out Unsigned_64)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the total inode count of the FS, allocated and unallocated.
   --  @param Key   FS Key.
   --  @param Count Count.
   procedure Get_Inode_Count (Key : FS_Handle; Count : out Unsigned_64)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the total block count of the FS, allocated and unallocated.
   --  @param Key                FS Key.
   --  @param Free_Blocks        Free blocks for the highest priviledge level.
   --  @param Free_Unpriviledged Free blocks for everyone.
   procedure Get_Free_Blocks
      (Key                : FS_Handle;
       Free_Blocks        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the total inode count of the FS, allocated and unallocated.
   --  @param Key                FS Key.
   --  @param Free_Inodes        Free inodes for the highest priviledge level.
   --  @param Free_Unpriviledged Free inodes for everyone.
   procedure Get_Free_Inodes
      (Key                : FS_Handle;
       Free_Inodes        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Get the total length of a file name in the FS.
   --  @param Key    FS Key.
   --  @param Length Maximum length of a file name.
   procedure Get_Max_Length (Key : FS_Handle; Length : out Unsigned_64)
      with Pre => Is_Initialized and Key /= Error_Handle;
   ----------------------------------------------------------------------------
   --  Status returned from file operations as result.
   type FS_Status is
      (FS_Success,       --  Success, only good value for ease of checking.
       FS_Invalid_Value, --  One of the passed values is no good.
       FS_Is_Directory,  --  The operation does not accept directories!
       FS_Not_Supported, --  The operation is not supported for this FS.
       FS_Not_Allowed,   --  Bad permissions for interacting with the inode.
       FS_RO_Failure,    --  The FS is read-only, but write access is needed.
       FS_IO_Failure,    --  The underlying device errored out.
       FS_Loop);         --  Too many symlinks were encountered resolving path.

   --  Open a file with an absolute path inside the mount.
   --  @param Key        Relative FS Handle to start opening.
   --  @param Relative   Relative directory inode to open from.
   --  @param Path       Path to be accessed inside Relative, or absolute.
   --  @param Final_Key  Final FS Handle we end on.
   --  @param Ino        Found inode, if any.
   --  @param Success    Returned status for the operation.
   --  @param User       UID to check against, 0 for root/bypass checks.
   --  @param Want_Read  True for read permission, False for not.
   --  @param Want_Write True for write permission, False for not.
   --  @param Do_Follow  If true, follow symlinks, hard links are always used.
   procedure Open
      (Key        : FS_Handle;
       Relative   : File_Inode_Number;
       Path       : String;
       Final_Key  : out FS_Handle;
       Ino        : out File_Inode_Number;
       Success    : out FS_Status;
       User       : Unsigned_32;
       Want_Read  : Boolean;
       Want_Write : Boolean;
       Do_Follow  : Boolean := True)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Create an inode inside a mount.
   --  @param Key      FS Handle to open.
   --  @param Relative Relative directory inode to create from.
   --  @param Path     Path to be created inside Relative, or absolute.
   --  @param Typ      Type of file to create.
   --  @param Mode     Mode to use for the created file.
   --  @param User     UID to check against, 0 for root/bypass checks.
   --  @param Status   Status for the operation.
   procedure Create_Node
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       Typ      : File_Type;
       Mode     : File_Mode;
       User     : Unsigned_32;
       Status   : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Create a symlink with a target inside a mount.
   --  @param Key      FS Handle to open.
   --  @param Relative Relative directory inode to create from.
   --  @param Path     Path to be created inside Relative, or absolute.
   --  @param Target   Target of the symlink, it is not checked in any way.
   --  @param Mode     Mode to use for the created symlink.
   --  @param User     UID to check against, 0 for root/bypass checks.
   --  @param Status   Status for the operation.
   procedure Create_Symbolic_Link
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       Target   : String;
       Mode     : Unsigned_32;
       User     : Unsigned_32;
       Status   : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Create a hard link inside a mount with a target.
   --  @param Key             FS Handle to open.
   --  @param Relative_Path   Relative directory inode to create from.
   --  @param Path            Path inside the mount, must not exist.
   --  @param Relative_Target Relative directory inode to link to.
   --  @param Target          Target of the link, must exist.
   --  @param User            UID to check against, 0 for root/bypass checks.
   --  @param Status          Status for the operation.
   procedure Create_Hard_Link
      (Key             : FS_Handle;
       Relative_Path   : File_Inode_Number;
       Path            : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       User            : Unsigned_32;
       Status          : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Rename a file to a target, optionally keeping it in the process.
   --  @param Key             FS Handle to open.
   --  @param Relative_Source Relative directory inode to create from.
   --  @param Source          Path to be renamed inside Relative, or absolute.
   --  @param Relative_Target Relative directory inode to rename to.
   --  @param Target          Target of the rename, must not exist.
   --  @param Keep            Keep the source instead of plainly renaming it.
   --  @param User            UID to check against, 0 for root/bypass checks.
   --  @param Status          Status for the operation.
   procedure Rename
      (Key             : FS_Handle;
       Relative_Source : File_Inode_Number;
       Source          : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       Keep            : Boolean;
       User            : Unsigned_32;
       Status          : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Queue a file for deletion inside a mount.
   --  @param Key      FS Handle to open.
   --  @param Relative Relative directory inode to unlink from.
   --  @param Path     Absolute path inside the mount, must exist.
   --  @param User     UID to check against, 0 for root/bypass checks.
   --  @param Status   Status for the operation.
   procedure Unlink
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       User     : Unsigned_32;
       Status   : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Signal to the FS we do not need this inode anymore.
   --  @param Key FS handle to operate on.
   --  @param Ino Inode to signal to close.
   procedure Close (Key : FS_Handle; Ino : File_Inode_Number)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Read the entries of an opened directory.
   --  @param Key       FS handle to operate on.
   --  @param Ino       Inode to operate on.
   --  @param Offset    Offset to start from.
   --  @param Entities  Where to store the read entries, as many as possible.
   --  @param Ret_Count The count of entries, even if num > Entities'Length.
   --  @param Success   Status for the operation.
   procedure Read_Entries
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Read the contents of a symbolic link.
   --  @param Key       FS handle to operate on.
   --  @param Ino       Inode to operate on.
   --  @param Path      Buffer to store the read path.
   --  @param Ret_Count Symlink character count, even if num > Path'Length.
   --  @param Success   Status for the operation.
   procedure Read_Symbolic_Link
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Path      : out String;
       Ret_Count : out Natural;
       Success   : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Read from a regular file.
   --  @param Key         FS Handle to open.
   --  @param Ino         Inode to operate on.
   --  @param Offset      Offset to read from.
   --  @param Data        Place to write read data.
   --  @param Ret_Count   How many items were read into Data until EOF.
   --  @param Is_Blocking True if the call is to be blocking.
   --  @param Success     Status for the operation.
   procedure Read
      (Key         : FS_Handle;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Write to a regular file.
   --  @param Key         FS Handle to open.
   --  @param Ino         Inode to operate on.
   --  @param Offset      Offset to write to.
   --  @param Data        Data to write
   --  @param Ret_Count   How many items were written until EOF.
   --  @param Is_Blocking True if the call is to be blocking.
   --  @param Success     Status for the operation.
   procedure Write
      (Key         : FS_Handle;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

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
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Truncate a file to size 0.
   --  @param Key      FS Handle to open.
   --  @param Ino      Inode to operate on.
   --  @param New_Size New size for the file to adopt.
   --  @param Status   Status for the operation.
   procedure Truncate
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       New_Size : Unsigned_64;
       Status   : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Do an FS-specific ioctl on the inode.
   --  @param Key     FS Handle to open.
   --  @param Ino     Inode to operate on.
   --  @param Request FS-Specific request to issue.
   --  @param Arg     Address of an optional argument for the FS.
   --  @param Status  Status for the operation.
   procedure IO_Control
      (Key     : FS_Handle;
       Ino     : File_Inode_Number;
       Request : Unsigned_64;
       Arg     : System.Address;
       Status  : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Do an FS-specific mmap operation on a file.
   --  @param Key     FS Handle to open.
   --  @param Ino     Inode to operate on.
   --  @param Map     Map to map to.
   --  @param Offset  Offset inside the fall to map to.
   --  @param Address Address to map to.
   --  @param Length  Length of the map.
   --  @param Flags   Flags to use for the mapping.
   --  @param Status  Status for the operation.
   procedure Mmap
      (Key     : FS_Handle;
       Ino     : File_Inode_Number;
       Map     : Arch.MMU.Page_Table_Acc;
       Offset  : Unsigned_64;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions;
       Status  : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Do an FS-specific poll operation on a file.
   --  @param Key       FS Handle to open.
   --  @param Ino       Inode to operate on.
   --  @param Can_Read  True if the file is ready for reading.
   --  @param Can_Write True if the file is ready for writing.
   --  @param Is_Error  True if the file is errored out.
   procedure Poll
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Synchronize the whole FS driver-specific caches and used device.
   --  @param Key FS Handle to open.
   --  @return Status for the operation.
   function Synchronize (Key : FS_Handle) return FS_Status
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Synchronize the contents of a file cached by the FS driver.
   --  @param Key       FS Handle to open.
   --  @param Ino       Inode to operate on.
   --  @param Data_Only Flush only the data if possible, and not the metadata.
   --  @return Status for the operation.
   function Synchronize
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Change the mode of an inode.
   --  @param Key    FS Handle to use.
   --  @param Ino    Inode to change the mode of.
   --  @param Mode   Mode to change the inode to.
   --  @param Status Status of the operation.
   procedure Change_Mode
      (Key    : FS_Handle;
       Ino    : File_Inode_Number;
       Mode   : File_Mode;
       Status : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Change the owner of an inode.
   --  @param Key    FS Handle to use.
   --  @param Ino    Inode to change the mode of.
   --  @param Owner  Owner to change ownership to.
   --  @param Group  Group to change ownership to.
   --  @param Status Status of the operation.
   procedure Change_Owner
      (Key    : FS_Handle;
       Ino    : File_Inode_Number;
       Owner  : Unsigned_32;
       Group  : Unsigned_32;
       Status : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Check access of for an inode.
   --  @param Key         FS Handle to use.
   --  @param Ino         Inode to change the mode of.
   --  @param Exists_Only If True, only check for existance.
   --  @param Can_Read    If True, check for the ability to read.
   --  @param Can_Write   If True, check for the ability to write.
   --  @param Can_Exec    If True, check for the ability to execute.
   --  @param Real_UID    Real UID to check against.
   --  @param Status      Status of the operation.
   procedure Check_Access
      (Key         : FS_Handle;
       Ino         : File_Inode_Number;
       Exists_Only : Boolean;
       Can_Read    : Boolean;
       Can_Write   : Boolean;
       Can_Exec    : Boolean;
       Real_UID    : Unsigned_32;
       Status      : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;

   --  Change the access times of an inode.
   --  @param Key                FS Handle to use.
   --  @param Ino                Inode to change the mode of.
   --  @param Access_Seconds     Access timestamp epoch seconds.
   --  @param Access_Nanoseconds Access timestamp nanoseconds.
   --  @param Modify_Seconds     Modification timestamp epoch seconds.
   --  @param Modify_Nanoseconds Modification timestamp nanoseconds.
   --  @param Status             Status of the operation.
   procedure Change_Access_Times
      (Key                : FS_Handle;
       Ino                : File_Inode_Number;
       Access_Seconds     : Unsigned_64;
       Access_Nanoseconds : Unsigned_64;
       Modify_Seconds     : Unsigned_64;
       Modify_Nanoseconds : Unsigned_64;
       Status             : out FS_Status)
      with Pre => Is_Initialized and Key /= Error_Handle;
   ----------------------------------------------------------------------------
   --  FS-independent versions of operations, that rely on the driver to search
   --  for the appropiate FS, or operate on several FSes.

   --  Open a file with an absolute path system-wide.
   --  @param Path       Absolute path inside the mount, creation is not done.
   --  @param Key        Guessed FS handle.
   --  @param Ino        Found inode, if any.
   --  @param Success    Returned status for the operation.
   --  @param User       UID to check against, 0 for root/bypass checks.
   --  @param Want_Read  True for read permission, False for not.
   --  @param Want_Write True for write permission, False for not.
   --  @param Do_Follow  True to follow symlinks, False for not following em.
   procedure Open
      (Path       : String;
       Key        : out FS_Handle;
       Ino        : out File_Inode_Number;
       Success    : out FS_Status;
       User       : Unsigned_32;
       Want_Read  : Boolean;
       Want_Write : Boolean;
       Do_Follow  : Boolean := True)
      with Pre  => Is_Initialized,
           Post => (if Success = FS_Success then
                    Key /= Error_Handle else True);

   --  Synchronize all FSs mounted on the system, FSs with no implemented
   --  synchronization routines are ignored.
   --  @param Success True on success. False if any FSs failed for IO reasons.
   procedure Synchronize (Success : out Boolean)
      with Pre => Is_Initialized;

   --  Create several kinds of files.
   --  @param Path    System-wide absolute path.
   --  @param Typ     File type to create.
   --  @param Mode    Mode to set for the created inode.
   --  @param Success Status of the operation.
   --  @param User    UID to check against, 0 for root/bypass checks.
   procedure Create_Node
      (Path    : String;
       Typ     : File_Type;
       Mode    : File_Mode;
       Success : out FS_Status;
       User    : Unsigned_32)
      with Pre => Is_Initialized;
   ----------------------------------------------------------------------------
   --  Check whether a path is absolute.
   --  @param Path to check.
   --  @return True if absolute, False if not.
   function Is_Absolute (Path : String) return Boolean;

   --  Apply a umask to a mode.
   --  @param Mode  Mode to use.
   --  @param Umask Umask to use.
   --  @return The resulting mode.
   function Apply_Umask (Mode, Umask : File_Mode) return File_Mode is
      (File_Mode (Unsigned_32 (Mode) and not Unsigned_32 (Umask)));
   ----------------------------------------------------------------------------
   --  Ghost function for checking whether the vfs handling is initialized.
   function Is_Initialized return Boolean with Ghost;

private

   type FS_Handle is new Natural range 0 .. 5;
   Max_Symlink_Loop   : constant   Natural := 8;
   Error_Handle       : constant FS_Handle := 0;
   Path_Buffer_Length : constant   Natural := 100;

   type Mount_Data is record
      Mounted_Dev : Device_Handle;
      Mounted_FS  : FS_Type;
      FS_Data     : System.Address;
      Path_Length : Natural range 0 .. Path_Buffer_Length;
      Path_Buffer : String (1 .. Path_Buffer_Length);
      Base_Key    : FS_Handle;
      Base_Ino    : File_Inode_Number;
      Root_Ino    : File_Inode_Number;
   end record;

   type Mount_Registry     is array (FS_Handle range 1 .. 5) of Mount_Data;
   type Mount_Registry_Acc is access Mount_Registry;

   Mounts       : Mount_Registry_Acc;
   Mounts_Mutex : aliased Lib.Synchronization.Binary_Semaphore;

   function Is_Initialized return Boolean is (Mounts /= null);

   function Can_Access_File
      (User       : Unsigned_32;
       File_Owner : Unsigned_32;
       Mode       : File_Mode;
       Want_Read  : Boolean;
       Want_Write : Boolean;
       Want_Exec  : Boolean) return Boolean;
end VFS;
