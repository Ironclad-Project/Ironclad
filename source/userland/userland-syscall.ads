--  userland-syscall.ads: Syscall list and implementation.
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
with Arch.Context;
with Arch.MMU;
with Userland.Process; use Userland.Process;

package Userland.Syscall with SPARK_Mode => Off is
   --  Error conditions for syscalls.
   --  The representation values are arbitrary.
   type Errno_Value is (
      Error_No_Error,        --  No error
      Error_Not_Big_Enough,  --  ERANGE
      Error_Bad_Access,      --  EACCES
      Error_Would_Block,     --  EAGAIN
      Error_Busy,            --  EBUSY
      Error_Child,           --  ECHILD
      Error_Would_Fault,     --  EFAULT
      Error_Invalid_Value,   --  EINVAL
      Error_IO,              --  EIO
      Error_Too_Many_Files,  --  EMFILE
      Error_String_Too_Long, --  ENAMETOOLONG
      Error_No_Entity,       --  ENOENT
      Error_Not_Implemented, --  ENOSYS
      Error_Not_A_TTY,       --  ENOTTY
      Error_Invalid_Seek,    --  ESPIPE
      Error_Bad_Search,      --  ESRCH
      Error_Bad_File         --  EBADFD
   );
   for Errno_Value use (
      Error_No_Error        => 0,
      Error_Not_Big_Enough  => 3,
      Error_Bad_Access      => 1002,
      Error_Would_Block     => 1006,
      Error_Busy            => 1010,
      Error_Child           => 1012,
      Error_Would_Fault     => 1020,
      Error_Invalid_Value   => 1026,
      Error_IO              => 1027,
      Error_Too_Many_Files  => 1031,
      Error_String_Too_Long => 1036,
      Error_No_Entity       => 1043,
      Error_Not_Implemented => 1051,
      Error_Not_A_TTY       => 1058,
      Error_Invalid_Seek    => 1069,
      Error_Bad_Search      => 1070,
      Error_Bad_File        => 1081
   );

   --  AT_ directives for path-relative syscalls.
   AT_FDCWD : constant := Natural'Last;

   --  Enable syscall tracing.
   procedure Set_Tracing (Value : Boolean);

   --  Exit the callee thread, flushing open files.
   procedure Sys_Exit (Code : Unsigned_64; Errno : out Errno_Value);

   --  Set arch-specific thread state.
   function Arch_PRCtl
      (Code     : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

   --  Open a file.
   O_RDONLY   : constant := 2#000001#;
   O_WRONLY   : constant := 2#000010#;
   O_APPEND   : constant := 2#000100#;
   O_CLOEXEC  : constant := 2#001000#;
   O_NOFOLLOW : constant := 2#010000#;
   O_NONBLOCK : constant := 2#100000#;
   function Open
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Close a file.
   function Close
      (File_D : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Read from a file.
   function Read
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Write to a file.
   function Write
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Operate with the file offset.
   SEEK_SET     : constant := 1;
   SEEK_CURRENT : constant := 2;
   SEEK_END     : constant := 4;
   function Seek
      (File_D : Unsigned_64;
       Offset : Unsigned_64;
       Whence : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Mmap, the one and only.
   Protection_Write   : constant := 2#010#;
   Protection_Execute : constant := 2#100#;
   Map_Fixed : constant := 2#0100#;
   Map_Anon  : constant := 2#1000#;
   function Mmap
      (Hint       : Unsigned_64;
       Length     : Unsigned_64;
       Protection : Unsigned_64;
       Flags      : Unsigned_64;
       File_D     : Unsigned_64;
       Offset     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64;

   --  Mmap^-1
   function Munmap
      (Address    : Unsigned_64;
       Length     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64;

   --  Get the callee PID.
   function Get_PID return Unsigned_64;

   --  Get the PID of the parent of the callee.
   function Get_Parent_PID return Unsigned_64;

   --  Execute.
   function Exec
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Create processes and threads with different flavours.
   CLONE_PARENT : constant := 2#01#;
   CLONE_THREAD : constant := 2#10#;
   function Clone
      (Callback  : Unsigned_64;
       Call_Arg  : Unsigned_64;
       Stack     : Unsigned_64;
       Flags     : Unsigned_64;
       TLS_Addr  : Unsigned_64;
       GP_State  : Arch.Context.GP_Context;
       FP_State  : Arch.Context.FP_Context;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Wait.
   Wait_WNOHANG : constant := 2#000010#;
   function Wait
      (Waited_PID : Unsigned_64;
       Exit_Addr  : Unsigned_64;
       Options    : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64;

   --  uname.
   type UTS_Name is record
      System_Name : String (1 .. 65);
      Node_Name   : String (1 .. 65);
      Release     : String (1 .. 65);
      Version     : String (1 .. 65);
      Machine     : String (1 .. 65);
   end record;
   function Uname
      (Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   function Set_Hostname
      (Address : Unsigned_64;
       Length  : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   --  Stat.
   type Time_Spec is record
      Seconds     : Unsigned_64;
      Nanoseconds : Unsigned_64;
   end record;
   Stat_IFIFO : constant := 16#01000#;
   Stat_IFCHR : constant := 16#02000#;
   Stat_IFDIR : constant := 16#04000#;
   Stat_IFBLK : constant := 16#06000#;
   Stat_IFREG : constant := 16#08000#;
   Stat_IFLNK : constant := 16#0A000#;
   type Stat is record
      Device_Number : Unsigned_64;
      Inode_Number  : Unsigned_64;
      Mode          : Unsigned_32;
      Number_Links  : Unsigned_32;
      UID           : Unsigned_32;
      GID           : Unsigned_32;
      Inner_Device  : Unsigned_64;
      File_Size     : Unsigned_64;
      Access_Time   : Time_Spec;
      Modify_Time   : Time_Spec;
      Create_Time   : Time_Spec;
      Block_Size    : Unsigned_64;
      Block_Count   : Unsigned_64;
   end record;
   for Stat use record
      Device_Number at 0 range   0 ..  63;
      Inode_Number  at 0 range  64 .. 127;
      Mode          at 0 range 128 .. 159;
      Number_Links  at 0 range 160 .. 191;
      UID           at 0 range 192 .. 223;
      GID           at 0 range 224 .. 255;
      Inner_Device  at 0 range 256 .. 319;
      File_Size     at 0 range 320 .. 383;
      Access_Time   at 0 range 384 .. 511;
      Modify_Time   at 0 range 512 .. 639;
      Create_Time   at 0 range 640 .. 767;
      Block_Size    at 0 range 768 .. 831;
      Block_Count   at 0 range 832 .. 895;
   end record;
   for Stat'Size use 896;
   AT_EMPTY_PATH       : constant := 2#01#;
   AT_SYMLINK_NOFOLLOW : constant := 2#10#;
   function LStat
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Stat_Addr : Unsigned_64;
       Flags     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Get current working directory.
   function Get_CWD
      (Buffer : Unsigned_64;
       Length : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Get current working directory.
   function Chdir
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  IO control.
   function IOCTL
      (FD       : Unsigned_64;
       Request  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

   --  Yield.
   function Sched_Yield (Errno : out Errno_Value) return Unsigned_64;

   --  Change scheduling deadlines.
   function Set_Deadlines
      (Run_Time, Period : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64;

   --  Create a pair of pipes.
   function Pipe
      (Result_Addr : Unsigned_64;
       Flags       : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64;

   --  Dup functions.
   function Dup
      (Old_FD : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;
   function Dup2
      (Old_FD, New_FD : Unsigned_64;
       Errno          : out Errno_Value) return Unsigned_64;

   --  Fetch some system information.
   SC_PAGESIZE      : constant := 1;
   SC_OPEN_MAX      : constant := 2;
   SC_HOST_NAME_MAX : constant := 3;
   SC_AVPHYS_PAGES  : constant := 4;
   SC_PHYS_PAGES    : constant := 5;
   SC_NPROC_ONLN    : constant := 6;
   function Sysconf
      (Request : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   Access_Exists    : constant := 2#0001#;
   Access_Can_Read  : constant := 2#0010#;
   Access_Can_Write : constant := 2#0100#;
   Access_Can_Exec  : constant := 2#1000#;
   function Sys_Access
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Flags     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Managing scheduling of a thread.
   Thread_MONO : constant := 2#1#;
   function Get_Thread_Sched
      (Errno : out Errno_Value) return Unsigned_64;
   function Set_Thread_Sched
      (Flags : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64;

   --  Multiplexed operation for files.
   FD_CLOEXEC      : constant := 1;
   F_DUPFD         : constant := 1;
   F_DUPFD_CLOEXEC : constant := 2;
   F_GETFD         : constant := 3;
   F_SETFD         : constant := 4;
   F_GETFL         : constant := 5;
   F_SETFL         : constant := 6;
   function Fcntl
      (FD       : Unsigned_64;
       Command  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

   --  Exit the callee thread.
   procedure Exit_Thread (Errno : out Errno_Value);

   --  Bypassing /dev/(u)random for getting random data.
   function Get_Random
     (Address : Unsigned_64;
      Length  : Unsigned_64;
      Errno   : out Errno_Value) return Unsigned_64;

   --  Change protection from memory regions.
   function MProtect
     (Address    : Unsigned_64;
      Length     : Unsigned_64;
      Protection : Unsigned_64;
      Errno      : out Errno_Value) return Unsigned_64;

   --  Set MAC capabilities of the caller process.
   MAC_EXIT_ITSELF   : constant := 2#00000001#;
   MAC_CREATE_OTHERS : constant := 2#00000010#;
   MAC_CHANGE_SCHED  : constant := 2#00000100#;
   MAC_ACC_ENTROPY   : constant := 2#00001000#;
   MAC_ALLOC_MEM     : constant := 2#00010000#;
   MAC_DEALLOC_MEM   : constant := 2#00100000#;
   MAC_MANAGE_NET    : constant := 2#01000000#;
   MAC_MANAGE_MOUNTS : constant := 2#10000000#;
   function Set_MAC_Capabilities
      (Bits  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64;

   --  Lock the MAC from further significant modification.
   function Lock_MAC (Errno : out Errno_Value) return Unsigned_64;

   --  Add a file MAC filter.
   type MAC_Filter is record
      Path   : String (1 .. 75);
      Length : Natural range 0 .. 75;
      Perms  : Unsigned_32;
   end record;
   MAC_FILTER_INC_FILES : constant := 2#0000001#;
   MAC_FILTER_INC_DIRS  : constant := 2#0000010#;
   MAC_FILTER_R         : constant := 2#0000100#;
   MAC_FILTER_W         : constant := 2#0001000#;
   MAC_FILTER_EXEC      : constant := 2#0010000#;
   function Add_MAC_Filter
      (Filter_Addr : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64;

   --  Set the enforcement policy of the MAC.
   MAC_DENY            : constant := 2#001#;
   MAC_DENY_AND_SCREAM : constant := 2#010#;
   MAC_KILL            : constant := 2#100#;
   function Set_MAC_Enforcement
      (Action : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Mount a filesystem.
   MNT_EXT   : constant := 1;
   MNT_FAT32 : constant := 2;
   function Mount
      (Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       FSType      : Unsigned_64;
       MountFlags  : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64;

   --  Unmount a filesystem.
   MNT_FORCE : constant := 1;
   function Umount
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Read the contents of a symlink.
   function Readlink
      (Dir_FD      : Unsigned_64;
       Path_Addr   : Unsigned_64;
       Path_Len    : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64;

   --  Get directory entities.
   DT_FIFO : constant := 1;
   DT_CHR  : constant := 2;
   DT_DIR  : constant := 4;
   DT_BLK  : constant := 6;
   DT_LNK  : constant := 7;
   DT_REG  : constant := 8;
   type Dirent is record
      D_Ino    : Unsigned_64;
      D_Off    : Unsigned_64;
      D_Reclen : Unsigned_16;
      D_Type   : Unsigned_8;
      D_Name   : String (1 .. 61);
   end record with Size => 640;
   for Dirent use record
      D_Ino    at 0 range   0 ..  63;
      D_Off    at 0 range  64 .. 127;
      D_Reclen at 0 range 128 .. 143;
      D_Type   at 0 range 144 .. 151;
      D_Name   at 0 range 152 .. 639;
   end record;
   type Dirents is array (Unsigned_64 range <>) of Dirent with Pack;
   function GetDEnts
      (FD          : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64;

   --  Synchronize devices and kernel caches.
   function Sync (Errno : out Errno_Value) return Unsigned_64;

   --  Create a file.
   CREATE_REG : constant := 1;
   CREATE_DIR : constant := 2;
   CREATE_SYM : constant := 3;
   function Create
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       File_T    : Unsigned_64;
       Mode      : Unsigned_64;
       Extra     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Deletes a file.
   function Delete
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64;

   --  Truncates a file to a new size.
   function Truncate
      (FD       : Unsigned_64;
       New_Size : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

private

   --  Do the actual exiting.
   procedure Do_Exit (Proc : Process_Data_Acc; Code : Unsigned_8);

   --  Handle AT_ directive.
   procedure Compound_AT_Path
      (AT_Directive : Natural;
       Curr_Proc    : Process_Data_Acc;
       Extension    : String;
       Result       : out String;
       Count        : out Natural);

   --  Translate mmap permissions.
   function Get_Mmap_Prot (P : Unsigned_64) return Arch.MMU.Page_Permissions;

   --  Execute the policy chose by the user for the process.
   procedure Execute_MAC_Failure (Name : String; Curr_Proc : Process_Data_Acc);
end Userland.Syscall;
