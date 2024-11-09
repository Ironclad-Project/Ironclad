--  userland-syscall.ads: Syscall implementation.
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
with System.Storage_Elements; use System.Storage_Elements;
with Arch.Context;
with Arch.MMU;
with IPC.PTY;
with System;
with Scheduler; use Scheduler;
with Userland.MAC;
with Memory;
with Networking;
with Userland.Process; use Userland.Process;
with VFS;              use VFS;
with IPC.Socket;       use IPC.Socket;
with IPC.FIFO;         use IPC.FIFO;
use IPC.PTY;

package Userland.Syscall is
   --  Error conditions for syscalls.
   --  The representation values are arbitrary.
   type Errno_Value is
      (Error_No_Error,        --  No error
       Error_Not_Big_Enough,  --  ERANGE
       Error_Bad_Access,      --  EACCES
       Error_Would_Block,     --  EAGAIN
       Error_Busy,            --  EBUSY
       Error_Child,           --  ECHILD
       Error_Would_Fault,     --  EFAULT
       Error_File_Too_Big,    --  EFBIG
       Error_Invalid_ID,      --  EIDRM
       Error_Invalid_Value,   --  EINVAL
       Error_IO,              --  EIO
       Error_Is_Directory,    --  EISDIR
       Error_File_Loop,       --  ELOOP
       Error_Too_Many_Files,  --  EMFILE
       Error_String_Too_Long, --  ENAMETOOLONG
       Error_No_Entity,       --  ENOENT
       Error_No_Memory,       --  ENOMEM
       Error_Not_Implemented, --  ENOSYS
       Error_Not_Connected,   --  ENOTCONN
       Error_Not_A_TTY,       --  ENOTTY
       Error_Not_Supported,   --  ENOTSUPP
       Error_Bad_Permissions, --  EPERM
       Error_Read_Only_FS,    --  EROFS
       Error_Invalid_Seek,    --  ESPIPE
       Error_Bad_Search,      --  ESRCH
       Error_Bad_File);       --  EBADFD
   for Errno_Value use
      (Error_No_Error        => 0,
       Error_Not_Big_Enough  => 3,
       Error_Bad_Access      => 1002,
       Error_Would_Block     => 1006,
       Error_Busy            => 1010,
       Error_Child           => 1012,
       Error_Would_Fault     => 1020,
       Error_File_Too_Big    => 1021,
       Error_Invalid_ID      => 1023,
       Error_Invalid_Value   => 1026,
       Error_IO              => 1027,
       Error_Is_Directory    => 1029,
       Error_File_Loop       => 1030,
       Error_Too_Many_Files  => 1031,
       Error_String_Too_Long => 1036,
       Error_No_Entity       => 1043,
       Error_No_Memory       => 1047,
       Error_Not_Implemented => 1051,
       Error_Not_Connected   => 1052,
       Error_Not_A_TTY       => 1058,
       Error_Not_Supported   => 1060,
       Error_Bad_Permissions => 1063,
       Error_Read_Only_FS    => 1068,
       Error_Invalid_Seek    => 1069,
       Error_Bad_Search      => 1070,
       Error_Bad_File        => 1081);

   --  AT_ directives for path-relative syscalls and common flags.
   AT_FDCWD            : constant := Natural'Last;
   AT_EMPTY_PATH       : constant := 2#01#;
   AT_SYMLINK_NOFOLLOW : constant := 2#10#;
   AT_EACCESS          : constant := 512;

   --  Exit the callee thread, flushing open files.
   procedure Sys_Exit
      (Code     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Set arch-specific thread state.
   procedure Arch_PRCtl
      (Code     : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Open a file.
   O_RDONLY   : constant := 2#000001#;
   O_WRONLY   : constant := 2#000010#;
   O_APPEND   : constant := 2#000100#;
   O_CLOEXEC  : constant := 2#001000#;
   O_NOFOLLOW : constant := 2#010000#;
   O_NONBLOCK : constant := 2#100000#;
   procedure Open
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Close a file.
   procedure Close
      (File_D   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Read from a file.
   procedure Read
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Write to a file.
   procedure Write
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Operate with the file offset.
   SEEK_SET     : constant := 1;
   SEEK_CURRENT : constant := 2;
   SEEK_END     : constant := 4;
   procedure Seek
      (File_D   : Unsigned_64;
       Offset   : Unsigned_64;
       Whence   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Mmap, the one and only.
   PROT_NONE   : constant := 2#000#;
   PROT_READ   : constant := 2#001#;
   PROT_WRITE  : constant := 2#010#;
   PROT_EXEC   : constant := 2#100#;
   MAP_PRIVATE   : constant := 2#00001#;
   MAP_SHARED    : constant := 2#00010#;
   MAP_FIXED     : constant := 2#00100#;
   MAP_ANON      : constant := 2#01000#;
   MAP_NORESERVE : constant := 2#10000#;
   procedure Mmap
      (Hint       : Unsigned_64;
       Length     : Unsigned_64;
       Protection : Unsigned_64;
       Flags      : Unsigned_64;
       File_D     : Unsigned_64;
       Offset     : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value);

   --  Mmap^-1
   procedure Munmap
      (Address  : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Get the callee PID.
   procedure Get_PID (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Get the PID of the parent of the callee.
   procedure Get_PPID (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Execute.
   procedure Exec
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Create processes and threads with different flavours.
   CLONE_PARENT : constant := 2#01#;
   CLONE_THREAD : constant := 2#10#;
   procedure Clone
      (Callback : Unsigned_64;
       Call_Arg : Unsigned_64;
       Stack    : Unsigned_64;
       Flags    : Unsigned_64;
       TLS_Addr : Unsigned_64;
       Cluster  : Unsigned_64;
       GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Wait.
   WNOHANG     : constant := 2#0000000010#;
   WIFEXITED   : constant := 2#1000000000#;
   WIFSIGNALED : constant := 16#400#;
   procedure Wait
      (Waited_PID : Unsigned_64;
       Exit_Addr  : Unsigned_64;
       Options    : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value);

   --  Create a socket.
   AF_INET        : constant := 1;
   AF_INET6       : constant := 2;
   AF_UNIX        : constant := 3;
   SOCK_DGRAM     : constant := 2#000000000000000001#;
   SOCK_RAW       : constant := 2#000000000000000010#;
   SOCK_STREAM    : constant := 2#000000000000000100#;
   SOCK_SEQPACKET : constant := 2#000000000000001000#;
   SOCK_NONBLOCK  : constant := 2#001000000000000000#;
   SOCK_CLOEXEC   : constant := 2#010000000000000000#;
   SOCK_CLOFORK   : constant := 2#100000000000000000#;
   procedure Socket
      (Domain   : Unsigned_64;
       DataType : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Set hostname.
   procedure Set_Hostname
      (Address  : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Stat.
   type Time_Spec is record
      Seconds     : Unsigned_64;
      Nanoseconds : Unsigned_64;
   end record with Pack, Size => 128;
   Stat_IFIFO : constant := 16#1000#;
   Stat_IFCHR : constant := 16#2000#;
   Stat_IFDIR : constant := 16#4000#;
   Stat_IFBLK : constant := 16#6000#;
   Stat_IFREG : constant := 16#8000#;
   Stat_IFLNK : constant := 16#A000#;
   Stat_ISOCK : constant := 16#C000#;
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
   procedure FStat
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Stat_Addr : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Set current working directory.
   procedure Chdir
      (FD       : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  IO control.
   procedure IOCTL
      (FD       : Unsigned_64;
       Request  : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Yield.
   procedure Sched_Yield (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Delete a thread cluster.
   procedure Delete_Thread_Cluster
      (Cluster  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Create a pair of pipes.
   procedure Pipe
      (Result_Addr : Unsigned_64;
       Flags       : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Get the current UID.
   procedure Get_UID (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Rename files.
   RENAME_NOREPLACE : constant := 2#1#;
   procedure Rename
      (Source_FD   : Unsigned_64;
       Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Target_FD   : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       Flags       : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Fetch some system information.
   SC_PAGESIZE         : constant := 1;
   SC_OPEN_MAX         : constant := 2;
   SC_HOST_NAME_MAX    : constant := 3;
   SC_AVPHYS_PAGES     : constant := 4;
   SC_PHYS_PAGES       : constant := 5;
   SC_NPROCESSORS_ONLN : constant := 6;
   SC_TOTAL_PAGES      : constant := 7;
   SC_LIST_PROCS       : constant := 8;
   SC_LIST_MOUNTS      : constant := 9;
   SC_UNAME            : constant := 10;
   SC_CHILD_MAX        : constant := 11;
   SC_LIST_THREADS     : constant := 12;
   SC_LIST_CLUSTERS    : constant := 13;
   SC_LIST_NETINTER    : constant := 14;
   SC_DUMPLOGS         : constant := 15;
   SC_NGROUPS_MAX      : constant := 16;
   SC_SYMLOOP_MAX      : constant := 17;
   SC_LIST_FILELOCKS   : constant := 18;
   SC_LOADAVG          : constant := 19;
   SC_MEMINFO          : constant := 20;
   SC_FAILURE_POLICIES : constant := 21;

   PROC_IS_TRACED : constant := 2#01#;
   PROC_EXITED    : constant := 2#10#;
   type Proc_Info is record
      Identifier  : String (1 .. 20);
      Id_Len      : Unsigned_16;
      Parent_PID  : Unsigned_16;
      Process_PID : Unsigned_16;
      UID         : Unsigned_32;
      Flags       : Unsigned_32;
   end record with Pack;
   type Proc_Info_Arr is array (Natural range <>) of Proc_Info;

   type Mount_Info is record
      FS_Type       : Unsigned_32;
      Flags         : Unsigned_32;
      Source        : String (1 .. 20);
      Source_Len    : Unsigned_32;
      Location      : String (1 .. 20);
      Location_Len  : Unsigned_32;
      Block_Size    : Unsigned_64;
      Fragment_Size : Unsigned_64;
      Size_In_Frags : Unsigned_64;
      Free_Blocks   : Unsigned_64;
      Free_BlocksU  : Unsigned_64;
      Inode_Count   : Unsigned_64;
      Free_Inodes   : Unsigned_64;
      Free_InodesU  : Unsigned_64;
      Max_File_Name : Unsigned_64;
   end record;
   type Mount_Info_Arr is array (Natural range <>) of Mount_Info;

   type UTS_Name is record
      System_Name : String (1 .. 65);
      Node_Name   : String (1 .. 65);
      Release     : String (1 .. 65);
      Version     : String (1 .. 65);
      Machine     : String (1 .. 65);
   end record;

   type Thread_Info is record
      Thread_Id   : Unsigned_16;
      Niceness    : Unsigned_16;
      Cluster_Id  : Unsigned_16;
      Process_PID : Unsigned_16;
   end record with Pack;
   type Thread_Info_Arr is array (Natural range <>) of Thread_Info;

   type Cluster_Info is record
      Cluster_Id : Unsigned_16;
      Cluster_Fl : Unsigned_16;
      Cluster_Q  : Unsigned_16;
   end record with Pack;
   type Cluster_Info_Arr is array (Natural range <>) of Cluster_Info;

   NETINTR_BLOCKED : constant := 1;

   type Interface_Info is record
      Name        : String (1 .. 65);
      Flags       : Unsigned_64;
      MAC         : Networking.MAC_Address;
      IPv4        : Networking.IPv4_Address;
      IPv4_Subnet : Networking.IPv4_Address;
      IPv6        : Networking.IPv6_Address;
      IPv6_Subnet : Networking.IPv6_Address;
   end record with Pack;
   type Interface_Arr is array (Natural range <>) of Interface_Info;

   type Flock_Info is record
      PID    : Unsigned_32;
      Mode   : Unsigned_32;
      Start  : Unsigned_64;
      Length : Unsigned_64;
      FS     : Unsigned_64;
      Ino    : Unsigned_64;
   end record;
   type Flock_Info_Arr is array (Natural range <>) of Flock_Info;

   type Load_Arr is array (1 .. 3) of Unsigned_32;

   type Mem_Info is record
      Phys_Total     : Unsigned_64;
      Phys_Available : Unsigned_64;
      Phys_Free      : Unsigned_64;
      Shared_Usage   : Unsigned_64;
      Kernel_Usage   : Unsigned_64;
      Table_Usage    : Unsigned_64;
      Poison_Usage   : Unsigned_64;
   end record;

   procedure Sysconf
      (Request  : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Spawn a process just like clone+exec would.
   procedure Spawn
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Caps_Addr : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Get the current thread id.
   procedure Get_TID (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Manage an existing thread cluster or create a new one.
   SCHED_RR   : constant := 2#001#;
   SCHED_COOP : constant := 2#010#;
   SCHED_INTR : constant := 2#100#;
   procedure Manage_Thread_Cluster
      (Cluster    : Unsigned_64;
       Flags      : Unsigned_64;
       Quantum    : Unsigned_64;
       Percentage : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value);

   --  Multiplexed operation for files.
   F_DUPFD         : constant := 1;
   F_DUPFD_CLOEXEC : constant := 2;
   F_GETFD         : constant := 3;
   F_SETFD         : constant := 4;
   F_GETFL         : constant := 5;
   F_SETFL         : constant := 6;
   F_GETPIPE_SZ    : constant := 7;
   F_SETPIPE_SZ    : constant := 8;
   F_GETLK         : constant := 9;
   F_SETLK         : constant := 10;
   F_SETLKW        : constant := 11;

   FD_CLOEXEC : constant := 2#01#;
   FD_CLOFORK : constant := 2#10#;

   F_RDLCK : constant := 1;
   F_UNLCK : constant := 2;
   F_WRLCK : constant := 3;

   type Flock_Data is record
      Lock_Type : Unsigned_16;
      Whence    : Unsigned_16;
      Start     : Unsigned_64;
      Length    : Unsigned_64;
      PID       : Unsigned_32;
   end record;

   procedure Fcntl
      (FD       : Unsigned_64;
       Command  : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Exit the callee thread.
   procedure Exit_Thread (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Bypassing /dev/(u)random for getting random data.
   procedure Get_Random
     (Address  : Unsigned_64;
      Length   : Unsigned_64;
      Returned : out Unsigned_64;
      Errno    : out Errno_Value);

   --  Change protection from memory regions.
   procedure MProtect
     (Address    : Unsigned_64;
      Length     : Unsigned_64;
      Protection : Unsigned_64;
      Returned   : out Unsigned_64;
      Errno      : out Errno_Value);

   --  Set MAC capabilities of the caller process.
   MAC_CAP_SCHED     : constant := 2#000000000000001#;
   MAC_CAP_SPAWN     : constant := 2#000000000000010#;
   MAC_CAP_ENTROPY   : constant := 2#000000000000100#;
   MAC_CAP_SYS_MEM   : constant := 2#000000000001000#;
   MAC_CAP_USE_NET   : constant := 2#000000000010000#;
   MAC_CAP_SYS_NET   : constant := 2#000000000100000#;
   MAC_CAP_SYS_MNT   : constant := 2#000000001000000#;
   MAC_CAP_SYS_PWR   : constant := 2#000000010000000#;
   MAC_CAP_PTRACE    : constant := 2#000000100000000#;
   MAC_CAP_SETUID    : constant := 2#000001000000000#;
   MAC_CAP_SYS_MAC   : constant := 2#000010000000000#;
   MAC_CAP_CLOCK     : constant := 2#000100000000000#;
   MAC_CAP_SIGNALALL : constant := 2#001000000000000#;
   MAC_CAP_SETGID    : constant := 2#010000000000000#;
   MAC_CAP_IPC       : constant := 2#100000000000000#;
   procedure Set_MAC_Capabilities
      (Bits     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Get the MAC capabilities of the caller process.
   procedure Get_MAC_Capabilities
      (Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Add a file to MAC.
   MAC_PERM_CONTENTS : constant := 2#000001#;
   MAC_PERM_READ     : constant := 2#000010#;
   MAC_PERM_WRITE    : constant := 2#000100#;
   MAC_PERM_EXEC     : constant := 2#001000#;
   MAC_PERM_APPEND   : constant := 2#010000#;
   MAC_PERM_FLOCK    : constant := 2#100000#;
   procedure Add_MAC_Permissions
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Set the enforcement policy of the MAC.
   MAC_DENY            : constant := 2#001#;
   MAC_DENY_AND_SCREAM : constant := 2#010#;
   MAC_KILL            : constant := 2#100#;
   procedure Set_MAC_Enforcement
      (Action   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Mount a filesystem.
   MNT_EXT : constant := 1;
   MNT_FAT : constant := 2;
   MNT_DEV : constant := 3;
   MS_RDONLY   : constant := 2#001#;
   MS_REMOUNT  : constant := 2#010#;
   MS_RELATIME : constant := 2#100#;
   procedure Mount
      (Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       FSType      : Unsigned_64;
       Flags       : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Unmount a filesystem.
   MNT_FORCE : constant := 1;
   procedure Umount
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Read the contents of a symlink.
   procedure Readlink
      (Dir_FD      : Unsigned_64;
       Path_Addr   : Unsigned_64;
       Path_Len    : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

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
   procedure GetDEnts
      (FD          : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Synchronize devices and kernel caches.
   procedure Sync (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Create a node, be it a file, directory, or others (not links).
   procedure MakeNode
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Dev       : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Unlinks a file.
   procedure Unlink
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Truncates a file to a new size.
   procedure Truncate
      (FD       : Unsigned_64;
       New_Size : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Bind a socket to an address.
   type SockAddr_In is record
      Sin_Family : Unsigned_32;
      Sin_Port   : Unsigned_16;
      Padding    : Unsigned_16;
      Sin_Addr   : Networking.IPv4_Address;
   end record with Pack;
   type SockAddr_In6 is record
      Sin6_Family   : Unsigned_32;
      Sin6_Port     : Unsigned_16;
      Padding       : Unsigned_16;
      Sin6_FlowInfo : Unsigned_32;
      Sin6_Addr     : Networking.IPv6_Address;
      Sin6_Scope_ID : Unsigned_32;
   end record with Pack;
   type SockAddr_UNIX is record
      Sun_Family : Unsigned_32;
      Length     : Unsigned_32;
      Path       : System.Address;
   end record with Pack;
   procedure Bind
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Create a symbolic link.
   procedure Symlink
      (Dir_FD      : Unsigned_64;
       Path_Addr   : Unsigned_64;
       Path_Len    : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       Mode        : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Connect a socket to an address.
   procedure Connect
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Create a pair of ptys.
   procedure Open_PTY
      (Result_Addr : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Synchronize the data of a file, instead of system-wide.
   procedure FSync
      (FD       : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Make a hard link.
   procedure Link
      (Source_Dir  : Unsigned_64;
       Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Desto_Dir   : Unsigned_64;
       Desto_Addr  : Unsigned_64;
       Desto_Len   : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Trace process, with several debug utilities.
   PTRACE_SYSCALL_PIPE : constant := 1;
   procedure PTrace
      (Request     : Unsigned_64;
       Traced_PID  : Unsigned_64;
       Traced_Addr : Unsigned_64;
       Result_Addr : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value);

   --  Turn a socket into a passive listener for connections.
   procedure Listen
      (Sock_FD  : Unsigned_64;
       Backlog  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Accept a connection.
   procedure Sys_Accept
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Get a resource limit.
   RLIMIT_CORE   : constant := 1;
   RLIMIT_CPU    : constant := 2;
   RLIMIT_FSIZE  : constant := 4;
   RLIMIT_NOFILE : constant := 5;
   RLIMIT_STACK  : constant := 6;
   RLIMIT_AS     : constant := 7;
   procedure Get_RLimit
      (Limit    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Set a resource limit.
   procedure Set_RLimit
      (Limit    : Unsigned_64;
       Data     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Check access to files.
   F_OK : constant := 2#0001#;
   R_OK : constant := 2#0010#;
   W_OK : constant := 2#0100#;
   X_OK : constant := 2#1000#;
   procedure FAccess
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Poll on a series of events.
   POLLIN   : constant := 2#00000001#;
   POLLOUT  : constant := 2#00000010#;
   POLLPRI  : constant := 2#00000100#;
   POLLHUP  : constant := 2#00001000#;
   POLLERR  : constant := 2#00010000#;
   POLLNVAL : constant := 2#01000000#;
   type Poll_FD is record
      FD         : Unsigned_32;
      Events     : Unsigned_16;
      Out_Events : Unsigned_16;
   end record with Size => 64;
   type Poll_FDs is array (Unsigned_64 range <>) of Poll_FD;
   procedure PPoll
      (FDs_Addr  : Unsigned_64;
       FDs_Count : Unsigned_64;
       Timeout   : Unsigned_64;
       Sigmask   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Get the current effective UID.
   procedure Get_EUID (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Set the global and effective UIDs.
   procedure Set_UIDs
      (UID      : Unsigned_64;
       EUID     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Change the mode of the passed file.
   procedure Fchmod
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Change the umask of the calling process.
   procedure Umask
      (Mode     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   RB_HALT      : constant := 1;
   RB_POWEROFF  : constant := 2;
   RB_RESTART   : constant := 3;
   RB_ERROR_RET : constant := 2#1#;
   procedure Reboot
      (Command  : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Change the mode of the passed file.
   procedure Fchown
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       User      : Unsigned_64;
       Group     : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Read from the passed offset.
   procedure PRead
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Offset   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Write from the passed offset.
   procedure PWrite
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Offset   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure Get_Sock_Name
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   procedure Get_Peer_Name
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   SHUT_RD   : constant := 2#01#;
   SHUT_RDWR : constant := 2#10#;
   SHUT_WR   : constant := 2#11#;
   procedure Shutdown
      (Sock_FD  : Unsigned_64;
       How      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Fast mutexes (yes, really!).
   FUTEX_WAIT : constant := 1;
   FUTEX_WAKE : constant := 2;

   type Futex_Item is record
      Address  : Unsigned_64;
      Expected : Unsigned_32;
      Flags    : Unsigned_32;
   end record;
   type Futex_Item_Arr is array (Natural range <>) of Futex_Item;
   procedure Futex
      (Operation : Unsigned_64;
       Address   : Unsigned_64;
       Count     : Unsigned_64;
       Timeout   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Clock operations.
   CLOCK_REALTIME  : constant := 0;
   CLOCK_MONOTONIC : constant := 1;
   CLOCK_GETRES    : constant := 0;
   CLOCK_GETTIME   : constant := 1;
   CLOCK_SETTIME   : constant := 2;
   procedure Clock
      (Operation : Unsigned_64;
       Clock_ID  : Unsigned_64;
       Address   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   --  Sleepy sleep.
   TIMER_ABSTIME : constant := 1;
   procedure Clock_Nanosleep
      (Clock_ID     : Unsigned_64;
       Flags        : Unsigned_64;
       Request_Addr : Unsigned_64;
       Remain_Addr  : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value);

   --   Get usage statistics for the callee process.
   type RUsage is record
      User_Time   : Time_Spec;
      System_Time : Time_Spec;
   end record;

   RUSAGE_SELF     : constant := 1;
   RUSAGE_CHILDREN : constant := 2;
   procedure Get_RUsage
      (Who        : Unsigned_64;
       Usage_Addr : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value);

   procedure RecvFrom
      (Sock_FD   : Unsigned_64;
       Buffer    : Unsigned_64;
       Count     : Unsigned_64;
       Flags     : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   procedure SendTo
      (Sock_FD   : Unsigned_64;
       Buffer    : Unsigned_64;
       Count     : Unsigned_64;
       Flags     : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   NETINTER_SET_BLOCK      : constant := 1;
   NETINTER_SET_STATIC_IP4 : constant := 2;
   NETINTER_SET_STATIC_IP6 : constant := 3;
   type Addr4_NetInterface is record
      IP  : Networking.IPv4_Address;
      Sub : Networking.IPv4_Address;
   end record with Pack;
   type Addr6_NetInterface is record
      IP  : Networking.IPv6_Address;
      Sub : Networking.IPv6_Address;
   end record with Pack;
   procedure Config_NetInterface
      (InterDev  : Unsigned_64;
       Operation : Unsigned_64;
       Arg_Addr  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   procedure UTimes
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Time_Addr : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value);

   procedure Create_TCluster
      (Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure Switch_TCluster
      (Cluster  : Unsigned_64;
       Thread   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;
   procedure Sigprocmask
      (How      : Unsigned_64;
       Set_Addr : Unsigned_64;
       Old_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   SIG_ERR : constant := Integer_Address'Last;
   SIG_IGN : constant := Integer_Address'Last - 1;
   SIG_DFL : constant := Integer_Address'Last - 2;
   type Sigaction_Info is record
      Handler : System.Address; --  void handler(int, siginfo_t *, void *);
      Mask    : Unsigned_64;
      Flags   : Unsigned_32;
   end record;
   procedure Sigaction
      (Signal   : Unsigned_64;
       Act_Addr : Unsigned_64;
       Old_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure Send_Signal
      (Target   : Unsigned_64;
       Signal   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   PRIO_PROCESS : constant := 1;
   PRIO_PGRP    : constant := 2;
   PRIO_USER    : constant := 3;
   PRIO_THREAD  : constant := 4;
   procedure Get_Prio
      (Which    : Unsigned_64;
       Who      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure Set_Prio
      (Which    : Unsigned_64;
       Who      : Unsigned_64;
       Nice     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure Get_GID (Returned : out Unsigned_64; Errno : out Errno_Value);

   procedure Get_EGID (Returned : out Unsigned_64; Errno : out Errno_Value);

   --  Set the global and effective GIDs.
   procedure Set_GIDs
      (GID      : Unsigned_64;
       EGID     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Get complementary group list.
   procedure Get_Groups
      (Count    : Unsigned_64;
       Addr     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Set complementary group list.
   procedure Set_Groups
      (Count    : Unsigned_64;
       Addr     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Get the name of a tty based on file descriptor.
   procedure TTY_Name
      (FD       : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Advise the future use of a file.
   POSIX_FADV_NORMAL     : constant := 1;
   POSIX_FADV_SEQUENTIAL : constant := 2;
   POSIX_FADV_NOREUSE    : constant := 3;
   POSIX_FADV_DONTNEED   : constant := 4;
   POSIX_FADV_WILLNEED   : constant := 5;
   POSIX_FADV_RANDOM     : constant := 6;
   procedure FAdvise
      (FD       : Unsigned_64;
       Offset   : Unsigned_64;
       Length   : Unsigned_64;
       Advice   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   SHM_RDONLY : constant := 8#10000#;
   SHM_RND    : constant := 8#20000#;
   procedure SHMAt
      (ID       : Unsigned_64;
       Addr     : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   type IPC_Perms is record
      IPC_Perm_Key : Unsigned_32;
      UID          : Unsigned_32;
      GID          : Unsigned_32;
      CUID         : Unsigned_32;
      CGID         : Unsigned_32;
      Mode         : Unsigned_32;
      IPC_Perm_Seq : Unsigned_32;
   end record;

   type SHMID_DS is record
      SHM_Perm   : IPC_Perms;
      SHM_SegSz  : Unsigned_64;
      SHM_ATime  : Unsigned_64;
      SHM_DTime  : Unsigned_64;
      SHM_CTime  : Unsigned_64;
      SHM_CPID   : Unsigned_32;
      SHM_LPID   : Unsigned_32;
      SHM_NAttch : Unsigned_64;
   end record;

   IPC_RMID : constant := 0;
   IPC_SET  : constant := 1;
   IPC_STAT : constant := 2;
   procedure SHMCtl
      (ID       : Unsigned_64;
       CMD      : Unsigned_64;
       Buffer   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure SHMDt
      (Address  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   IPC_PRIVATE : constant := 0;
   IPC_CREAT   : constant := 8#1000#;
   IPC_EXCL    : constant := 8#2000#;
   IPC_NOWAIT  : constant := 8#4000#;
   procedure SHMGet
      (Key      : Unsigned_64;
       Size     : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   SOL_SOCKET : constant := 1;

   SO_ACCEPTCONN : constant := 1;
   SO_ERROR      : constant := 5;
   SO_SNDBUF     : constant := 13;
   SO_TYPE       : constant := 16;

   procedure GetSockOpt
      (Sock     : Unsigned_64;
       Level    : Unsigned_64;
       Opt      : Unsigned_64;
       Addr     : Unsigned_64;
       Len      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure SetSockOpt
      (Sock     : Unsigned_64;
       Level    : Unsigned_64;
       Opt      : Unsigned_64;
       Addr     : Unsigned_64;
       Len      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure Get_Thread_Name
      (TID      : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   procedure Set_Thread_Name
      (TID      : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   MEMORY_FAIL_PANIC     : constant := 1;
   MEMORY_FAIL_SOFT_KILL : constant := 2;
   MEMORY_FAIL_HARD_KILL : constant := 3;
   OOM_ALLOW_PROC_KILL   : constant := 1;
   type Failure_Struct is record
      Memory_Failure : Unsigned_64;
      OOM_Failure    : Unsigned_64;
   end record;

   procedure Failure_Policy
      (Old_Addr : Unsigned_64;
       New_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);
   ----------------------------------------------------------------------------
   --  Exit the current process in a POSIX standard-compliant way with the
   --  provided code.
   procedure Do_Exit (Proc : PID; Code : Unsigned_8);
   procedure Do_Exit (Proc : PID; Sig : Signal);

   --  Pre and post syscall hook.
   procedure Pre_Syscall_Hook (State : Arch.Context.GP_Context);
   procedure Post_Syscall_Hook (State : Arch.Context.GP_Context);

   --  Check whether an address may be mapped by the user.
   function Check_Userland_Mappability
      (Map        : Arch.MMU.Page_Table_Acc;
       Addr       : Memory.Virtual_Address;
       Byte_Count : Unsigned_64) return Boolean;

private

   procedure Do_Remote_Exit (Proc : PID; Code : Unsigned_8);

   procedure Common_Syscall_Hook
      (Thread : TID;
       State  : Arch.Context.GP_Context);

   --  Translate an FS_Status to Errno.
   procedure Translate_Status
      (Status         : VFS.FS_Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value);

   procedure Translate_Status
      (Status         : IPC.Socket.Socket_Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value);

   procedure Translate_Status
      (Status         : IPC.FIFO.Pipe_Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value);

   procedure Translate_Status
      (Status         : IPC.PTY.Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value);

   --  Execute a path, with an argv and envp into the passed process.
   procedure Exec_Into_Process
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Proc      : PID;
       Success   : out Boolean;
       Errno     : out Errno_Value);

   --  Go from C string to Ada string.
   function To_String (Addr : System.Address) return String_Acc;

   --  Translate mmap permissions.
   function Get_Mmap_Prot (P : Unsigned_64) return Arch.MMU.Page_Permissions;

   --  Execute the policy chose by the user for the process.
   procedure Execute_MAC_Failure (Name : String; Curr_Proc : PID);

   --  Inner PTY IOCTL.
   procedure PTY_IOCTL
      (P          : IPC.PTY.Inner_Acc;
       Is_Primary : Boolean;
       Request    : Unsigned_64;
       Argument   : System.Address;
       Success    : out Boolean);

   --  Set MAC capabilities for a process from a bitmap.
   procedure Set_MAC_Capabilities (Proc : PID; Bits : Unsigned_64);

   --  Transform a rlimit into a MAC limit.
   procedure MAC_Syscall_To_Kernel
      (Val     : Unsigned_64;
       Success : out Boolean;
       Limit   : out MAC.Limit_Type);

   --  Check limits and then add file.
   procedure Check_Add_File
      (Process : PID;
       File    : File_Description_Acc;
       Success : out Boolean;
       FD      : out Natural;
       Start   : Natural := 0);

   function Check_Userland_Access
      (Map        : Arch.MMU.Page_Table_Acc;
       Addr       : Memory.Virtual_Address;
       Byte_Count : Unsigned_64) return Boolean;

   procedure Resolve_AT_Directive
      (Proc   : PID;
       Dir_FD : Unsigned_64;
       FS     : out VFS.FS_Handle;
       Ino    : out VFS.File_Inode_Number);

   procedure Translate_Signal
      (Val     : Unsigned_64;
       Sig     : out Signal;
       Success : out Boolean);
end Userland.Syscall;
