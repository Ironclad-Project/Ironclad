--  userland-syscall.ads: Syscall implementation.
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
with IPC.PTY;
with System;
with Userland.MAC;
with Memory;
with Userland.Process; use Userland.Process;
with VFS;              use VFS;
with IPC.Socket;       use IPC.Socket;
with IPC.FIFO;         use IPC.FIFO;

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
       Error_Invalid_Value,   --  EINVAL
       Error_IO,              --  EIO
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
       Error_Invalid_Value   => 1026,
       Error_IO              => 1027,
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
   PROT_READ  : constant := 2#00001#;
   PROT_WRITE : constant := 2#00010#;
   PROT_EXEC  : constant := 2#00100#;
   MAP_FIXED  : constant := 2#00100#;
   MAP_ANON   : constant := 2#01000#;
   MAP_WC     : constant := 2#10000#;
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
   Wait_EXITED : constant := 2#1000000000#;
   procedure Wait
      (Waited_PID : Unsigned_64;
       Exit_Addr  : Unsigned_64;
       Options    : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value);

   --  Create a socket.
   AF_UNIX       : constant := 3;
   SOCK_DGRAM    : constant := 2#000000000000000001#;
   SOCK_RAW      : constant := 2#000000000000000010#;
   SOCK_STREAM   : constant := 2#000000000000000100#;
   SOCK_NONBLOCK : constant := 2#010000000000000000#;
   SOCK_CLOEXEC  : constant := 2#100000000000000000#;
   procedure Socket
      (Domain   : Unsigned_64;
       DataType : Unsigned_64;
       Protocol : Unsigned_64;
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
   end record;
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
   SC_PAGESIZE      : constant := 1;
   SC_OPEN_MAX      : constant := 2;
   SC_HOST_NAME_MAX : constant := 3;
   SC_AVPHYS_PAGES  : constant := 4;
   SC_PHYS_PAGES    : constant := 5;
   SC_NPROC_ONLN    : constant := 6;
   SC_TOTAL_PAGES   : constant := 7;
   SC_LIST_PROCS    : constant := 8;
   SC_LIST_MOUNTS   : constant := 9;
   SC_UNAME         : constant := 10;
   SC_CHILD_MAX     : constant := 11;
   SC_LIST_THREADS  : constant := 12;
   SC_LIST_CLUSTERS : constant := 13;

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
      FS_Type      : Unsigned_32;
      Flags        : Unsigned_32;
      Source       : String (1 .. 20);
      Source_Len   : Unsigned_32;
      Location     : String (1 .. 20);
      Location_Len : Unsigned_32;
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
   FD_CLOEXEC      : constant := 1;
   F_DUPFD         : constant := 1;
   F_DUPFD_CLOEXEC : constant := 2;
   F_GETFD         : constant := 3;
   F_SETFD         : constant := 4;
   F_GETFL         : constant := 5;
   F_SETFL         : constant := 6;
   F_GETPIPE_SZ    : constant := 7;
   F_SETPIPE_SZ    : constant := 8;
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
   MAC_CAP_SCHED   : constant := 2#00000000001#;
   MAC_CAP_SPAWN   : constant := 2#00000000010#;
   MAC_CAP_ENTROPY : constant := 2#00000000100#;
   MAC_CAP_SYS_MEM : constant := 2#00000001000#;
   MAC_CAP_USE_NET : constant := 2#00000010000#;
   MAC_CAP_SYS_NET : constant := 2#00000100000#;
   MAC_CAP_SYS_MNT : constant := 2#00001000000#;
   MAC_CAP_SYS_PWR : constant := 2#00010000000#;
   MAC_CAP_PTRACE  : constant := 2#00100000000#;
   MAC_CAP_SETUID  : constant := 2#01000000000#;
   MAC_CAP_SYS_MAC : constant := 2#10000000000#;
   procedure Set_MAC_Capabilities
      (Bits     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Get the MAC capabilities of the caller process.
   procedure Get_MAC_Capabilities
      (Returned : out Unsigned_64;
       Errno    : out Errno_Value);

   --  Add a file to MAC.
   MAC_PERM_CONTENTS : constant := 2#0000001#;
   MAC_PERM_READ     : constant := 2#0000010#;
   MAC_PERM_WRITE    : constant := 2#0000100#;
   MAC_PERM_EXEC     : constant := 2#0001000#;
   MAC_PERM_APPEND   : constant := 2#0010000#;
   MAC_PERM_FLOCK    : constant := 2#0100000#;
   MAC_PERM_DEV      : constant := 2#1000000#;
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
   MNT_QNX : constant := 3;
   MS_RDONLY : constant := 2#01#;
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
   type SockAddr_UNIX (Length : Natural) is record
      Sun_Family : Unsigned_32;
      Path       : String (1 .. Length);
   end record;
   for SockAddr_UNIX use record
      Sun_Family at 0 range 0 .. 31;
   end record;
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
      (Result_Addr  : Unsigned_64;
       Termios_Addr : Unsigned_64;
       Window_Addr  : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value);

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
   RLIMIT_DATA   : constant := 3;
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
   POLLERR  : constant := 2#00001000#;
   POLLHUP  : constant := 2#00010000#;
   POLLNVAL : constant := 2#01000000#;
   type Poll_FD is record
      FD         : Unsigned_32;
      Events     : Unsigned_16;
      Out_Events : Unsigned_16;
   end record with Size => 64;
   type Poll_FDs is array (Unsigned_64 range <>) of Poll_FD;
   procedure Poll
      (FDs_Addr  : Unsigned_64;
       FDs_Count : Unsigned_64;
       Timeout   : Unsigned_64;
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
   ----------------------------------------------------------------------------
   --  Exit the current process in a POSIX standard-compliant way with the
   --  provided code.
   procedure Do_Exit (Proc : PID; Code : Unsigned_8);

   --  Pre and post syscall hook.
   procedure Pre_Syscall_Hook (State : Arch.Context.GP_Context);
   procedure Post_Syscall_Hook (State : Arch.Context.GP_Context);

   --  Check whether an address may be mapped by the user.
   function Check_Userland_Mappability
      (Map        : Arch.MMU.Page_Table_Acc;
       Addr       : Memory.Virtual_Address;
       Byte_Count : Unsigned_64) return Boolean;

private

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

   --  Translate mmap permissions.
   function Get_Mmap_Prot
      (Prot  : Unsigned_64;
       Perms : Unsigned_64) return Arch.MMU.Page_Permissions;

   --  Execute the policy chose by the user for the process.
   procedure Execute_MAC_Failure (Name : String; Curr_Proc : PID);

   --  Inner PTY IOCTL.
   procedure PTY_IOCTL
      (P        : IPC.PTY.Inner_Acc;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out Boolean);

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

   function Compare
      (Seconds1, Nanoseconds1 : Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64) return Boolean;

   procedure Increment
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64);
end Userland.Syscall;
