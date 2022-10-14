--  userland-syscall.ads: Syscall list and implementation.
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
with VFS.File;
with Arch.Context;
with Arch.MMU;

package Userland.Syscall with SPARK_Mode => Off is
   --  Error conditions for syscalls.
   --  The representation values are arbitrary.
   type Errno_Value is (
      Error_No_Error,        --  No error
      Error_Not_Big_Enough,  --  ERANGE
      Error_Bad_Access,      --  EACCES
      Error_Would_Block,     --  EAGAIN
      Error_Child,           --  ECHILD
      Error_Would_Fault,     --  EFAULT
      Error_Invalid_Value,   --  EINVAL
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
      Error_Child           => 1012,
      Error_Would_Fault     => 1020,
      Error_Invalid_Value   => 1026,
      Error_Too_Many_Files  => 1031,
      Error_String_Too_Long => 1036,
      Error_No_Entity       => 1043,
      Error_Not_Implemented => 1051,
      Error_Not_A_TTY       => 1058,
      Error_Invalid_Seek    => 1069,
      Error_Bad_Search      => 1070,
      Error_Bad_File        => 1081
   );

   --  Enable syscall tracing.
   procedure Set_Tracing (Value : Boolean);

   --  Exit the callee thread, flushing open files.
   procedure Syscall_Exit (Error_Code : Unsigned_64);

   --  Set arch-specific thread state.
   function Syscall_Arch_PRCtl
      (Code     : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

   --  Open a file.
   O_RDONLY   : constant := 2#000000001#;
   O_WRONLY   : constant := 2#000000010#;
   O_RDWR     : constant := 2#000000011#;
   O_APPEND   : constant := 2#000000100#;
   O_CREAT    : constant := 2#000001000#;
   O_CLOEXEC  : constant := 2#000010000#;
   O_NOFOLLOW : constant := 2#100000000#;
   function Syscall_Open
      (Address : Unsigned_64;
       Flags   : Unsigned_64;
       Mode    : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   --  Close a file.
   function Syscall_Close
      (File_D : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Read from a file.
   function Syscall_Read
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Write to a file.
   function Syscall_Write
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Operate with the file offset.
   SEEK_SET     : constant := 1;
   SEEK_CURRENT : constant := 2;
   SEEK_END     : constant := 4;
   function Syscall_Seek
      (File_D : Unsigned_64;
       Offset : Unsigned_64;
       Whence : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Mmap, the one and only.
   Protection_Write   : constant := 2#010#;
   Protection_Execute : constant := 2#100#;
   Map_Fixed : constant := 2#0100#;
   Map_Anon  : constant := 2#1000#;
   function Syscall_Mmap
      (Hint       : Unsigned_64;
       Length     : Unsigned_64;
       Protection : Unsigned_64;
       Flags      : Unsigned_64;
       File_D     : Unsigned_64;
       Offset     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64;

   --  Mmap^-1
   function Syscall_Munmap
      (Address    : Unsigned_64;
       Length     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64;

   --  Get the callee PID.
   function Syscall_Get_PID return Unsigned_64;

   --  Get the PID of the parent of the callee.
   function Syscall_Get_Parent_PID return Unsigned_64;

   --  Execute.
   function Syscall_Exec
      (Address : Unsigned_64;
       Argv    : Unsigned_64;
       Envp    : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   --  Fork the callee process.
   function Syscall_Fork
      (State_To_Fork : Arch.Context.GP_Context_Acc;
       Errno         : out Errno_Value) return Unsigned_64;

   --  Wait.
   Wait_WNOHANG : constant := 2#000010#;
   function Syscall_Wait
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
   function Syscall_Uname
      (Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   function Syscall_Set_Hostname
      (Address : Unsigned_64;
       Length  : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   --  File descriptor Stat.
   type Time_Spec is record
      Seconds     : Unsigned_64;
      Nanoseconds : Unsigned_64;
   end record;
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
   function Syscall_FStat
      (File_D  : Unsigned_64;
       Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   --  Path stat.
   function Syscall_LStat
      (Path    : Unsigned_64;
       Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   --  Get current working directory.
   function Syscall_Get_CWD
      (Buffer : Unsigned_64;
       Length : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;

   --  Get current working directory.
   function Syscall_Chdir
      (Path  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64;

   --  IO control.
   function Syscall_IOCTL
      (FD       : Unsigned_64;
       Request  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

   --  Yield.
   function Syscall_Sched_Yield (Errno : out Errno_Value) return Unsigned_64;

   --  Get thread priority.
   Which_Process : constant := 1;
   function Syscall_Get_Priority
      (Which, Who : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64;

   --  Set thread priority.
   function Syscall_Set_Priority
      (Which, Who, Prio : Unsigned_64;
       Errno            : out Errno_Value) return Unsigned_64;

   --  Dup functions.
   function Syscall_Dup
      (Old_FD : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64;
   function Syscall_Dup2
      (Old_FD, New_FD : Unsigned_64;
       Errno          : out Errno_Value) return Unsigned_64;
   function Syscall_Dup3
      (Old_FD, New_FD : Unsigned_64;
       Flags          : Unsigned_64;
       Errno          : out Errno_Value) return Unsigned_64;

   Access_Exists    : constant := 2#0001#;
   Access_Can_Read  : constant := 2#0010#;
   Access_Can_Write : constant := 2#0100#;
   Access_Can_Exec  : constant := 2#1000#;
   function Syscall_Access
      (Path, Mode : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64;

   --  Managing scheduling of a thread.
   Thread_RT     : constant := 2#0001#;
   Thread_MONO   : constant := 2#0010#;
   Thread_MLOCK  : constant := 2#0100#;
   Thread_BANNED : constant := 2#1000#;
   function Syscall_Get_Thread_Sched
      (Errno : out Errno_Value) return Unsigned_64;
   function Syscall_Set_Thread_Sched
      (Flags : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64;

   --  Multiplexed operation for files.
   F_GETFD    : constant := 3;
   F_SETFD    : constant := 4;
   FD_CLOEXEC : constant := 1;
   function Syscall_Fcntl
      (FD       : Unsigned_64;
       Command  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

   --  "posix_spawn"-like utility.
   function Syscall_Spawn
      (Address : Unsigned_64;
       Argv    : Unsigned_64;
       Envp    : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64;

   --  Bypassing /dev/(u)random for getting random data.
   function Syscall_Get_Random
     (Address : Unsigned_64;
      Length  : Unsigned_64;
      Errno   : out Errno_Value) return Unsigned_64;

   --  Change protection from memory regions.
   function Syscall_MProtect
     (Address    : Unsigned_64;
      Length     : Unsigned_64;
      Protection : Unsigned_64;
      Errno      : out Errno_Value) return Unsigned_64;

   --  Cryptographic functions exposed to userland.
   function Syscall_Crypto_Request
      (Request  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64;

private

   function Get_Mmap_Prot (P : Unsigned_64) return Arch.MMU.Page_Permissions;

   function Inner_Stat
      (F       : VFS.File.File_Acc;
       Address : Unsigned_64) return Boolean;
end Userland.Syscall;
