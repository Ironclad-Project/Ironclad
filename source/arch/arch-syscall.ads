--  arch-syscall.ads: Specification of the syscall table and implementation.
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

with Interfaces;      use Interfaces;
with Arch.Interrupts; use Arch.Interrupts;

package Arch.Syscall is
   --  Here lie the definitions of the kernel's syscalls and the entrypoint
   --  that dispatches them.

   --  Enable syscall tracing.
   procedure Set_Tracing (Value : Boolean);

   --  Entrypoint of the syscall dispatcher.
   procedure Syscall_Handler (Number : Integer; State : access ISR_GPRs)
      with Convention => C;

private

   --  Exit the callee thread, flushing open files.
   procedure Syscall_Exit (Error_Code : Unsigned_64);

   --  Set an address to be thread-local storage.
   function Syscall_Set_TCB
      (Address : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64;

   --  Open a file.
   O_RDONLY : constant := 2#0001#;
   O_WRONLY : constant := 2#0010#;
   O_RDWR   : constant := 2#0011#;
   O_APPEND : constant := 2#0100#;
   O_CREAT  : constant := 2#1000#;
   function Syscall_Open
      (Address : Unsigned_64;
       Flags   : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64;

   --  Close a file.
   function Syscall_Close
      (File_D : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64;

   --  Read from a file.
   function Syscall_Read
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64;

   --  Write to a file.
   function Syscall_Write
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64;

   --  Operate with the file offset.
   SEEK_SET     : constant := 1;
   SEEK_CURRENT : constant := 2;
   SEEK_END     : constant := 4;
   function Syscall_Seek
      (File_D : Unsigned_64;
       Offset : Unsigned_64;
       Whence : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64;

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
       Errno      : out Unsigned_64) return Unsigned_64;

   --  Mmap^-1
   function Syscall_Munmap
      (Address    : Unsigned_64;
       Length     : Unsigned_64;
       Errno      : out Unsigned_64) return Unsigned_64;

   --  Get the callee PID.
   function Syscall_Get_PID return Unsigned_64;

   --  Get the PID of the parent of the callee.
   function Syscall_Get_Parent_PID return Unsigned_64;

   --  Set thread preference.
   function Syscall_Thread_Preference
      (Preference : Unsigned_64;
       Errno      : out Unsigned_64) return Unsigned_64;

   --  Execute.
   function Syscall_Exec
      (Address : Unsigned_64;
       Argv    : Unsigned_64;
       Envp    : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64;

   --  Fork.
   function Syscall_Fork
      (State_To_Fork : access ISR_GPRs;
       Errno         : out Unsigned_64) return Unsigned_64;

   --  Wait.
   function Syscall_Wait
      (Waited_PID : Unsigned_64;
       Exit_Addr  : Unsigned_64;
       Options    : Unsigned_64;
       Errno      : out Unsigned_64) return Unsigned_64;

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
       Errno   : out Unsigned_64) return Unsigned_64;

   function Syscall_Set_Hostname
      (Address : Unsigned_64;
       Length  : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64;

   --  File descriptor Stat.
   type Time_Spec is record
      Seconds     : Unsigned_64;
      Nanoseconds : Unsigned_64;
   end record;
   Stat_IFREG : constant := 16#08000#;
   Stat_IFCHR : constant := 16#02000#;
   Stat_IFBLK : constant := 16#06000#;
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
       Errno   : out Unsigned_64) return Unsigned_64;

   --  Path stat.
   function Syscall_LStat
      (Path    : Unsigned_64;
       Address : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64;

   --  Get current working directory.
   function Syscall_Get_CWD
      (Buffer : Unsigned_64;
       Length : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64;

   --  Get current working directory.
   function Syscall_Chdir
      (Path  : Unsigned_64;
       Errno : out Unsigned_64) return Unsigned_64;
end Arch.Syscall;
