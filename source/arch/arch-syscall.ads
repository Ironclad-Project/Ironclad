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
end Arch.Syscall;
