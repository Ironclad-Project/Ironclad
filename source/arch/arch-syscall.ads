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

   --  Print a C-style string to the kernel reporting.
   function Syscall_Log
      (Address : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64;

   --  Exit the callee thread, flushing open files.
   procedure Syscall_Exit (Error_Code : Integer);

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

   --  Allocate a block.
   function Syscall_Alloc
      (Count : Unsigned_64;
       Errno : out Unsigned_64) return Unsigned_64;

   --  Free a block.
   procedure Syscall_Free (Address : Unsigned_64);
end Arch.Syscall;
