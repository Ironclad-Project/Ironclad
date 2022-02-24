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

   --  Print a C-style string to the kernel reporting.
   procedure Syscall_Log (Address : Unsigned_64; Errno : out Unsigned_64);

   --  Exit the callee thread, flushing open files.
   procedure Syscall_Exit (Error_Code : Integer; Errno : out Unsigned_64);

   --  Set an address to be thread-local storage.
   procedure Syscall_Set_TCB (Addr : Unsigned_64; Errno : out Unsigned_64);

   --  Entrypoint of the syscall dispatcher.
   procedure Syscall_Handler (Number : Integer; State : access ISR_GPRs)
      with Convention => C;
end Arch.Syscall;
