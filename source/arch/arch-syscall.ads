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

with System;
with Interfaces; use Interfaces;

package Arch.Syscall is
   --  All syscalls and the mechanism has to be made with C in mind, its the
   --  only ABI we can rely on.

   procedure Syscall_Sleep (MSec : Unsigned_32) with Convention => C;

   type Syscall_Table is array (0 .. 9) of System.Address
      with Size => 10 * 64;

   --  TODO: Placeholder, fill pls.
   Syscalls : constant Syscall_Table := (
      0 => Syscall_Sleep'Address,
      1 => Syscall_Sleep'Address,
      2 => Syscall_Sleep'Address,
      3 => Syscall_Sleep'Address,
      4 => Syscall_Sleep'Address,
      5 => Syscall_Sleep'Address,
      6 => Syscall_Sleep'Address,
      7 => Syscall_Sleep'Address,
      8 => Syscall_Sleep'Address,
      9 => Syscall_Sleep'Address
   ) with Convention => C, Export => True, External_Name => "syscall_table";

   procedure Syscall_Entry
      with Import => True, External_Name => "syscall_entry";
end Arch.Syscall;
