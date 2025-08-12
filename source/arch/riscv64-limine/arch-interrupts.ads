--  arch-exceptions.ads: Specification of interrupt utilities.
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

package Arch.Interrupts is
   --  Passed to every interrupt called ever as an access.
   --  XXX: The order and contents are tied down by arch-trap.S, if these are
   --  ever changed, change those too.
   type Frame is record
      X1,  X2,  X3,  X4,  X5,  X6  : Unsigned_64;
      X7,  X8,  X9,  X10, X11, X12 : Unsigned_64;
      X13, X14, X15, X16, X17, X18 : Unsigned_64;
      X19, X20, X21, X22, X23, X24 : Unsigned_64;
      X25, X26, X27, X28, X29, X30 : Unsigned_64;
      X31, SSTATUS, SEPC           : Unsigned_64;
   end record with Pack, Size => 2112;
   type Frame_Acc is access all Frame;

   procedure Initialize;
   procedure Load_Trap_Vector;

   type Interrupt_Index is range 1 .. 100;
   type Interrupt_Handler is access procedure;
   procedure Setup_Interrupt
      (Handler : Interrupt_Handler;
       Index   : out Interrupt_Index;
       Success : out Boolean);
   procedure Unload_Interrupt (Index : Interrupt_Index);

private

   procedure Handle_Trap (Ctx : not null Frame_Acc)
      with Export, Convention => C, External_Name => "handle_trap";

   procedure Handle_Syscall (Ctx : not null Frame_Acc);
   ----------------------------------------------------------------------------
   procedure trap_entry
      with Import, Convention => C, External_Name => "trap_entry";
end Arch.Interrupts;
