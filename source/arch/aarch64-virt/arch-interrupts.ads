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
   type ISR_GPRs is record
      Placeholder : Integer;
   end record with Convention => C;

   type Interrupt_Type is (
      Current_EL_SP0_Synchronous,
      Current_EL_SP0_IRQ,
      Current_EL_SP0_FIQ,
      Current_EL_SP0_SError,
      Current_EL_SPX_Synchronous,
      Current_EL_SPX_IRQ,
      Current_EL_SPX_FIQ,
      Current_EL_SPX_SError,
      Lower_EL_64_Synchronous,
      Lower_EL_64_SPX_IRQ,
      Lower_EL_64_SPX_FIQ,
      Lower_EL_64_SPX_SError,
      Lower_EL_32_Synchronous,
      Lower_EL_32_SPX_IRQ,
      Lower_EL_32_SPX_FIQ,
      Lower_EL_32_SPX_SError
   );
   for Interrupt_Type use (
      Current_EL_SP0_Synchronous => 0,
      Current_EL_SP0_IRQ         => 1,
      Current_EL_SP0_FIQ         => 2,
      Current_EL_SP0_SError      => 3,
      Current_EL_SPX_Synchronous => 4,
      Current_EL_SPX_IRQ         => 5,
      Current_EL_SPX_FIQ         => 6,
      Current_EL_SPX_SError      => 7,
      Lower_EL_64_Synchronous    => 8,
      Lower_EL_64_SPX_IRQ        => 9,
      Lower_EL_64_SPX_FIQ        => 10,
      Lower_EL_64_SPX_SError     => 11,
      Lower_EL_32_Synchronous    => 12,
      Lower_EL_32_SPX_IRQ        => 13,
      Lower_EL_32_SPX_FIQ        => 14,
      Lower_EL_32_SPX_SError     => 15
   );

   procedure Exception_Handler
      (Interrupt     : Interrupt_Type;
       Syndrome      : Unsigned_64;
       Link_Address  : System.Address;
       State         : Unsigned_64;
       Fault_Address : System.Address)
      with Convention => C, Export, External_Name => "exception_handler";
end Arch.Interrupts;
