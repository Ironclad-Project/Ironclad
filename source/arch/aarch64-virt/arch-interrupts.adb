--  arch-exceptions.adb: Interrupt utilities.
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

with Lib.Messages;
with Lib.Panic;

package body Arch.Interrupts is
   procedure Exception_Handler
      (Interrupt     : Interrupt_Type;
       Syndrome      : Unsigned_64;
       Link_Address  : System.Address;
       State         : Unsigned_64;
       Fault_Address : System.Address)
   is
      Execution_Type : constant Unsigned_64 := Shift_Right (Syndrome, 26);
      Execution_Msg  : constant String := (case Execution_Type is
         when 16#01# => "Trapped wfi/wfe",
         when 16#0E# => "Illegal execution",
         when 16#15# => "System call",
         when 16#20# => "Instruction abort, lower EL",
         when 16#21# => "Instruction abort, same EL",
         when 16#22# => "Instruction alignment fault",
         when 16#24# => "Data abort, lower EL",
         when 16#25# => "Data abort, same EL",
         when 16#26# => "Stack alignment fault",
         when 16#2c# => "Floating point",
         when others => "Unknown");
   begin
      Lib.Messages.Put (Execution_Msg & " (");
      Lib.Messages.Put (Execution_Type, False, True);
      Lib.Messages.Put (") catched at IP ");
      Lib.Messages.Put (Link_Address);
      Lib.Messages.Put (", Fault Address ");
      Lib.Messages.Put (Fault_Address);
      Lib.Messages.Put (", State ");
      Lib.Messages.Put (State, False, True);
      Lib.Messages.Put_Line ("");

      Lib.Panic.Hard_Panic ("Fatal " & Interrupt_Type'Image (Interrupt));
   end Exception_Handler;
end Arch.Interrupts;
