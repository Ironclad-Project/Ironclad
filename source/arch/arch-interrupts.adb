--  arch-interrupts.adb: Setup and management of interrupts.
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

with System.Machine_Code;
with Arch.APIC;
with Lib.Panic;
with Lib.Messages;

package body Arch.Interrupts is
   procedure Exception_Handler (Number : Integer; State : access ISR_GPRs) is
      Exception_Text : constant array (0 .. 30) of String (1 .. 3) := (
         0  => "#DE", 1  => "#DB", 2  => "???", 3  => "#BP",
         4  => "#OF", 5  => "#BR", 6  => "#UD", 7  => "#NM",
         8  => "#DF", 9  => "???", 10 => "#TS", 11 => "#NP",
         12 => "#SS", 13 => "#GP", 14 => "#PF", 15 => "???",
         16 => "#MF", 17 => "#AC", 18 => "#MC", 19 => "#XM",
         20 => "#VE", 21 => "#CP", 22 .. 27 => "???",
         28 => "#HV", 29 => "#VC", 30 => "#SX"
      );
   begin
      if State.Error_Code /= 0 then
         Lib.Messages.Put ("Error code: ");
         Lib.Messages.Put (State.Error_Code, False, True);
         Lib.Messages.Put_Line ("");
      end if;
      Lib.Panic.Hard_Panic (Exception_Text (Number));
   end Exception_Handler;

   procedure Set_Interrupt_Flag (Enable : Boolean) is
      package SM renames System.Machine_Code;
   begin
      if Enable then
         SM.Asm ("sti", Clobber  => "memory", Volatile => True);
      else
         SM.Asm ("cli", Clobber  => "memory", Volatile => True);
      end if;
   end Set_Interrupt_Flag;

   procedure Default_ISR_Handler is
   begin
      Lib.Panic.Soft_Panic ("Default ISR triggered");
      Arch.APIC.LAPIC_EOI;
   end Default_ISR_Handler;

   procedure Spurious_Handler is
   begin
      Lib.Panic.Hard_Panic ("LAPIC Spurious interrupt occured");
   end Spurious_Handler;
end Arch.Interrupts;
