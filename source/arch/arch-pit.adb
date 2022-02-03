--  arch-pit.adb: Programmable Interval Timer driver.
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

with Arch.APIC;
with Arch.CPU;
with Arch.IDT;
with Arch.Wrappers;

package body Arch.PIT is
   PIT_IRQ           : constant := 33;
   PIT_Divisor       : constant := 1193180;
   PIT_Frequency     : constant := 1000;
   PIT_Channel0_Port : constant := 16#40#;
   PIT_Command_Port  : constant := 16#43#;

   PIT_Enabled   : Boolean  := False;
   Handler_Index : IDT.IDT_Index;

   function Init return Boolean is
      New_Divisor : constant Unsigned_32 := PIT_Divisor / PIT_Frequency;
      Low_Divisor : constant Unsigned_32 := Shift_Right (New_Divisor, 8);

      Low8  : constant Unsigned_8 := Unsigned_8 (New_Divisor and 16#FF#);
      High8 : constant Unsigned_8 := Unsigned_8 (Low_Divisor and 16#FF#);

      BSP_LAPIC_ID : constant Unsigned_32 := Arch.CPU.Core_LAPICs (1);
   begin
      --  Setup the PIT.
      Arch.Wrappers.Port_Out (PIT_Command_Port,  16#36#);
      Arch.Wrappers.Port_Out (PIT_Channel0_Port, Low8);
      Arch.Wrappers.Port_Out (PIT_Channel0_Port, High8);

      --  Prepare the uptime and register the interrupt only for the BSP.
      Uptime := 0;
      if not Arch.IDT.Load_ISR (IRQ_Handler'Address, Handler_Index) then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect
         (BSP_LAPIC_ID, PIT_IRQ, Handler_Index, True)
      then
         return False;
      end if;
      PIT_Enabled := True;
      return True;
   end Init;

   procedure Sleep (Milliseconds : Positive) is
      Target : constant Unsigned_64 := Uptime + Unsigned_64 (Milliseconds);
   begin
      if not PIT_Enabled then
         return;
      end if;

      while Target > Uptime loop null; end loop;
   end Sleep;

   procedure IRQ_Handler is
   begin
      Uptime := Uptime + 1;
      Arch.APIC.LAPIC_EOI;
   end IRQ_Handler;
end Arch.PIT;
