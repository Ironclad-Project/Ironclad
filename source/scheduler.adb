--  scheduler.adb: Scheduler.
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
with Arch.APIC;
with Arch.CPU;
with Arch.IDT;
with Arch.Interrupts;
with Lib.Messages;

package body Scheduler is
   --  Core locals.
   type Core_Local_Info is record
      LAPIC_Timer_Hz : Unsigned_64;
   end record;
   type Cores_Local_Info is array (Positive range <>) of Core_Local_Info;
   type Cores_Local_Info_Acc is access Cores_Local_Info;
   Core_Locals : Cores_Local_Info_Acc;

   --  Scheduler information.
   Scheduler_Vector : Arch.IDT.IDT_Index;

   function Init return Boolean is
   begin
      --  Allocate the vector for the LAPIC timer ISRs.
      if not Arch.IDT.Load_ISR (Scheduler_ISR'Address, Scheduler_Vector) then
         return False;
      end if;

      --  Allocate core locals.
      Core_Locals := new Cores_Local_Info (1 .. Arch.CPU.Core_Count);
      Is_Initialized := True;
      return True;
   end Init;

   procedure Idle_Core is
      I : constant Positive := Arch.CPU.Get_Core_Number;
   begin
      --  Check we are initialized and have all the data.
      while not Is_Initialized loop null; end loop;
      if Core_Locals (I).LAPIC_Timer_Hz = 0 then
         Core_Locals (I).LAPIC_Timer_Hz := Arch.APIC.LAPIC_Timer_Calibrate;
      end if;

      declare
         Hz : constant Unsigned_64 := Core_Locals (I).LAPIC_Timer_Hz;
      begin
         Arch.Interrupts.Set_Interrupt_Flag (False);
         Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz, 20000);
         Arch.Interrupts.Set_Interrupt_Flag (True);
         loop null; end loop;
      end;
   end Idle_Core;

   procedure Scheduler_ISR is
   begin
      Lib.Messages.Put_Line ("A new earthrise");
      loop null; end loop;
   end Scheduler_ISR;
end Scheduler;
