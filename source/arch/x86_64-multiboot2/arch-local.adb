--  arch-local.adb: Architecture-specific CPU-local storage.
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

with Arch.CPU;
with Arch.APIC;
with Arch.Snippets;
with Arch.Interrupts;
with Interfaces; use Interfaces;

package body Arch.Local with SPARK_Mode => Off is
   procedure Reschedule_In (Microseconds : Natural) is
   begin
      APIC.LAPIC_Timer_Oneshot (
         Interrupts.Scheduler_Interrupt,
         CPU.Get_Local.LAPIC_Timer_Hz,
         Unsigned_64 (Microseconds)
      );
   end Reschedule_In;

   procedure Reschedule_ASAP is
      Core_LAPIC : constant Unsigned_32 := CPU.Get_Local.LAPIC_ID;
   begin
      --  Force rescheduling by calling the ISR vector directly.
      APIC.LAPIC_Send_IPI (Core_LAPIC, Interrupts.Scheduler_Interrupt);
   end Reschedule_ASAP;

   function Fetch_TCB return System.Address is
   begin
      return To_Address (Integer_Address (Snippets.Read_FS));
   end Fetch_TCB;

   procedure Load_TCB (TCB : System.Address) is
   begin
      Snippets.Write_FS (Unsigned_64 (To_Integer (TCB)));
   end Load_TCB;

   procedure Set_Stacks (User_Stack, Kernel_Stack : System.Address) is
   begin
      CPU.Get_Local.User_Stack   := Unsigned_64 (To_Integer (User_Stack));
      CPU.Get_Local.Kernel_Stack := Unsigned_64 (To_Integer (Kernel_Stack));
   end Set_Stacks;

   function Get_Current_Thread return Scheduler.TID is
   begin
      return CPU.Get_Local.Current_Thread;
   end Get_Current_Thread;

   function Get_Current_Process return Userland.Process.PID is
   begin
      return CPU.Get_Local.Current_Process;
   end Get_Current_Process;

   procedure Set_Current_Thread (Thread : Scheduler.TID) is
   begin
      CPU.Get_Local.Current_Thread := Thread;
   end Set_Current_Thread;

   procedure Set_Current_Process (Proc : Userland.Process.PID) is
   begin
      CPU.Get_Local.Current_Process := Proc;
   end Set_Current_Process;
end Arch.Local;
