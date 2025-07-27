--  arch-local.adb: Architecture-specific CPU-local storage.
--  Copyright (C) 2024 streaksu
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
with System.Machine_Code;
with Arch.Snippets;
with Arch.CPU;
with Panic;

package body Arch.Local is
   procedure Reschedule_In (Microseconds : Natural) is
   begin
      null;
   end Reschedule_In;

   procedure Reschedule_ASAP is
   begin
      null;
   end Reschedule_ASAP;

   function Fetch_TCB return System.Address is
      Ret : System.Address;
   begin
      System.Machine_Code.Asm
         ("mv %0, tp",
          Outputs  => System.Address'Asm_Output ("=r", Ret),
          Volatile => True);
      return Ret;
   end Fetch_TCB;

   procedure Load_TCB (TCB : System.Address) is
   begin
      System.Machine_Code.Asm
         ("mv tp, %0",
          Inputs   => System.Address'Asm_Input ("r", TCB),
          Volatile => True);
   end Load_TCB;

   procedure Set_Stacks
      (Core : Context.Core_Context;
       Kernel_Stack : System.Address)
   is
      Is_Ints : constant Boolean := Snippets.Interrupts_Enabled;
   begin
      if Is_Ints then Snippets.Disable_Interrupts; end if;
      CPU.Get_Local.User_Stack   := Core;
      CPU.Get_Local.Kernel_Stack := Unsigned_64 (To_Integer (Kernel_Stack));
      if Is_Ints then Snippets.Enable_Interrupts; end if;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception setting stacks");
   end Set_Stacks;

   function Get_Current_Thread return Scheduler.TID is
      Returned : Scheduler.TID;
      Is_Ints  : constant Boolean := Snippets.Interrupts_Enabled;
   begin
      if Is_Ints then Snippets.Disable_Interrupts; end if;
      Returned := CPU.Get_Local.Current_Thread;
      if Is_Ints then Snippets.Enable_Interrupts; end if;
      return Returned;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception getting current thread");
   end Get_Current_Thread;

   function Get_Current_Process return Userland.Process.PID is
      Returned : Userland.Process.PID;
      Is_Ints  : constant Boolean := Snippets.Interrupts_Enabled;
   begin
      if Is_Ints then Snippets.Disable_Interrupts; end if;
      Returned := CPU.Get_Local.Current_Process;
      if Is_Ints then Snippets.Enable_Interrupts; end if;
      return Returned;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception getting current process");
   end Get_Current_Process;

   procedure Set_Current_Thread (Thread : Scheduler.TID) is
      Is_Ints : constant Boolean := Snippets.Interrupts_Enabled;
   begin
      if Is_Ints then Snippets.Disable_Interrupts; end if;
      CPU.Get_Local.Current_Thread := Thread;
      if Is_Ints then Snippets.Enable_Interrupts; end if;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception setting current thread");
   end Set_Current_Thread;

   procedure Set_Current_Process (Proc : Userland.Process.PID) is
      Is_Ints : constant Boolean := Snippets.Interrupts_Enabled;
   begin
      if Is_Ints then Snippets.Disable_Interrupts; end if;
      CPU.Get_Local.Current_Process := Proc;
      if Is_Ints then Snippets.Enable_Interrupts; end if;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception setting current process");
   end Set_Current_Process;
end Arch.Local;
