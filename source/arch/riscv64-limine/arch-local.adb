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
   begin
      return System.Null_Address;
   end Fetch_TCB;

   procedure Load_TCB (TCB : System.Address) is
   begin
      null;
   end Load_TCB;

   procedure Set_Stacks
      (Core : Context.Core_Context;
       Kernel_Stack : System.Address)
   is
   begin
      null;
   end Set_Stacks;

   --  Fetch and set the current thread and process.
   function Get_Current_Thread return Scheduler.TID is
   begin
      return Scheduler.Error_TID;
   end Get_Current_Thread;

   function Get_Current_Process return Userland.Process.PID is
   begin
      return Userland.Process.Error_PID;
   end Get_Current_Process;

   procedure Set_Current_Thread (Thread : Scheduler.TID) is
   begin
      null;
   end Set_Current_Thread;

   procedure Set_Current_Process (Proc : Userland.Process.PID) is
   begin
      null;
   end Set_Current_Process;
end Arch.Local;
