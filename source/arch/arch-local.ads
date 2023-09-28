--  arch-local.ads: Architecture-specific CPU-local storage.
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

with Scheduler;  use Scheduler;
with Interfaces; use Interfaces;
with Userland.Process;

package Arch.Local is
   --  Forcing reschedules using architecture-specific methods.
   procedure Reschedule_In (Microseconds : Natural);
   procedure Reschedule_ASAP;

   --  Save and restore TCB pointer local storage.
   function Fetch_TCB return System.Address;
   procedure Load_TCB (TCB : System.Address);

   --  Set the user and kernel stack for return from userland.
   procedure Set_Stacks (User_Stack, Kernel_Stack : System.Address);

   --  Fetch and set the current thread and process.
   function Get_Current_Thread return Scheduler.TID;
   function Get_Current_Process return Userland.Process.PID;
   procedure Set_Current_Thread (Thread : Scheduler.TID);
   procedure Set_Current_Process (Proc : Userland.Process.PID);
   ----------------------------------------------------------------------------
   --  Types of clocks managed by the architecture.
   type Clock_Type is
      (Clock_Real_Time,   --  Wall time, subject to system time being changed.
       Clock_Monotonic);  --  Only goes forward, unsettable, inexorable.

   --  Get the resolution of the passed clock.
   procedure Get_Resolution
      (Clock       : Clock_Type;
       Seconds     : out Unsigned_64;
       Nanoseconds : out Unsigned_64;
       Success     : out Boolean);

   --  Get the time from the passed clock.
   procedure Get_Time
      (Clock       : Clock_Type;
       Seconds     : out Unsigned_64;
       Nanoseconds : out Unsigned_64;
       Success     : out Boolean);

   --  Set the time for the passed clock.
   procedure Set_Time
      (Clock       : Clock_Type;
       Seconds     : Unsigned_64;
       Nanoseconds : Unsigned_64;
       Success     : out Boolean)
      with Pre => Clock /= Clock_Monotonic;
end Arch.Local;
