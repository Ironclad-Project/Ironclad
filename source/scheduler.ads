--  scheduler.ads: Thread scheduler.
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

with System;
with Interfaces; use Interfaces;
with Memory; use Memory;
with Userland;
with Userland.ELF;
with Memory.MMU;
with Arch.Context;
with Time; use Time;

package Scheduler is
   --  Types to represent threads.
   type TID is private;
   Error_TID : constant TID;

   --  Scheduler policies we support.
   type Policy is
      (Policy_FIFO,   --  First-In-First-Out.
       Policy_RR,     --  Flat round-robin using the passed quantum.
       Policy_Other); --  Like _Other but for daemons and other low latency.
   ----------------------------------------------------------------------------
   --  Initialize the scheduler, return true on success, false on failure.
   procedure Init (Success : out Boolean);

   --  Use when doing nothing and we want the scheduler to put us to work.
   --  Doubles as the function to initialize core locals.
   procedure Idle_Core with No_Return;

   --  The three below make a userland thread held: it is not run until
   --  Release_Thread lets it go, which Userland.Process.Add_Thread does as
   --  it lists the thread in its process, so a thread cannot end before its
   --  process knows of it. Delete_Thread takes a held thread never let go.

   --  Creates a userland thread, and queues it for execution.
   --  Return thread ID or 0 on failure.
   procedure Create_User_Thread
      (Address    : Virtual_Address;
       Args       : Userland.Argument_Arr;
       Env        : Userland.Environment_Arr;
       Map        : Memory.MMU.Page_Table_Acc;
       Vector     : Userland.ELF.Auxval;
       Pol        : Policy;
       Stack_Size : Unsigned_64;
       PID        : Natural;
       New_TID    : out TID);

   --  Create a userland thread with no arguments.
   procedure Create_User_Thread
      (Address    : Virtual_Address;
       Map        : Memory.MMU.Page_Table_Acc;
       Stack_Addr : Unsigned_64;
       TLS_Addr   : Unsigned_64;
       Pol        : Policy;
       Argument   : Unsigned_64;
       PID        : Natural;
       New_TID    : out TID);

   --  Create a user thread with a context.
   procedure Create_User_Thread
      (GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Map      : Memory.MMU.Page_Table_Acc;
       Pol      : Policy;
       PID      : Natural;
       TCB      : System.Address;
       New_TID  : out TID);

   --  Let a thread made by Create_User_Thread run.
   --  @param Thread Thread to let go, which must be held.
   procedure Release_Thread (Thread : TID);

   --  Removes a thread, kernel or user, from existence (if it exists).
   procedure Delete_Thread (Thread : TID);

   --  If interruptible, give up the rest of our execution time and go back to
   --  rescheduling, else just return.
   procedure Yield_If_Able;

   --  Make the callee thread be dequeued.
   procedure Bail with No_Return;

   --  Whether the calling thread has been deleted while running.
   function Is_Doomed return Boolean;

   --  Get runtime times of the thread.
   procedure Get_Runtimes (Thread : TID; System, User : out Time.Timestamp);

   --  Signal to the scheduler that a thread has entered or exited kernel
   --  space (for time keeping reasons).
   procedure Signal_Kernel_Entry (Thread : TID);
   procedure Signal_Kernel_Exit (Thread : TID);

   --  Do not schedule the calling thread until a certain timestamp is hit.
   procedure Suspend_Until
      (Clock      : Time.Clock_Type;
       Start_Time : Time.Timestamp);

   --  Do not schedule the calling thread, at all, and do not yield, but
   --  return.
   procedure Mark_Suspend;

   --  Lift the suspension of a thread.
   procedure Lift_Suspension (Thread : TID);

   --  Check whether a thread is suspended.
   procedure Is_Suspended (Thread : TID; Suspended : out Boolean);
   ----------------------------------------------------------------------------
   --  Threads can wait on keys, the addresses of the objects they wait for,
   --  and sleep until another thread calls Wake_Event with one of them or a
   --  timeout expires. A wait looks like:
   --
   --     Begin_Wait, and Add_Wait_Key for every object;
   --     loop
   --        Clear_Wake, check the condition, and exit if it holds;
   --        Wait_Event (Deadline, Max_Micros);
   --     end loop;
   --     End_Wait;
   --
   --  Wait_Event returns at once for wakes after Clear_Wake, so wakes that
   --  come between the check and the sleep are not lost.

   --  Start a wait for the calling thread, with no keys and no wakes.
   procedure Begin_Wait;

   --  Add a key to the wait of the calling thread.
   --  @param Key     Key to be woken by.
   --  @param Success False if there is no room for more keys, in which case
   --                 the thread is not woken for Key and must poll.
   procedure Add_Wait_Key (Key : System.Address; Success : out Boolean);

   --  Forget the wakes received by the calling thread.
   procedure Clear_Wake;

   --  Sleep until the calling thread is woken or a timeout expires, returning
   --  at once if it was woken since the last Clear_Wake.
   --  @param Deadline   Monotonic time to sleep until at most.
   --  @param Max_Micros Microseconds to sleep for at most.
   procedure Wait_Event (Deadline : Time.Timestamp; Max_Micros : Natural);

   --  Deadline for sleeps only bounded by Max_Micros.
   No_Deadline : constant Time.Timestamp := (Unsigned_64'Last, 0);

   --  Max_Micros for threads woken for every change they wait on, and for
   --  those that have to look again on their own.
   Woken_Sleep_Micros  : constant := 5_000_000;
   Polled_Sleep_Micros : constant := 10_000;

   --  End the wait of the calling thread, dropping its keys.
   procedure End_Wait;

   --  Wake the threads waiting on a key.
   procedure Wake_Event (Key : System.Address);
   ----------------------------------------------------------------------------
   --  Some scheduling algorithms allow priority, in those cases, it is
   --  interacted with using POSIX-compatible niceness.
   subtype Niceness is Integer range -20 .. 20;
   subtype Priority is Natural range 0 .. 100;
   Default_Niceness : constant Niceness := 0;
   Default_Priority : constant Priority := 0;
   Default_RR_NS_Interval : constant Natural := 100_000_000;

   function Get_Niceness (Thread : TID) return Niceness;
   procedure Set_Niceness (Thread : TID; Nice : Niceness);

   function Get_Priority (Thread : TID) return Priority;
   procedure Set_Priority (Thread : TID; Prio : Priority);

   procedure Set_Policy (Thread : TID; Pol : Policy);
   procedure Set_RR_Interval (Thread : TID; RR_Sec, RR_NS : Unsigned_64);

   procedure Get_Name (Thread : TID; Name : out String; Len : out Natural);
   procedure Set_Name (Thread : TID; Name : String; Success : out Boolean);
   ----------------------------------------------------------------------------
   procedure Get_Signal_Stack
      (Thread      : TID;
       Addr        : out System.Address;
       Size        : out Unsigned_64;
       Is_Disabled : out Boolean;
       Is_Using    : out Boolean);

   procedure Set_Signal_Stack
      (Thread      : TID;
       Addr        : System.Address;
       Size        : Unsigned_64;
       Is_Disabled : Boolean;
       Success     : out Boolean);

   procedure Launch_Signal_Thread
      (Signal_Number    : Unsigned_64;
       Handle, Restorer : System.Address;
       Is_Altstack      : Boolean;
       Success          : out Boolean);

   procedure Exit_Signal_And_Reschedule;
   ----------------------------------------------------------------------------
   --  Get the number of processes set to run over various periods of time.
   --  @param Avg_1  1 minute average  * 100.
   --  @param Avg_5  5 minute average  * 100.
   --  @param Avg_15 15 minute average * 100.
   procedure Get_Load_Averages (Avg_1, Avg_5, Avg_15 : out Unsigned_32);
   ----------------------------------------------------------------------------
   --  Hook to be called by the architecture for reescheduling of the callee
   --  core.
   procedure Scheduler_ISR (State : in out Arch.Context.GP_Context);
   ----------------------------------------------------------------------------
   --  Functions to convert from IDs to user readable values and viceversa.
   function Convert (Thread : TID) return Natural;
   function Convert (Value : Natural) return TID;

   --  Whether Thread is alive and runs for the process numbered PID, a plain
   --  number since Userland.Process withs this spec, so naming one of its
   --  types here would be a cycle.
   function Is_Alive (Thread : TID; PID : Natural) return Boolean;

   type Thread_Listing is record
      Thread : TID;
      Proc   : Natural;
   end record;
   type Thread_Listing_Arr is array (Natural range <>) of Thread_Listing;

   --  List all threads on the system.
   --  @param List  Where to write all the thread information.
   --  @param Total Total count of processes, even if it is > List'Length.
   procedure List_All (List : out Thread_Listing_Arr; Total : out Natural);

private

   type TID is new Natural range 0 .. 512;
   Error_TID : constant  TID := 0;

   Is_Initialized : Boolean := False
      with Atomic, Volatile, Async_Readers => True, Async_Writers => True,
           Effective_Reads => True, Effective_Writes => True;

   procedure Next_From_Nothing (Timeout : out Natural; Next : out TID);
   procedure Next_FIFO (Curr : TID; Timeout : out Natural; Next : out TID);
   procedure Next_RR (Curr : TID; Timeout : out Natural; Next : out TID);
   procedure Next_Other (Curr : TID; Timeout : out Natural; Next : out TID);

   procedure Waiting_Spot with No_Return;
   procedure Evaluate_Runnable (T : TID; Can_Run : out Boolean);
   procedure Evaluate_Suspended (T : TID; Suspended : out Boolean);
end Scheduler;
