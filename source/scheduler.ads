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

   --  The highest thread number the scheduler will ever hand out.
   Max_Thread_Number : constant Natural;

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
   --  One other than the caller goes at once if it is in userland, and is
   --  doomed if it is in the kernel.
   procedure Delete_Thread (Thread : TID);

   --  A removed thread goes on running until its core next reschedules. Wait
   --  until no removed thread of a process is still on a core with the map
   --  of the process, as a map destroyed meanwhile would be pulled from
   --  under them.
   --  @param PID Process of the threads, a plain number as for Is_Alive.
   procedure Wait_For_Removed (PID : Natural);

   --  If interruptible, give up the rest of our execution time and go back to
   --  rescheduling, else just return. A suspended thread that is given the
   --  core back with nothing else to run waits for an interrupt first.
   procedure Yield_If_Able;

   --  Make the callee thread be dequeued.
   procedure Bail with No_Return;

   --  Whether the calling thread has been deleted while running.
   function Is_Doomed return Boolean;

   --  Whether the calling thread has to give up any wait it is in and
   --  return from it as interrupted, which a doomed thread does and so does
   --  one with a signal it can take: a caught signal is delivered on the way
   --  out of the call, which a wait has to end for.
   function Is_Interrupted return Boolean;

   --  Get runtime times of the thread.
   procedure Get_Runtimes (Thread : TID; System, User : out Time.Timestamp);

   --  Signal to the scheduler that a thread has entered or exited kernel
   --  space (for time keeping reasons).
   procedure Signal_Kernel_Entry (Thread : TID);
   procedure Signal_Kernel_Exit (Thread : TID);

   --  Do not schedule the calling thread until a certain timestamp is hit,
   --  or until it is interrupted, see Is_Interrupted.
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
   --        Clear_Wake, check the condition, and exit if it holds or the
   --        thread is interrupted, see Is_Interrupted;
   --        Wait_Event (Deadline, Max_Micros);
   --     end loop;
   --     End_Wait;
   --
   --  Wait_Event returns at once for wakes after Clear_Wake, so wakes that
   --  come between the check and the sleep are not lost. An interrupted
   --  thread gives up the wait to leave at the edge of its syscall: deleting
   --  a thread ends its sleep, see Delete_Thread, and so does a signal it
   --  can take, see Wake_For_Signal.

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

   --  Get the threads a signal was just made pending for onto a core, where
   --  a sleeping one finds a signal it can take and gives its sleep up.
   --  @param PID    Process of the threads, a plain number as for Is_Alive.
   --  @param Thread The one thread the signal is for, or Error_TID for all.
   procedure Wake_For_Signal (PID : Natural; Thread : TID);
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
   --  A thread has its own set of blocked signals.
   --  It is kept as the bitmap userland passes, bit N - 1 standing for
   --  signal N, so that this package does not name signals.
   --  @param Thread Thread to operate on.
   --  @param Mask   The signals it has blocked.
   procedure Get_Masked_Signals (Thread : TID; Mask : out Unsigned_64);
   procedure Set_Masked_Signals (Thread : TID; Mask : Unsigned_64);

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

   --  Record the userland state a thread entered the kernel with, which is
   --  what a call has to be aimed back at to be issued again (SA_RESTART).
   procedure Set_Syscall_State
      (Thread : TID;
       State  : Arch.Context.GP_Context);

   --  Aim a leaving call back at itself, so that it is issued again from the
   --  start with the arguments it was given.
   --  @param Thread Thread whose call is to be repeated.
   --  @param State  The state the call is leaving in, aimed back at it.
   procedure Restart_Syscall
      (Thread : TID;
       State  : in out Arch.Context.GP_Context);

   --  A call may give the thread a mask for its own duration, as ppoll
   --  does. The mask to go back to is restored by the call's own edge or, if
   --  a handler runs first, by that handler's return: the ucontext the
   --  handler is given carries it, so a signal the call unblocked is taken
   --  with the call's mask in force and sigreturn puts the original back.
   --  @param Thread Thread to operate on.
   --  @param Mask   Mask to go back to.
   --  @param Valid  True if there was a mask to go back to.
   procedure Set_Saved_Mask (Thread : TID; Mask : Unsigned_64);
   procedure Take_Saved_Mask
      (Thread : TID;
       Mask   : out Unsigned_64;
       Valid  : out Boolean);

   --  Enter a signal handler on the interrupted thread itself: write the
   --  frame it is to run on (a siginfo_t, a ucontext_t describing the state
   --  being interrupted, and the address of the restorer to return to) onto
   --  its stack or its alternate stack, and aim the state it is leaving the
   --  kernel with at the handler.
   --  @param State       State the thread is leaving the kernel with, which
   --                     the handler is entered with instead.
   --  @param Info_Code   si_code of the signal.
   --  @param Info_PID    si_pid of the signal.
   --  @param Info_UID    si_uid of the signal.
   --  @param Info_Status si_status of the signal.
   --  @param Info_Value  si_value of the signal, which a queued one
   --                     carries and every other one leaves at 0.
   --  @param Old_Mask    Mask the handler's return is to restore.
   --  @param Fault_Addr  Address a hardware fault happened at, which for a
   --                     fault takes the place of the sender's identity.
   --  @param Is_Fault    True when the signal came of a hardware fault, so
   --                     that the siginfo carries si_addr, which shares its
   --                     place with si_pid and si_uid.
   --  @param Success     False if the frame could not be written, as when
   --                     the stack is not userland memory or another thread
   --                     unmapped it.
   procedure Enter_Signal_Handler
      (State            : in out Arch.Context.GP_Context;
       Signal_Number    : Unsigned_64;
       Handle, Restorer : System.Address;
       Is_Altstack      : Boolean;
       Info_Code        : Unsigned_32;
       Info_PID         : Unsigned_32;
       Info_UID         : Unsigned_32;
       Info_Status      : Unsigned_32;
       Info_Value       : Unsigned_64;
       Old_Mask         : Unsigned_64;
       Fault_Addr       : Unsigned_64 := 0;
       Is_Fault         : Boolean     := False;
       Success          : out Boolean);

   --  The siginfo_t of our glibc, as bytes. A signal accepted rather than
   --  delivered (sigwaitinfo, sigtimedwait) hands userland the same
   --  structure a handler is given, so it is built here, where the signal
   --  frame is written, and those calls only copy it out.
   Siginfo_Size : constant := 128;
   --  The alignment is stated because the layout is written through it: a
   --  plain byte array is 1-aligned, and overlaying a record with 32-bit
   --  components on one may be erroneous (Ada 2022 RM 13.3(27)).
   type Siginfo_Bytes is array (1 .. Siginfo_Size) of Unsigned_8
      with Alignment => 8;

   --  Lay out one siginfo_t.
   --  @param Number Signal number, si_signo.
   --  @param Code   si_code, where the signal came from.
   --  @param PID    si_pid, who sent it.
   --  @param UID    si_uid, their real user.
   --  @param Status si_status, a child's exit code or killing signal.
   --  @param Value  si_value, what a queued signal carries.
   --  @param Into   The bytes, ready to be given to userland.
   procedure Build_Siginfo
      (Number : Unsigned_64;
       Code   : Unsigned_32;
       PID    : Unsigned_32;
       UID    : Unsigned_32;
       Status : Unsigned_32;
       Value  : Unsigned_64;
       Into   : out Siginfo_Bytes);

   --  Put back what a handler interrupted, from the ucontext_t it was given
   --  and may have edited. The segments and the flags the kernel owns are
   --  never taken from userland.
   --  @param State    State the call is leaving in, replaced by the restored
   --                  one, return registers included.
   --  @param New_Mask Mask the restored state is to run under.
   --  @param Success  False if the frame could not be read back.
   procedure Leave_Signal_Handler
      (State    : in out Arch.Context.GP_Context;
       New_Mask : out Unsigned_64;
       Success  : out Boolean);
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

   --  The process a thread runs for, a plain number for the same reason
   --  Is_Alive takes one.
   --  @param Thread Thread to ask about.
   --  @return The process's identifier, or 0 when there is no such thread.
   function Get_Thread_Process (Thread : TID) return Natural;

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
   Max_Thread_Number : constant Natural := Natural (TID'Last);

   Is_Initialized : Boolean := False
      with Atomic, Volatile, Async_Readers => True, Async_Writers => True,
           Effective_Reads => True, Effective_Writes => True;

   procedure Next_From_Nothing (Timeout : out Natural; Next : out TID);
   procedure Next_FIFO (Curr : TID; Timeout : out Natural; Next : out TID);
   procedure Next_RR (Curr : TID; Timeout : out Natural; Next : out TID);
   procedure Next_Other (Curr : TID; Timeout : out Natural; Next : out TID);

   procedure Waiting_Spot with No_Return;

   --  Wait for an interrupt if the calling thread is suspended, see
   --  Yield_If_Able.
   procedure Wait_If_Suspended (Thread : TID);

   --  Get a thread that was made runnable onto a core waiting for an
   --  interrupt, if there is one to take it.
   procedure Kick (Thread : TID);

   --  Mark whether the calling core waits for an interrupt with nothing to
   --  run. The calling thread must not change cores meanwhile.
   procedure Set_Waiting (Value : Boolean);

   --  Whether the calling core still has a thread to let go, or runs one
   --  that has gone.
   function Is_Retire_Pending return Boolean;

   --  Get the microseconds until the soonest end of the suspension of a
   --  thread that is not running, with the scheduler lock held.
   --  @param Max_Micros Microseconds to give at most.
   --  @param Micros     The microseconds, at least 1.
   procedure Next_Wake (Max_Micros : Natural; Micros : out Natural);
   procedure Evaluate_Runnable (T : TID; Can_Run : out Boolean);
   procedure Evaluate_Suspended (T : TID; Suspended : out Boolean);
   procedure Drop_Thread (Thread : TID);
end Scheduler;
