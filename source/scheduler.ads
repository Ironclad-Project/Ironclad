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
with Arch.MMU;
with Arch.Context;

package Scheduler is
   --  Types to represent threads and thread clusters.
   type TID  is private;
   type TCID is private;
   Error_TID  : constant TID;
   Error_TCID : constant TCID;
   ----------------------------------------------------------------------------
   --  Initialize the scheduler, return true on success, false on failure.
   function Init return Boolean;

   --  Use when doing nothing and we want the scheduler to put us to work.
   --  Doubles as the function to initialize core locals.
   procedure Idle_Core with No_Return;

   --  Creates a userland thread, and queues it for execution.
   --  Return thread ID or 0 on failure.
   function Create_User_Thread
      (Address    : Virtual_Address;
       Args       : Userland.Argument_Arr;
       Env        : Userland.Environment_Arr;
       Map        : Arch.MMU.Page_Table_Acc;
       Vector     : Userland.ELF.Auxval;
       Cluster    : TCID;
       Stack_Size : Unsigned_64;
       PID        : Natural) return TID;

   --  Create a userland thread with no arguments.
   function Create_User_Thread
      (Address    : Virtual_Address;
       Map        : Arch.MMU.Page_Table_Acc;
       Stack_Addr : Unsigned_64;
       TLS_Addr   : Unsigned_64;
       Cluster    : TCID;
       PID        : Natural) return TID;

   --  Create a user thread with a context.
   function Create_User_Thread
      (GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Map      : Arch.MMU.Page_Table_Acc;
       Cluster  : TCID;
       PID      : Natural;
       TCB      : System.Address) return TID;

   --  Removes a thread, kernel or user, from existance (if it exists).
   procedure Delete_Thread (Thread : TID);

   --  If interruptible, give up the rest of our execution time and go back to
   --  rescheduling, else just return.
   procedure Yield_If_Able;

   --  Make the callee thread be dequed.
   procedure Bail with No_Return;

   --  Get runtime times of the thread.
   procedure Get_Runtime_Times
      (Thread : TID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64);

   --  Signal to the scheduler that a thread has entered or exited kernel
   --  space (for time keeping reasons).
   procedure Signal_Kernel_Entry (Thread : TID);
   procedure Signal_Kernel_Exit (Thread : TID);
   ----------------------------------------------------------------------------
   --  Cluster creation, deletion, and management.

   --  Algorithms used inside clusters for internal scheduling.
   type Cluster_Algorithm is
      (Cluster_RR,           --  Cluster will do a priority round robin.
       Cluster_Cooperative); --  Cluster will do cooperative scheduling.

   function Set_Scheduling_Algorithm
      (Cluster          : TCID;
       Algo             : Cluster_Algorithm;
       Quantum          : Natural;
       Is_Interruptible : Boolean) return Boolean;

   function Set_Time_Slice (Cluster : TCID; Per : Natural) return Boolean;

   function Create_Cluster return TCID;
   function Delete_Cluster (Cluster : TCID) return Boolean;

   function Switch_Cluster (Cluster : TCID; Thread : TID) return Boolean;
   ----------------------------------------------------------------------------
   --  Some scheduling algorithms allow priority, in those cases, it is
   --  interacted with using POSIX-compatible niceness.
   subtype Niceness is Integer range -20 .. 20;
   Default_Niceness : constant Niceness := 0;

   function Get_Niceness (Thread : TID) return Niceness;
   procedure Set_Niceness (Thread : TID; Nice : Niceness);

   procedure Get_Name (Thread : TID; Name : out String; Len : out Natural);
   procedure Set_Name (Thread : TID; Name : String; Success : out Boolean);
   ----------------------------------------------------------------------------
   --  Get the number of processes set to run over various periods of time.
   --  @param Avg_1  1 minute average  * 100.
   --  @param Avg_5  5 minute average  * 100.
   --  @param Avg_15 15 minute average * 100.
   procedure Get_Load_Averages (Avg_1, Avg_5, Avg_15 : out Unsigned_32);
   ----------------------------------------------------------------------------
   --  Hook to be called by the architecture for reescheduling of the callee
   --  core.
   procedure Scheduler_ISR (State : Arch.Context.GP_Context);
   ----------------------------------------------------------------------------
   --  Functions to convert from IDs to user readable values and viceversa.
   function Convert (Thread : TID) return Natural;
   function Convert (Group : TCID) return Natural;
   function Convert (Value : Natural) return TID;
   function Convert (Value : Natural) return TCID;

   type Thread_Listing is record
      Thread  : TID;
      Cluster : TCID;
      Proc    : Natural;
   end record;
   type Thread_Listing_Arr is array (Natural range <>) of Thread_Listing;

   type Cluster_Listing is record
      Cluster      : TCID;
      Cluster_Algo : Cluster_Algorithm;
      Cluster_Int  : Boolean;
      Cluster_Quan : Natural;
   end record;
   type Cluster_Listing_Arr is array (Natural range <>) of Cluster_Listing;

   --  List all threads on the system.
   --  @param List  Where to write all the thread information.
   --  @param Total Total count of processes, even if it is > List'Length.
   procedure List_All (List : out Thread_Listing_Arr; Total : out Natural);
   procedure List_All (List : out Cluster_Listing_Arr; Total : out Natural);

private

   type TID  is new Natural range 0 .. 100;
   type TCID is new Natural range 0 ..  20;
   Error_TID  : constant  TID := 0;
   Error_TCID : constant TCID := 0;

   Is_Initialized : Boolean
      with Atomic, Volatile, Async_Readers => True, Async_Writers => True,
           Effective_Reads => True, Effective_Writes => True;

   procedure Waiting_Spot with No_Return;

   function Has_Available_Time (C : TCID) return Boolean;

   function Is_Switchable (T : TID; C : TCID) return Boolean;

   procedure Add_Bucket_And_Shift (Last_Bucket : Unsigned_32);

   function Convert (Thread : TID) return Natural is (Natural (Thread));
   function Convert (Group : TCID) return Natural is (Natural (Group));
   function Convert (Value : Natural) return TID is
      ((if Value > Natural (TID'Last) then Error_TID else TID (Value)));
   function Convert (Value : Natural) return TCID is
      ((if Value > Natural (TCID'Last) then Error_TCID else TCID (Value)));
end Scheduler;
