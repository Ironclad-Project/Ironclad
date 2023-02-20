--  scheduler.ads: Specification of the scheduler.
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

with System;
with Interfaces; use Interfaces;
with Memory.Virtual;
with Memory; use Memory;
with Userland;
with Userland.ELF;
with Arch.Context;

package Scheduler with SPARK_Mode => Off is
   --  True if the scheduler is initialized.
   Is_Initialized : Boolean with Volatile;

   --  Initialize the scheduler, return true on success, false on failure.
   function Init return Boolean;

   --  Use when doing nothing and we want the scheduler to put us to work.
   --  Doubles as the function to initialize core locals.
   procedure Idle_Core with No_Return;

   --  Creates a userland thread, and queues it for execution.
   --  Return thread ID or 0 on failure.
   type TID is new Natural;
   function Create_User_Thread
      (Address    : Virtual_Address;
       Args       : Userland.Argument_Arr;
       Env        : Userland.Environment_Arr;
       Map        : Memory.Virtual.Page_Map_Acc;
       Vector     : Userland.ELF.Auxval;
       Stack_Top  : Unsigned_64;
       PID        : Natural;
       Exec_Stack : Boolean := True) return TID;

   --  Create a userland thread with no arguments.
   function Create_User_Thread
      (Address    : Virtual_Address;
       Map        : Memory.Virtual.Page_Map_Acc;
       Stack_Addr : Unsigned_64;
       TLS_Addr   : Unsigned_64;
       PID        : Natural) return TID;

   --  Create a user thread with a context.
   function Create_User_Thread
      (GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Map      : Memory.Virtual.Page_Map_Acc;
       PID      : Natural;
       TCB      : System.Address) return TID;

   --  Removes a thread, kernel or user, from existance (if it exists).
   procedure Delete_Thread (Thread : TID);

   --  Change run time and period of a thread (in ns).
   function Set_Deadlines
      (Thread : TID; Run_Time, Period : Positive) return Boolean;

   --  Set whether a thread is real time or not, along with monothreading.
   --  Real time thread semantics are described in the documentation.
   function Is_Mono_Thread (Thread : TID) return Boolean;
   procedure Set_Mono_Thread (Thread : TID; Is_Mono : Boolean);

   --  Give up the rest of our execution time for some other process.
   procedure Yield;

   --  Make the callee thread be dequed.
   procedure Bail with No_Return;

   --  Hook to be called by the architecture for reescheduling of the callee
   --  core.
   procedure Scheduler_ISR (State : Arch.Context.GP_Context);

private

   function Update_Priorities return Boolean;
   function Is_Thread_Present (Thread : TID) return Boolean;
end Scheduler;
