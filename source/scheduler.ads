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
   procedure Idle_Core;

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

   --  Create a user thread with a context.
   function Create_User_Thread
      (State : Arch.Context.GP_Context_Acc;
       Map   : Memory.Virtual.Page_Map_Acc;
       PID   : Natural) return TID;

   --  Removes a thread, kernel or user, from existance (if it exists).
   procedure Delete_Thread (Thread : TID);

   --  Sets whether a thread is allowed to execute or not (if it exists).
   procedure Ban_Thread (Thread : TID; Is_Banned : Boolean);

   --  Get and set the priority of a thread.
   function Get_Thread_Priority (Thread : TID) return Integer;
   procedure Set_Thread_Priority (Thread : TID; Priority : Integer);

   --  Set whether a thread is real time or not, along with monothreading.
   --  Real time thread semantics are described in the documentation.
   function Is_Mono_Thread (Thread : TID) return Boolean;
   procedure Set_Mono_Thread (Thread : TID; Is_Mono : Boolean);
   function Is_RT_Thread (Thread : TID) return Boolean;
   procedure Set_RT_Thread (Thread : TID; Is_RT : Boolean);

   --  Give up the rest of our execution time for some other process.
   procedure Yield;

   --  Delete and yield the current thread.
   procedure Bail;

   --  Hook to be called by the architecture for reescheduling of the callee
   --  core.
   procedure Scheduler_ISR (State : not null Arch.Context.GP_Context_Acc);

private

   function Find_Free_TID return TID;
   function Is_Thread_Present (Thread : TID) return Boolean;
end Scheduler;
