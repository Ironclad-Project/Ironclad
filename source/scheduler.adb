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

with System;
with Lib.Synchronization;
with Lib.Panic;
with System.Storage_Elements; use System.Storage_Elements;
with Userland.Process;
with Arch;
with Arch.MMU;
with Arch.Local;
with Arch.Snippets;
with Lib.Messages;
with Lib;

package body Scheduler with SPARK_Mode => Off is
   --  Thread information.
   type Thread_Info is record
      State          : aliased Arch.Context.GP_Context;
      Is_Present     : Boolean;
      Is_Running     : Boolean;
      Is_Monothread  : Boolean;
      TCB_Pointer    : System.Address;
      PageMap        : Memory.Virtual.Page_Map_Acc;
      Stack          : Virtual_Address;
      Kernel_Stack   : Virtual_Address;
      FP_Region      : aliased Arch.Context.FP_Context;
      Process        : Userland.Process.Process_Data_Acc;
      Time_Since_Run : Natural;
      Priority       : Positive;
      Run_Time       : Positive;
      Period         : Positive;
   end record;
   type Thread_Info_Arr is array (TID range 1 .. 50) of Thread_Info;

   --  Storing scheduler information independent from scheduling algo.
   Scheduler_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Thread_Pool     : access Thread_Info_Arr;

   --  Default period and run time.
   Default_Run_Time : constant :=  3000;
   Default_Period   : constant := 10000;

   --  Stack size of new threads.
   Stack_Size : constant := 16#200000#;

   function Init return Boolean is
   begin
      --  Allocate core locals and finishing touches.
      Thread_Pool := new Thread_Info_Arr;
      Is_Initialized := True;
      Lib.Synchronization.Release (Scheduler_Mutex);
      return True;
   end Init;

   procedure Idle_Core is
   begin
      while not Is_Initialized loop null; end loop;
      Arch.Local.Reschedule_In (Default_Run_Time);
      Arch.Snippets.Enable_Interrupts;
      loop Arch.Snippets.Wait_For_Interrupt; end loop;
   end Idle_Core;

   function Create_User_Thread
      (Address    : Virtual_Address;
       Args       : Userland.Argument_Arr;
       Env        : Userland.Environment_Arr;
       Map        : Memory.Virtual.Page_Map_Acc;
       Vector     : Userland.ELF.Auxval;
       Stack_Top  : Unsigned_64;
       PID        : Natural;
       Exec_Stack : Boolean := True) return TID
   is
      New_TID : TID;
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);

      --  Find a new TID.
      New_TID := Find_Free_TID;
      if New_TID = 0 then
         goto End_Return;
      end if;

      --  Initialize thread state.
      declare
         type Stack8  is array (1 ..       Stack_Size) of Unsigned_8;
         type Stack64 is array (1 .. (Stack_Size / 8)) of Unsigned_64;
         type Stack_Acc is access Stack8;

         User_Stack_8  : constant Stack_Acc := new Stack8;
         User_Stack_64 : Stack64 with Address => User_Stack_8.all'Address;
         User_Stack_Addr : constant Virtual_Address :=
            To_Integer (User_Stack_8.all'Address) + Stack_Size;

         Kernel_Stack : constant Stack_Acc := new Stack8;
         Kernel_Stack_Addr : constant Virtual_Address :=
            To_Integer (Kernel_Stack.all'Address) + Stack_Size;

         Index_8  : Natural := User_Stack_8'Last;
         Index_64 : Natural := User_Stack_8'Last;

         Stack_Permissions : constant Arch.MMU.Page_Permissions := (
            User_Accesible => True,
            Read_Only      => False,
            Executable     => Exec_Stack,
            Global         => False,
            Write_Through  => False
         );
      begin
         Thread_Pool (New_TID).Is_Present    := True;
         Thread_Pool (New_TID).Is_Running    := False;
         Thread_Pool (New_TID).Is_Monothread := False;
         Thread_Pool (New_TID).PageMap       := Map;
         Thread_Pool (New_TID).Stack         := User_Stack_Addr;
         Thread_Pool (New_TID).Kernel_Stack  := Kernel_Stack_Addr;
         Thread_Pool (New_TID).Process    := Userland.Process.Get_By_PID (PID);
         Thread_Pool (New_TID).Run_Time := Default_Run_Time;
         Thread_Pool (New_TID).Period   := Default_Period;
         if not Update_Priorities then
            Thread_Pool (New_TID).Is_Present := False;
            return 0;
         end if;

         --  Map the user stack.
         if not Memory.Virtual.Map_Range (
            Thread_Pool (New_TID).PageMap,
            Virtual_Address (Stack_Top),
            To_Integer (User_Stack_8.all'Address) - Memory_Offset,
            Stack_Size,
            Stack_Permissions
         )
         then
            New_TID := 0;
            goto End_Return;
         end if;

         --  Set up FPU control word and MXCSR as defined by SysV.
         Arch.Context.Init_FP_Context (Thread_Pool (New_TID).FP_Region'Access);

         --  Load env into the stack.
         for En of reverse Env loop
            User_Stack_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
            for C of reverse En.all loop
               User_Stack_8 (Index_8) := Character'Pos (C);
               Index_8 := Index_8 - 1;
            end loop;
         end loop;

         --  Load argv into the stack.
         for Arg of reverse Args loop
            User_Stack_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
            for C of reverse Arg.all loop
               User_Stack_8 (Index_8) := Character'Pos (C);
               Index_8 := Index_8 - 1;
            end loop;
         end loop;

         --  Get the equivalent 64-bit stack index and align it to 16 bytes.
         Index_64 := (Index_8 / 8) - ((Index_8 / 8) mod 16);
         Index_64 := Index_64 - ((Args'Length + Env'Length + 3) mod 2);

         --  Load auxval.
         User_Stack_64 (Index_64 - 0) := 0;
         User_Stack_64 (Index_64 - 1) := Userland.ELF.Auxval_Null;
         User_Stack_64 (Index_64 - 2) := Vector.Entrypoint;
         User_Stack_64 (Index_64 - 3) := Userland.ELF.Auxval_Entrypoint;
         User_Stack_64 (Index_64 - 4) := Vector.Program_Headers;
         User_Stack_64 (Index_64 - 5) := Userland.ELF.Auxval_Program_Headers;
         User_Stack_64 (Index_64 - 6) := Vector.Program_Header_Count;
         User_Stack_64 (Index_64 - 7) := Userland.ELF.Auxval_Header_Count;
         User_Stack_64 (Index_64 - 8) := Vector.Program_Header_Size;
         User_Stack_64 (Index_64 - 9) := Userland.ELF.Auxval_Header_Size;
         Index_64 := Index_64 - 10;

         --  Load envp taking into account the pointers at the beginning.
         Index_8 := User_Stack_8'Last;
         User_Stack_64 (Index_64) := 0; --  Null at the end of envp.
         Index_64 := Index_64 - 1;
         for En of reverse Env loop
            Index_8 := (Index_8 - En.all'Length) - 1;
            User_Stack_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Load argv into the stack.
         User_Stack_64 (Index_64) := 0; --  Null at the end of argv.
         Index_64 := Index_64 - 1;
         for Arg of reverse Args loop
            Index_8 := (Index_8 - Arg.all'Length) - 1;
            User_Stack_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Write argc and we are done!
         User_Stack_64 (Index_64) := Args'Length;
         Index_64 := Index_64 - 1;

         --  Initialize context information.
         Arch.Context.Init_GP_Context (
            Thread_Pool (New_TID).State'Access,
            To_Address
               (Integer_Address (Stack_Top + (Unsigned_64 (Index_64) * 8))),
            To_Address (Address)
         );
      end;

   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex);
      return New_TID;
   end Create_User_Thread;

   function Create_User_Thread
      (State : Arch.Context.GP_Context_Acc;
       Map   : Memory.Virtual.Page_Map_Acc;
       PID   : Natural) return TID
   is
      type Stack8    is array (1 .. Stack_Size) of Unsigned_8;
      type Stack_Acc is access Stack8;

      New_TID           : TID := 0;
      Kernel_Stack      : constant Stack_Acc := new Stack8;
      Kernel_Stack_Addr : constant Virtual_Address :=
            To_Integer (Kernel_Stack.all'Address) + Stack_Size;
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);

      --  Find a new TID.
      New_TID := Find_Free_TID;
      if New_TID = 0 then
         goto End_Return;
      end if;

      --  Copy the state.
      Thread_Pool (New_TID).Is_Present    := True;
      Thread_Pool (New_TID).Is_Running    := False;
      Thread_Pool (New_TID).Is_Monothread := False;
      Thread_Pool (New_TID).PageMap       := Map;
      Thread_Pool (New_TID).Kernel_Stack  := Kernel_Stack_Addr;
      Thread_Pool (New_TID).TCB_Pointer   := Arch.Local.Fetch_TCB;
      Thread_Pool (New_TID).State         := State.all;
      Thread_Pool (New_TID).State.RAX     := 0;
      Thread_Pool (New_TID).Process       := Userland.Process.Get_By_PID (PID);
      Thread_Pool (New_TID).Run_Time      := Default_Run_Time;
      Thread_Pool (New_TID).Period        := Default_Period;
      Arch.Context.Save_FP_Context (Thread_Pool (New_TID).FP_Region'Access);
      if not Update_Priorities then
         Thread_Pool (New_TID).Is_Present := False;
         return 0;
      end if;

   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex);
      return New_TID;
   end Create_User_Thread;

   procedure Delete_Thread (Thread : TID) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex);
         Thread_Pool (Thread).Is_Present := False;
         if Thread_Pool (Thread).Is_Running then
            Arch.Local.Reschedule_Cores_ASAP (Thread);
         end if;
         Lib.Synchronization.Release (Scheduler_Mutex);
      end if;
   end Delete_Thread;

   function Set_Deadlines
      (Thread : TID; Run_Time, Period : Positive) return Boolean
   is
      Ret : Boolean;
      Old_Run_Time, Old_Period : Positive;
   begin
      if Run_Time > Period or not Is_Thread_Present (Thread) then
         return False;
      end if;

      Lib.Synchronization.Seize (Scheduler_Mutex);
      Old_Run_Time := Thread_Pool (Thread).Run_Time;
      Old_Period   := Thread_Pool (Thread).Period;
      Thread_Pool (Thread).Run_Time := Run_Time;
      Thread_Pool (Thread).Period   := Period;

      Ret := Update_Priorities;
      if not Ret then
         Thread_Pool (Thread).Run_Time := Old_Run_Time;
         Thread_Pool (Thread).Period   := Old_Period;
      end if;

      Lib.Synchronization.Release (Scheduler_Mutex);
      return Ret;
   end Set_Deadlines;

   function Is_Mono_Thread (Thread : TID) return Boolean is
      Ret : Boolean := False;
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex);
         Ret := Thread_Pool (Thread).Is_Monothread;
         Lib.Synchronization.Release (Scheduler_Mutex);
      end if;
      return Ret;
   end Is_Mono_Thread;

   procedure Set_Mono_Thread (Thread : TID; Is_Mono : Boolean) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex);
         Thread_Pool (Thread).Is_Monothread := Is_Mono;
         Lib.Synchronization.Release (Scheduler_Mutex);
      end if;
   end Set_Mono_Thread;

   procedure Yield is
   begin
      Arch.Local.Reschedule_ASAP;
   end Yield;

   procedure Bail is
   begin
      Delete_Thread (Arch.Local.Get_Current_Thread);
      Arch.Local.Set_Current_Thread (0);
      Idle_Core;
   end Bail;

   function Find_Free_TID return TID is
   begin
      for I in Thread_Pool'Range loop
         if not Thread_Pool (I).Is_Present then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free_TID;

   function Update_Priorities return Boolean is
      Periods_LCM    : Positive := 1;
      Total_Run_Time : Natural  := 0;
      Temp           : Natural;
   begin
      --  Find period LCM, lowest period, and total run time of all available
      --  tasks.
      for Th of Thread_Pool.all loop
         if Th.Is_Present then
            Periods_LCM := Lib.Least_Common_Multiple (Periods_LCM, Th.Period);
            Total_Run_Time := Total_Run_Time + Th.Run_Time;
         end if;
      end loop;

      --  Check validity using Least upper bound -> Inf = ln 2 = (aprox 69%).
      Temp := Total_Run_Time / (Periods_LCM / 100);
      if Temp > 69 then
         Lib.Messages.Put      (Temp);
         Lib.Messages.Put_Line ("% > 69% (Least upper bound -> Inf)");
         Lib.Messages.Put_Line ("Tasks might not be schedulable");
      end if;

      --  Now that we know all the data is valid, update the priorities.
      --  The lower the period, the higher the priority.
      for Th of Thread_Pool.all loop
         if Th.Is_Present then
            Th.Time_Since_Run := 0;
            Th.Priority := Periods_LCM / Th.Period;
         end if;
      end loop;

      return True;
   end Update_Priorities;

   procedure Scheduler_ISR (State : not null Arch.Context.GP_Context_Acc) is
      Did_Seize          : Boolean;
      Current_TID        : constant TID := Arch.Local.Get_Current_Thread;
      Next_TID           : TID          := Current_TID;
      Current_Increment  : Natural := 0;
      Max_Available_Prio : Natural := 0;
   begin
      --  Get how much time we come from running and update time since run.
      if Current_TID /= 0 then
         Current_Increment := Thread_Pool (Current_TID).Run_Time;
      else
         Current_Increment := Default_Run_Time;
      end if;

      --  Try to lock before we do modifications.
      Lib.Synchronization.Try_Seize (Scheduler_Mutex, Did_Seize);
      if not Did_Seize then
         Arch.Local.Reschedule_In (Current_Increment);
         return;
      end if;

      --  Get the next thread for execution by searching on the thread pool
      --  for the thread that can run and has the highest priority that is not
      --  the current one. In the same sweep, update time since last run.
      for I in Thread_Pool'Range loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running then
            Thread_Pool (I).Time_Since_Run := Thread_Pool (I).Time_Since_Run
                                            + Current_Increment;

            if Thread_Pool (I).Priority > Max_Available_Prio and
               Thread_Pool (I).Time_Since_Run >= Thread_Pool (I).Period
            then
               Next_TID := I;
               Max_Available_Prio := Thread_Pool (I).Priority;
            end if;
         end if;
      end loop;

      --  Rearm for the next attempt if there are no available threads.
      if Next_TID = Current_TID then
         Lib.Synchronization.Release (Scheduler_Mutex);
         Arch.Local.Reschedule_In (Current_Increment);
         return;
      end if;

      --  We found a suitable TID, so we save state and start context switch.
      if Current_TID /= 0 then
         Thread_Pool (Current_TID).Is_Running  := False;
         Thread_Pool (Current_TID).TCB_Pointer := Arch.Local.Fetch_TCB;
         Thread_Pool (Current_TID).State       := State.all;
         Arch.Context.Save_FP_Context
            (Thread_Pool (Current_TID).FP_Region'Access);
      end if;

      --  Assign the next TID as our current one.
      Arch.Local.Set_Current_Thread (Next_TID);
      Thread_Pool (Next_TID).Is_Running := True;
      Thread_Pool (Next_TID).Time_Since_Run := Thread_Pool (Next_TID).Run_Time;

      --  Rearm the timer for next tick if we are not doing a monothread.
      Lib.Synchronization.Release (Scheduler_Mutex);
      if not Thread_Pool (Next_TID).Is_Monothread then
         Arch.Local.Reschedule_In (Thread_Pool (Next_TID).Run_Time);
      end if;

      --  Reset state.
      if not Memory.Virtual.Make_Active (Thread_Pool (Next_TID).PageMap) then
         Lib.Panic.Hard_Panic ("Could not make reschedule map active");
      end if;
      Arch.Local.Set_Current_Process (Thread_Pool (Next_TID).Process);
      Arch.Local.Set_Kernel_Stack
         (To_Address (Thread_Pool (Next_TID).Kernel_Stack));
      Arch.Local.Load_TCB (Thread_Pool (Next_TID).TCB_Pointer);
      Arch.Context.Load_FP_Context (Thread_Pool (Next_TID).FP_Region'Access);
      Arch.Context.Load_GP_Context (Thread_Pool (Next_TID).State'Access);
      loop Arch.Snippets.Wait_For_Interrupt; end loop;
   end Scheduler_ISR;

   function Is_Thread_Present (Thread : TID) return Boolean is
   begin
      return Thread /= 0                      and then
             Thread <= TID (Thread_Pool'Last) and then
             Thread_Pool (Thread).Is_Present;
   end Is_Thread_Present;
end Scheduler;
