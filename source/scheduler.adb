--  scheduler.adb: Thread scheduler.
--  Copyright (C) 2023 streaksu
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
with Arch.Local;
with Arch.Snippets;
with Lib;
with Lib.Messages;
with Arch.CPU;
with Ada.Unchecked_Deallocation;

package body Scheduler with SPARK_Mode => Off is
   Stack_Size  : constant := 16#20000#; --  Fat userland needs fat 128KiB.
   Kernel_Size : constant := 16#02000#; --  Fashionably small stack for the us!
   type Thread_Stack     is array (1 ..       Stack_Size) of Unsigned_8;
   type Thread_Stack_64  is array (1 .. (Stack_Size / 8)) of Unsigned_64;
   type Kernel_Stack     is array (1 ..      Kernel_Size) of Unsigned_8;
   type Kernel_Stack_Acc is access Kernel_Stack;
   procedure Free is new Ada.Unchecked_Deallocation
      (Kernel_Stack, Kernel_Stack_Acc);

   type Thread_Cluster is record
      Is_Present : Boolean;
      Algorithm  : Cluster_Algorithm;
      RR_Quantum : Natural;
      Percentage : Natural range 0 .. 100;
      Progress   : Natural range 0 .. 100;
   end record;
   type Cluster_Arr is array (TCID range 1 .. TCID'Last) of Thread_Cluster;
   type Cluster_Arr_Acc is access Cluster_Arr;

   type Thread_Info is record
      Is_Present    : Boolean;
      Is_Running    : Boolean;
      Cluster       : TCID;
      TCB_Pointer   : System.Address;
      PageMap       : Arch.MMU.Page_Table_Acc;
      Kernel_Stack  : Kernel_Stack_Acc;
      User_Stack    : Unsigned_64;
      GP_State      : Arch.Context.GP_Context;
      FP_State      : Arch.Context.FP_Context;
      Process       : Userland.Process.PID;
      Yield_Mutex   : aliased Lib.Synchronization.Binary_Semaphore;
   end record;
   type Thread_Info_Arr     is array (TID range 1 .. TID'Last) of Thread_Info;
   type Thread_Info_Arr_Acc is access Thread_Info_Arr;

   --  Storing scheduler information independent from scheduling algo.
   Scheduler_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Cluster_Pool    : Cluster_Arr_Acc;
   Thread_Pool     : Thread_Info_Arr_Acc;

   function Init return Boolean is
   begin
      --  Initialize registries.
      Thread_Pool  := new Thread_Info_Arr;
      Cluster_Pool := new Cluster_Arr;
      for Th of Thread_Pool.all loop
         Th.Is_Present   := False;
         Th.Kernel_Stack := null;
      end loop;
      for Cl of Cluster_Pool.all loop
         Cl.Is_Present := False;
      end loop;

      --  Create the default cluster.
      Cluster_Pool (Cluster_Pool'First).Is_Present := True;
      Cluster_Pool (Cluster_Pool'First).Algorithm  := Cluster_RR;
      Cluster_Pool (Cluster_Pool'First).RR_Quantum := 4000;
      Cluster_Pool (Cluster_Pool'First).Percentage := 100;

      Is_Initialized := True;
      Lib.Synchronization.Release (Scheduler_Mutex);
      return True;
   end Init;

   procedure Idle_Core is
   begin
      while not Is_Initialized loop Arch.Snippets.Pause; end loop;
      Arch.Local.Reschedule_ASAP;
      Arch.Snippets.Enable_Interrupts;
      loop Arch.Snippets.Wait_For_Interrupt; end loop;
   end Idle_Core;

   function Create_User_Thread
      (Address : Virtual_Address;
       Args    : Userland.Argument_Arr;
       Env     : Userland.Environment_Arr;
       Map     : Arch.MMU.Page_Table_Acc;
       Vector  : Userland.ELF.Auxval;
       Cluster : TCID;
       PID     : Natural) return TID
   is
      Stack_Permissions : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accesible => True,
          Can_Read          => True,
          Can_Write         => True,
          Can_Execute       => False,
          Is_Global         => False,
          Is_Write_Combine  => False);

      New_TID   : TID;
      GP_State  : Arch.Context.GP_Context;
      FP_State  : Arch.Context.FP_Context;
      Result    : System.Address;
      Stack_Top : Unsigned_64;
      Success   : Boolean;
   begin
      --  Initialize thread state. Start by mapping the user stack.
      Stack_Top := Userland.Process.Get_Stack_Base
         (Userland.Process.Convert (PID));
      Userland.Process.Set_Stack_Base
         (Userland.Process.Convert (PID), Stack_Top + Stack_Size);
      Arch.MMU.Map_Allocated_Range
         (Map            => Map,
          Physical_Start => Result,
          Virtual_Start  => To_Address (Virtual_Address (Stack_Top)),
          Length         => Stack_Size,
          Permissions    => Stack_Permissions,
          Success        => Success);
      if not Success then
         goto Error_1;
      end if;

      declare
         Stk_8  : Thread_Stack    with Import, Address => Result;
         Stk_64 : Thread_Stack_64 with Import, Address => Result;
         Index_8  : Natural := Stk_8'Last;
         Index_64 : Natural := Stk_64'Last;
      begin
         --  Load env into the stack.
         for En of reverse Env loop
            Stk_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
            for C of reverse En.all loop
               Stk_8 (Index_8) := Character'Pos (C);
               Index_8 := Index_8 - 1;
            end loop;
         end loop;

         --  Load argv into the stack.
         for Arg of reverse Args loop
            Stk_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
            for C of reverse Arg.all loop
               Stk_8 (Index_8) := Character'Pos (C);
               Index_8 := Index_8 - 1;
            end loop;
         end loop;

         --  Get the equivalent 64-bit stack index and align it to 16 bytes.
         Index_64 := (Index_8 / 8) - ((Index_8 / 8) mod 16);
         Index_64 := Index_64 - ((Args'Length + Env'Length + 3) mod 2);

         --  Load auxval.
         Stk_64 (Index_64 - 0) := 0;
         Stk_64 (Index_64 - 1) := Userland.ELF.Auxval_Null;
         Stk_64 (Index_64 - 2) := Vector.Entrypoint;
         Stk_64 (Index_64 - 3) := Userland.ELF.Auxval_Entrypoint;
         Stk_64 (Index_64 - 4) := Vector.Program_Headers;
         Stk_64 (Index_64 - 5) := Userland.ELF.Auxval_Program_Headers;
         Stk_64 (Index_64 - 6) := Vector.Program_Header_Count;
         Stk_64 (Index_64 - 7) := Userland.ELF.Auxval_Header_Count;
         Stk_64 (Index_64 - 8) := Vector.Program_Header_Size;
         Stk_64 (Index_64 - 9) := Userland.ELF.Auxval_Header_Size;
         Index_64 := Index_64 - 10;

         --  Load envp taking into account the pointers at the beginning.
         Index_8 := Stk_8'Last;
         Stk_64 (Index_64) := 0; --  Null at the end of envp.
         Index_64 := Index_64 - 1;
         for En of reverse Env loop
            Index_8 := (Index_8 - En.all'Length) - 1;
            Stk_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Load argv into the stack.
         Stk_64 (Index_64) := 0; --  Null at the end of argv.
         Index_64 := Index_64 - 1;
         for Arg of reverse Args loop
            Index_8 := (Index_8 - Arg.all'Length) - 1;
            Stk_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Write argc and we are done!
         Stk_64 (Index_64) := Args'Length;
         Index_64 := Index_64 - 1;

         --  Initialize context information.
         Arch.Context.Init_GP_Context
            (GP_State,
             To_Address (Integer_Address (Stack_Top +
                                          Unsigned_64 (Index_64 * 8))),
             To_Address (Address));
         Arch.Context.Init_FP_Context (FP_State);

         New_TID := Create_User_Thread
            (GP_State => GP_State,
             FP_State => FP_State,
             Map       => Map,
             PID       => PID,
             Cluster   => Cluster,
             TCB       => System.Null_Address);
         if New_TID /= Error_TID then
            return New_TID;
         end if;
      end;

   <<Error_1>>
      return Error_TID;
   end Create_User_Thread;

   function Create_User_Thread
      (Address    : Virtual_Address;
       Map        : Arch.MMU.Page_Table_Acc;
       Stack_Addr : Unsigned_64;
       TLS_Addr   : Unsigned_64;
       Cluster    : TCID;
       PID        : Natural) return TID
   is
      GP_State : Arch.Context.GP_Context;
      FP_State : Arch.Context.FP_Context;
   begin
      Arch.Context.Init_GP_Context
         (GP_State,
          To_Address (Integer_Address (Stack_Addr)),
          To_Address (Address));
      Arch.Context.Init_FP_Context (FP_State);
      return Create_User_Thread
         (GP_State => GP_State,
          FP_State => FP_State,
          Map      => Map,
          PID      => PID,
          Cluster  => Cluster,
          TCB      => To_Address (Integer_Address (TLS_Addr)));
   end Create_User_Thread;

   function Create_User_Thread
      (GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Map      : Arch.MMU.Page_Table_Acc;
       Cluster  : TCID;
       PID      : Natural;
       TCB      : System.Address) return TID
   is
      New_TID : TID := Error_TID;
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);

      --  Find a new TID.
      for I in Thread_Pool'Range loop
         if not Thread_Pool (I).Is_Present then
            New_TID := I;
            goto Found_TID;
         end if;
      end loop;
      goto End_Return;

   <<Found_TID>>
      Thread_Pool (New_TID).Is_Present := True;
      Thread_Pool (New_TID).Is_Running := False;
      Thread_Pool (New_TID).Cluster := Cluster;
      Thread_Pool (New_TID).PageMap := Map;
      Thread_Pool (New_TID).Kernel_Stack := new Kernel_Stack;
      Thread_Pool (New_TID).User_Stack := 0;
      Thread_Pool (New_TID).TCB_Pointer := TCB;
      Thread_Pool (New_TID).GP_State := GP_State;
      Thread_Pool (New_TID).FP_State := FP_State;
      Thread_Pool (New_TID).Process := Userland.Process.Convert (PID);
      Lib.Synchronization.Release (Thread_Pool (New_TID).Yield_Mutex);

      Arch.Context.Success_Fork_Result (Thread_Pool (New_TID).GP_State);

   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex);
      return New_TID;
   end Create_User_Thread;

   procedure Delete_Thread (Thread : TID) is
   begin
      if Thread_Pool (Thread).Is_Present then
         Lib.Synchronization.Seize (Scheduler_Mutex);
         Thread_Pool (Thread).Is_Present := False;
         Lib.Synchronization.Release (Scheduler_Mutex);
      end if;
   end Delete_Thread;

   procedure Yield is
      Curr_TID : constant TID := Arch.Local.Get_Current_Thread;
   begin
      if Curr_TID /= Error_TID then
         Lib.Synchronization.Seize (Thread_Pool (Curr_TID).Yield_Mutex);
         Arch.Local.Reschedule_ASAP;
         Lib.Synchronization.Seize   (Thread_Pool (Curr_TID).Yield_Mutex);
         Lib.Synchronization.Release (Thread_Pool (Curr_TID).Yield_Mutex);
      end if;
   end Yield;

   procedure Bail is
   begin
      Arch.Snippets.Disable_Interrupts;
      Delete_Thread (Arch.Local.Get_Current_Thread);
      Idle_Core;
   end Bail;
   ----------------------------------------------------------------------------
   function Set_Scheduling_Algorithm
      (Cluster : TCID;
       Algo    : Cluster_Algorithm;
       Quantum : Natural) return Boolean
   is
   begin
      Cluster_Pool (Cluster).Algorithm  := Algo;
      Cluster_Pool (Cluster).RR_Quantum := Quantum;
      return True;
   end Set_Scheduling_Algorithm;

   function Set_Time_Slice (Cluster : TCID; Per : Natural) return Boolean is
      Consumed : Natural := 0;
   begin
      for I in Cluster_Pool'Range loop
         if Cluster_Pool (I).Is_Present and I /= Cluster then
            Consumed := Consumed + Cluster_Pool (I).Percentage;
         end if;
      end loop;

      if Cluster_Pool (Cluster).Is_Present and Per <= (100 - Consumed) then
         Cluster_Pool (Cluster).Percentage := Per;
         return True;
      else
         return False;
      end if;
   end Set_Time_Slice;

   function Create_Cluster return TCID is
   begin
      for I in Cluster_Pool'Range loop
         if not Cluster_Pool (I).Is_Present then
            Cluster_Pool (I) :=
               (Is_Present => True,
                Algorithm  => Cluster_RR,
                RR_Quantum => 4000,
                Percentage => 0,
                Progress   => 0);
            return I;
         end if;
      end loop;

      return Error_TCID;
   end Create_Cluster;

   function Delete_Cluster (Cluster : TCID) return Boolean is
   begin
      if Cluster_Pool (Cluster).Is_Present then
         Cluster_Pool (Cluster).Is_Present := False;
         return True;
      else
         return False;
      end if;
   end Delete_Cluster;
   ----------------------------------------------------------------------------
   procedure Scheduler_ISR (State : Arch.Context.GP_Context) is
      Current_TID  : constant TID := Arch.Local.Get_Current_Thread;
      Next_TID     :          TID := Error_TID;
      Curr_Cluster :         TCID := Error_TCID;
      Next_Cluster :         TCID := Error_TCID;
      Timeout      :      Natural;
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);

      --  Establish the current cluster, if any, and do thread preparations.
      if Current_TID /= Error_TID then
         Curr_Cluster := Thread_Pool (Current_TID).Cluster;
         Lib.Synchronization.Release (Thread_Pool (Current_TID).Yield_Mutex);
      end if;

      --  If we come from a cluster, and said cluster still has available time,
      --  we schedule a new thread from the cluster, if it has no time, search
      --  for another cluster with time, if none have time, idle.
      --
      --  If we come from no cluster, we just need to search for the first
      --  available task, and let the rest pick up from there.
      if Curr_Cluster /= Error_TCID then
         --  Find the next cluster.
         if Cluster_Pool (Curr_Cluster).Percentage >
            Cluster_Pool (Curr_Cluster).Progress
         then
            Next_Cluster := Curr_Cluster;
         else
            Lib.Messages.Warn ("We should not get here!");
         end if;

         --  Find the next thread from said cluster using the algorithm.
         --  Cluter_Cooperative finds new threads the same way as RR, so we
         --  do not need to handle it especially.
         for I in Current_TID + 1 .. Thread_Pool'Last loop
            if Thread_Pool (I).Is_Present     and
               not Thread_Pool (I).Is_Running and
               Thread_Pool (I).Cluster = Next_Cluster
            then
               Next_TID := I;
               goto Found_TID_TCID_Combo;
            end if;
         end loop;
         if Current_TID /= Error_TID then
            for I in Thread_Pool'First .. Current_TID loop
               if Thread_Pool (I).Is_Present     and
                  not Thread_Pool (I).Is_Running and
                  Thread_Pool (I).Cluster = Next_Cluster
               then
                  Next_TID := I;
                  goto Found_TID_TCID_Combo;
               end if;
            end loop;
            if Thread_Pool (Current_TID).Cluster = Next_Cluster then
               Next_TID := Current_TID;
               goto Found_TID_TCID_Combo;
            end if;
         end if;
      else
         for I in Thread_Pool'Range loop
            if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running
            then
               Next_TID := I;
               Next_Cluster := Thread_Pool (I).Cluster;
               goto Found_TID_TCID_Combo;
            end if;
         end loop;
      end if;

      Lib.Synchronization.Release (Scheduler_Mutex);
      Arch.Local.Reschedule_ASAP;
      return;

   <<Found_TID_TCID_Combo>>
      --  Get the timeout.
      case Cluster_Pool (Next_Cluster).Algorithm is
         when Cluster_RR => Timeout := Cluster_Pool (Next_Cluster).RR_Quantum;
         when Cluster_Cooperative => Timeout := 0;
      end case;

      --  Optimization if we are scheduling the same thread.
      if Current_TID = Next_TID then
         Arch.Local.Reschedule_In (Timeout);
         Lib.Synchronization.Release (Scheduler_Mutex);
         return;
      end if;

      --  Save state.
      if Current_TID /= Error_TID then
         Thread_Pool (Current_TID).Is_Running := False;

         if Thread_Pool (Current_TID).Is_Present then
            Thread_Pool (Current_TID).TCB_Pointer := Arch.Local.Fetch_TCB;
            Thread_Pool (Current_TID).GP_State    := State;
            Thread_Pool (Current_TID).User_Stack :=
                Arch.CPU.Get_Local.User_Stack;
            Arch.Context.Save_FP_Context (Thread_Pool (Current_TID).FP_State);
         elsif Thread_Pool (Current_TID).Kernel_Stack /= null then
            Free (Thread_Pool (Current_TID).Kernel_Stack);
         end if;
      end if;

      --  Assign the next TID.
      Arch.Local.Set_Current_Thread (Next_TID);
      Thread_Pool (Next_TID).Is_Running := True;

      --  Rearm the timer for next tick and unlock.
      Arch.Local.Reschedule_In (Timeout);
      Lib.Synchronization.Release (Scheduler_Mutex);

      --  Reset state.
      if not Arch.MMU.Make_Active (Thread_Pool (Next_TID).PageMap) then
         Lib.Panic.Hard_Panic ("Could not make reschedule map active");
      end if;
      Arch.Local.Set_Current_Process (Thread_Pool (Next_TID).Process);
      Arch.Local.Set_Stacks
         (To_Address (Integer_Address (Thread_Pool (Next_TID).User_Stack)),
          Thread_Pool (Next_TID).Kernel_Stack (Kernel_Stack'Last)'Address);
      Arch.Local.Load_TCB (Thread_Pool (Next_TID).TCB_Pointer);
      Arch.Context.Load_FP_Context (Thread_Pool (Next_TID).FP_State);
      Arch.Context.Load_GP_Context (Thread_Pool (Next_TID).GP_State);
   end Scheduler_ISR;
end Scheduler;
