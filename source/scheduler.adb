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

package body Scheduler with SPARK_Mode => Off is
   --  Thread information.
   type Thread_Info is record
      State         : aliased Arch.Context.GP_Context;
      Is_Present    : Boolean;
      Is_Banned     : Boolean;
      Is_Running    : Boolean;
      Is_Monothread : Boolean;
      Is_Real_Time  : Boolean;
      Priority      : Integer;
      TCB_Pointer   : System.Address;
      PageMap       : Memory.Virtual.Page_Map_Acc;
      Stack         : Virtual_Address;
      Kernel_Stack  : Virtual_Address;
      FP_Region     : aliased Arch.Context.FP_Context;
      Process       : Userland.Process.Process_Data_Acc;
   end record;
   type Thread_Info_Arr is array (TID range 1 .. 256) of Thread_Info;

   --  Scheduler information.
   Scheduler_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Thread_Pool     : access Thread_Info_Arr;

   --  Time slices assigned to each thread depending on preference.
   --  (In microseconds).
   Priority_Slices : constant array (-20 .. 19) of Natural := (
      -20 .. -15 => 30000, -- Critical, this MUST run.
      -14 .. -10 => 20000, -- High priority.
       -9 ..  -1 => 15000, -- Highish priority, will run for a bit.
               0 => 10000, -- Average.
        1 ..   9 =>  8000, -- Lowish priority.
       10 ..  15 =>  6000, -- Low priority.
       16 ..  19 =>  5000  -- Piss priority.
   );

   --  Stack size of new threads.
   Stack_Size : constant := 16#200000#;

   function Init return Boolean is
   begin
      --  Allocate core locals and finishing touches.
      Thread_Pool := new Thread_Info_Arr;
      Is_Initialized := True;
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return True;
   end Init;

   procedure Idle_Core is
   begin
      while not Is_Initialized loop null; end loop;
      Arch.Local.Reschedule_In (Priority_Slices (0));
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
      Lib.Synchronization.Seize (Scheduler_Mutex'Access);

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
         Thread_Pool (New_TID).Is_Banned     := False;
         Thread_Pool (New_TID).Is_Running    := False;
         Thread_Pool (New_TID).Is_Monothread := False;
         Thread_Pool (New_TID).Is_Real_Time  := False;
         Thread_Pool (New_TID).Priority      := 0;
         Thread_Pool (New_TID).PageMap       := Map;
         Thread_Pool (New_TID).Stack         := User_Stack_Addr;
         Thread_Pool (New_TID).Kernel_Stack  := Kernel_Stack_Addr;
         Thread_Pool (New_TID).Process    := Userland.Process.Get_By_PID (PID);

         --  Map the user stack.
         if not Memory.Virtual.Map_Range (
            Thread_Pool (New_TID).PageMap,
            Virtual_Address (Stack_Top),
            To_Integer (User_Stack_8.all'Address) - Memory_Offset,
            Stack_Size,
            Stack_Permissions
         )
         then
            Lib.Panic.Soft_Panic ("Could not map a TID stack");
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
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
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
      Lib.Synchronization.Seize (Scheduler_Mutex'Access);

      --  Find a new TID.
      New_TID := Find_Free_TID;
      if New_TID = 0 then
         goto End_Return;
      end if;

      --  Copy the state.
      Thread_Pool (New_TID).Is_Present    := True;
      Thread_Pool (New_TID).Is_Banned     := False;
      Thread_Pool (New_TID).Is_Running    := False;
      Thread_Pool (New_TID).Is_Monothread := False;
      Thread_Pool (New_TID).Is_Real_Time  := False;
      Thread_Pool (New_TID).PageMap       := Map;
      Thread_Pool (New_TID).Priority      := 0;
      Thread_Pool (New_TID).Kernel_Stack  := Kernel_Stack_Addr;
      Thread_Pool (New_TID).TCB_Pointer   := Arch.Local.Fetch_TCB;
      Thread_Pool (New_TID).State         := State.all;
      Thread_Pool (New_TID).State.RAX     := 0;
      Thread_Pool (New_TID).Process       := Userland.Process.Get_By_PID (PID);
      Arch.Context.Save_FP_Context (Thread_Pool (New_TID).FP_Region'Access);

   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return New_TID;
   end Create_User_Thread;

   procedure Delete_Thread (Thread : TID) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Thread_Pool (Thread).Is_Present := False;
         if Thread_Pool (Thread).Is_Running then
            Arch.Local.Reschedule_Cores_ASAP (Thread);
         end if;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
   end Delete_Thread;

   procedure Ban_Thread (Thread : TID; Is_Banned : Boolean) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Thread_Pool (Thread).Is_Banned := Is_Banned;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
   end Ban_Thread;

   function Get_Thread_Priority (Thread : TID) return Integer is
   begin
      if Is_Thread_Present (Thread) then
         return Thread_Pool (Thread).Priority;
      else
         return -1;
      end if;
   end Get_Thread_Priority;

   procedure Set_Thread_Priority (Thread : TID; Priority : Integer) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         if Priority > Priority_Slices'Last then
            Thread_Pool (Thread).Priority := Priority_Slices'Last;
         elsif Priority < Priority_Slices'First then
            Thread_Pool (Thread).Priority := Priority_Slices'First;
         else
            Thread_Pool (Thread).Priority := Priority;
         end if;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
   end Set_Thread_Priority;

   function Is_Mono_Thread (Thread : TID) return Boolean is
      Ret : Boolean := False;
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Ret := Thread_Pool (Thread).Is_Monothread;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
      return Ret;
   end Is_Mono_Thread;

   procedure Set_Mono_Thread (Thread : TID; Is_Mono : Boolean) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Thread_Pool (Thread).Is_Monothread := Is_Mono;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
   end Set_Mono_Thread;

   function Is_RT_Thread (Thread : TID) return Boolean is
      Ret : Boolean := False;
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Ret := Thread_Pool (Thread).Is_Real_Time;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
      return Ret;
   end Is_RT_Thread;

   procedure Set_RT_Thread (Thread : TID; Is_RT : Boolean) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Thread_Pool (Thread).Is_Real_Time := Is_RT;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
   end Set_RT_Thread;

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
      for I in Thread_Pool'First .. Thread_Pool'Last loop
         if not Thread_Pool (I).Is_Present then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free_TID;

   procedure Scheduler_ISR (State : not null Arch.Context.GP_Context_Acc) is
      Current_TID, Next_TID : TID;
      Rearm_Period : Natural := Priority_Slices (0);
   begin
      if Lib.Synchronization.Try_Seize (Scheduler_Mutex'Access) = False then
         Arch.Local.Reschedule_In (Rearm_Period);
         return;
      end if;

      --  Get the next thread for execution.
      Current_TID := Arch.Local.Get_Current_Thread;
      Next_TID    := 0;
      for I in Current_TID + 1 .. Thread_Pool'Last loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Banned and
            not Thread_Pool (I).Is_Running and I /= Current_TID
         then
            Next_TID := I;
            goto Found_TID;
         end if;
      end loop;
      for I in Thread_Pool'First .. Current_TID loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Banned and
            not Thread_Pool (I).Is_Running and I /= Current_TID
         then
            Next_TID := I;
            goto Found_TID;
         end if;
      end loop;

      --  Rearm for the next attempt.
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      if Current_TID /= 0 then
         Rearm_Period := Priority_Slices (Thread_Pool (Current_TID).Priority);
      end if;
      Arch.Local.Reschedule_In (Rearm_Period);
      return;

   <<Found_TID>>
      --  Save state.
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

      --  Rearm the timer for next tick if we are not doing a monothread.
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      if not Thread_Pool (Next_TID).Is_Monothread then
         Arch.Local.Reschedule_In
            (Priority_Slices (Thread_Pool (Next_TID).Priority));
      end if;

      --  Reset state.
      if not Memory.Virtual.Make_Active (Thread_Pool (Next_TID).PageMap) then
         Lib.Panic.Soft_Panic ("Could not make reschedule map active");
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
