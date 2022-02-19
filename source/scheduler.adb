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
with Arch.APIC;
with Arch.CPU;
with Arch.IDT;
with Arch.GDT;
with Arch.Wrappers;
with Lib.Synchronization;
with System.Machine_Code;     use System.Machine_Code;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;

package body Scheduler is
   --  Core locals.
   type Core_Local_Info is record
      LAPIC_Timer_Hz : Unsigned_64;
      Current_TID    : TID;
      Core_TSS       : Arch.GDT.TSS;
   end record;
   type Cores_Local_Info is array (Positive range <>) of Core_Local_Info;
   type Cores_Local_Info_Acc is access Cores_Local_Info;

   --  Thread information.
   type Thread_Info is record
      State        : Arch.Interrupts.ISR_GPRs;
      Is_Present   : Boolean;
      Is_Banned    : Boolean;
      Is_Running   : Boolean;
      Preference   : Unsigned_8;
      FS           : Unsigned_64;
      PageMap      : Memory.Virtual.Page_Map;
      Stack        : Virtual_Address;
      Kernel_Stack : Virtual_Address;
   end record;
   type Thread_Info_Arr is array (TID range 1 .. 256) of Thread_Info;

   --  Scheduler information.
   Scheduler_Mutex  : aliased Lib.Synchronization.Binary_Semaphore;
   Thread_Pool      : access Thread_Info_Arr;
   Scheduler_Vector : Arch.IDT.IDT_Index;
   Core_Locals      : Cores_Local_Info_Acc;

   --  Time slices assigned to each thread depending on preference.
   --  (In microseconds).
   Preference_Slices : constant array (Unsigned_8 range 1 .. 8)
      of Unsigned_64 := (
      1 =>  5000, -- Lowest, this should run as little as possible.
      2 =>  6000, -- Low, doesnt need to run for long.
      3 =>  8000, -- Low-ish, run a bit less than average.
      4 => 10000, -- Average.
      5 => 15000, -- High preference, will run for a bit.
      6 => 20000, -- Highest preference,will run a lot more than average.
      7 => 25000, -- Urgent, needs to run a fair while.
      8 => 30000  -- Critical, this MUST run.
   );

   --  Stack size of new threads.
   Stack_Size : constant := 16#200000#;

   function Init return Boolean is
   begin
      --  Allocate the vector for the LAPIC timer ISRs.
      if not Arch.IDT.Load_ISR (Scheduler_ISR'Address, Scheduler_Vector) then
         return False;
      end if;

      --  Allocate core locals and finishing touches.
      Core_Locals := new Cores_Local_Info (1 .. Arch.CPU.Core_Count);
      Thread_Pool := new Thread_Info_Arr;
      Is_Initialized := True;
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return True;
   end Init;

   procedure Idle_Core is
      I : constant Positive := Arch.CPU.Get_Core_Number;
   begin
      --  Check we are initialized and have all the data.
      while not Is_Initialized loop null; end loop;

      if Core_Locals (I).LAPIC_Timer_Hz = 0 then
         Core_Locals (I).LAPIC_Timer_Hz := Arch.APIC.LAPIC_Timer_Calibrate;
         Core_Locals (I).Current_TID    := 0;
         Arch.GDT.Load_TSS (Core_Locals (I).Core_TSS'Address);
      end if;

      declare
         Hz : constant Unsigned_64 := Core_Locals (I).LAPIC_Timer_Hz;
      begin
         --  Arm for Scheduler_ISR to do the rest of the job from us.
         Arch.Interrupts.Set_Interrupt_Flag (False);
         Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz, 20000);
         Arch.Interrupts.Set_Interrupt_Flag (True);
         loop Arch.Wrappers.HLT; end loop;
      end;
   end Idle_Core;

   function Create_Kernel_Thread
      (Address  : Virtual_Address;
       Argument : Unsigned_64) return TID is
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
         type Stack is array (1 .. Stack_Size) of Unsigned_8;
         type Stack_Acc is access Stack;
         New_Stack : constant Stack_Acc := new Stack;
         Stack_Addr : constant Virtual_Address :=
            To_Integer (New_Stack.all'Address) + Stack_Size;
      begin
         Thread_Pool (New_TID).Is_Present   := True;
         Thread_Pool (New_TID).Is_Banned    := False;
         Thread_Pool (New_TID).Is_Running   := False;
         Thread_Pool (New_TID).Preference   := 4;
         Thread_Pool (New_TID).PageMap      := Memory.Virtual.Kernel_Map.all;
         Thread_Pool (New_TID).Stack        := Stack_Addr;
         Thread_Pool (New_TID).State.CS     := Arch.GDT.Kernel_Code64_Segment;
         Thread_Pool (New_TID).State.DS     := Arch.GDT.Kernel_Data64_Segment;
         Thread_Pool (New_TID).State.ES     := Arch.GDT.Kernel_Data64_Segment;
         Thread_Pool (New_TID).State.SS     := Arch.GDT.Kernel_Data64_Segment;
         Thread_Pool (New_TID).State.RFLAGS := 16#202#;
         Thread_Pool (New_TID).State.RIP    := Unsigned_64 (Address);
         Thread_Pool (New_TID).State.RDI    := Argument;
         Thread_Pool (New_TID).State.RBP    := 0;
         Thread_Pool (New_TID).State.RSP    := Unsigned_64 (Stack_Addr);
      end;

   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return New_TID;
   end Create_Kernel_Thread;

   function Create_User_Thread
      (Address : Virtual_Address;
       Args    : Userland.Argument_Arr;
       Env     : Userland.Environment_Arr;
       Map     : Memory.Virtual.Page_Map) return TID is
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
         type Stack is array (1 .. Stack_Size) of Unsigned_8;
         type Stack_Acc is access Stack;
         New_Stack        : constant Stack_Acc := new Stack;
         New_Kernel_Stack : constant Stack_Acc := new Stack;
         Stack_Addr : constant Virtual_Address :=
            To_Integer (New_Stack.all'Address) + Stack_Size;
         KStack_Addr : constant Virtual_Address :=
            To_Integer (New_Kernel_Stack.all'Address) + Stack_Size;
         RSP_Val : constant Unsigned_64 :=
            Unsigned_64 (Stack_Addr) - Memory_Offset;
      begin
         Thread_Pool (New_TID).Is_Present   := True;
         Thread_Pool (New_TID).Is_Banned    := False;
         Thread_Pool (New_TID).Is_Running   := False;
         Thread_Pool (New_TID).Preference   := 4;
         Thread_Pool (New_TID).PageMap      := Map;
         Thread_Pool (New_TID).Stack        := Stack_Addr;
         Thread_Pool (New_TID).Kernel_Stack := KStack_Addr;
         Thread_Pool (New_TID).State.CS   := Arch.GDT.User_Code64_Segment or 3;
         Thread_Pool (New_TID).State.DS   := Arch.GDT.User_Data64_Segment or 3;
         Thread_Pool (New_TID).State.ES   := Arch.GDT.User_Data64_Segment or 3;
         Thread_Pool (New_TID).State.SS   := Arch.GDT.User_Data64_Segment or 3;
         Thread_Pool (New_TID).State.RFLAGS := 16#202#;
         Thread_Pool (New_TID).State.RIP    := Unsigned_64 (Address);
         Thread_Pool (New_TID).State.RBP    := 0;
         Thread_Pool (New_TID).State.RSP    := RSP_Val;

         --  Map the stacks.
         Memory.Virtual.Map_Range (
            Thread_Pool (New_TID).PageMap,
            16#C0000000000#,
            To_Integer (New_Stack.all'Address) - Memory_Offset,
            Stack_Size,
            (Present         => True,
             Read_Write      => True,
             User_Supervisor => True,
             Write_Through   => False,
             Cache_Disable   => False,
             Accessed        => False,
             Dirty           => False,
             PAT             => False,
             Global          => False),
            True);
      end;
   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return New_TID;
   end Create_User_Thread;

   procedure Delete_Thread (Thread : TID) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Thread_Pool (Thread).Is_Present := False;
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

   procedure Set_Thread_Preference (Thread : TID; Preference : Unsigned_8) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Thread_Pool (Thread).Preference := Preference;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
   end Set_Thread_Preference;

   procedure Yield is
      Core        : constant Positive    := Arch.CPU.Get_Core_Number;
      Core_LAPIC  : constant Unsigned_32 := Arch.CPU.Core_LAPICs (Core);
      Current_TID : constant TID         := Core_Locals (Core).Current_TID;
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex'Access);
      --  Force rescheduling by calling the ISR vector directly.
      if Current_TID /= 0 then
         Arch.APIC.LAPIC_Send_IPI (Core_LAPIC, Scheduler_Vector);
      end if;
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
   end Yield;

   function Find_Free_TID return TID is
   begin
      for I in Thread_Pool'First .. Thread_Pool'Last loop
         if not Thread_Pool (I).Is_Present then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free_TID;

   procedure Scheduler_ISR
      (Number : Unsigned_32;
       State  : access Arch.Interrupts.ISR_GPRs)
   is
      Core : constant Positive    := Arch.CPU.Get_Core_Number;
      Hz   : constant Unsigned_64 := Core_Locals (Core).LAPIC_Timer_Hz;
      Current_TID : TID;
      Next_TID    : TID;
      pragma Unreferenced (Number);
   begin
      if Lib.Synchronization.Try_Seize (Scheduler_Mutex'Access) = False then
         Arch.APIC.LAPIC_EOI;
         Arch.APIC.LAPIC_Timer_Stop;
         Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz, 20000);
         return;
      end if;

      Arch.APIC.LAPIC_Timer_Stop;
      Arch.Interrupts.Set_Interrupt_Flag (False);

      --  Get the next thread for execution.
      Current_TID := Core_Locals (Core).Current_TID;
      Next_TID    := 0;
      for I in Current_TID + 1 .. TID (Thread_Pool'Last) loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Banned and
            not Thread_Pool (I).Is_Running
         then
            Next_TID := I;
            goto Found_TID;
         end if;
      end loop;
      for I in 1 .. Current_TID loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Banned and
            not Thread_Pool (I).Is_Running
         then
            Next_TID := I;
            goto Found_TID;
         end if;
      end loop;

      --  Handle what to do if we didnt find a valid TID to switch, either keep
      --  executing, or arm and wait for an eventual thread.
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      Arch.APIC.LAPIC_EOI;
      if Current_TID /= 0 then
         Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz,
            Preference_Slices (Thread_Pool (Current_TID).Preference));
      else
         Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz,
            Preference_Slices (4));
      end if;
      return;

   <<Found_TID>>
      --  Save state.
      if Current_TID /= 0 then
         Thread_Pool (Current_TID).Is_Running := False;
         Thread_Pool (Current_TID).FS         := Arch.Wrappers.Read_FS;
         Thread_Pool (Current_TID).State      := State.all;
      end if;

      --  Assign the next TID as our current one.
      Core_Locals (Core).Current_TID    := Next_TID;
      Thread_Pool (Next_TID).Is_Running := True;

      --  Rearm the timer for next tick.
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz,
         Preference_Slices (Thread_Pool (Next_TID).Preference));
      Arch.APIC.LAPIC_EOI;

      --  Load kernel stack in the TSS.
      Core_Locals (Core).Core_TSS.Stack_Ring0 :=
         To_Address (Thread_Pool (Next_TID).Kernel_Stack);

      --  Reset state.
      Memory.Virtual.Make_Active (Thread_Pool (Next_TID).PageMap);
      Arch.Wrappers.Write_FS (Thread_Pool (Next_TID).FS);
      Asm (
         "mov %0, %%rsp"   & LF & HT &
         "pop %%rax"       & LF & HT &
         "mov %%eax, %%ds" & LF & HT &
         "pop %%rax"       & LF & HT &
         "mov %%eax, %%es" & LF & HT &
         "pop %%rax"       & LF & HT &
         "pop %%rbx"       & LF & HT &
         "pop %%rcx"       & LF & HT &
         "pop %%rdx"       & LF & HT &
         "pop %%rsi"       & LF & HT &
         "pop %%rdi"       & LF & HT &
         "pop %%rbp"       & LF & HT &
         "pop %%r8"        & LF & HT &
         "pop %%r9"        & LF & HT &
         "pop %%r10"       & LF & HT &
         "pop %%r11"       & LF & HT &
         "pop %%r12"       & LF & HT &
         "pop %%r13"       & LF & HT &
         "pop %%r14"       & LF & HT &
         "pop %%r15"       & LF & HT &
         "add $8, %%rsp"   & LF & HT &
         "swapgs"          & LF & HT &
         "iretq",
         Inputs => System.Address'Asm_Input
            ("rm", Thread_Pool (Next_TID).State'Address),
         Clobber => "memory",
         Volatile => True
      );
      loop Arch.Wrappers.HLT; end loop;
   end Scheduler_ISR;

   function Is_Thread_Present (Thread : TID) return Boolean is
   begin
      return Thread /= 0 and Thread <= TID (Thread_Pool'Last) and
         Thread_Pool (Thread).Is_Present;
   end Is_Thread_Present;
end Scheduler;
