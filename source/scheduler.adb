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
      Is_Userspace : Boolean;
      State        : Arch.Interrupts.ISR_GPRs;
      Is_Present   : Boolean;
      Is_Banned    : Boolean;
      Is_Running   : Boolean;
      Preference   : Positive;
      FS           : Unsigned_64;
      PageMap      : Memory.Virtual.Page_Map_Acc;
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
   Preference_Slices : constant array (1 .. 8) of Unsigned_64 := (
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
         Thread_Pool (New_TID).Is_Userspace := False;
         Thread_Pool (New_TID).Is_Present   := True;
         Thread_Pool (New_TID).Is_Banned    := False;
         Thread_Pool (New_TID).Is_Running   := False;
         Thread_Pool (New_TID).Preference   := 4;
         Thread_Pool (New_TID).PageMap      := Memory.Virtual.Kernel_Map;
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
      (Address   : Virtual_Address;
       Args      : Userland.Argument_Arr;
       Env       : Userland.Environment_Arr;
       Map       : Memory.Virtual.Page_Map_Acc;
       Vector    : Userland.ELF.Auxval;
       Stack_Top : Unsigned_64) return TID is
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

         Map_Flags : constant Memory.Virtual.Page_Flags := (
            Present         => True,
            Read_Write      => True,
            User_Supervisor => True,
            Write_Through   => False,
            Cache_Disable   => False,
            Accessed        => False,
            Dirty           => False,
            PAT             => False,
            Global          => False
         );
      begin
         Thread_Pool (New_TID).Is_Userspace := True;
         Thread_Pool (New_TID).Is_Present   := True;
         Thread_Pool (New_TID).Is_Banned    := False;
         Thread_Pool (New_TID).Is_Running   := False;
         Thread_Pool (New_TID).Preference   := 4;
         Thread_Pool (New_TID).PageMap      := Map;
         Thread_Pool (New_TID).Stack        := User_Stack_Addr;
         Thread_Pool (New_TID).Kernel_Stack := Kernel_Stack_Addr;
         Thread_Pool (New_TID).State.CS   := Arch.GDT.User_Code64_Segment or 3;
         Thread_Pool (New_TID).State.DS   := Arch.GDT.User_Data64_Segment or 3;
         Thread_Pool (New_TID).State.ES   := Arch.GDT.User_Data64_Segment or 3;
         Thread_Pool (New_TID).State.SS   := Arch.GDT.User_Data64_Segment or 3;
         Thread_Pool (New_TID).State.RFLAGS := 16#202#;
         Thread_Pool (New_TID).State.RIP    := Unsigned_64 (Address);
         Thread_Pool (New_TID).State.RBP    := 0;

         --  Map the user stack.
         Memory.Virtual.Map_Range (
            Thread_Pool (New_TID).PageMap,
            Virtual_Address (Stack_Top),
            To_Integer (User_Stack_8.all'Address) - Memory_Offset,
            Stack_Size + Memory.Virtual.Page_Size,
            Map_Flags,
            False,
            True
         );

         --  Load env into the stack.
         for En of reverse Env loop
            User_Stack_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
            for C of reverse En.all loop
               User_Stack_8 (Index_8) := Character'Pos (C);
               Index_8 := Index_8 - 1;
            end loop;
            User_Stack_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
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

         --  Get the equivalent 64-bit stack index and start loading.
         Index_64 := (Index_8 / 8) - 1;

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
            Index_8 := Index_8 - En.all'Length - 1;
            User_Stack_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Load argv into the stack.
         User_Stack_64 (Index_64) := 0; --  Null at the end of argv.
         Index_64 := Index_64 - 1;
         for Arg of reverse Args loop
            Index_8 := Index_8 - Arg.all'Length - 1;
            User_Stack_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Write argc and we are done!
         User_Stack_64 (Index_64) := Args'Length;
         Index_64 := Index_64 - 1;
         Thread_Pool (New_TID).State.RSP :=
            Stack_Top + (Unsigned_64 (Index_64) * 8);
      end;
   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return New_TID;
   end Create_User_Thread;

   function Create_User_Thread
      (State : access ISR_GPRs;
       Map   : Memory.Virtual.Page_Map_Acc) return TID
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
      Thread_Pool (New_TID).Is_Userspace := True;
      Thread_Pool (New_TID).Is_Present   := True;
      Thread_Pool (New_TID).Is_Banned    := False;
      Thread_Pool (New_TID).Is_Running   := False;
      Thread_Pool (New_TID).PageMap      := Map;
      Thread_Pool (New_TID).Preference   := 4;
      Thread_Pool (New_TID).Kernel_Stack := Kernel_Stack_Addr;
      Thread_Pool (New_TID).FS           := Arch.Wrappers.Read_FS;
      Thread_Pool (New_TID).State        := State.all;
      Thread_Pool (New_TID).State.RAX    := 0;

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
            for I in Core_Locals'Range loop
               if Core_Locals (I).Current_TID = Thread then
                  Arch.APIC.LAPIC_Send_IPI
                     (Arch.CPU.Core_LAPICs (I), Scheduler_Vector);
                  exit;
               end if;
            end loop;
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

   function Get_Thread_Preference (Thread : TID) return Natural is
   begin
      if Is_Thread_Present (Thread) then
         return Thread_Pool (Thread).Preference;
      else
         return 0;
      end if;
   end Get_Thread_Preference;

   procedure Set_Thread_Preference (Thread : TID; Preference : Positive) is
   begin
      if Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         if Preference > Preference_Slices'Last then
            Thread_Pool (Thread).Preference := Preference_Slices'Last;
         else
            Thread_Pool (Thread).Preference := Preference;
         end if;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
   end Set_Thread_Preference;

   procedure Yield is
      Core        : constant Positive    := Arch.CPU.Get_Core_Number;
      Core_LAPIC  : constant Unsigned_32 := Arch.CPU.Core_LAPICs (Core);
      Current_TID : constant TID         := Core_Locals (Core).Current_TID;
   begin
      --  Force rescheduling by calling the ISR vector directly.
      if Current_TID /= 0 then
         Arch.APIC.LAPIC_Send_IPI (Core_LAPIC, Scheduler_Vector);
      end if;
      loop Arch.Wrappers.HLT; end loop;
   end Yield;

   procedure Bail is
      Core        : constant Positive := Arch.CPU.Get_Core_Number;
      Current_TID : constant TID      := Core_Locals (Core).Current_TID;
   begin
      Delete_Thread (Current_TID);
      Yield;
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

   function Is_Userspace return Boolean is
      Core   : constant Positive := Arch.CPU.Get_Core_Number;
      Thread : constant TID      := Core_Locals (Core).Current_TID;
      Result : Boolean           := False;
   begin
      if Is_Initialized and Is_Thread_Present (Thread) then
         Lib.Synchronization.Seize (Scheduler_Mutex'Access);
         Result := Thread_Pool (Thread).Is_Userspace;
         Lib.Synchronization.Release (Scheduler_Mutex'Access);
      end if;
      return Result;
   end Is_Userspace;

   function Get_Current_Thread return TID is
      Core   : constant Positive := Arch.CPU.Get_Core_Number;
      Thread : constant TID      := Core_Locals (Core).Current_TID;
   begin
      return Thread;
   end Get_Current_Thread;

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
         Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz, 20000);
         return;
      end if;

      --  Get the next thread for execution.
      Current_TID := Core_Locals (Core).Current_TID;
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
