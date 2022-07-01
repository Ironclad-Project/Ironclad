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
with Userland.Process;
with Arch;
with Arch.Snippets;
with Arch.MMU;

package body Scheduler is
   --  Thread information.
   type FP_Region_Arr is array (1 .. 512) of Unsigned_8 with Alignment => 16;
   type Thread_Info is record
      State        : Arch.Interrupts.ISR_GPRs;
      Is_Present   : Boolean;
      Is_Banned    : Boolean;
      Is_Running   : Boolean;
      Priority     : Integer;
      FS           : Unsigned_64;
      PageMap      : Memory.Virtual.Page_Map_Acc;
      Stack        : Virtual_Address;
      Kernel_Stack : Virtual_Address;
      FP_Region    : FP_Region_Arr;
      Process      : Userland.Process.Process_Data_Acc;
   end record;
   type Thread_Info_Arr is array (TID range 1 .. 256) of Thread_Info;

   --  Scheduler information.
   Scheduler_Mutex  : aliased Lib.Synchronization.Binary_Semaphore;
   Thread_Pool      : access Thread_Info_Arr;
   Scheduler_Vector : Arch.IDT.IDT_Index;

   --  Time slices assigned to each thread depending on preference.
   --  (In microseconds).
   Priority_Slices : constant array (-20 .. 19) of Unsigned_64 := (
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
      --  Allocate the vector for the LAPIC timer ISRs.
      if not Arch.IDT.Load_ISR (Scheduler_ISR'Address, Scheduler_Vector) then
         return False;
      end if;

      --  Allocate core locals and finishing touches.
      Thread_Pool := new Thread_Info_Arr;
      Is_Initialized := True;
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return True;
   end Init;

   procedure Idle_Core is
   begin
      --  Check we are initialized and have all the data.
      while not Is_Initialized loop null; end loop;

      --  Get the LAPIC timer hz that we couldn't get earlier because circular
      --  dependency on the LAPIC timer <-> PIT <-> HPET.
      if Arch.CPU.Get_Local.LAPIC_Timer_Hz = 0 then
         Arch.CPU.Get_Local.LAPIC_Timer_Hz := Arch.APIC.LAPIC_Timer_Calibrate;
      end if;

      --  Arm for Scheduler_ISR to do the rest of the job from us.
      Arch.Snippets.Disable_Interrupts;
      Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector,
         Arch.CPU.Get_Local.LAPIC_Timer_Hz, 20000);
      Arch.Snippets.Enable_Interrupts;
      loop Arch.Wrappers.HLT; end loop;
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
         Thread_Pool (New_TID).Priority     := 0;
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
       Stack_Top : Unsigned_64;
       PID       : Natural) return TID
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

         Map_Flags : constant Arch.MMU.Page_Permissions := (
            User_Accesible => True,
            Read_Only      => False,
            Executable     => True,
            Global         => False,
            Write_Through  => False
         );
      begin
         Thread_Pool (New_TID).Is_Present   := True;
         Thread_Pool (New_TID).Is_Banned    := False;
         Thread_Pool (New_TID).Is_Running   := False;
         Thread_Pool (New_TID).Priority     := 0;
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
         Thread_Pool (New_TID).Process    := Userland.Process.Get_By_PID (PID);

         --  Map the user stack.
         Memory.Virtual.Map_Range (
            Thread_Pool (New_TID).PageMap,
            Virtual_Address (Stack_Top),
            To_Integer (User_Stack_8.all'Address) - Memory_Offset,
            Stack_Size + Memory.Virtual.Page_Size,
            Map_Flags
         );

         --  Set up FPU control word and MXCSR as defined by SysV.
         Arch.Wrappers.FP_Restore (Thread_Pool (New_TID).FP_Region'Address);
         Arch.Wrappers.Load_x87_Control_Word (2#1100111111#);
         Arch.Wrappers.Load_MXCSR (2#1111110000000#);
         Arch.Wrappers.FP_Save (Thread_Pool (New_TID).FP_Region'Address);

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
         Thread_Pool (New_TID).State.RSP :=
            Stack_Top + (Unsigned_64 (Index_64) * 8);
      end;

   <<End_Return>>
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      return New_TID;
   end Create_User_Thread;

   function Create_User_Thread
      (State : access ISR_GPRs;
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
      Thread_Pool (New_TID).Is_Present   := True;
      Thread_Pool (New_TID).Is_Banned    := False;
      Thread_Pool (New_TID).Is_Running   := False;
      Thread_Pool (New_TID).PageMap      := Map;
      Thread_Pool (New_TID).Priority     := 0;
      Thread_Pool (New_TID).Kernel_Stack := Kernel_Stack_Addr;
      Thread_Pool (New_TID).FS           := Arch.Wrappers.Read_FS;
      Thread_Pool (New_TID).State        := State.all;
      Thread_Pool (New_TID).State.RAX    := 0;
      Thread_Pool (New_TID).Process      := Userland.Process.Get_By_PID (PID);

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
            for I in Arch.CPU.Core_Locals'Range loop
               if Arch.CPU.Core_Locals (I).Current_Thread = Thread then
                  Arch.APIC.LAPIC_Send_IPI
                     (Arch.CPU.Core_Locals (I).LAPIC_ID, Scheduler_Vector);
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

   procedure Yield is
      Core_LAPIC : constant Unsigned_32 := Arch.CPU.Get_Local.LAPIC_ID;
   begin
      --  Force rescheduling by calling the ISR vector directly.
      Arch.APIC.LAPIC_Send_IPI (Core_LAPIC, Scheduler_Vector);
   end Yield;

   procedure Bail is
   begin
      Delete_Thread (Arch.CPU.Get_Local.Current_Thread);
      Arch.CPU.Get_Local.Current_Thread := 0;
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

   procedure Scheduler_ISR
      (Number : Unsigned_32;
       State  : access Arch.Interrupts.ISR_GPRs)
   is
      Hz : constant Unsigned_64 := Arch.CPU.Get_Local.LAPIC_Timer_Hz;
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
      Current_TID := Arch.CPU.Get_Local.Current_Thread;
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
            Priority_Slices (Thread_Pool (Current_TID).Priority));
      else
         Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz,
            Priority_Slices (0));
      end if;
      return;

   <<Found_TID>>
      --  Save state.
      if Current_TID /= 0 then
         Thread_Pool (Current_TID).Is_Running := False;
         Thread_Pool (Current_TID).FS         := Arch.Wrappers.Read_FS;
         Thread_Pool (Current_TID).State      := State.all;
         Arch.Wrappers.FP_Save (Thread_Pool (Current_TID).FP_Region'Address);
      end if;

      --  Assign the next TID as our current one.
      Arch.CPU.Get_Local.Current_Thread := Next_TID;
      Thread_Pool (Next_TID).Is_Running := True;

      --  Rearm the timer for next tick.
      Lib.Synchronization.Release (Scheduler_Mutex'Access);
      Arch.APIC.LAPIC_Timer_Oneshot (Scheduler_Vector, Hz,
         Priority_Slices (Thread_Pool (Next_TID).Priority));
      Arch.APIC.LAPIC_EOI;

      --  Load kernel stack in the TSS.
      Arch.CPU.Get_Local.Core_TSS.Stack_Ring0 :=
         To_Address (Thread_Pool (Next_TID).Kernel_Stack);
      Arch.CPU.Get_Local.Current_Process := Thread_Pool (Next_TID).Process;

      --  Reset state.
      Memory.Virtual.Make_Active (Thread_Pool (Next_TID).PageMap);
      Arch.Wrappers.Write_FS (Thread_Pool (Next_TID).FS);
      Arch.Wrappers.FP_Restore (Thread_Pool (Next_TID).FP_Region'Address);
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
      return Thread /= 0 and then Thread <= TID (Thread_Pool'Last) and then
         Thread_Pool (Thread).Is_Present;
   end Is_Thread_Present;
end Scheduler;
