--  arch-cpu.adb: CPU management routines.
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

with System.Machine_Code; use System.Machine_Code;
with Arch.APIC;
with Memory.MMU;
with Arch.IDT;
with Alignment;
with Panic;
with Arch.Snippets;
with Arch.Context;
with System; use System;
with Memory;

package body Arch.CPU with SPARK_Mode => Off is
   type Interrupt_Stack is array (1 .. Memory.Kernel_Stack_Size) of Unsigned_8;
   type Interrupt_Stack_Acc is access Interrupt_Stack;

   SMP_Request : Limine.SMP_Request :=
      (Base  =>
         (ID       => Limine.SMP_ID,
          Revision => 0,
          Response => System.Null_Address),
       Flags => Limine.SMP_ENABLE_X2APIC)
      with Export, Async_Writers;

   procedure Init_Cores is
      BSP_LAPIC_ID : Unsigned_32;
      New_Stk      : constant Interrupt_Stack_Acc := new Interrupt_Stack;
      New_Stk_Top  : constant System.Address := New_Stk (New_Stk'Last)'Address;
      Idx          : Natural := 2;

      SMPPonse : Limine.SMP_Response
         with Import, Address => SMP_Request.Base.Response;
   begin
      --  Check we got a limine answer at all, else, single core time.
      if SMP_Request.Base.Response /= System.Null_Address then
         Core_Count   := Natural (SMPPonse.CPU_Count);
         BSP_LAPIC_ID := SMPPonse.BSP_LAPIC_ID;
      else
         Core_Count   := 1;
         Get_BSP_LAPIC_ID (BSP_LAPIC_ID);
      end if;

      --  Initialize the locals list, and initialize the cores.
      Core_Locals := new Core_Local_Arr (1 .. Core_Count);
      Init_Common (1, BSP_LAPIC_ID, Unsigned_64 (To_Integer (New_Stk_Top)));
      Context.Setup_XSAVE (Global_Use_XSAVE, Global_FPU_Size);
      Save_MTRRs;

      --  Initialize the other cores.
      if Core_Count > 1 then
         declare
            SMP_CPUs : Limine.CPU_Info_Arr (1 .. SMPPonse.CPU_Count)
               with Import, Address => SMPPonse.CPUs;
         begin
            for CPU of SMP_CPUs loop
               if CPU.LAPIC_ID /= BSP_LAPIC_ID then
                     CPU.Extra_Arg := Unsigned_64 (Idx);
                     CPU.Addr      := Core_Bootstrap'Address;
                     Idx           := Idx + 1;
               end if;
            end loop;
         end;
      end if;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception when initializing cores");
   end Init_Cores;

   function Get_Local return Core_Local_Acc is
      Locals : Core_Local_Acc;
   begin
      --  XXX: We are making the guarantee this can never be null, which it
      --  can if the scheduler does not swap gs correctly.
      Asm ("mov %%gs:0, %0",
           Outputs  => Core_Local_Acc'Asm_Output ("=a", Locals),
           Volatile => True);
      return Locals;
   end Get_Local;
   ----------------------------------------------------------------------------
   procedure Save_MTRRs is
      Count : constant Unsigned_64 := Snippets.Read_MSR (16#FE#) and 16#FF#;
      I : Natural := 0;
   begin
      Saved_MTRRs := new MTRR_Store (1 .. Natural (Count * 2) + 11 + 1);
      while I <= Natural (Count * 2) - 1 loop
         Saved_MTRRs (I + 1) := Snippets.Read_MSR (16#200# + Unsigned_32 (I));
         Saved_MTRRs (I + 2) := Snippets.Read_MSR (16#201# + Unsigned_32 (I));
         I := I + 2;
      end loop;

      Saved_MTRRs (Natural (Count * 2) + 1) := Snippets.Read_MSR (16#250#);
      Saved_MTRRs (Natural (Count * 2) + 2) := Snippets.Read_MSR (16#258#);
      Saved_MTRRs (Natural (Count * 2) + 3) := Snippets.Read_MSR (16#259#);
      Saved_MTRRs (Natural (Count * 2) + 4) := Snippets.Read_MSR (16#268#);
      Saved_MTRRs (Natural (Count * 2) + 5) := Snippets.Read_MSR (16#269#);
      Saved_MTRRs (Natural (Count * 2) + 6) := Snippets.Read_MSR (16#26A#);
      Saved_MTRRs (Natural (Count * 2) + 7) := Snippets.Read_MSR (16#26B#);
      Saved_MTRRs (Natural (Count * 2) + 8) := Snippets.Read_MSR (16#26C#);
      Saved_MTRRs (Natural (Count * 2) + 9) := Snippets.Read_MSR (16#26D#);
      Saved_MTRRs (Natural (Count * 2) + 10) := Snippets.Read_MSR (16#26E#);
      Saved_MTRRs (Natural (Count * 2) + 11) := Snippets.Read_MSR (16#26F#);
      Saved_MTRRs (Natural (Count * 2) + 12) := Snippets.Read_MSR (16#2FF#);
      Saved_MTRRs (Natural (Count * 2) + 12) :=
         Saved_MTRRs (Natural (Count * 2) + 12) and not Shift_Left (1, 11);
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception when saving MTRRs");
   end Save_MTRRs;

   procedure Restore_MTRRs is
      Count : constant Unsigned_64 := Snippets.Read_MSR (16#FE#) and 16#FF#;
      CR0  : constant Unsigned_64 := Snippets.Read_CR0;
      CR3  : constant Unsigned_64 := Snippets.Read_CR3;
      MTRR : constant Unsigned_64 := Snippets.Read_MSR (16#2FF#);
      Temp_CR0 : Unsigned_64;
      I : Natural := 0;
   begin
      Temp_CR0 := (CR0 or Shift_Left (1, 30)) and not Shift_Left (1, 29);
      Snippets.Write_CR0 (Temp_CR0);
      Snippets.Invalidate_Caches;
      Snippets.Write_CR3 (CR3);
      Snippets.Write_MSR (16#2FF#, MTRR and not Shift_Left (1, 11));

      while I <= Natural (Count * 2) - 1 loop
         Snippets.Write_MSR (16#200# + Unsigned_32 (I), Saved_MTRRs (I + 1));
         Snippets.Write_MSR (16#201# + Unsigned_32 (I), Saved_MTRRs (I + 2));
         I := I + 2;
      end loop;

      Snippets.Write_MSR (16#250#, Saved_MTRRs (Natural (Count * 2) + 1));
      Snippets.Write_MSR (16#258#, Saved_MTRRs (Natural (Count * 2) + 2));
      Snippets.Write_MSR (16#259#, Saved_MTRRs (Natural (Count * 2) + 3));
      Snippets.Write_MSR (16#268#, Saved_MTRRs (Natural (Count * 2) + 4));
      Snippets.Write_MSR (16#269#, Saved_MTRRs (Natural (Count * 2) + 5));
      Snippets.Write_MSR (16#26A#, Saved_MTRRs (Natural (Count * 2) + 6));
      Snippets.Write_MSR (16#26B#, Saved_MTRRs (Natural (Count * 2) + 7));
      Snippets.Write_MSR (16#26C#, Saved_MTRRs (Natural (Count * 2) + 8));
      Snippets.Write_MSR (16#26D#, Saved_MTRRs (Natural (Count * 2) + 9));
      Snippets.Write_MSR (16#26E#, Saved_MTRRs (Natural (Count * 2) + 10));
      Snippets.Write_MSR (16#26F#, Saved_MTRRs (Natural (Count * 2) + 11));
      Snippets.Write_MSR (16#2FF#, Saved_MTRRs (Natural (Count * 2) + 12));
      Snippets.Write_MSR (16#2FF#, MTRR or Shift_Left (1, 11));
      Snippets.Write_CR3 (CR3);
      Snippets.Write_CR0 (CR0);
      Snippets.Invalidate_Caches;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception when restoring MTRRs");
   end Restore_MTRRs;

   procedure Core_Bootstrap (Info : access Limine.SMP_CPU_Info) is
      New_Stk : constant Interrupt_Stack_Acc := new Interrupt_Stack;
      New_Stk_Top : constant System.Address := New_Stk (New_Stk'Last)'Address;
   begin
      Init_Core
         (Core_Number => Natural (Info.Extra_Arg),
          LAPIC_ID    => Unsigned_8 (Info.LAPIC_ID),
          Stack_Top   => Unsigned_64 (To_Integer (New_Stk_Top)));
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception when bootstrapping core");
   end Core_Bootstrap;

   procedure Init_Core
      (Core_Number : Positive;
       LAPIC_ID    : Unsigned_8;
       Stack_Top   : Unsigned_64)
   is
      Discard : Boolean;
   begin
      --  Load the global GDT, IDT, mappings, and LAPIC.
      GDT.Load_GDT;
      IDT.Load_IDT;
      Discard := Memory.MMU.Make_Active (Memory.MMU.Kernel_Table);
      APIC.Init_Core_LAPIC;

      --  Load several goodies.
      Init_Common (Core_Number, Unsigned_32 (LAPIC_ID), Stack_Top);
      Restore_MTRRs;

      --  Send the core to idle, waiting for the scheduler to tell it to do
      --  something, from here, we lose control. Farewell, core.
      Scheduler.Idle_Core;
   end Init_Core;

   procedure Init_Common
      (Core_Number : Positive;
       LAPIC       : Unsigned_32;
       Stack_Top   : Unsigned_64)
   is
      package A is new Alignment (Unsigned_32);

      PAT_MSR   : constant := 16#00000277#;
      EFER_MSR  : constant := 16#C0000080#;
      STAR_MSR  : constant := 16#C0000081#;
      LSTAR_MSR : constant := 16#C0000082#;
      FMASK_MSR : constant := 16#C0000084#;
      --  UCET_MSR  : constant := 16#000006A0#;
      --  SCET_MSR  : constant := 16#000006A2#;

      CR0   : Unsigned_64 := Snippets.Read_CR0;
      CR4   : Unsigned_64 := Snippets.Read_CR4;
      XCR0  : Unsigned_64 := 0;
      PAT   : Unsigned_64 := Snippets.Read_MSR (PAT_MSR);
      EFER  : Unsigned_64 := Snippets.Read_MSR (EFER_MSR);
      STAR  : Unsigned_64;
      LSTAR : Unsigned_64;
      FMASK : Unsigned_64;
      EAX, EBX, ECX, EDX : Unsigned_32;
      Success : Boolean;
      Locals_Addr : Unsigned_64;

      procedure Syscall_Entry with Import, External_Name => "syscall_entry";
   begin
      --  Enable WP and SSE/2.
      CR0 := CR0 or Shift_Left (1, 16);
      CR0 := (CR0 and (not Shift_Left (1, 2))) or Shift_Left (1, 1);
      CR4 := CR4 or Shift_Left (3, 9);

      --  Enable global page tables.
      CR4 := CR4 or Shift_Left (1, 7);

      --  Enable several security features if present.
      Snippets.Get_CPUID (7, 0, EAX, EBX, ECX, EDX, Success);
      if Success then
         if (ECX and Shift_Left (1, 2)) /= 0 then
            CR4 := CR4 or Shift_Left (1, 11); --  UMIP.
         end if;
         if (EBX and Shift_Left (1, 7)) /= 0 then
            CR4 := CR4 or Shift_Left (1, 20); --  SMEP.
         end if;
         if (EBX and Shift_Left (1, 20)) /= 0 then
            CR4 := CR4 or Shift_Left (1, 21); --  SMAP.
            Arch.Snippets.Disable_Userland_Memory_Access;
            Global_Use_SMAP := True;
         end if;

         --  FIXME: Uncommenting this makes the system crash either in the
         --  kernel IBT checks or the user IBT checks, these have to be fixed.
         --  Hopefully once Linux 6.18 releases so IBT works under KVM QEMU.
         --  if (EDX and Shift_Left (1, 20)) /= 0 then
         --     CR4 := CR4 or Shift_Left (1, 23); --  Enable CET.
         --     Snippets.Write_MSR (UCET_MSR, 2#100#); --  Enable just IBT.
         --     Snippets.Write_MSR (SCET_MSR, 2#100#); --  Enable just IBT.
         --  end if;
      end if;

      --  Check XSAVE support.
      --  XXX: Every core will write to the global locations for data, but that
      --  is fine because they will always be the same for all cores.
      Snippets.Get_CPUID (1, 0, EAX, EBX, ECX, EDX, Success);
      if Success and then ((ECX and Shift_Left (1, 26)) /= 0) then
         Global_Use_XSAVE := True;
         CR4  := CR4 or Shift_Left (1, 18);
         XCR0 := 2#11#; --  Set xsave to be used for x87 and SSE.

         --  Check and enable AVX support.
         if ((ECX and Shift_Left (1, 28)) /= 0) then
            XCR0 := XCR0 or Shift_Left (1, 2);
         end if;

         --  Check and enable AVX512 foundation support.
         Snippets.Get_CPUID (7, 0, EAX, EBX, ECX, EDX, Success);
         if Success and then ((EBX and Shift_Left (1, 17)) /= 0) then
            XCR0 := XCR0 or Shift_Left (2#1#, 5); --  Enable OPMASK.
            XCR0 := XCR0 or Shift_Left (2#1#, 6); --  Enable the ZMM regs.
            XCR0 := XCR0 or Shift_Left (2#1#, 7); --  Enable more ZMM regs.
         end if;

         --  Get the size of the xsave area.
         Snippets.Get_CPUID (16#D#, 0, EAX, EBX, ECX, EDX, Success);
         Global_FPU_Size := A.Align_Up (ECX, Memory.MMU.Page_Size);

         Snippets.Write_CR4 (CR4);
         Snippets.Write_XCR (0, XCR0);
      else
         Global_Use_XSAVE := False;
         Global_FPU_Size  := A.Align_Up (512, Memory.MMU.Page_Size);
      end if;

      --  Enable SYSCALL instructions.
      EFER  := EFER or 1;
      STAR  := 16#0033002800000000#;
      LSTAR := Unsigned_64 (To_Integer (Syscall_Entry'Address));
      FMASK := Unsigned_64 (not Unsigned_32 (2));

      --  Initialise the PAT (write-protect / write-combining).
      PAT := PAT and (16#FFFFFFFF#);
      PAT := PAT or  Shift_Left (Unsigned_64 (16#0105#), 32);

      --  Write the final configuration.
      Snippets.Write_CR0 (CR0);
      Snippets.Write_CR4 (CR4);
      Snippets.Write_MSR (PAT_MSR, PAT);
      Snippets.Write_MSR (EFER_MSR, EFER);
      Snippets.Write_MSR (STAR_MSR, STAR);
      Snippets.Write_MSR (LSTAR_MSR, LSTAR);
      Snippets.Write_MSR (FMASK_MSR, FMASK);

      --  Prepare the core local structure and set it in GS.
      Locals_Addr := Unsigned_64
         (To_Integer (Core_Locals (Core_Number)'Address));
      Core_Locals (Core_Number) :=
         (Self             => Core_Locals (Core_Number)'Access,
          Kernel_Stack     => 0,
          User_Stack       => 0,
          Number           => Core_Number,
          LAPIC_ID         => LAPIC,
          LAPIC_Timer_Hz   => <>,
          Core_TSS         => <>,
          Current_Thread   => Scheduler.Error_TID,
          Current_Process  => Userland.Process.Error_PID,
          Invalidate_Lock  => Synchronization.Unlocked_Semaphore,
          Invalidate_Map   => 0,
          Invalidate_Start => System.Null_Address,
          Invalidate_End   => System.Null_Address);
      APIC.LAPIC_Timer_Calibrate (Core_Locals (Core_Number).LAPIC_Timer_Hz);

      Snippets.Write_GS        (Locals_Addr);
      Snippets.Write_Kernel_GS (Locals_Addr);

      --  Load the TSS.
      Core_Locals (Core_Number).Core_TSS.Stack_Ring0 :=
         To_Address (Integer_Address (Stack_Top));
      GDT.Load_TSS (Core_Locals (Core_Number).Core_TSS'Address);
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception when initializing core");
   end Init_Common;

   procedure Get_BSP_LAPIC_ID (ID : out Unsigned_32) is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Success : Boolean;
   begin
      Snippets.Get_CPUID (1, 0, EAX, EBX, ECX, EDX, Success);
      if not Success then
         Panic.Hard_Panic ("Could not get BSP");
      end if;
      ID := Shift_Right (EBX, 24) and 16#FF#;
   end Get_BSP_LAPIC_ID;
end Arch.CPU;
