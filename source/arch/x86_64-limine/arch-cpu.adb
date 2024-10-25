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
with Arch.MMU;
with Arch.IDT;
with Arch.Snippets;
with Arch.Context;
with System; use System;

package body Arch.CPU with SPARK_Mode => Off is
   type Interrupt_Stack is array (1 .. 16#4000#) of Unsigned_8;
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
         BSP_LAPIC_ID := Get_BSP_LAPIC_ID;
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
   end Restore_MTRRs;

   procedure Core_Bootstrap (Info : access Limine.SMP_CPU_Info) is
      New_Stk : constant Interrupt_Stack_Acc := new Interrupt_Stack;
      New_Stk_Top : constant System.Address := New_Stk (New_Stk'Last)'Address;
   begin
      Init_Core
         (Core_Number => Natural (Info.Extra_Arg),
          LAPIC_ID    => Unsigned_8 (Info.LAPIC_ID),
          Stack_Top   => Unsigned_64 (To_Integer (New_Stk_Top)));
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
      Discard := Arch.MMU.Make_Active (Arch.MMU.Kernel_Table);
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
      PAT_MSR   : constant := 16#00000277#;
      EFER_MSR  : constant := 16#C0000080#;
      STAR_MSR  : constant := 16#C0000081#;
      LSTAR_MSR : constant := 16#C0000082#;
      FMASK_MSR : constant := 16#C0000084#;
      UCET_MSR  : constant := 16#000006A0#;
      SCET_MSR  : constant := 16#000006A2#;

      CR0   : Unsigned_64 := Snippets.Read_CR0;
      CR4   : Unsigned_64 := Snippets.Read_CR4;
      XCR0  : Unsigned_64 := 0;
      PAT   : Unsigned_64 := Snippets.Read_MSR (PAT_MSR);
      EFER  : Unsigned_64 := Snippets.Read_MSR (EFER_MSR);
      STAR  : Unsigned_64;
      LSTAR : Unsigned_64;
      FMASK : Unsigned_64;
      EAX, EBX, ECX, EDX : Unsigned_32;

      Locals_Addr : constant Unsigned_64 :=
         Unsigned_64 (To_Integer (Core_Locals (Core_Number)'Address));

      procedure Syscall_Entry with Import, External_Name => "syscall_entry";
   begin
      --  Enable WP and SSE/2.
      CR0 := CR0 or Shift_Left (1, 16);
      CR0 := (CR0 and (not Shift_Left (1, 2))) or Shift_Left (1, 1);
      CR4 := CR4 or Shift_Left (3, 9);

      --  Enable and configure MCE handling.
      CR4 := CR4 or Shift_Left (1, 6);

      --  Enable several security features if present.
      Snippets.Get_CPUID (7, 0, EAX, EBX, ECX, EDX);
      if (ECX and Shift_Left (1, 2)) /= 0 then
         CR4 := CR4 or Shift_Left (1, 11); --  UMIP.
      end if;
      if (EBX and Shift_Left (1, 7)) /= 0 then
         CR4 := CR4 or Shift_Left (1, 20); --  SMEP.
      end if;
      if (EDX and Shift_Left (1, 20)) /= 0 then
         Snippets.Write_MSR (UCET_MSR, 2#100#); --  Enable just IBT.
         Snippets.Write_MSR (SCET_MSR, 2#100#); --  Enable just IBT.
      end if;

      --  Check XSAVE support.
      --  XXX: Every core will write to the global locations for data, but that
      --  is fine because they will always be the same for all cores.
      Snippets.Get_CPUID (1, 0, EAX, EBX, ECX, EDX);
      if (ECX and Shift_Left (1, 26)) /= 0 then
         Global_Use_XSAVE := True;
         CR4  := CR4 or Shift_Left (1, 18);
         XCR0 := 2#11#; --  Set xsave to be used for x87 and SSE.

         --  Check and enable AVX support.
         if ((ECX and Shift_Left (1, 28)) /= 0) then
            XCR0 := XCR0 or Shift_Left (1, 2);
         end if;

         --  Check and enable AVX512 foundation support.
         Snippets.Get_CPUID (7, 0, EAX, EBX, ECX, EDX);
         if ((EBX and Shift_Left (1, 17)) /= 0) then
            XCR0 := XCR0 or Shift_Left (2#1#, 5); --  Enable OPMASK.
            XCR0 := XCR0 or Shift_Left (2#1#, 6); --  Enable the ZMM regs.
            XCR0 := XCR0 or Shift_Left (2#1#, 7); --  Enable more ZMM regs.
         end if;

         --  Get the size of the xsave area.
         Snippets.Get_CPUID (16#D#, 0, EAX, EBX, ECX, EDX);
         Global_FPU_Size := ECX;

         Snippets.Write_CR4 (CR4);
         Snippets.Write_XCR (0, XCR0);
      else
         Global_Use_XSAVE := False;
         Global_FPU_Size  := 512;
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
      Core_Locals (Core_Number) :=
         (Self            => Core_Locals (Core_Number)'Access,
          Number          => Core_Number,
          LAPIC_ID        => LAPIC,
          LAPIC_Timer_Hz  => APIC.LAPIC_Timer_Calibrate,
          Current_Thread  => Scheduler.Error_TID,
          Current_Process => Userland.Process.Error_PID,
          others          => <>);
      Snippets.Write_GS        (Locals_Addr);
      Snippets.Write_Kernel_GS (Locals_Addr);

      --  Load the TSS.
      Core_Locals (Core_Number).Core_TSS.Stack_Ring0 :=
         To_Address (Integer_Address (Stack_Top));
      GDT.Load_TSS (Core_Locals (Core_Number).Core_TSS'Address);
   end Init_Common;

   function Get_BSP_LAPIC_ID return Unsigned_32 is
      EAX, EBX, ECX, EDX : Unsigned_32;
   begin
      Snippets.Get_CPUID (1, 0, EAX, EBX, ECX, EDX);
      return Shift_Right (EBX, 24) and 16#FF#;
   end Get_BSP_LAPIC_ID;
end Arch.CPU;
