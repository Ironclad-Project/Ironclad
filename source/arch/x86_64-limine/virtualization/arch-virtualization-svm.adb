--  arch-virtualization-svm.adb: AMD-V virtualization code.
--  Copyright (C) 2026 mintsuki, streaksu
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

with Arch.Snippets;
with Arch.MMU;
with Arch.CPU;
with Memory.Physical;
with Memory.MMU;
with System.Machine_Code; use System.Machine_Code;

package body Arch.Virtualization.SVM with SPARK_Mode => Off is
   --  MSR addresses
   EFER_MSR      : constant := 16#C0000080#;
   VM_CR_MSR     : constant := 16#C0010114#;
   VM_HSAVE_PA   : constant := 16#C0010117#;

   --  VM_CR bits
   VM_CR_SVMDIS : constant := Shift_Left (Unsigned_64'(1), 4);

   --  EFER bits
   EFER_SVME : constant := Shift_Left (Unsigned_64'(1), 12);

   --  Host save area addresses (one per core, up to 256 cores), a core with
   --  one being enabled already.
   Host_Save_Areas : array (1 .. 256) of Integer_Address := [others => 0];

   --  Track initialization state
   SVM_Initialized : Boolean := False;

   procedure Initialize (Success : out Boolean) is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Value : Unsigned_64;
      Host_Save_Addr : Integer_Address;
      Core_Num : Positive;
   begin
      --  Check SVM is present via CPUID.
      Arch.Snippets.Get_CPUID (16#80000001#, 0, EAX, EBX, ECX, EDX, Success);
      if not Success then
         return;
      end if;
      if (ECX and Shift_Left (1, 2)) = 0 then
         Success := False;
         return;
      end if;

      --  Check if SVM is disabled by BIOS via VM_CR MSR.
      Value := Snippets.Read_MSR (VM_CR_MSR);
      if (Value and VM_CR_SVMDIS) /= 0 then
         Success := False;
         return;
      end if;

      Core_Num := Arch.CPU.Get_Local.Number;
      if Host_Save_Areas (Core_Num) /= 0 then
         Success := True;
         return;
      end if;

      --  Enable the SVME bit in EFER.
      Value := Snippets.Read_MSR (EFER_MSR);
      Snippets.Write_MSR (EFER_MSR, Value or EFER_SVME);

      --  Allocate and set up host save area for this core.
      Memory.Physical.Alloc (Memory.MMU.Page_Size, Host_Save_Addr);
      if Host_Save_Addr = 0 then
         Success := False;
         return;
      end if;

      --  Store the virtual address for later reference.
      Host_Save_Areas (Core_Num) := Host_Save_Addr;

      --  Convert to physical address and write to VM_HSAVE_PA MSR.
      Host_Save_Addr := Host_Save_Addr - Arch.MMU.Memory_Offset;
      Snippets.Write_MSR (VM_HSAVE_PA, Unsigned_64 (Host_Save_Addr));

      SVM_Initialized := True;
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Initialize;

   function Is_Initialized return Boolean is
   begin
      return SVM_Initialized;
   end Is_Initialized;

   procedure FPU_Save (Area : out FPU_State_Area) is
      Area_Addr : constant Unsigned_64 :=
         Unsigned_64 (To_Integer (Area'Address));
   begin
      Asm
         ("fxsave64 (%%rax)",
          Inputs   => Unsigned_64'Asm_Input ("a", Area_Addr),
          Clobber  => "memory",
          Volatile => True);
   end FPU_Save;

   procedure FPU_Restore (Area : FPU_State_Area) is
      Area_Addr : constant Unsigned_64 :=
         Unsigned_64 (To_Integer (Area'Address));
   begin
      Asm
         ("fxrstor64 (%%rax)",
          Inputs   => Unsigned_64'Asm_Input ("a", Area_Addr),
          Volatile => True);
   end FPU_Restore;

   function XSAVE_Supported return Boolean is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Success : Boolean;
   begin
      Arch.Snippets.Get_CPUID (1, 0, EAX, EBX, ECX, EDX, Success);
      return Success and (ECX and Shift_Left (1, 26)) /= 0;
   end XSAVE_Supported;

   function Get_XSAVE_Size (XCR0_Mask : Unsigned_64) return Unsigned_32 is
      pragma Unreferenced (XCR0_Mask);
      EAX, EBX, ECX, EDX : Unsigned_32;
      Success : Boolean;
   begin
      Arch.Snippets.Get_CPUID (16#D#, 0, EAX, EBX, ECX, EDX, Success);
      return (if Success then ECX else 0);
   end Get_XSAVE_Size;

   function Get_XCR0_Max return Unsigned_64 is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Success : Boolean;
   begin
      Arch.Snippets.Get_CPUID (16#D#, 0, EAX, EBX, ECX, EDX, Success);
      if Success then
         return Shift_Left (Unsigned_64 (EDX), 32) or Unsigned_64 (EAX);
      else
         return 0;
      end if;
   end Get_XCR0_Max;

   function Get_XCR0 return Unsigned_64 is
      Lo, Hi : Unsigned_32;
   begin
      Asm
         ("xgetbv",
          Outputs  => [Unsigned_32'Asm_Output ("=a", Lo),
                       Unsigned_32'Asm_Output ("=d", Hi)],
          Inputs   => Unsigned_32'Asm_Input ("c", 0),
          Volatile => True);
      return Shift_Left (Unsigned_64 (Hi), 32) or Unsigned_64 (Lo);
   end Get_XCR0;

   procedure Set_XCR0 (Value : Unsigned_64) is
      Lo : constant Unsigned_64 := Value and 16#FFFFFFFF#;
      Hi : constant Unsigned_64 := Shift_Right (Value, 32);
   begin
      Asm
         ("xsetbv",
          Inputs   => [Unsigned_32'Asm_Input ("a", Unsigned_32 (Lo)),
                       Unsigned_32'Asm_Input ("d", Unsigned_32 (Hi)),
                       Unsigned_32'Asm_Input ("c", 0)],
          Volatile => True);
   exception
      when Constraint_Error =>
         null;
   end Set_XCR0;

   procedure XSAVE_Save (Area : out XSAVE_Area; XCR0_Mask : Unsigned_64) is
      Lo : constant Unsigned_64 := XCR0_Mask and 16#FFFFFFFF#;
      Hi : constant Unsigned_64 := Shift_Right (XCR0_Mask, 32);
   begin
      Asm
         ("xsave64 (%%rdi)",
          Inputs   => [System.Address'Asm_Input ("D", Area'Address),
                       Unsigned_32'Asm_Input ("a", Unsigned_32 (Lo)),
                       Unsigned_32'Asm_Input ("d", Unsigned_32 (Hi))],
          Clobber  => "memory",
          Volatile => True);
   exception
      when Constraint_Error =>
         null;
   end XSAVE_Save;

   procedure XSAVE_Restore (Area : XSAVE_Area; XCR0_Mask : Unsigned_64) is
      Lo : constant Unsigned_64 := XCR0_Mask and 16#FFFFFFFF#;
      Hi : constant Unsigned_64 := Shift_Right (XCR0_Mask, 32);
   begin
      Asm
         ("xrstor64 (%%rdi)",
          Inputs  => [System.Address'Asm_Input ("D", Area'Address),
                      Unsigned_32'Asm_Input ("a", Unsigned_32 (Lo)),
                      Unsigned_32'Asm_Input ("d", Unsigned_32 (Hi))],
         Volatile => True);
   exception
      when Constraint_Error =>
         null;
   end XSAVE_Restore;

   procedure VMRUN (VMCB_PA : Unsigned_64; GPRs : in out Guest_GPRs) is
   begin
      --  The VMRUN sequence:
      --  1. Save host GPRs (callee-saved per ABI, rest are scratch)
      --  2. Load guest GPRs from GPRs structure
      --  3. Load VMCB physical address into RAX
      --  4. CLGI - clear GIF to prevent interrupts during entry
      --  5. VMRUN - enter guest mode
      --  6. On VMEXIT: save guest GPRs back to structure
      --  7. STGI - restore GIF
      --  8. Restore host GPRs

      Asm
         ("pushq %%rbx;"          & --  Save callee-saved host registers
          "pushq %%rbp;"          &
          "pushq %%r12;"          &
          "pushq %%r13;"          &
          "pushq %%r14;"          &
          "pushq %%r15;"          &
          --  Save RDI (GPRs pointer) and RSI (VMCB_PA) for later
          "pushq %%rdi;"          &
          "pushq %%rsi;"          &
          --  Load guest GPRs from structure at [RDI]
          "movq 0x00(%%rdi), %%rbx;"  &
          "movq 0x08(%%rdi), %%rcx;"  &
          "movq 0x10(%%rdi), %%rdx;"  &
          --  Load RSI before RDI since we need RDI for addressing
          "movq 0x18(%%rdi), %%rsi;"  &
          "movq 0x28(%%rdi), %%rbp;"  &
          "movq 0x30(%%rdi), %%r8;"   &
          "movq 0x38(%%rdi), %%r9;"   &
          "movq 0x40(%%rdi), %%r10;"  &
          "movq 0x48(%%rdi), %%r11;"  &
          "movq 0x50(%%rdi), %%r12;"  &
          "movq 0x58(%%rdi), %%r13;"  &
          "movq 0x60(%%rdi), %%r14;"  &
          "movq 0x68(%%rdi), %%r15;"  &
          --  Load guest RDI last (overwrites our pointer)
          "movq 0x20(%%rdi), %%rdi;"  &
          --  Get VMCB_PA from stack into RAX
          "movq (%%rsp), %%rax;"      &
          --  CLGI - Clear Global Interrupt Flag
          "clgi;"                     &
          --  VMRUN - Enter guest mode
          "vmrun %%rax;"              &
          --  STGI - Set Global Interrupt Flag
          "stgi;"                     &
          --  Save guest GPRs back to structure
          --  Get GPRs pointer from stack (offset 8, RSI is at 0)
          "xchgq %%rdi, 8(%%rsp);"    &
          --  RDI now has GPRs ptr, guest RDI on stack at 8(RSP)
          "movq %%rbx, 0x00(%%rdi);"  &
          "movq %%rcx, 0x08(%%rdi);"  &
          "movq %%rdx, 0x10(%%rdi);"  &
          "movq %%rsi, 0x18(%%rdi);"  &
          --  Get guest RDI from stack and save it
          "movq 8(%%rsp), %%rax;"     &
          "movq %%rax, 0x20(%%rdi);"  &
          "movq %%rbp, 0x28(%%rdi);"  &
          "movq %%r8,  0x30(%%rdi);"  &
          "movq %%r9,  0x38(%%rdi);"  &
          "movq %%r10, 0x40(%%rdi);"  &
          "movq %%r11, 0x48(%%rdi);"  &
          "movq %%r12, 0x50(%%rdi);"  &
          "movq %%r13, 0x58(%%rdi);"  &
          "movq %%r14, 0x60(%%rdi);"  &
          "movq %%r15, 0x68(%%rdi);"  &
          --  Pop saved RSI/VMCB_PA and RDI/GPRs ptr
          "addq $16, %%rsp;"          &
          --  Restore callee-saved host registers
          "popq %%r15;"               &
          "popq %%r14;"               &
          "popq %%r13;"               &
          "popq %%r12;"               &
          "popq %%rbp;"               &
          "popq %%rbx",
          Inputs   => [System.Address'Asm_Input ("D", GPRs'Address),
                       Unsigned_64'Asm_Input ("S", VMCB_PA)],
          Clobber  => "memory,cc,rax,rcx,rdx,r8,r9,r10,r11",
          Volatile => True);
   end VMRUN;
end Arch.Virtualization.SVM;
