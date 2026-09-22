--  arch-vmx.adb: Intel VT-x virtualization code.
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

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System.Machine_Code;
with Arch.Snippets;
with Arch.CPU;
with Arch.MMU;
with Arch.GDT;
with Memory.Physical;
with Memory.MMU;

package body Arch.Virtualization.VMX with SPARK_Mode => Off is
   IA32_FEATURE_CONTROL_MSR : constant := 16#3A#;

   VMXON_LOCK_FLAG   : constant := 2#001#;
   VMXON_ENABLE_FLAG : constant := 2#100#;

   --  The VMXON region of each core, a core with one being enabled already.
   VMXON_Regions   : array (1 .. 256) of Integer_Address := [others => 0];
   VMX_Initialized : Boolean := False;

   --  Whether INVEPT has its single-context type (IA32_VMX_EPT_VPID_CAP bit
   --  25); Initialize refuses VMX unless it has that or all-context (bit 26).
   Single_Context_INVEPT : Boolean := False;

   --  CR0/CR4 fixed bits (populated during initialization)
   CR0_Fixed0 : Unsigned_64 := 0;
   CR0_Fixed1 : Unsigned_64 := 16#FFFF_FFFF_FFFF_FFFF#;
   CR4_Fixed0 : Unsigned_64 := 0;
   CR4_Fixed1 : Unsigned_64 := 16#FFFF_FFFF_FFFF_FFFF#;

   function Is_Initialized return Boolean is
   begin
      return VMX_Initialized;
   end Is_Initialized;

   procedure Initialize (Success : out Boolean) is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Value, Revision_ID : Unsigned_64;
      Success_U16 : Unsigned_16;
      Core_Num : Positive;
      VMXON_Region_Addr : Integer_Address;
   begin
      Core_Num := Arch.CPU.Get_Local.Number;
      if VMXON_Regions (Core_Num) /= 0 then
         Success := True;
         return;
      end if;

      --  Check VMX is present via CPUID.1:ECX[5]
      Arch.Snippets.Get_CPUID
         (Leaf    => 1,
          Subleaf => 0,
          EAX     => EAX,
          EBX     => EBX,
          ECX     => ECX,
          EDX     => EDX,
          Success => Success);
      if not Success then
         return;
      end if;
      if (ECX and Shift_Left (1, 5)) = 0 then
         Success := False;
         return;
      end if;

      --  Make sure VMX is enabled in the MSR. If it is locked already, we
      --  cannot enable it, that's how the CPU works.
      Value := Snippets.Read_MSR (IA32_FEATURE_CONTROL_MSR);
      if ((Value and VMXON_LOCK_FLAG) /= 0) then
         if (Value and VMXON_ENABLE_FLAG) = 0 then
            Success := False;
            return;
         end if;
      else
         Value := Value or VMXON_LOCK_FLAG or VMXON_ENABLE_FLAG;
         Snippets.Write_MSR (IA32_FEATURE_CONTROL_MSR, Value);
      end if;

      --  VMCS_Setup asks for EPT through Adjust_Controls, which leaves out a
      --  control the processor does not allow, so the secondary controls must
      --  exist (IA32_VMX_PROCBASED_CTLS bit 63) and offer EPT
      --  (IA32_VMX_PROCBASED_CTLS2 bit 33) (Intel SDM 325462-092US, Vol. 3D
      --  A.3.3). Only then does IA32_VMX_EPT_VPID_CAP exist, and it must offer
      --  the 4-level walk (bit 6) and write-back structures (bit 14) of the
      --  EPT pointer VMCS_Setup writes, and INVEPT (bit 20) of a single-
      --  (bit 25) or all-context (bit 26) type, which a VCPU's entries need
      --  (Vol. 3D A.10).
      declare
         One  : constant Unsigned_64 := 1;
         Caps : Unsigned_64;
      begin
         if (Snippets.Read_MSR (IA32_VMX_PROCBASED_CTLS) and
             Shift_Left (One, 63)) = 0 or else
            (Snippets.Read_MSR (IA32_VMX_PROCBASED_CTLS2) and
             Shift_Left (One, 33)) = 0
         then
            Success := False;
            return;
         end if;
         Caps := Snippets.Read_MSR (IA32_VMX_EPT_VPID_CAP);
         if (Caps and Shift_Left (One, 6)) = 0 or
            (Caps and Shift_Left (One, 14)) = 0 or
            (Caps and Shift_Left (One, 20)) = 0 or
            (Caps and (Shift_Left (One, 25) or Shift_Left (One, 26))) = 0
         then
            Success := False;
            return;
         end if;
         Single_Context_INVEPT := (Caps and Shift_Left (One, 25)) /= 0;
      end;

      --  Read CR0/CR4 fixed bits MSRs
      CR0_Fixed0 := Snippets.Read_MSR (IA32_VMX_CR0_FIXED0);
      CR0_Fixed1 := Snippets.Read_MSR (IA32_VMX_CR0_FIXED1);
      CR4_Fixed0 := Snippets.Read_MSR (IA32_VMX_CR4_FIXED0);
      CR4_Fixed1 := Snippets.Read_MSR (IA32_VMX_CR4_FIXED1);

      --  Enable NE in CR0 (required) and set fixed bits
      Value := Snippets.Read_CR0;
      Value := Value or 2#100000#;  --  NE (bit 5)
      Value := (Value or CR0_Fixed0) and CR0_Fixed1;
      Snippets.Write_CR0 (Value);

      --  Enable VMX in CR4 (bit 13) and set fixed bits
      Value := Snippets.Read_CR4;
      Value := Value or Shift_Left (1, 13);  --  VMXE
      Value := (Value or CR4_Fixed0) and CR4_Fixed1;
      Snippets.Write_CR4 (Value);

      --  Enable VMXON.
      --  VMXON region = 4KB page containing revision ID from IA32_VMX_BASIC
      Revision_ID := Snippets.Read_MSR (IA32_VMX_BASIC);
      Memory.Physical.Alloc (Memory.MMU.Page_Size, VMXON_Region_Addr);
      if VMXON_Region_Addr = 0 then
         Success := False;
         return;
      end if;

      --  Zero the region first
      declare
         type Byte_Array is array (0 .. 4095) of Unsigned_8;
         Region_Bytes : Byte_Array
            with Import, Address => To_Address (VMXON_Region_Addr);
      begin
         Region_Bytes := [others => 0];
      end;

      --  Write revision ID to first 4 bytes
      declare
         Region : Unsigned_32
            with Import, Address => To_Address (VMXON_Region_Addr);
      begin
         Region := Unsigned_32 (Revision_ID and 16#7FFF_FFFF#);
      end;

      --  Convert to physical address for VMXON
      declare
         VMXON_PA : Integer_Address;
      begin
         VMXON_PA := VMXON_Region_Addr - Arch.MMU.Memory_Offset;
         System.Machine_Code.Asm
            ("xorw %%dx, %%dx;" &
             "movw $0x1, %%cx;" &
             "vmxon %1;"        &
             "cmovnc %%cx, %%dx",
              Outputs  => Unsigned_16'Asm_Output ("=d", Success_U16),
              Inputs   => Integer_Address'Asm_Input ("m", VMXON_PA),
              Clobber  => "memory,cc,rcx",
              Volatile => True);
      end;

      Success := Success_U16 /= 0;
      if Success then
         VMXON_Regions (Core_Num) := VMXON_Region_Addr;
         VMX_Initialized := True;
      else
         Memory.Physical.Free (Interfaces.C.size_t (VMXON_Region_Addr));
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Write_VMCS_Revision - Write revision ID to VMCS region before VMCLEAR
   ---------------------------------------------------------------------------
   procedure Write_VMCS_Revision (VMCS_VA : Integer_Address) is
      Revision : constant Unsigned_64 := Snippets.Read_MSR (IA32_VMX_BASIC);
      type VMCS_Header_Ptr is access all Unsigned_32;
      function To_Header is new Ada.Unchecked_Conversion
         (Integer_Address, VMCS_Header_Ptr);
      Header : constant VMCS_Header_Ptr := To_Header (VMCS_VA);
   begin
      --  Write revision ID to first 4 bytes (bits 30:0, bit 31 must be 0)
      Header.all := Unsigned_32 (Revision and 16#7FFFFFFF#);
   exception
      when Constraint_Error =>
         null;
   end Write_VMCS_Revision;

   ---------------------------------------------------------------------------
   --  VMCLEAR - Clear VMCS launch state
   ---------------------------------------------------------------------------
   procedure VMCLEAR (VMCS_PA : Unsigned_64; Success : out Boolean) is
      RFlags   : Unsigned_64;
      PA_Local : constant Unsigned_64 := VMCS_PA;
   begin
      System.Machine_Code.Asm
         ("vmclear %1;"  &
          "pushfq;"      &
          "popq %0",
          Outputs  => Unsigned_64'Asm_Output ("=r", RFlags),
          Inputs   => Unsigned_64'Asm_Input ("m", PA_Local),
          Clobber  => "memory,cc",
          Volatile => True);
      --  CF=1 or ZF=1 indicates failure
      Success := (RFlags and 2#1000001#) = 0;
   end VMCLEAR;

   ---------------------------------------------------------------------------
   --  VMPTRLD - Load VMCS pointer
   ---------------------------------------------------------------------------
   procedure VMPTRLD (VMCS_PA : Unsigned_64; Success : out Boolean) is
      RFlags   : Unsigned_64;
      PA_Local : constant Unsigned_64 := VMCS_PA;
   begin
      System.Machine_Code.Asm
         ("vmptrld %1;"  &
          "pushfq;"      &
          "popq %0",
          Outputs  => Unsigned_64'Asm_Output ("=r", RFlags),
          Inputs   => Unsigned_64'Asm_Input ("m", PA_Local),
          Clobber  => "memory,cc",
          Volatile => True);
      Success := (RFlags and 2#1000001#) = 0;
   end VMPTRLD;

   ---------------------------------------------------------------------------
   --  VMPTRST - Store current VMCS pointer
   ---------------------------------------------------------------------------
   function VMPTRST return Unsigned_64 is
      Result : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("vmptrst %0",
          Outputs  => Unsigned_64'Asm_Output ("=m", Result),
          Clobber  => "memory",
          Volatile => True);
      return Result;
   end VMPTRST;

   ---------------------------------------------------------------------------
   --  INVEPT - Invalidate cached EPT translations (Intel SDM 325462-092US,
   --  Vol. 3C 31.4.3.1)
   ---------------------------------------------------------------------------
   procedure INVEPT (EPTP : Unsigned_64; Success : out Boolean) is
      type Descriptor is record
         EPT_Pointer : Unsigned_64;
         Reserved    : Unsigned_64;
      end record with Size => 128;
      Desc   : constant Descriptor := (EPT_Pointer => EPTP, Reserved => 0);
      Kind   : constant Unsigned_64 :=
         (if Single_Context_INVEPT then 1 else 2);
      RFlags : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("invept %1, %2;" &
          "pushfq;"         &
          "popq %0",
          Outputs  => Unsigned_64'Asm_Output ("=r", RFlags),
          Inputs   => [Descriptor'Asm_Input ("m", Desc),
                       Unsigned_64'Asm_Input ("r", Kind)],
          Clobber  => "memory,cc",
          Volatile => True);
      Success := (RFlags and 2#1000001#) = 0;
   end INVEPT;

   ---------------------------------------------------------------------------
   --  VMX_Read - Read a VMCS field
   ---------------------------------------------------------------------------
   function VMX_Read (Encoding : Unsigned_64) return Unsigned_64 is
      Value : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("vmread %%rax, %0",
          Outputs  => Unsigned_64'Asm_Output ("=rm", Value),
          Inputs   => Unsigned_64'Asm_Input ("a", Encoding),
          Clobber  => "cc",
          Volatile => True);
      return Value;
   end VMX_Read;

   ---------------------------------------------------------------------------
   --  VMX_Write - Write a VMCS field
   ---------------------------------------------------------------------------
   procedure VMX_Write (Encoding, Value : Unsigned_64; Success : out Boolean)
   is
      RFlags : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("vmwrite %%rdx, %%rax;" &
          "pushfq;"               &
          "popq %0",
          Outputs  => Unsigned_64'Asm_Output ("=r", RFlags),
          Inputs   => [Unsigned_64'Asm_Input ("a", Encoding),
                       Unsigned_64'Asm_Input ("d", Value)],
          Clobber  => "memory,cc",
          Volatile => True);
      Success := (RFlags and 2#1000001#) = 0;
   end VMX_Write;

   ---------------------------------------------------------------------------
   --  Helper to write a VMCS field (ignoring success for convenience)
   ---------------------------------------------------------------------------
   procedure VMCS_Write_Unchecked (Encoding, Value : Unsigned_64) is
      Dummy : Boolean;
   begin
      VMX_Write (Encoding, Value, Dummy);
   end VMCS_Write_Unchecked;

   ---------------------------------------------------------------------------
   --  Adjust control field value to satisfy allowed-0 and allowed-1 bits
   --  from the capability MSR (low 32 bits = allowed-0, high 32 = allowed-1)
   ---------------------------------------------------------------------------
   function Adjust_Controls
      (Value : Unsigned_32;
       MSR   : Unsigned_64) return Unsigned_32
   is
   begin
      --  Set bits that must be 1, clear bits that must be 0
      return (Value or Unsigned_32 (MSR and 16#FFFFFFFF#)) and
             Unsigned_32 (Shift_Right (MSR, 32) and 16#FFFFFFFF#);
   exception
      when Constraint_Error =>
         return 0;
   end Adjust_Controls;

   ---------------------------------------------------------------------------
   --  Apply_CR0_Fixed - Apply CR0 fixed bits for guest state
   ---------------------------------------------------------------------------
   function Apply_CR0_Fixed
      (Value : Unsigned_64;
       For_Unrestricted_Paging : Boolean) return Unsigned_64
   is
      Fixed0_Adj : Unsigned_64 := CR0_Fixed0;
   begin
      if For_Unrestricted_Paging then
         --  With unrestricted guest, PE (bit 0) and PG (bit 31) not required
         Fixed0_Adj := Fixed0_Adj and not 16#8000_0001#;
      end if;
      return (Value or Fixed0_Adj) and CR0_Fixed1;
   end Apply_CR0_Fixed;

   ---------------------------------------------------------------------------
   --  Apply_CR4_Fixed - Apply CR4 fixed bits for guest state
   --  Preserves LA57 (bit 12) even if not in CR4_Fixed1 because the guest
   --  can use 5-level paging independently of whether host VMX uses it.
   ---------------------------------------------------------------------------
   function Apply_CR4_Fixed (Value : Unsigned_64) return Unsigned_64 is
      LA57_Bit : constant Unsigned_64 := Value and 16#1000#;
      Result   : Unsigned_64;
   begin
      Result := (Value or CR4_Fixed0) and CR4_Fixed1;
      Result := Result or LA57_Bit;
      return Result;
   end Apply_CR4_Fixed;

   ---------------------------------------------------------------------------
   --  VMCS_Setup - Initialize VMCS with default 32-bit protected mode guest
   ---------------------------------------------------------------------------
   procedure VMCS_Setup
      (VMCS_VA   : Integer_Address;
       EPT_PA    : Unsigned_64;
       IOPM_PA   : Unsigned_64;
       MSRPM_PA  : Unsigned_64;
       VPID_Val  : Unsigned_16;
       Success   : out Boolean)
   is
      VMCS_PA   : Unsigned_64;
      Revision  : Unsigned_64;
      OK        : Boolean;
      Pin_Ctls  : Unsigned_32;
      Proc_Ctls : Unsigned_32;
      Proc2_Ctls : Unsigned_32;
      Exit_Ctls : Unsigned_32;
      Entry_Ctls : Unsigned_32;
      Host_CR0  : Unsigned_64;
      Host_CR3  : Unsigned_64;
      Host_CR4  : Unsigned_64;
      Host_EFER : Unsigned_64;
   begin
      Success := False;

      --  Calculate physical address
      VMCS_PA := Unsigned_64 (VMCS_VA - Arch.MMU.Memory_Offset);

      --  Zero the VMCS
      declare
         type Byte_Array is array (0 .. 4095) of Unsigned_8;
         VMCS_Bytes : Byte_Array
            with Import, Address => To_Address (VMCS_VA);
      begin
         VMCS_Bytes := [others => 0];
      end;

      --  Write revision ID to first 4 bytes (must NOT set bit 31)
      Revision := Snippets.Read_MSR (IA32_VMX_BASIC);
      declare
         VMCS_Rev : Unsigned_32
            with Import, Address => To_Address (VMCS_VA);
      begin
         VMCS_Rev := Unsigned_32 (Revision and 16#7FFFFFFF#);
      end;

      --  Clear and load the VMCS
      VMCLEAR (VMCS_PA, OK);
      if not OK then
         return;
      end if;
      VMPTRLD (VMCS_PA, OK);
      if not OK then
         return;
      end if;

      --  Read capability MSRs for control fields
      --  Use TRUE MSRs if available (bit 55 of IA32_VMX_BASIC)
      declare
         Basic_MSR : constant Unsigned_64 :=
            Snippets.Read_MSR (IA32_VMX_BASIC);
         Use_True  : constant Boolean :=
            (Basic_MSR and Shift_Left (Unsigned_64'(1), 55)) /= 0;
         Pin_MSR   : Unsigned_64;
         Proc_MSR  : Unsigned_64;
         Exit_MSR  : Unsigned_64;
         Entry_MSR : Unsigned_64;
         Proc2_MSR : Unsigned_64;
      begin
         if Use_True then
            Pin_MSR := Snippets.Read_MSR (IA32_VMX_TRUE_PINBASED);
            Proc_MSR := Snippets.Read_MSR (IA32_VMX_TRUE_PROCBASED);
            Exit_MSR := Snippets.Read_MSR (IA32_VMX_TRUE_EXIT);
            Entry_MSR := Snippets.Read_MSR (IA32_VMX_TRUE_ENTRY);
         else
            Pin_MSR := Snippets.Read_MSR (IA32_VMX_PINBASED_CTLS);
            Proc_MSR := Snippets.Read_MSR (IA32_VMX_PROCBASED_CTLS);
            Exit_MSR := Snippets.Read_MSR (IA32_VMX_EXIT_CTLS);
            Entry_MSR := Snippets.Read_MSR (IA32_VMX_ENTRY_CTLS);
         end if;
         Proc2_MSR := Snippets.Read_MSR (IA32_VMX_PROCBASED_CTLS2);

         --  Pin-based controls: NMI exiting, external interrupt exiting
         Pin_Ctls := Adjust_Controls
            (PIN_EXTERNAL_INTERRUPT_EXIT or PIN_NMI_EXIT, Pin_MSR);

         --  Primary processor-based controls
         Proc_Ctls := Adjust_Controls
            (CPU_HLT_EXIT or                --  Exit on HLT
             CPU_USE_IO_BITMAPS or          --  Use I/O bitmaps
             CPU_USE_MSR_BITMAPS or         --  Use MSR bitmap
             CPU_SECONDARY_CONTROLS,        --  Enable secondary controls
             Proc_MSR);

         --  Secondary processor-based controls
         Proc2_Ctls := Adjust_Controls
            (CPU2_ENABLE_EPT or             --  Enable EPT
             CPU2_ENABLE_VPID or            --  Enable VPID
             CPU2_UNRESTRICTED_GUEST,       --  Allow real mode guests
             Proc2_MSR);

         --  VM-exit controls: Host address space size (64-bit host)
         Exit_Ctls := Adjust_Controls
            (EXIT_HOST_ADDR_SPACE_SIZE or   --  64-bit host
             EXIT_SAVE_IA32_EFER or         --  Save guest EFER
             EXIT_LOAD_IA32_EFER,           --  Load host EFER
             Exit_MSR);

         --  VM-entry controls
         Entry_Ctls := Adjust_Controls
            (ENTRY_LOAD_IA32_EFER,          --  Load guest EFER
             Entry_MSR);
      end;

      --  Write control fields
      VMCS_Write_Unchecked (VMCS_PIN_BASED_EXEC_CTRL, Unsigned_64 (Pin_Ctls));
      VMCS_Write_Unchecked (VMCS_PRIMARY_EXEC_CTRL, Unsigned_64 (Proc_Ctls));
      VMCS_Write_Unchecked
         (VMCS_SECONDARY_EXEC_CTRL, Unsigned_64 (Proc2_Ctls));
      VMCS_Write_Unchecked (VMCS_EXIT_CONTROLS, Unsigned_64 (Exit_Ctls));
      VMCS_Write_Unchecked (VMCS_ENTRY_CONTROLS, Unsigned_64 (Entry_Ctls));

      --  Exception bitmap: intercept #GP (13) and #PF (14)
      VMCS_Write_Unchecked (VMCS_EXCEPTION_BITMAP,
         Shift_Left (Unsigned_64'(1), 13) or Shift_Left (Unsigned_64'(1), 14));

      --  Page fault error code mask/match (intercept all page faults)
      VMCS_Write_Unchecked (VMCS_PAGE_FAULT_ERR_MASK, 0);
      VMCS_Write_Unchecked (VMCS_PAGE_FAULT_ERR_MATCH, 0);

      --  CR3 target count (0 = exit on all CR3 loads)
      VMCS_Write_Unchecked (VMCS_CR3_TARGET_COUNT, 0);

      --  MSR store/load counts
      VMCS_Write_Unchecked (VMCS_EXIT_MSR_STORE_COUNT, 0);
      VMCS_Write_Unchecked (VMCS_EXIT_MSR_LOAD_COUNT, 0);
      VMCS_Write_Unchecked (VMCS_ENTRY_MSR_LOAD_COUNT, 0);

      --  I/O bitmaps (2 x 4KB = 8KB total)
      VMCS_Write_Unchecked (VMCS_IO_BITMAP_A, IOPM_PA);
      VMCS_Write_Unchecked (VMCS_IO_BITMAP_B, IOPM_PA + 16#1000#);

      --  MSR bitmap
      VMCS_Write_Unchecked (VMCS_MSR_BITMAP, MSRPM_PA);

      --  EPT pointer (WB memory type, 4-level page walk)
      VMCS_Write_Unchecked (VMCS_EPT_POINTER,
         EPT_PA or EPT_POINTER_WB_4LEVEL);

      --  VPID
      VMCS_Write_Unchecked (VMCS_VPID, Unsigned_64 (VPID_Val));

      --  TSC offset
      VMCS_Write_Unchecked (VMCS_TSC_OFFSET, 0);

      --  CR0/CR4 guest/host masks and read shadows
      --  Mask = 0 means guest can modify CR0/CR4 freely
      VMCS_Write_Unchecked (VMCS_CR0_GUEST_HOST_MASK, 0);
      VMCS_Write_Unchecked (VMCS_CR4_GUEST_HOST_MASK, 0);
      VMCS_Write_Unchecked (VMCS_CR0_READ_SHADOW, 0);
      VMCS_Write_Unchecked (VMCS_CR4_READ_SHADOW, 0);

      --  Entry interrupt info (no injection initially)
      VMCS_Write_Unchecked (VMCS_ENTRY_INTR_INFO, 0);

      -------------------------------------------------------------------------
      --  Host State - Where to return after VM exit
      -------------------------------------------------------------------------
      Host_CR0 := Snippets.Read_CR0;
      Host_CR3 := Snippets.Read_CR3;
      Host_CR4 := Snippets.Read_CR4;
      Host_EFER := Snippets.Read_MSR (16#C000_0080#);  --  IA32_EFER

      VMCS_Write_Unchecked (VMCS_HOST_CR0, Host_CR0);
      VMCS_Write_Unchecked (VMCS_HOST_CR3, Host_CR3);
      VMCS_Write_Unchecked (VMCS_HOST_CR4, Host_CR4);
      VMCS_Write_Unchecked (VMCS_HOST_IA32_EFER, Host_EFER);

      --  Host segment selectors (clear RPL bits)
      VMCS_Write_Unchecked (VMCS_HOST_CS_SELECTOR,
         Unsigned_64 (Arch.GDT.Kernel_Code64_Segment) and 16#FFF8#);
      VMCS_Write_Unchecked (VMCS_HOST_SS_SELECTOR,
         Unsigned_64 (Arch.GDT.Kernel_Data64_Segment) and 16#FFF8#);
      VMCS_Write_Unchecked (VMCS_HOST_DS_SELECTOR,
         Unsigned_64 (Arch.GDT.Kernel_Data64_Segment) and 16#FFF8#);
      VMCS_Write_Unchecked (VMCS_HOST_ES_SELECTOR,
         Unsigned_64 (Arch.GDT.Kernel_Data64_Segment) and 16#FFF8#);
      VMCS_Write_Unchecked (VMCS_HOST_FS_SELECTOR, 0);
      VMCS_Write_Unchecked (VMCS_HOST_GS_SELECTOR, 0);
      VMCS_Write_Unchecked (VMCS_HOST_TR_SELECTOR,
         Unsigned_64 (Arch.GDT.TSS_Segment) and 16#FFF8#);

      --  Host FS/GS base (will be set properly before VMLAUNCH)
      VMCS_Write_Unchecked (VMCS_HOST_FS_BASE, Snippets.Read_FS);
      VMCS_Write_Unchecked (VMCS_HOST_GS_BASE, Snippets.Read_GS);

      --  Host TR base, GDTR base, IDTR base - read via inline assembly
      declare
         --  SGDT/SIDT store 10 bytes: 2-byte limit + 8-byte base
         type DTR_Struct is record
            Limit : Unsigned_16;
            Base  : Unsigned_64;
         end record with Pack;
         GDTR     : DTR_Struct;
         IDTR     : DTR_Struct;
         TR_Base  : Unsigned_64;
      begin
         --  Read GDTR using SGDT
         System.Machine_Code.Asm
            ("sgdt %0",
             Outputs  => DTR_Struct'Asm_Output ("=m", GDTR),
             Volatile => True);
         --  Read IDTR using SIDT
         System.Machine_Code.Asm
            ("sidt %0",
             Outputs  => DTR_Struct'Asm_Output ("=m", IDTR),
             Volatile => True);
         --  For TR base, we need to read the TSS descriptor from GDT
         --  TSS descriptor is at GDT + TSS_Segment, and spans 16 bytes
         declare
            type TSS_Desc is record
               Limit_Low    : Unsigned_16;
               Base_Low     : Unsigned_16;
               Base_Mid     : Unsigned_8;
               Attrib1      : Unsigned_8;
               Attrib2      : Unsigned_8;
               Base_High    : Unsigned_8;
               Base_Upper   : Unsigned_32;
               Reserved     : Unsigned_32;
            end record with Pack;
            Desc_Addr : constant Integer_Address :=
               Integer_Address (GDTR.Base) + Arch.GDT.TSS_Segment;
            Desc : TSS_Desc
               with Import, Address => To_Address (Desc_Addr);
         begin
            TR_Base := Unsigned_64 (Desc.Base_Low) or
               Shift_Left (Unsigned_64 (Desc.Base_Mid), 16) or
               Shift_Left (Unsigned_64 (Desc.Base_High), 24) or
               Shift_Left (Unsigned_64 (Desc.Base_Upper), 32);
         end;

         VMCS_Write_Unchecked (VMCS_HOST_TR_BASE, TR_Base);
         VMCS_Write_Unchecked (VMCS_HOST_GDTR_BASE, GDTR.Base);
         VMCS_Write_Unchecked (VMCS_HOST_IDTR_BASE, IDTR.Base);
      end;

      --  Host SYSENTER MSRs (may be 0 if not used)
      VMCS_Write_Unchecked (VMCS_HOST_IA32_SYSENTER_CS, 0);
      VMCS_Write_Unchecked (VMCS_HOST_SYSENTER_ESP, 0);
      VMCS_Write_Unchecked (VMCS_HOST_SYSENTER_EIP, 0);

      --  Host RSP and RIP will be set by VMLAUNCH_VMRESUME assembly

      -------------------------------------------------------------------------
      --  Guest State - Initial 32-bit protected mode, no paging
      -------------------------------------------------------------------------

      --  Guest CR0: PE=1, ET=1 (protected mode, no paging)
      --  With unrestricted guest enabled, PE and PG are NOT required to be 1
      declare
         Guest_CR0   : Unsigned_64 := 16#11#;  --  PE | ET
         Fixed0_Adj  : Unsigned_64 := CR0_Fixed0;
         Unrestricted : constant Boolean :=
            (Proc2_Ctls and CPU2_UNRESTRICTED_GUEST) /= 0;
      begin
         if Unrestricted then
            --  Clear PE (bit 0) and PG (bit 31) requirements
            Fixed0_Adj := Fixed0_Adj and not 16#8000_0001#;
         end if;
         Guest_CR0 := (Guest_CR0 or Fixed0_Adj) and CR0_Fixed1;
         VMCS_Write_Unchecked (VMCS_GUEST_CR0, Guest_CR0);
      end;

      VMCS_Write_Unchecked (VMCS_GUEST_CR3, 0);

      --  Guest CR4: Apply fixed bits
      declare
         Guest_CR4 : Unsigned_64 := 0;
      begin
         Guest_CR4 := (Guest_CR4 or CR4_Fixed0) and CR4_Fixed1;
         VMCS_Write_Unchecked (VMCS_GUEST_CR4, Guest_CR4);
      end;

      --  Guest EFER (no LME/LMA initially)
      VMCS_Write_Unchecked (VMCS_GUEST_IA32_EFER, 0);

      --  Guest DR7
      VMCS_Write_Unchecked (VMCS_GUEST_DR7, 16#0000_0400#);

      --  Guest RFLAGS (bit 1 must be set)
      VMCS_Write_Unchecked (VMCS_GUEST_RFLAGS, 2);

      --  Guest RIP, RSP (will be set by userspace)
      VMCS_Write_Unchecked (VMCS_GUEST_RIP, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_RSP, 0);

      --  Guest segments - 32-bit protected mode flat model
      --  CS: selector 0x08, flat, execute/read
      VMCS_Write_Unchecked (VMCS_GUEST_CS_SELECTOR, 16#08#);
      VMCS_Write_Unchecked (VMCS_GUEST_CS_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_CS_LIMIT, 16#FFFF_FFFF#);
      VMCS_Write_Unchecked
         (VMCS_GUEST_CS_AR, Unsigned_64 (SEG_AR_CODE_32_DPL0));

      --  DS, ES, SS, FS, GS: selector 0x10, flat, read/write
      VMCS_Write_Unchecked (VMCS_GUEST_DS_SELECTOR, 16#10#);
      VMCS_Write_Unchecked (VMCS_GUEST_DS_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_DS_LIMIT, 16#FFFF_FFFF#);
      VMCS_Write_Unchecked
         (VMCS_GUEST_DS_AR, Unsigned_64 (SEG_AR_DATA_32_DPL0));

      VMCS_Write_Unchecked (VMCS_GUEST_ES_SELECTOR, 16#10#);
      VMCS_Write_Unchecked (VMCS_GUEST_ES_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_ES_LIMIT, 16#FFFF_FFFF#);
      VMCS_Write_Unchecked
         (VMCS_GUEST_ES_AR, Unsigned_64 (SEG_AR_DATA_32_DPL0));

      VMCS_Write_Unchecked (VMCS_GUEST_SS_SELECTOR, 16#10#);
      VMCS_Write_Unchecked (VMCS_GUEST_SS_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_SS_LIMIT, 16#FFFF_FFFF#);
      VMCS_Write_Unchecked
         (VMCS_GUEST_SS_AR, Unsigned_64 (SEG_AR_DATA_32_DPL0));

      VMCS_Write_Unchecked (VMCS_GUEST_FS_SELECTOR, 16#10#);
      VMCS_Write_Unchecked (VMCS_GUEST_FS_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_FS_LIMIT, 16#FFFF_FFFF#);
      VMCS_Write_Unchecked
         (VMCS_GUEST_FS_AR, Unsigned_64 (SEG_AR_DATA_32_DPL0));

      VMCS_Write_Unchecked (VMCS_GUEST_GS_SELECTOR, 16#10#);
      VMCS_Write_Unchecked (VMCS_GUEST_GS_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_GS_LIMIT, 16#FFFF_FFFF#);
      VMCS_Write_Unchecked
         (VMCS_GUEST_GS_AR, Unsigned_64 (SEG_AR_DATA_32_DPL0));

      --  TR: busy 32-bit TSS
      VMCS_Write_Unchecked (VMCS_GUEST_TR_SELECTOR, 16#18#);
      VMCS_Write_Unchecked (VMCS_GUEST_TR_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_TR_LIMIT, 16#67#);  --  Minimum TSS.
      VMCS_Write_Unchecked (VMCS_GUEST_TR_AR, Unsigned_64 (SEG_AR_TSS_BUSY));

      --  LDTR: unusable
      VMCS_Write_Unchecked (VMCS_GUEST_LDTR_SELECTOR, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_LDTR_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_LDTR_LIMIT, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_LDTR_AR, Unsigned_64 (SEG_AR_UNUSABLE));

      --  GDTR: will be set by userspace if needed
      VMCS_Write_Unchecked (VMCS_GUEST_GDTR_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_GDTR_LIMIT, 0);

      --  IDTR: will be set by userspace if needed
      VMCS_Write_Unchecked (VMCS_GUEST_IDTR_BASE, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_IDTR_LIMIT, 0);

      --  Guest activity state: active
      VMCS_Write_Unchecked (VMCS_GUEST_ACTIVITY_STATE,
         Unsigned_64 (ACTIVITY_STATE_ACTIVE));

      --  Guest interruptibility: none
      VMCS_Write_Unchecked (VMCS_GUEST_INTERRUPTIBILITY, 0);

      --  Guest pending debug exceptions: none
      VMCS_Write_Unchecked (VMCS_GUEST_PENDING_DBG_EXCEP, 0);

      --  Guest VMCS link pointer: -1 (no nested virtualization)
      VMCS_Write_Unchecked (VMCS_GUEST_VMCS_LINK_PTR, 16#FFFF_FFFF_FFFF_FFFF#);

      --  Guest SYSENTER MSRs
      VMCS_Write_Unchecked (VMCS_GUEST_IA32_SYSENTER_CS, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_SYSENTER_ESP, 0);
      VMCS_Write_Unchecked (VMCS_GUEST_SYSENTER_EIP, 0);

      --  Guest PAT (default value)
      VMCS_Write_Unchecked (VMCS_GUEST_IA32_PAT, 16#0007_0406_0007_0406#);

      --  Guest IA32_DEBUGCTL
      VMCS_Write_Unchecked (VMCS_GUEST_IA32_DEBUGCTL, 0);

      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end VMCS_Setup;

   ---------------------------------------------------------------------------
   --  VMLAUNCH_VMRESUME - Run guest with GPR save/restore
   ---------------------------------------------------------------------------
   procedure VMLAUNCH_VMRESUME
      (VMCS_PA   : Unsigned_64;
       GPRs      : in out Guest_GPRs;
       Is_Launch : Boolean;
       Success   : out Boolean)
   is
      pragma Unreferenced (VMCS_PA);

      --  VMCS is already loaded via VMPTRLD before this call
      --  Status: 0 = success (VMEXIT happened), 1 = VMLAUNCH/VMRESUME failed
      Status : Unsigned_64 := 0;
   begin
      --  The assembly routine:
      --  1. Saves all host GPRs
      --  2. Loads guest GPRs from GPRs parameter
      --  3. Updates host RSP/RIP in VMCS
      --  4. Executes VMLAUNCH or VMRESUME
      --  5. On VMEXIT: saves guest GPRs back to GPRs parameter
      --  6. Restores host GPRs
      if Is_Launch then
         System.Machine_Code.Asm
            ("pushq %%rbx;"              & --  Save host GPRs we'll clobber
             "pushq %%rcx;"              &
             "pushq %%rdx;"              &
             "pushq %%rsi;"              &
             "pushq %%rdi;"              &
             "pushq %%rbp;"              &
             "pushq %%r8;"               &
             "pushq %%r9;"               &
             "pushq %%r10;"              &
             "pushq %%r11;"              &
             "pushq %%r12;"              &
             "pushq %%r13;"              &
             "pushq %%r14;"              &
             "pushq %%r15;"              &
             --  Save GPRs pointer in r15 for later
             --  Note: %0 = Status (output), %1 = GPRs'Address (input)
             "movq %1, %%r15;"           &
             --  Update HOST_RIP in VMCS (return address after VMLAUNCH)
             "movq $0x6c16, %%rax;"      &  --  VMCS_HOST_RIP encoding
             "leaq 1f(%%rip), %%rdx;"    &
             "vmwrite %%rdx, %%rax;"     &
             --  Load guest GPRs from structure (except R15)
             "movq 0x08(%%r15), %%rbx;"  &  --  RBX
             "movq 0x10(%%r15), %%rcx;"  &  --  RCX
             "movq 0x18(%%r15), %%rdx;"  &  --  RDX
             "movq 0x20(%%r15), %%rsi;"  &  --  RSI
             "movq 0x28(%%r15), %%rdi;"  &  --  RDI
             "movq 0x30(%%r15), %%rbp;"  &  --  RBP
             "movq 0x38(%%r15), %%r8;"   &  --  R8
             "movq 0x40(%%r15), %%r9;"   &  --  R9
             "movq 0x48(%%r15), %%r10;"  &  --  R10
             "movq 0x50(%%r15), %%r11;"  &  --  R11
             "movq 0x58(%%r15), %%r12;"  &  --  R12
             "movq 0x60(%%r15), %%r13;"  &  --  R13
             "movq 0x68(%%r15), %%r14;"  &  --  R14
             --  Save GPRs pointer on stack, then load guest R15
             "pushq %%r15;"              &  --  Save GPRs pointer
             "movq 0x70(%%r15), %%r15;"  &  --  R15 = guest R15
             --  NOW update HOST_RSP
             --  (after the final push so VMEXIT restores correctly)
             "movq $0x6c14, %%rax;"      &  --  VMCS_HOST_RSP encoding
             "movq %%rsp, %%rdx;"        &
             "vmwrite %%rdx, %%rax;"     &
             --  Load guest RAX last (we used rax/rdx for vmwrite)
             "movq (%%rsp), %%rax;"      &  --  Reload GPRs pointer temporarily
             "movq 0x00(%%rax), %%rax;"  &  --  RAX = guest RAX
             --  Execute VMLAUNCH
             "vmlaunch;"                 &
             --  If we get here, VMLAUNCH failed
             --  Clean up: discard GPRs pointer from stack before restore
             "addq $8, %%rsp;"           &
             "stc;"                      &  --  Set carry flag = failure
             "jmp 3f;"                   &
             --  VMEXIT returns here (HOST_RIP target)
             "1:;"                       &
             --  Save guest GPRs - first recover GPRs pointer from stack
             "xchgq %%r15, (%%rsp);"     &  --  Swap guest R15 with saved ptr
             "movq %%rax, 0x00(%%r15);"  &  --  Save RAX
             "movq %%rbx, 0x08(%%r15);"  &
             "movq %%rcx, 0x10(%%r15);"  &
             "movq %%rdx, 0x18(%%r15);"  &
             "movq %%rsi, 0x20(%%r15);"  &
             "movq %%rdi, 0x28(%%r15);"  &
             "movq %%rbp, 0x30(%%r15);"  &
             "movq %%r8, 0x38(%%r15);"   &
             "movq %%r9, 0x40(%%r15);"   &
             "movq %%r10, 0x48(%%r15);"  &
             "movq %%r11, 0x50(%%r15);"  &
             "movq %%r12, 0x58(%%r15);"  &
             "movq %%r13, 0x60(%%r15);"  &
             "movq %%r14, 0x68(%%r15);"  &
             "popq %%rax;"               &  --  Get guest R15 from stack
             "movq %%rax, 0x70(%%r15);"  &  --  Save R15
             "clc;"                      &  --  Clear carry flag = success
             "3:;"                       &
             --  Restore host GPRs
             "popq %%r15;"               &
             "popq %%r14;"               &
             "popq %%r13;"               &
             "popq %%r12;"               &
             "popq %%r11;"               &
             "popq %%r10;"               &
             "popq %%r9;"                &
             "popq %%r8;"                &
             "popq %%rbp;"               &
             "popq %%rdi;"               &
             "popq %%rsi;"               &
             "popq %%rdx;"               &
             "popq %%rcx;"               &
             "popq %%rbx;"               &
             --  Convert carry flag to Status: CF=1 means failure (status=1)
             "sbbq %%rax, %%rax;"        &  --  RAX = -1 if CF, 0 otherwise
             "negq %%rax;"               &  --  RAX = 1 if CF, 0 otherwise
             "movq %%rax, %0",
             Outputs  => (Unsigned_64'Asm_Output ("=m", Status)),
             Inputs   => System.Address'Asm_Input ("r", GPRs'Address),
             Clobber  => "memory,cc,rax",
             Volatile => True);
      else
         --  VMRESUME - same logic but uses vmresume instead of vmlaunch
         System.Machine_Code.Asm
            ("pushq %%rbx;"              &
             "pushq %%rcx;"              &
             "pushq %%rdx;"              &
             "pushq %%rsi;"              &
             "pushq %%rdi;"              &
             "pushq %%rbp;"              &
             "pushq %%r8;"               &
             "pushq %%r9;"               &
             "pushq %%r10;"              &
             "pushq %%r11;"              &
             "pushq %%r12;"              &
             "pushq %%r13;"              &
             "pushq %%r14;"              &
             "pushq %%r15;"              &
             --  Save GPRs pointer in r15 for later
             --  Note: %0 = Status (output), %1 = GPRs'Address (input)
             "movq %1, %%r15;"           &
             --  Update HOST_RIP in VMCS
             "movq $0x6c16, %%rax;"      &
             "leaq 1f(%%rip), %%rdx;"    &
             "vmwrite %%rdx, %%rax;"     &
             --  Load guest GPRs (except R15 and RAX)
             "movq 0x08(%%r15), %%rbx;"  &
             "movq 0x10(%%r15), %%rcx;"  &
             "movq 0x18(%%r15), %%rdx;"  &
             "movq 0x20(%%r15), %%rsi;"  &
             "movq 0x28(%%r15), %%rdi;"  &
             "movq 0x30(%%r15), %%rbp;"  &
             "movq 0x38(%%r15), %%r8;"   &
             "movq 0x40(%%r15), %%r9;"   &
             "movq 0x48(%%r15), %%r10;"  &
             "movq 0x50(%%r15), %%r11;"  &
             "movq 0x58(%%r15), %%r12;"  &
             "movq 0x60(%%r15), %%r13;"  &
             "movq 0x68(%%r15), %%r14;"  &
             --  Save GPRs pointer, load guest R15
             "pushq %%r15;"              &
             "movq 0x70(%%r15), %%r15;"  &
             --  NOW update HOST_RSP (after final push)
             "movq $0x6c14, %%rax;"      &
             "movq %%rsp, %%rdx;"        &
             "vmwrite %%rdx, %%rax;"     &
             --  Load guest RAX last
             "movq (%%rsp), %%rax;"      &
             "movq 0x00(%%rax), %%rax;"  &
             "vmresume;"                 &
             --  If we get here, VMRESUME failed
             --  Clean up: discard GPRs pointer from stack before restore
             "addq $8, %%rsp;"           &
             "stc;"                      &  --  Set carry flag = failure
             "jmp 3f;"                   &
             "1:;"                       &
             "xchgq %%r15, (%%rsp);"     &
             "movq %%rax, 0x00(%%r15);"  &
             "movq %%rbx, 0x08(%%r15);"  &
             "movq %%rcx, 0x10(%%r15);"  &
             "movq %%rdx, 0x18(%%r15);"  &
             "movq %%rsi, 0x20(%%r15);"  &
             "movq %%rdi, 0x28(%%r15);"  &
             "movq %%rbp, 0x30(%%r15);"  &
             "movq %%r8, 0x38(%%r15);"   &
             "movq %%r9, 0x40(%%r15);"   &
             "movq %%r10, 0x48(%%r15);"  &
             "movq %%r11, 0x50(%%r15);"  &
             "movq %%r12, 0x58(%%r15);"  &
             "movq %%r13, 0x60(%%r15);"  &
             "movq %%r14, 0x68(%%r15);"  &
             "popq %%rax;"               &
             "movq %%rax, 0x70(%%r15);"  &
             "clc;"                      &  --  Clear carry flag = success
             "3:;"                       &
             "popq %%r15;"               &
             "popq %%r14;"               &
             "popq %%r13;"               &
             "popq %%r12;"               &
             "popq %%r11;"               &
             "popq %%r10;"               &
             "popq %%r9;"                &
             "popq %%r8;"                &
             "popq %%rbp;"               &
             "popq %%rdi;"               &
             "popq %%rsi;"               &
             "popq %%rdx;"               &
             "popq %%rcx;"               &
             "popq %%rbx;"               &
             --  Convert carry flag to Status: CF=1 means failure (status=1)
             "sbbq %%rax, %%rax;"        &  --  RAX = -1 if CF, 0 otherwise
             "negq %%rax;"               &  --  RAX = 1 if CF, 0 otherwise
             "movq %%rax, %0",
             Outputs  => (Unsigned_64'Asm_Output ("=m", Status)),
             Inputs   => System.Address'Asm_Input ("r", GPRs'Address),
             Clobber  => "memory,cc,rax",
             Volatile => True);
      end if;

      Success := (Status = 0);
   end VMLAUNCH_VMRESUME;
end Arch.Virtualization.VMX;
