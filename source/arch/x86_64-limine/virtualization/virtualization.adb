--  virtualization.adb: Virtualization module of the kernel.
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

with Arch.Virtualization;
with Arch.Virtualization.SVM;
with Arch.Virtualization.VMX;
with Arch.IDT;
with Arch.MMU;
with Arch.Snippets;
with Interfaces.C;
with Memory.Physical;
with Memory.MMU;
with Messages;
with System.Storage_Elements; use System.Storage_Elements;
with Synchronization; use Synchronization;

package body Virtualization with SPARK_Mode => Off is
   --  Virtualization backend type
   type Virt_Backend is (Backend_None, Backend_SVM, Backend_VMX);
   Current_Backend : Virt_Backend := Backend_None;

   --  Debug registers DR0-DR3 (not in VMCB/VMCS, saved separately)
   type DR_0_3_Array is array (0 .. 3) of Unsigned_64;

   --  VCPU state.
   type VCPU_State is record
      Active         : Boolean;
      IOPM_Addr      : Integer_Address;  --  I/O permission map
      MSRPM_Addr     : Integer_Address;  --  MSR permission map
      NPT_Addr       : Integer_Address;  --  Nested/Extended page tables
      FPU_Addr       : Integer_Address;  --  Page-aligned FPU/XSAVE buffer
      Event_Pending  : Boolean;          --  True if event to inject
      Pending_Event  : NVMM_Event_Info;  --  Event to inject on next run
      Stop_Requested : Boolean;          --  True if VCPU should stop
      DRs_0_3        : DR_0_3_Array;
      Assigned_ASID  : Unsigned_32;      --  SVM: ASID, VMX: VPID
      TSC_Offset_Val : Unsigned_64;
      V_TPR          : Unsigned_8;       --  Virtual TPR (CR8)
      V_IRQ          : Boolean;          --  Virtual interrupt pending
      V_Intr_Prio    : Unsigned_8;       --  Virtual interrupt priority
      V_Intr_Vector  : Unsigned_8;       --  Virtual interrupt vector
      V_Intr_Masking : Boolean;          --  Virtual interrupt masking enabled
      XCR0_Value     : Unsigned_64;      --  Guest XCR0 value

      --  SVM-specific.
      VMCB_Addr      : Integer_Address;
      VMCB_Phys      : Unsigned_64;
      SVM_GPRs       : Arch.Virtualization.SVM.Guest_GPRs;

      --  VMX-specific.
      VMCS_Addr      : Integer_Address;
      VMCS_Phys      : Unsigned_64;
      VMX_Launched   : Boolean;
      VMX_GPRs       : Arch.Virtualization.VMX.Guest_GPRs;
      VMX_MSRs       : Arch.Virtualization.VMX.Guest_MSRs;
   end record;
   type VCPU_Array is array (VCPU_ID) of VCPU_State;

   --  Machine state.
   type Machine_State is record
      Active : Boolean;
      Owner  : Unsigned_64;  --  Process ID of owner (future use)
      VCPUs  : VCPU_Array;
      Lock   : aliased Binary_Semaphore;
   end record;
   type Machine_Array is array (1 .. Max_Virtual_Machines) of Machine_State;

   --  Global state
   Has_Initialized : Boolean := False;
   Machines        : Machine_Array with Suppress_Initialization;
   Machines_Lock   : aliased Binary_Semaphore := Unlocked_Semaphore;

   --  XSAVE support (detected during initialization)
   Has_XSAVE       : Boolean := False;
   Host_XCR0_Max   : Unsigned_64 := 0;  --  Maximum supported XCR0 value

   --  ASID management (ASIDs 1-255, 0 is invalid)
   Max_ASID        : constant := 255;
   type ASID_Bitmap is array (1 .. Max_ASID) of Boolean;
   ASID_In_Use     : ASID_Bitmap := [others => False];
   ASID_Lock       : aliased Binary_Semaphore := Unlocked_Semaphore;
   Next_ASID_Hint  : Unsigned_32 := 1;  --  Hint for next free ASID

   --  Data for serialization.
   type Seg_Bytes is array (0 .. 15) of Unsigned_8;

   function Is_Supported return Boolean is
   begin
      return Has_Initialized;
   end Is_Supported;

   procedure Initialize is
      SVM_OK : Boolean := False;
      VMX_OK : Boolean := False;
   begin
      --  Try SVM first (AMD)
      Arch.Virtualization.SVM.Initialize (SVM_OK);
      if SVM_OK then
         Current_Backend := Backend_SVM;
         Has_Initialized := True;
      else
         --  Try VMX (Intel)
         Arch.Virtualization.VMX.Initialize (VMX_OK);
         if VMX_OK then
            Current_Backend := Backend_VMX;
            Has_Initialized := True;
         end if;
      end if;

      if Has_Initialized then
         --  Initialize all machines as inactive
         for I in 1 .. Max_Virtual_Machines loop
            Machines (I).Active := False;
            Machines (I).Lock := Unlocked_Semaphore;
         end loop;

         --  Detect XSAVE support (same for both SVM and VMX)
         Has_XSAVE := Arch.Virtualization.SVM.XSAVE_Supported;
         if Has_XSAVE then
            Host_XCR0_Max := Arch.Virtualization.SVM.Get_XCR0_Max;
         end if;
      end if;
   end Initialize;

   function Machine_Create return Machine_ID is
      ID : Machine_ID := Invalid_Machine;
   begin
      if not Has_Initialized then
         return Invalid_Machine;
      end if;

      Seize (Machines_Lock);
      for I in 1 .. Max_Virtual_Machines loop
         if not Machines (I).Active then
            Machines (I).Active := True;
            Machines (I).Owner := 0;
            --  Initialize VCPUs as inactive
            for J in VCPU_ID loop
               Machines (I).VCPUs (J).Active := False;
               --  SVM-specific fields
               Machines (I).VCPUs (J).VMCB_Addr := 0;
               Machines (I).VCPUs (J).VMCB_Phys := 0;
               --  VMX-specific fields
               Machines (I).VCPUs (J).VMCS_Addr := 0;
               Machines (I).VCPUs (J).VMCS_Phys := 0;
               Machines (I).VCPUs (J).VMX_Launched := False;
               --  Common fields
               Machines (I).VCPUs (J).IOPM_Addr := 0;
               Machines (I).VCPUs (J).MSRPM_Addr := 0;
               Machines (I).VCPUs (J).NPT_Addr := 0;
               Machines (I).VCPUs (J).Stop_Requested := False;
               Machines (I).VCPUs (J).DRs_0_3 := [others => 0];
               Machines (I).VCPUs (J).Assigned_ASID := 0;
               Machines (I).VCPUs (J).TSC_Offset_Val := 0;
               Machines (I).VCPUs (J).V_TPR := 0;
               Machines (I).VCPUs (J).V_IRQ := False;
               Machines (I).VCPUs (J).V_Intr_Prio := 0;
               Machines (I).VCPUs (J).V_Intr_Vector := 0;
               Machines (I).VCPUs (J).V_Intr_Masking := False;
               --  XCR0 defaults to 1 (x87 FPU only)
               Machines (I).VCPUs (J).XCR0_Value := 1;
            end loop;
            ID := Machine_ID (I);
            exit;
         end if;
      end loop;
      Release (Machines_Lock);

      return ID;
   end Machine_Create;

   function Machine_Destroy (ID : Machine_ID) return Boolean is
   begin
      if not Has_Initialized or ID = Invalid_Machine then
         return False;
      end if;

      Seize (Machines_Lock);
      if not Machines (Positive (ID)).Active then
         Release (Machines_Lock);
         return False;
      end if;

      --  Destroy all VCPUs first
      for J in VCPU_ID loop
         if Machines (Positive (ID)).VCPUs (J).Active then
            if Machines (Positive (ID)).VCPUs (J).VMCB_Addr /= 0 then
               Memory.Physical.Free
                  (Interfaces.C.size_t
                     (Machines (Positive (ID)).VCPUs (J).VMCB_Addr));
            end if;
            if Machines (Positive (ID)).VCPUs (J).IOPM_Addr /= 0 then
               Memory.Physical.Free
                  (Interfaces.C.size_t
                     (Machines (Positive (ID)).VCPUs (J).IOPM_Addr));
            end if;
            if Machines (Positive (ID)).VCPUs (J).MSRPM_Addr /= 0 then
               Memory.Physical.Free
                  (Interfaces.C.size_t
                     (Machines (Positive (ID)).VCPUs (J).MSRPM_Addr));
            end if;
            if Machines (Positive (ID)).VCPUs (J).NPT_Addr /= 0 then
               Memory.Physical.Free
                  (Interfaces.C.size_t
                     (Machines (Positive (ID)).VCPUs (J).NPT_Addr));
            end if;
            Machines (Positive (ID)).VCPUs (J).Active := False;
            Machines (Positive (ID)).VCPUs (J).VMCB_Addr := 0;
            Machines (Positive (ID)).VCPUs (J).IOPM_Addr := 0;
            Machines (Positive (ID)).VCPUs (J).MSRPM_Addr := 0;
            Machines (Positive (ID)).VCPUs (J).NPT_Addr := 0;
         end if;
      end loop;

      Machines (Positive (ID)).Active := False;
      Release (Machines_Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Machine_Destroy;

   function VCPU_Create (Mach : Machine_ID; CPU : VCPU_ID) return Boolean is
      CS_Addr  : Integer_Address;  --  Control Structure (VMCB or VMCS)
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      if Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;  --  Already exists
      end if;

      --  Allocate control structure (VMCB or VMCS) - both 4KB page-aligned
      Memory.Physical.Alloc (Memory.MMU.Page_Size, CS_Addr);
      if CS_Addr = 0 then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Allocate IOPM (12KB = 3 pages) - I/O permission bitmap
      declare
         IOPM_Addr_Local : Integer_Address;
         IOPM_Size : constant := 16#3000#;  --  12KB
      begin
         Memory.Physical.Alloc (IOPM_Size, IOPM_Addr_Local);
         if IOPM_Addr_Local = 0 then
            Memory.Physical.Free (Interfaces.C.size_t (CS_Addr));
            Release (Machines (Positive (Mach)).Lock);
            return False;
         end if;
         Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr := IOPM_Addr_Local;
         --  Initialize to all 1s (intercept all I/O by default)
         declare
            type Byte_Array is array (0 .. 12287) of Unsigned_8;
            Mem : Byte_Array
               with Import, Address => To_Address (IOPM_Addr_Local);
         begin
            Mem := [others => 16#FF#];
         end;
      end;

      --  Allocate MSRPM (8KB = 2 pages) - MSR permission bitmap
      declare
         MSRPM_Addr_Local : Integer_Address;
         MSRPM_Size : constant := 16#2000#;  --  8KB
         IOPM_Addr_Tmp : Integer_Address;
      begin
         Memory.Physical.Alloc (MSRPM_Size, MSRPM_Addr_Local);
         if MSRPM_Addr_Local = 0 then
            Memory.Physical.Free (Interfaces.C.size_t (CS_Addr));
            IOPM_Addr_Tmp := Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr;
            Memory.Physical.Free (Interfaces.C.size_t (IOPM_Addr_Tmp));
            Release (Machines (Positive (Mach)).Lock);
            return False;
         end if;
         Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr := MSRPM_Addr_Local;
         --  Initialize to all 1s (intercept all MSR access by default)
         declare
            type Byte_Array is array (0 .. 8191) of Unsigned_8;
            Mem : Byte_Array
               with Import, Address => To_Address (MSRPM_Addr_Local);
         begin
            Mem := [others => 16#FF#];
         end;
      end;

      --  Allocate NPT (nested page tables) - 6 pages for 4GB identity map
      --  PML4 (1 page) -> PDPT (1 page) -> 4x PD (4 pages, 512x2MB each = 4GB)
      declare
         NPT_Addr_Local : Integer_Address;
         NPT_Size : constant := 16#7000#;  --  28KB (7 pages, includes PT0)
         IOPM_Tmp : Integer_Address;
         MSRPM_Tmp : Integer_Address;
      begin
         Memory.Physical.Alloc (NPT_Size, NPT_Addr_Local);
         if NPT_Addr_Local = 0 then
            Memory.Physical.Free (Interfaces.C.size_t (CS_Addr));
            IOPM_Tmp := Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr;
            Memory.Physical.Free (Interfaces.C.size_t (IOPM_Tmp));
            MSRPM_Tmp := Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr;
            Memory.Physical.Free (Interfaces.C.size_t (MSRPM_Tmp));
            Release (Machines (Positive (Mach)).Lock);
            return False;
         end if;
         Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr := NPT_Addr_Local;

         --  Set up identity-mapped nested page tables for 4GB
         --  Page 0: PML4
         --  Page 1: PDPT
         --  Pages 2-5: PD[0..3] (each covers 1GB with 512x2MB pages)
         declare
            type U64_Array is array (Natural range <>) of Unsigned_64;
            NPT_PA : constant Unsigned_64 :=
               Unsigned_64 (NPT_Addr_Local - Arch.MMU.Memory_Offset);
            PML4 : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr_Local);
            PDPT : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr_Local + 4096);
            PD0 : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr_Local + 8192);
            PD1 : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr_Local + 12288);
            PD2 : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr_Local + 16384);
            PD3 : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr_Local + 20480);
            PT0 : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr_Local + 24576);
         begin
            --  Zero all pages first
            PML4 := [others => 0];
            PDPT := [others => 0];
            PD0 := [others => 0];
            PD1 := [others => 0];
            PD2 := [others => 0];
            PD3 := [others => 0];

            --  Initialize PT0 with identity-mapped 4KB pages for first 2MB
            --  This allows fine-grained mappings later via GPA_Map
            for I in 0 .. 511 loop
               PT0 (I) := Unsigned_64 (I) * 16#1000# or 16#07#;  --  P+W+U
            end loop;

            --  PML4[0] -> PDPT (Present, Writable, User)
            PML4 (0) := (NPT_PA + 4096) or 16#07#;

            --  PDPT[0..3] -> PD[0..3] (Present, Writable, User)
            PDPT (0) := (NPT_PA + 8192) or 16#07#;
            PDPT (1) := (NPT_PA + 12288) or 16#07#;
            PDPT (2) := (NPT_PA + 16384) or 16#07#;
            PDPT (3) := (NPT_PA + 20480) or 16#07#;

            --  PD0[0] -> PT0 for 4KB pages in first 2MB
            --  Flags: Present(0), Writable(1), User(2) - NO PS bit
            PD0 (0) := (NPT_PA + 24576) or 16#07#;

            --  PD0[1..511]: identity map rest of first 1GB using 2MB pages
            --  PD1/2/3: identity map 1-4GB using 2MB pages
            --  Flags: Present(0), Writable(1), User(2), PS(7)=2MB page
            for I in 1 .. 511 loop
               PD0 (I) := Unsigned_64 (I) * 16#20_0000# or 16#87#;
            end loop;
            for I in 0 .. 511 loop
               PD1 (I) := (Unsigned_64 (I) + 512) * 16#20_0000# or 16#87#;
               PD2 (I) := (Unsigned_64 (I) + 1024) * 16#20_0000# or 16#87#;
               PD3 (I) := (Unsigned_64 (I) + 1536) * 16#20_0000# or 16#87#;
            end loop;
         end;
      end;

      --  Allocate FPU buffer (1 page, 4KB - guarantees 16-byte alignment for
      --  fxsave64/fxrstor64 which require 16-byte aligned memory operand)
      declare
         FPU_Addr_Local : Integer_Address;
         FPU_Size : constant := 16#1000#;  --  4KB (1 page)
         IOPM_Tmp : Integer_Address;
         MSRPM_Tmp : Integer_Address;
         NPT_Tmp : Integer_Address;
      begin
         Memory.Physical.Alloc (FPU_Size, FPU_Addr_Local);
         if FPU_Addr_Local = 0 then
            Memory.Physical.Free (Interfaces.C.size_t (CS_Addr));
            IOPM_Tmp := Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr;
            Memory.Physical.Free (Interfaces.C.size_t (IOPM_Tmp));
            MSRPM_Tmp := Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr;
            Memory.Physical.Free (Interfaces.C.size_t (MSRPM_Tmp));
            NPT_Tmp := Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;
            Memory.Physical.Free (Interfaces.C.size_t (NPT_Tmp));
            Release (Machines (Positive (Mach)).Lock);
            return False;
         end if;
         Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr := FPU_Addr_Local;

         --  Initialize FPU buffer with default x87/SSE state
         declare
            type Byte_Array is array (0 .. 511) of Unsigned_8;
            FPU_Buf : Byte_Array
               with Import, Address => To_Address (FPU_Addr_Local);
         begin
            FPU_Buf := [others => 0];
            --  Set FPU control word to 0x037F (default)
            FPU_Buf (0) := 16#7F#;
            FPU_Buf (1) := 16#03#;
            --  Set MXCSR to 0x1F80 (default)
            FPU_Buf (24) := 16#80#;
            FPU_Buf (25) := 16#1F#;
         end;
      end;

      --  Zero the control structure memory (4KB)
      declare
         type Byte_Array is array (0 .. 4095) of Unsigned_8;
         Mem : Byte_Array with Import, Address => To_Address (CS_Addr);
      begin
         Mem := [others => 0];
      end;

      --  Backend-specific setup
      if Current_Backend = Backend_SVM then
         --  Get VMCB pointer via address conversion
         declare
            VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
               with Import, Address => To_Address (CS_Addr);
         begin
            VMCB_Ptr := VMCB_Obj'Unchecked_Access;
         end;

         --  Set IOPM and MSRPM base physical addresses
         declare
            IOPM_VA : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr;
            MSRPM_VA : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr;
         begin
            VMCB_Ptr.Control.IOPM_Base_PA :=
               Unsigned_64 (IOPM_VA - Arch.MMU.Memory_Offset);
            VMCB_Ptr.Control.MSRPM_Base_PA :=
               Unsigned_64 (MSRPM_VA - Arch.MMU.Memory_Offset);
         end;

         --  Set up VMCB intercepts
         --  Exception intercepts: intercept #GP (13) and #PF (14)
         VMCB_Ptr.Control.Intercept_Exceptions :=
            Shift_Left (Unsigned_32'(1), 13) or  --  #GP
            Shift_Left (Unsigned_32'(1), 14);    --  #PF
         --  Misc_1 bits 0-15: INTR, NMI, SMI, INIT
         --  Misc_1 bits 16-31: CPUID, HLT, IOIO, MSR, SHUTDOWN
         VMCB_Ptr.Control.Intercept_Misc_1 :=
            Arch.Virtualization.SVM.INTERCEPT_INTR or
            Arch.Virtualization.SVM.INTERCEPT_NMI or
            Arch.Virtualization.SVM.INTERCEPT_SMI or
            Arch.Virtualization.SVM.INTERCEPT_INIT or
            Arch.Virtualization.SVM.INTERCEPT_CPUID or
            Arch.Virtualization.SVM.INTERCEPT_HLT or
            Arch.Virtualization.SVM.INTERCEPT_IOIO or
            Arch.Virtualization.SVM.INTERCEPT_MSR or
            Arch.Virtualization.SVM.INTERCEPT_SHUTDOWN;
         --  Misc_2: VMRUN (required!), XSETBV (for XCR0 virtualization)
         VMCB_Ptr.Control.Intercept_Misc_2 :=
            Arch.Virtualization.SVM.INTERCEPT_VMRUN or
            Arch.Virtualization.SVM.INTERCEPT_XSETBV;

         --  Allocate ASID dynamically
         declare
            ASID : constant Unsigned_32 := Allocate_ASID;
         begin
            if ASID = 0 then
               --  No ASID available, fall back to ASID 1 with TLB flush
               VMCB_Ptr.Control.Guest_ASID := 1;
               Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID := 0;
            else
               VMCB_Ptr.Control.Guest_ASID := ASID;
               Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID := ASID;
            end if;
         end;

         --  TSC offset (initially 0, can be set via API)
         VMCB_Ptr.Control.TSC_Offset := 0;
         Machines (Positive (Mach)).VCPUs (CPU).TSC_Offset_Val := 0;

         --  Initialize virtual interrupt state
         Machines (Positive (Mach)).VCPUs (CPU).V_TPR := 0;
         Machines (Positive (Mach)).VCPUs (CPU).V_IRQ := False;
         Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Prio := 0;
         Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Vector := 0;
         Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Masking := True;

         --  Set up V_Intr_Control for virtual interrupt masking
         --  Bit 24: V_INTR_MASKING - isolate guest IF from host
         VMCB_Ptr.Control.V_Intr_Control := Shift_Left (Unsigned_64'(1), 24);

         --  TLB control: flush all on first run
         VMCB_Ptr.Control.TLB_Control :=
            Arch.Virtualization.SVM.TLB_CONTROL_FLUSH_ALL;

         --  VMCB Clean = 0 means reload all state from VMCB on VMRUN
         VMCB_Ptr.Control.VMCB_Clean := 0;

         --  Enable nested paging (NPT) - identity mapped first 1GB
         VMCB_Ptr.Control.NP_Enable := 1;
         declare
            NPT_VA : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;
         begin
            VMCB_Ptr.Control.N_CR3 :=
               Unsigned_64 (NPT_VA - Arch.MMU.Memory_Offset);
         end;

         --  Use 32-bit PROTECTED MODE without paging
         --  This is simpler: PE=1, PG=0, no long mode
         --  NPT handles all address translation

         --  CS: 32-bit code segment
         VMCB_Ptr.State_Save.CS.Selector := 16#08#;  --  Ring 0 code
         VMCB_Ptr.State_Save.CS.Base := 0;
         VMCB_Ptr.State_Save.CS.Limit := 16#FFFF_FFFF#;
         --  VMCB/NVMM attrib format (same as NVMM x64_state_seg):
         --    bits 0-3=Type, bit 4=S, bits 5-6=DPL, bit 7=P,
         --    bit 8=AVL, bit 9=L, bit 10=D/B, bit 11=G
         --  P=1, D/B=1 (32-bit), G=1, S=1, Type=B (code exec/read/accessed)
         --  = 0x0C9B (G at bit 11, D/B at bit 10, P at bit 7, S at bit 4)
         VMCB_Ptr.State_Save.CS.Attrib := 16#0C9B#;

         --  DS/ES/FS/GS/SS: 32-bit data segments
         VMCB_Ptr.State_Save.DS.Selector := 16#10#;  --  Ring 0 data
         VMCB_Ptr.State_Save.DS.Base := 0;
         VMCB_Ptr.State_Save.DS.Limit := 16#FFFF_FFFF#;
         --  P=1, D/B=1, G=1, S=1, Type=3 (data r/w/accessed) = 0x0C93
         VMCB_Ptr.State_Save.DS.Attrib := 16#0C93#;

         VMCB_Ptr.State_Save.ES := VMCB_Ptr.State_Save.DS;
         VMCB_Ptr.State_Save.SS := VMCB_Ptr.State_Save.DS;
         VMCB_Ptr.State_Save.FS := VMCB_Ptr.State_Save.DS;
         VMCB_Ptr.State_Save.GS := VMCB_Ptr.State_Save.DS;

         --  RIP/RSP: Start at 0
         VMCB_Ptr.State_Save.RIP := 0;
         VMCB_Ptr.State_Save.RSP := 0;
         VMCB_Ptr.State_Save.RFLAGS := 16#2#;  --  Reserved bit 1 must be set

         --  CR0: Protected mode, NO paging (PE=1, ET=1, PG=0)
         VMCB_Ptr.State_Save.CR0 := 16#11#;

         --  CR3: 0 (no paging)
         VMCB_Ptr.State_Save.CR3 := 0;

         --  CR4: 0 (no PAE needed without paging)
         VMCB_Ptr.State_Save.CR4 := 0;

         --  DR6/DR7 defaults
         VMCB_Ptr.State_Save.DR6 := 16#FFFF_0FF0#;
         VMCB_Ptr.State_Save.DR7 := 16#0000_0400#;

         --  EFER: Only SVME (no LME/LMA for 32-bit mode)
         VMCB_Ptr.State_Save.EFER := 16#1000#;

         --  G_PAT - Guest PAT MSR (default value)
         VMCB_Ptr.State_Save.G_PAT := 16#0007_0406_0007_0406#;

         --  GDT/IDT - set valid values
         VMCB_Ptr.State_Save.GDTR.Base := 0;
         VMCB_Ptr.State_Save.GDTR.Limit := 16#FFFF#;
         VMCB_Ptr.State_Save.IDTR.Base := 0;
         VMCB_Ptr.State_Save.IDTR.Limit := 16#3FF#;  --  Real mode IVT limit

         --  TR: 32-bit busy TSS
         VMCB_Ptr.State_Save.TR.Selector := 0;
         VMCB_Ptr.State_Save.TR.Base := 0;
         VMCB_Ptr.State_Save.TR.Limit := 16#67#;  --  Minimum TSS limit
         VMCB_Ptr.State_Save.TR.Attrib := 16#008B#;  --  Present, busy TSS

         --  LDTR: null
         VMCB_Ptr.State_Save.LDTR.Selector := 0;
         VMCB_Ptr.State_Save.LDTR.Base := 0;
         VMCB_Ptr.State_Save.LDTR.Limit := 0;
         VMCB_Ptr.State_Save.LDTR.Attrib := 0;

         --  CPL: ring 0
         VMCB_Ptr.State_Save.CPL := 0;

         --  Store addresses
         Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr := CS_Addr;
         Machines (Positive (Mach)).VCPUs (CPU).VMCB_Phys :=
            Unsigned_64 (CS_Addr - Arch.MMU.Memory_Offset);

         --  Initialize GPRs to zero
         Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs :=
            (others => 0);
         --  End of SVM setup

      else
         declare
            VMCS_PA   : constant Unsigned_64 :=
               Unsigned_64 (CS_Addr - Arch.MMU.Memory_Offset);
            NPT_VA    : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;
            EPT_PA    : constant Unsigned_64 :=
               Unsigned_64 (NPT_VA - Arch.MMU.Memory_Offset);
            IOPM_VA   : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr;
            IOPM_PA   : constant Unsigned_64 :=
               Unsigned_64 (IOPM_VA - Arch.MMU.Memory_Offset);
            MSRPM_VA  : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr;
            MSRPM_PA  : constant Unsigned_64 :=
               Unsigned_64 (MSRPM_VA - Arch.MMU.Memory_Offset);
            VPID_Val  : constant Unsigned_16 := Unsigned_16 (Allocate_ASID);
            Clear_OK  : Boolean;
            Setup_OK  : Boolean;
         begin
            --  Store VMCS addresses
            Machines (Positive (Mach)).VCPUs (CPU).VMCS_Addr := CS_Addr;
            Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys := VMCS_PA;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_Launched := False;
            Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID :=
               Unsigned_32 (VPID_Val);

            --  Write VMCS revision ID before VMCLEAR
            Arch.Virtualization.VMX.Write_VMCS_Revision (CS_Addr);

            --  Clear VMCS before first use
            Arch.Virtualization.VMX.VMCLEAR (VMCS_PA, Clear_OK);
            if not Clear_OK then
               --  VMCLEAR failed - clean up and return
               Memory.Physical.Free (Interfaces.C.size_t (CS_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr));
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  Load VMCS pointer
            Arch.Virtualization.VMX.VMPTRLD (VMCS_PA, Setup_OK);
            if not Setup_OK then
               Memory.Physical.Free (Interfaces.C.size_t (CS_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr));
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  Setup VMCS fields
            Arch.Virtualization.VMX.VMCS_Setup
               (VMCS_VA   => CS_Addr,
                EPT_PA    => EPT_PA,
                IOPM_PA   => IOPM_PA,
                MSRPM_PA  => MSRPM_PA,
                VPID_Val  => VPID_Val,
                Success   => Setup_OK);

            if not Setup_OK then
               Memory.Physical.Free (Interfaces.C.size_t (CS_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr));
               Memory.Physical.Free (Interfaces.C.size_t
                  (Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr));
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  Initialize VMX GPRs to zero
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs :=
               (others => 0);
         end;
         --  End of VMX setup
      end if;

      --  Common finalization for both backends
      Machines (Positive (Mach)).VCPUs (CPU).Active := True;

      --  FPU buffer was already initialized during allocation above

      --  No pending event
      Machines (Positive (Mach)).VCPUs (CPU).Event_Pending := False;
      Machines (Positive (Mach)).VCPUs (CPU).Pending_Event :=
         (Event_Type => 0, Vector => 0, Has_Error => False, Error_Code => 0);

      --  No stop requested
      Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;

            Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Create;

   function VCPU_Destroy (Mach : Machine_ID; CPU : VCPU_ID) return Boolean is
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Free VMCB (SVM)
      if Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr /= 0 then
         Memory.Physical.Free
            (Interfaces.C.size_t
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr));
      end if;
      --  Free VMCS (VMX)
      if Machines (Positive (Mach)).VCPUs (CPU).VMCS_Addr /= 0 then
         Memory.Physical.Free
            (Interfaces.C.size_t
               (Machines (Positive (Mach)).VCPUs (CPU).VMCS_Addr));
      end if;
      --  Free IOPM
      if Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr /= 0 then
         Memory.Physical.Free
            (Interfaces.C.size_t
               (Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr));
      end if;
      --  Free MSRPM
      if Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr /= 0 then
         Memory.Physical.Free
            (Interfaces.C.size_t
               (Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr));
      end if;
      --  Free NPT
      if Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr /= 0 then
         Memory.Physical.Free
            (Interfaces.C.size_t
               (Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr));
      end if;
      --  Free FPU buffer
      if Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr /= 0 then
         Memory.Physical.Free
            (Interfaces.C.size_t
               (Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr));
      end if;

      --  Free ASID
      if Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID /= 0 then
         Free_ASID (Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID);
      end if;

      Machines (Positive (Mach)).VCPUs (CPU).Active := False;
      Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).VMCB_Phys := 0;
      Machines (Positive (Mach)).VCPUs (CPU).VMCS_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys := 0;
      Machines (Positive (Mach)).VCPUs (CPU).VMX_Launched := False;
      Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Destroy;

   function VCPU_Run
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Code : out Unsigned_64) return Boolean
   is
      VMCB_Phys : Unsigned_64;
      VMCB_Ptr  : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      Exit_Code := 0;

      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      VMCB_Phys := Machines (Positive (Mach)).VCPUs (CPU).VMCB_Phys;

      --  Get VMCB pointer via address conversion
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Verify EFER.SVME is set (bit 12)
      declare
         EFER_MSR : constant := 16#C0000080#;
         EFER_Val : constant Unsigned_64 := Arch.Snippets.Read_MSR (EFER_MSR);
      begin
         if (EFER_Val and Shift_Left (Unsigned_64'(1), 12)) = 0 then
            Messages.Put_Line ("VCPU_Run: ERROR - EFER.SVME not set!");
            Release (Machines (Positive (Mach)).Lock);
            return False;
         end if;
      end;

      --  Run the VCPU
      Arch.Virtualization.SVM.VMRUN
         (VMCB_PA => VMCB_Phys,
          GPRs    => Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs);

      --  Get exit code from VMCB
      Exit_Code := VMCB_Ptr.Control.Exit_Code;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Run;

   --  NVMM x64 state structure (matches userspace nvmm_x64_state)
   --  Types are defined in the spec (virtualization.ads):
   --    NVMM_Segment, NVMM_Seg_Array, NVMM_CR_Array, NVMM_MSR_Array, etc.
   --  Segment indices (from nvmm_x86.h)
   NVMM_X64_SEG_ES   : constant := 0;
   NVMM_X64_SEG_CS   : constant := 1;
   NVMM_X64_SEG_SS   : constant := 2;
   NVMM_X64_SEG_DS   : constant := 3;
   NVMM_X64_SEG_FS   : constant := 4;
   NVMM_X64_SEG_GS   : constant := 5;
   NVMM_X64_SEG_GDT  : constant := 6;
   NVMM_X64_SEG_IDT  : constant := 7;
   NVMM_X64_SEG_LDT  : constant := 8;
   NVMM_X64_SEG_TR   : constant := 9;

   --  CR indices (match nvmm_x86.h - NOT sparse like x86 register numbering)
   NVMM_X64_CR_CR0  : constant := 0;
   NVMM_X64_CR_CR2  : constant := 1;
   NVMM_X64_CR_CR3  : constant := 2;
   NVMM_X64_CR_CR4  : constant := 3;

   --  DR indices (match nvmm_x86.h - NOT sparse like x86 register numbering)
   NVMM_X64_DR_DR0  : constant := 0;
   NVMM_X64_DR_DR1  : constant := 1;
   NVMM_X64_DR_DR2  : constant := 2;
   NVMM_X64_DR_DR3  : constant := 3;
   NVMM_X64_DR_DR6  : constant := 4;
   NVMM_X64_DR_DR7  : constant := 5;

   --  MSR indices
   NVMM_X64_MSR_EFER         : constant := 0;
   NVMM_X64_MSR_STAR         : constant := 1;
   NVMM_X64_MSR_LSTAR        : constant := 2;
   NVMM_X64_MSR_CSTAR        : constant := 3;
   NVMM_X64_MSR_SFMASK       : constant := 4;
   NVMM_X64_MSR_KERNELGSBASE : constant := 5;
   NVMM_X64_MSR_SYSENTER_CS  : constant := 6;
   NVMM_X64_MSR_SYSENTER_ESP : constant := 7;
   NVMM_X64_MSR_SYSENTER_EIP : constant := 8;
   NVMM_X64_MSR_PAT          : constant := 9;

   type NVMM_X64_State is record
      Segs : NVMM_Seg_Array;
      GPRs : NVMM_GPR_Array;
      CRs  : NVMM_CR_Array;
      DRs  : NVMM_DR_Array;
      MSRs : NVMM_MSR_Array;
      Intr : Unsigned_64;
      FPU  : NVMM_FPU_State;
   end record;

   function VCPU_Get_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : out NVMM_GPR_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK : Boolean;
            G : Arch.Virtualization.VMX.Guest_GPRs renames
               Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  RAX from VMX_GPRs (not in VMCS)
            GPRs (NVMM_X64_GPR_RAX) := G.RAX;
            --  RSP, RIP, RFLAGS from VMCS
            GPRs (NVMM_X64_GPR_RSP) := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RSP);
            GPRs (NVMM_X64_GPR_RIP) := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RIP);
            GPRs (NVMM_X64_GPR_RFLAGS) := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS);
            --  Other GPRs from VMX_GPRs
            GPRs (NVMM_X64_GPR_RBX) := G.RBX;
            GPRs (NVMM_X64_GPR_RCX) := G.RCX;
            GPRs (NVMM_X64_GPR_RDX) := G.RDX;
            GPRs (NVMM_X64_GPR_RBP) := G.RBP;
            GPRs (NVMM_X64_GPR_RSI) := G.RSI;
            GPRs (NVMM_X64_GPR_RDI) := G.RDI;
            GPRs (NVMM_X64_GPR_R8)  := G.R8;
            GPRs (NVMM_X64_GPR_R9)  := G.R9;
            GPRs (NVMM_X64_GPR_R10) := G.R10;
            GPRs (NVMM_X64_GPR_R11) := G.R11;
            GPRs (NVMM_X64_GPR_R12) := G.R12;
            GPRs (NVMM_X64_GPR_R13) := G.R13;
            GPRs (NVMM_X64_GPR_R14) := G.R14;
            GPRs (NVMM_X64_GPR_R15) := G.R15;
         end;

         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  GPRs in VMCB: RAX, RSP, RIP, RFLAGS
      GPRs (NVMM_X64_GPR_RAX) := VMCB_Ptr.State_Save.RAX;
      GPRs (NVMM_X64_GPR_RSP) := VMCB_Ptr.State_Save.RSP;
      GPRs (NVMM_X64_GPR_RIP) := VMCB_Ptr.State_Save.RIP;
      GPRs (NVMM_X64_GPR_RFLAGS) := VMCB_Ptr.State_Save.RFLAGS;

      --  GPRs stored separately (not in VMCB, saved/restored around VMRUN)
      declare
         G : Arch.Virtualization.SVM.Guest_GPRs renames
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs;
      begin
         GPRs (NVMM_X64_GPR_RBX) := G.RBX;
         GPRs (NVMM_X64_GPR_RCX) := G.RCX;
         GPRs (NVMM_X64_GPR_RDX) := G.RDX;
         GPRs (NVMM_X64_GPR_RBP) := G.RBP;
         GPRs (NVMM_X64_GPR_RSI) := G.RSI;
         GPRs (NVMM_X64_GPR_RDI) := G.RDI;
         GPRs (NVMM_X64_GPR_R8)  := G.R8;
         GPRs (NVMM_X64_GPR_R9)  := G.R9;
         GPRs (NVMM_X64_GPR_R10) := G.R10;
         GPRs (NVMM_X64_GPR_R11) := G.R11;
         GPRs (NVMM_X64_GPR_R12) := G.R12;
         GPRs (NVMM_X64_GPR_R13) := G.R13;
         GPRs (NVMM_X64_GPR_R14) := G.R14;
         GPRs (NVMM_X64_GPR_R15) := G.R15;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Get_GPRs;

   function VCPU_Set_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : NVMM_GPR_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK  : Boolean;
            Dummy_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;
            --  RAX goes to VMX_GPRs (not in VMCS)
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX :=
               GPRs (NVMM_X64_GPR_RAX);
            --  RSP, RIP, RFLAGS go to VMCS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_RSP,
                GPRs (NVMM_X64_GPR_RSP), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_RIP,
                GPRs (NVMM_X64_GPR_RIP), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS,
                GPRs (NVMM_X64_GPR_RFLAGS), Dummy_OK);
            --  Other GPRs go to VMX_GPRs
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBX :=
               GPRs (NVMM_X64_GPR_RBX);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX :=
               GPRs (NVMM_X64_GPR_RCX);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX :=
               GPRs (NVMM_X64_GPR_RDX);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBP :=
               GPRs (NVMM_X64_GPR_RBP);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RSI :=
               GPRs (NVMM_X64_GPR_RSI);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDI :=
               GPRs (NVMM_X64_GPR_RDI);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R8 :=
               GPRs (NVMM_X64_GPR_R8);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R9 :=
               GPRs (NVMM_X64_GPR_R9);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R10 :=
               GPRs (NVMM_X64_GPR_R10);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R11 :=
               GPRs (NVMM_X64_GPR_R11);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R12 :=
               GPRs (NVMM_X64_GPR_R12);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R13 :=
               GPRs (NVMM_X64_GPR_R13);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R14 :=
               GPRs (NVMM_X64_GPR_R14);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R15 :=
               GPRs (NVMM_X64_GPR_R15);
            Release (Machines (Positive (Mach)).Lock);
            return True;
         end;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  GPRs in VMCB: RAX, RSP, RIP, RFLAGS
      VMCB_Ptr.State_Save.RAX := GPRs (NVMM_X64_GPR_RAX);
      VMCB_Ptr.State_Save.RSP := GPRs (NVMM_X64_GPR_RSP);
      VMCB_Ptr.State_Save.RIP := GPRs (NVMM_X64_GPR_RIP);
      VMCB_Ptr.State_Save.RFLAGS := GPRs (NVMM_X64_GPR_RFLAGS);

      --  GPRs stored separately (not in VMCB, saved/restored around VMRUN)
      declare
         G : Arch.Virtualization.SVM.Guest_GPRs renames
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs;
      begin
         G.RBX := GPRs (NVMM_X64_GPR_RBX);
         G.RCX := GPRs (NVMM_X64_GPR_RCX);
         G.RDX := GPRs (NVMM_X64_GPR_RDX);
         G.RBP := GPRs (NVMM_X64_GPR_RBP);
         G.RSI := GPRs (NVMM_X64_GPR_RSI);
         G.RDI := GPRs (NVMM_X64_GPR_RDI);
         G.R8  := GPRs (NVMM_X64_GPR_R8);
         G.R9  := GPRs (NVMM_X64_GPR_R9);
         G.R10 := GPRs (NVMM_X64_GPR_R10);
         G.R11 := GPRs (NVMM_X64_GPR_R11);
         G.R12 := GPRs (NVMM_X64_GPR_R12);
         G.R13 := GPRs (NVMM_X64_GPR_R13);
         G.R14 := GPRs (NVMM_X64_GPR_R14);
         G.R15 := GPRs (NVMM_X64_GPR_R15);
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Set_GPRs;

   function VCPU_Get_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : out NVMM_FPU_State) return Boolean
   is
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Copy from page-aligned FPU buffer to output
      declare
         FPU_Addr_Local : constant Integer_Address :=
            Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
         type Byte_Array is array (0 .. 511) of Unsigned_8;
         FPU_Buf : Byte_Array
            with Import, Address => To_Address (FPU_Addr_Local);
      begin
         for I in FPU'Range loop
            FPU (I) := FPU_Buf (I);
         end loop;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Get_FPU;

   function VCPU_Set_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : NVMM_FPU_State) return Boolean
   is
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Copy from input to page-aligned FPU buffer
      declare
         FPU_Addr_Local : constant Integer_Address :=
            Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
         type Byte_Array is array (0 .. 511) of Unsigned_8;
         FPU_Buf : Byte_Array
            with Import, Address => To_Address (FPU_Addr_Local);
      begin
         for I in FPU'Range loop
            FPU_Buf (I) := FPU (I);
         end loop;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Set_FPU;

   function VCPU_Get_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : out NVMM_Seg_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  ES
            Segs (NVMM_X64_SEG_ES) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_BASE));
            --  CS
            Segs (NVMM_X64_SEG_CS) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_BASE));
            --  SS
            Segs (NVMM_X64_SEG_SS) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_BASE));
            --  DS
            Segs (NVMM_X64_SEG_DS) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_BASE));
            --  FS
            Segs (NVMM_X64_SEG_FS) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_BASE));
            --  GS
            Segs (NVMM_X64_SEG_GS) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_BASE));
            --  GDTR (no selector or attrib)
            Segs (NVMM_X64_SEG_GDT) :=
               (0, 0,
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_BASE));
            --  IDTR (no selector or attrib)
            Segs (NVMM_X64_SEG_IDT) :=
               (0, 0,
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_BASE));
            --  LDTR
            Segs (NVMM_X64_SEG_LDT) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_BASE));
            --  TR
            Segs (NVMM_X64_SEG_TR) :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_BASE));
         end;

         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      Segs (NVMM_X64_SEG_ES) :=
         (VMCB_Ptr.State_Save.ES.Selector, VMCB_Ptr.State_Save.ES.Attrib,
          VMCB_Ptr.State_Save.ES.Limit, VMCB_Ptr.State_Save.ES.Base);
      Segs (NVMM_X64_SEG_CS) :=
         (VMCB_Ptr.State_Save.CS.Selector, VMCB_Ptr.State_Save.CS.Attrib,
          VMCB_Ptr.State_Save.CS.Limit, VMCB_Ptr.State_Save.CS.Base);
      Segs (NVMM_X64_SEG_SS) :=
         (VMCB_Ptr.State_Save.SS.Selector, VMCB_Ptr.State_Save.SS.Attrib,
          VMCB_Ptr.State_Save.SS.Limit, VMCB_Ptr.State_Save.SS.Base);
      Segs (NVMM_X64_SEG_DS) :=
         (VMCB_Ptr.State_Save.DS.Selector, VMCB_Ptr.State_Save.DS.Attrib,
          VMCB_Ptr.State_Save.DS.Limit, VMCB_Ptr.State_Save.DS.Base);
      Segs (NVMM_X64_SEG_FS) :=
         (VMCB_Ptr.State_Save.FS.Selector, VMCB_Ptr.State_Save.FS.Attrib,
          VMCB_Ptr.State_Save.FS.Limit, VMCB_Ptr.State_Save.FS.Base);
      Segs (NVMM_X64_SEG_GS) :=
         (VMCB_Ptr.State_Save.GS.Selector, VMCB_Ptr.State_Save.GS.Attrib,
          VMCB_Ptr.State_Save.GS.Limit, VMCB_Ptr.State_Save.GS.Base);
      Segs (NVMM_X64_SEG_GDT) :=
         (0, 0, VMCB_Ptr.State_Save.GDTR.Limit, VMCB_Ptr.State_Save.GDTR.Base);
      Segs (NVMM_X64_SEG_IDT) :=
         (0, 0, VMCB_Ptr.State_Save.IDTR.Limit, VMCB_Ptr.State_Save.IDTR.Base);
      Segs (NVMM_X64_SEG_LDT) :=
         (VMCB_Ptr.State_Save.LDTR.Selector, VMCB_Ptr.State_Save.LDTR.Attrib,
          VMCB_Ptr.State_Save.LDTR.Limit, VMCB_Ptr.State_Save.LDTR.Base);
      Segs (NVMM_X64_SEG_TR) :=
         (VMCB_Ptr.State_Save.TR.Selector, VMCB_Ptr.State_Save.TR.Attrib,
          VMCB_Ptr.State_Save.TR.Limit, VMCB_Ptr.State_Save.TR.Base);

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Get_Segs;

   function VCPU_Set_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : NVMM_Seg_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK  : Boolean;
            Dummy_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;
            --  ES
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_ES_SELECTOR,
                Unsigned_64 (Segs (NVMM_X64_SEG_ES).Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_ES).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_ES_BASE,
                Segs (NVMM_X64_SEG_ES).Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR,
                To_VMX_AR (Segs (NVMM_X64_SEG_ES).Attrib,
                           Segs (NVMM_X64_SEG_ES).Limit), Dummy_OK);
            --  CS
            --  VMX requires CS selector to be non-null even in unrestricted
            --  guest mode. Use selector 8 if selector is 0.
            if Segs (NVMM_X64_SEG_CS).Selector = 0 then
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR,
                   8, Dummy_OK);
            else
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR,
                   Unsigned_64 (Segs (NVMM_X64_SEG_CS).Selector), Dummy_OK);
            end if;
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_CS).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CS_BASE,
                Segs (NVMM_X64_SEG_CS).Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR,
                To_VMX_AR (Segs (NVMM_X64_SEG_CS).Attrib,
                           Segs (NVMM_X64_SEG_CS).Limit), Dummy_OK);
            --  SS
            --  VMX requires SS selector to be non-null if SS is usable.
            --  Use selector 10h if selector is 0 and SS is usable.
            if Segs (NVMM_X64_SEG_SS).Selector = 0 and then
               (Segs (NVMM_X64_SEG_SS).Attrib and 16#80#) /= 0
            then
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR,
                   16#10#, Dummy_OK);
            else
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR,
                   Unsigned_64 (Segs (NVMM_X64_SEG_SS).Selector), Dummy_OK);
            end if;
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_SS).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SS_BASE,
                Segs (NVMM_X64_SEG_SS).Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR,
                To_VMX_AR (Segs (NVMM_X64_SEG_SS).Attrib,
                           Segs (NVMM_X64_SEG_SS).Limit), Dummy_OK);
            --  DS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_SELECTOR,
                Unsigned_64 (Segs (NVMM_X64_SEG_DS).Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_DS).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_BASE,
                Segs (NVMM_X64_SEG_DS).Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR,
                To_VMX_AR (Segs (NVMM_X64_SEG_DS).Attrib,
                           Segs (NVMM_X64_SEG_DS).Limit), Dummy_OK);
            --  FS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_SELECTOR,
                Unsigned_64 (Segs (NVMM_X64_SEG_FS).Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_FS).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_BASE,
                Segs (NVMM_X64_SEG_FS).Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR,
                To_VMX_AR (Segs (NVMM_X64_SEG_FS).Attrib,
                           Segs (NVMM_X64_SEG_FS).Limit), Dummy_OK);
            --  GS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_SELECTOR,
                Unsigned_64 (Segs (NVMM_X64_SEG_GS).Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_GS).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_BASE,
                Segs (NVMM_X64_SEG_GS).Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR,
                To_VMX_AR (Segs (NVMM_X64_SEG_GS).Attrib,
                           Segs (NVMM_X64_SEG_GS).Limit), Dummy_OK);
            --  GDTR
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_GDT).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_BASE,
                Segs (NVMM_X64_SEG_GDT).Base, Dummy_OK);
            --  IDTR
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_IDT).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_BASE,
                Segs (NVMM_X64_SEG_IDT).Base, Dummy_OK);
            --  LDTR: if selector is 0, set VMX unusable bit
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_SELECTOR,
                Unsigned_64 (Segs (NVMM_X64_SEG_LDT).Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_LIMIT,
                Unsigned_64 (Segs (NVMM_X64_SEG_LDT).Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_BASE,
                Segs (NVMM_X64_SEG_LDT).Base, Dummy_OK);
            if Segs (NVMM_X64_SEG_LDT).Selector = 0 then
               --  Null LDTR selector: set VMX unusable bit
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR,
                   16#1_0000#, Dummy_OK);  --  Just unusable bit
            else
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR,
                   To_VMX_AR (Segs (NVMM_X64_SEG_LDT).Attrib,
                              Segs (NVMM_X64_SEG_LDT).Limit), Dummy_OK);
            end if;
            --  TR
            --  TR: VMX requires TR to always be usable (unlike SVM)
            --  If selector is 0, keep the VMCS_Setup defaults
            if Segs (NVMM_X64_SEG_TR).Selector /= 0 then
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_SELECTOR,
                   Unsigned_64 (Segs (NVMM_X64_SEG_TR).Selector), Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_LIMIT,
                   Unsigned_64 (Segs (NVMM_X64_SEG_TR).Limit), Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_BASE,
                   Segs (NVMM_X64_SEG_TR).Base, Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_AR,
                   To_VMX_AR (Segs (NVMM_X64_SEG_TR).Attrib,
                              Segs (NVMM_X64_SEG_TR).Limit), Dummy_OK);
            end if;
            Release (Machines (Positive (Mach)).Lock);
            return True;
         end;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Work around Ada representation clause issue: read attrib as raw bytes
      --  The Attrib field may have incorrect bit interpretation
      VMCB_Ptr.State_Save.ES.Selector := Segs (NVMM_X64_SEG_ES).Selector;
      VMCB_Ptr.State_Save.ES.Attrib := Get_Attrib_Raw (Segs (NVMM_X64_SEG_ES));
      VMCB_Ptr.State_Save.ES.Limit := Segs (NVMM_X64_SEG_ES).Limit;
      VMCB_Ptr.State_Save.ES.Base := Segs (NVMM_X64_SEG_ES).Base;
      VMCB_Ptr.State_Save.CS.Selector := Segs (NVMM_X64_SEG_CS).Selector;
      VMCB_Ptr.State_Save.CS.Attrib := Get_Attrib_Raw (Segs (NVMM_X64_SEG_CS));
      VMCB_Ptr.State_Save.CS.Limit := Segs (NVMM_X64_SEG_CS).Limit;
      VMCB_Ptr.State_Save.CS.Base := Segs (NVMM_X64_SEG_CS).Base;
      VMCB_Ptr.State_Save.SS.Selector := Segs (NVMM_X64_SEG_SS).Selector;
      VMCB_Ptr.State_Save.SS.Attrib := Get_Attrib_Raw (Segs (NVMM_X64_SEG_SS));
      VMCB_Ptr.State_Save.SS.Limit := Segs (NVMM_X64_SEG_SS).Limit;
      VMCB_Ptr.State_Save.SS.Base := Segs (NVMM_X64_SEG_SS).Base;
      VMCB_Ptr.State_Save.DS.Selector := Segs (NVMM_X64_SEG_DS).Selector;
      VMCB_Ptr.State_Save.DS.Attrib := Get_Attrib_Raw (Segs (NVMM_X64_SEG_DS));
      VMCB_Ptr.State_Save.DS.Limit := Segs (NVMM_X64_SEG_DS).Limit;
      VMCB_Ptr.State_Save.DS.Base := Segs (NVMM_X64_SEG_DS).Base;
      VMCB_Ptr.State_Save.FS.Selector := Segs (NVMM_X64_SEG_FS).Selector;
      VMCB_Ptr.State_Save.FS.Attrib := Get_Attrib_Raw (Segs (NVMM_X64_SEG_FS));
      VMCB_Ptr.State_Save.FS.Limit := Segs (NVMM_X64_SEG_FS).Limit;
      VMCB_Ptr.State_Save.FS.Base := Segs (NVMM_X64_SEG_FS).Base;
      VMCB_Ptr.State_Save.GS.Selector := Segs (NVMM_X64_SEG_GS).Selector;
      VMCB_Ptr.State_Save.GS.Attrib := Get_Attrib_Raw (Segs (NVMM_X64_SEG_GS));
      VMCB_Ptr.State_Save.GS.Limit := Segs (NVMM_X64_SEG_GS).Limit;
      VMCB_Ptr.State_Save.GS.Base := Segs (NVMM_X64_SEG_GS).Base;
      VMCB_Ptr.State_Save.GDTR.Limit := Segs (NVMM_X64_SEG_GDT).Limit;
      VMCB_Ptr.State_Save.GDTR.Base := Segs (NVMM_X64_SEG_GDT).Base;
      VMCB_Ptr.State_Save.IDTR.Limit := Segs (NVMM_X64_SEG_IDT).Limit;
      VMCB_Ptr.State_Save.IDTR.Base := Segs (NVMM_X64_SEG_IDT).Base;
      VMCB_Ptr.State_Save.LDTR.Selector := Segs (NVMM_X64_SEG_LDT).Selector;
      VMCB_Ptr.State_Save.LDTR.Attrib :=
         Get_Attrib_Raw (Segs (NVMM_X64_SEG_LDT));
      VMCB_Ptr.State_Save.LDTR.Limit := Segs (NVMM_X64_SEG_LDT).Limit;
      VMCB_Ptr.State_Save.LDTR.Base := Segs (NVMM_X64_SEG_LDT).Base;
      VMCB_Ptr.State_Save.TR.Selector := Segs (NVMM_X64_SEG_TR).Selector;
      VMCB_Ptr.State_Save.TR.Attrib := Get_Attrib_Raw (Segs (NVMM_X64_SEG_TR));
      VMCB_Ptr.State_Save.TR.Limit := Segs (NVMM_X64_SEG_TR).Limit;
      VMCB_Ptr.State_Save.TR.Base := Segs (NVMM_X64_SEG_TR).Base;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Set_Segs;

   function VCPU_Get_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : out NVMM_CR_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            CRs := [others => 0];
            CRs (NVMM_X64_CR_CR0) := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR0);
            --  CR2 not in VMCS, would need to track separately
            CRs (NVMM_X64_CR_CR2) := 0;
            CRs (NVMM_X64_CR_CR3) := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR3);
            CRs (NVMM_X64_CR_CR4) := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR4);
         end;

         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      CRs := [others => 0];
      CRs (NVMM_X64_CR_CR0) := VMCB_Ptr.State_Save.CR0;
      CRs (NVMM_X64_CR_CR2) := VMCB_Ptr.State_Save.CR2;
      CRs (NVMM_X64_CR_CR3) := VMCB_Ptr.State_Save.CR3;
      CRs (NVMM_X64_CR_CR4) := VMCB_Ptr.State_Save.CR4;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Get_CRs;

   function VCPU_Set_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : NVMM_CR_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK  : Boolean;
            Dummy_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  Apply CR0 fixed bits (e.g., NE must be 1)
            --  Use unrestricted paging mode = True since we allow PE=0, PG=0
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CR0,
                Arch.Virtualization.VMX.Apply_CR0_Fixed
                   (CRs (NVMM_X64_CR_CR0), True), Dummy_OK);
            --  CR2 not in VMCS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CR3,
                CRs (NVMM_X64_CR_CR3), Dummy_OK);
            --  Apply CR4 fixed bits (preserves LA57 for 5-level paging)
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CR4,
                Arch.Virtualization.VMX.Apply_CR4_Fixed
                   (CRs (NVMM_X64_CR_CR4)), Dummy_OK);
            Release (Machines (Positive (Mach)).Lock);
            return True;
         end;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      VMCB_Ptr.State_Save.CR0 := CRs (NVMM_X64_CR_CR0);
      VMCB_Ptr.State_Save.CR2 := CRs (NVMM_X64_CR_CR2);
      VMCB_Ptr.State_Save.CR3 := CRs (NVMM_X64_CR_CR3);
      VMCB_Ptr.State_Save.CR4 := CRs (NVMM_X64_CR_CR4);

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Set_CRs;

   function VCPU_Get_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : out NVMM_MSR_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend - read from VMCS and VMX_MSRs
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            MSRs := [others => 0];

            --  Read EFER and SYSENTER from VMCS
            MSRs (NVMM_X64_MSR_EFER) := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER);
            MSRs (NVMM_X64_MSR_SYSENTER_CS) :=
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_IA32_SYSENTER_CS);
            MSRs (NVMM_X64_MSR_SYSENTER_ESP) :=
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_ESP);
            MSRs (NVMM_X64_MSR_SYSENTER_EIP) :=
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_EIP);

            --  Read syscall MSRs from VMX_MSRs
            MSRs (NVMM_X64_MSR_STAR) :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.STAR;
            MSRs (NVMM_X64_MSR_LSTAR) :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.LSTAR;
            MSRs (NVMM_X64_MSR_CSTAR) :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.CSTAR;
            MSRs (NVMM_X64_MSR_SFMASK) :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.SFMASK;
            MSRs (NVMM_X64_MSR_KERNELGSBASE) :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.Kernel_GS_Base;
         end;

         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      MSRs := [others => 0];
      MSRs (NVMM_X64_MSR_EFER) := VMCB_Ptr.State_Save.EFER;
      MSRs (NVMM_X64_MSR_STAR) := VMCB_Ptr.State_Save.STAR;
      MSRs (NVMM_X64_MSR_LSTAR) := VMCB_Ptr.State_Save.LSTAR;
      MSRs (NVMM_X64_MSR_CSTAR) := VMCB_Ptr.State_Save.CSTAR;
      MSRs (NVMM_X64_MSR_SFMASK) := VMCB_Ptr.State_Save.SFMASK;
      MSRs (NVMM_X64_MSR_KERNELGSBASE) := VMCB_Ptr.State_Save.Kernel_GS_Base;
      MSRs (NVMM_X64_MSR_SYSENTER_CS) := VMCB_Ptr.State_Save.SYSENTER_CS;
      MSRs (NVMM_X64_MSR_SYSENTER_ESP) := VMCB_Ptr.State_Save.SYSENTER_ESP;
      MSRs (NVMM_X64_MSR_SYSENTER_EIP) := VMCB_Ptr.State_Save.SYSENTER_EIP;
      --  PAT and TSC are read-only, not stored in VMCB

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Get_MSRs;

   function VCPU_Set_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : NVMM_MSR_Array) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend - use VMCS for some MSRs, VMX_MSRs for others
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK  : Boolean;
            Dummy_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  EFER goes to VMCS (no SVME bit needed for VMX)
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER,
                MSRs (NVMM_X64_MSR_EFER), Dummy_OK);

            --  Update entry controls based on EFER.LMA (bit 10)
            --  IA-32e mode guest bit must match EFER.LMA
            declare
               Entry_Ctls : Unsigned_64 := Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS);
               LMA_Set : constant Boolean :=
                  (MSRs (NVMM_X64_MSR_EFER) and 16#400#) /= 0;
            begin
               if LMA_Set then
                  Entry_Ctls := Entry_Ctls or
                     Arch.Virtualization.VMX.ENTRY_IA32E_MODE_GUEST;
               else
                  Entry_Ctls := Entry_Ctls and
                     not Arch.Virtualization.VMX.ENTRY_IA32E_MODE_GUEST;
               end if;
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS,
                   Entry_Ctls, Dummy_OK);
            end;

            --  SYSENTER MSRs go to VMCS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_IA32_SYSENTER_CS,
                MSRs (NVMM_X64_MSR_SYSENTER_CS), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_ESP,
                MSRs (NVMM_X64_MSR_SYSENTER_ESP), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_EIP,
                MSRs (NVMM_X64_MSR_SYSENTER_EIP), Dummy_OK);

            --  Syscall MSRs go to VMX_MSRs (loaded manually before VM entry)
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.STAR :=
               MSRs (NVMM_X64_MSR_STAR);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.LSTAR :=
               MSRs (NVMM_X64_MSR_LSTAR);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.CSTAR :=
               MSRs (NVMM_X64_MSR_CSTAR);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.SFMASK :=
               MSRs (NVMM_X64_MSR_SFMASK);
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.Kernel_GS_Base :=
               MSRs (NVMM_X64_MSR_KERNELGSBASE);
         end;

         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Always ensure SVME (bit 12) is set for VMRUN to work
      VMCB_Ptr.State_Save.EFER := MSRs (NVMM_X64_MSR_EFER) or 16#1000#;
      VMCB_Ptr.State_Save.STAR := MSRs (NVMM_X64_MSR_STAR);
      VMCB_Ptr.State_Save.LSTAR := MSRs (NVMM_X64_MSR_LSTAR);
      VMCB_Ptr.State_Save.CSTAR := MSRs (NVMM_X64_MSR_CSTAR);
      VMCB_Ptr.State_Save.SFMASK := MSRs (NVMM_X64_MSR_SFMASK);
      VMCB_Ptr.State_Save.Kernel_GS_Base := MSRs (NVMM_X64_MSR_KERNELGSBASE);
      VMCB_Ptr.State_Save.SYSENTER_CS := MSRs (NVMM_X64_MSR_SYSENTER_CS);
      VMCB_Ptr.State_Save.SYSENTER_ESP := MSRs (NVMM_X64_MSR_SYSENTER_ESP);
      VMCB_Ptr.State_Save.SYSENTER_EIP := MSRs (NVMM_X64_MSR_SYSENTER_EIP);
      --  PAT and TSC are read-only

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Set_MSRs;

   function VCPU_Inject_Event
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Event : NVMM_Event_Info) return Boolean
   is
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      Machines (Positive (Mach)).VCPUs (CPU).Pending_Event := Event;
      Machines (Positive (Mach)).VCPUs (CPU).Event_Pending := True;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Inject_Event;

   function VCPU_Set_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
      State    : NVMM_X64_State with Import, Address => Addr;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend - use VMCS instead of VMCB
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK   : Boolean;
            Dummy_OK  : Boolean;
         begin
            --  Load VMCS pointer before writing
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  Copy segments if requested
            if (Flags and VCPU_STATE_SEGS) /= 0 then
               declare
                  SG : NVMM_Seg_Array renames State.Segs;
               begin
                  --  ES
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_SELECTOR,
                      Unsigned_64 (SG (NVMM_X64_SEG_ES).Selector), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_ES).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_BASE,
                      SG (NVMM_X64_SEG_ES).Base, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR,
                      To_VMX_AR (SG (NVMM_X64_SEG_ES).Attrib,
                                 SG (NVMM_X64_SEG_ES).Limit), Dummy_OK);
                  --  CS
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR,
                      Unsigned_64 (SG (NVMM_X64_SEG_CS).Selector), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_CS).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_BASE,
                      SG (NVMM_X64_SEG_CS).Base, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR,
                      To_VMX_AR (SG (NVMM_X64_SEG_CS).Attrib,
                                 SG (NVMM_X64_SEG_CS).Limit), Dummy_OK);
                  --  SS
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR,
                      Unsigned_64 (SG (NVMM_X64_SEG_SS).Selector), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_SS).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_BASE,
                      SG (NVMM_X64_SEG_SS).Base, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR,
                      To_VMX_AR (SG (NVMM_X64_SEG_SS).Attrib,
                                 SG (NVMM_X64_SEG_SS).Limit), Dummy_OK);
                  --  DS
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_SELECTOR,
                      Unsigned_64 (SG (NVMM_X64_SEG_DS).Selector), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_DS).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_BASE,
                      SG (NVMM_X64_SEG_DS).Base, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR,
                      To_VMX_AR (SG (NVMM_X64_SEG_DS).Attrib,
                                 SG (NVMM_X64_SEG_DS).Limit), Dummy_OK);
                  --  FS
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_SELECTOR,
                      Unsigned_64 (SG (NVMM_X64_SEG_FS).Selector), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_FS).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_BASE,
                      SG (NVMM_X64_SEG_FS).Base, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR,
                      To_VMX_AR (SG (NVMM_X64_SEG_FS).Attrib,
                                 SG (NVMM_X64_SEG_FS).Limit), Dummy_OK);
                  --  GS
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_SELECTOR,
                      Unsigned_64 (SG (NVMM_X64_SEG_GS).Selector), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_GS).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_BASE,
                      SG (NVMM_X64_SEG_GS).Base, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR,
                      To_VMX_AR (SG (NVMM_X64_SEG_GS).Attrib,
                                 SG (NVMM_X64_SEG_GS).Limit), Dummy_OK);
                  --  GDTR
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_GDT).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_BASE,
                      SG (NVMM_X64_SEG_GDT).Base, Dummy_OK);
                  --  IDTR
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_IDT).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_BASE,
                      SG (NVMM_X64_SEG_IDT).Base, Dummy_OK);
                  --  LDTR: if selector is 0, set VMX unusable bit
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_SELECTOR,
                      Unsigned_64 (SG (NVMM_X64_SEG_LDT).Selector), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_LIMIT,
                      Unsigned_64 (SG (NVMM_X64_SEG_LDT).Limit), Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_BASE,
                      SG (NVMM_X64_SEG_LDT).Base, Dummy_OK);
                  if SG (NVMM_X64_SEG_LDT).Selector = 0 then
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR,
                         16#1_0000#, Dummy_OK);
                  else
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR,
                         To_VMX_AR (SG (NVMM_X64_SEG_LDT).Attrib,
                                    SG (NVMM_X64_SEG_LDT).Limit), Dummy_OK);
                  end if;
                  --  TR: VMX requires TR to always be usable
                  --  If selector is 0, keep VMCS_Setup defaults
                  if SG (NVMM_X64_SEG_TR).Selector /= 0 then
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_GUEST_TR_SELECTOR,
                         Unsigned_64 (SG (NVMM_X64_SEG_TR).Selector),
                         Dummy_OK);
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_GUEST_TR_LIMIT,
                         Unsigned_64 (SG (NVMM_X64_SEG_TR).Limit), Dummy_OK);
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_GUEST_TR_BASE,
                         SG (NVMM_X64_SEG_TR).Base, Dummy_OK);
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_GUEST_TR_AR,
                         To_VMX_AR (SG (NVMM_X64_SEG_TR).Attrib,
                                    SG (NVMM_X64_SEG_TR).Limit), Dummy_OK);
                  end if;
               end;
            end if;

            --  Copy GPRs if requested
            if (Flags and VCPU_STATE_GPRS) /= 0 then
               --  RAX not in VMCS, use VMX_GPRs
               Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX :=
                  State.GPRs (NVMM_X64_GPR_RAX);
               --  RSP, RIP, RFLAGS are in VMCS
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_RSP,
                   State.GPRs (NVMM_X64_GPR_RSP), Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_RIP,
                   State.GPRs (NVMM_X64_GPR_RIP), Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS,
                   State.GPRs (NVMM_X64_GPR_RFLAGS), Dummy_OK);
               --  Other GPRs go to VMX_GPRs
               declare
                  G : NVMM_GPR_Array renames State.GPRs;
               begin
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBX :=
                     G (NVMM_X64_GPR_RBX);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX :=
                     G (NVMM_X64_GPR_RCX);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX :=
                     G (NVMM_X64_GPR_RDX);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RSI :=
                     G (NVMM_X64_GPR_RSI);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDI :=
                     G (NVMM_X64_GPR_RDI);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBP :=
                     G (NVMM_X64_GPR_RBP);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R8 :=
                     G (NVMM_X64_GPR_R8);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R9 :=
                     G (NVMM_X64_GPR_R9);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R10 :=
                     G (NVMM_X64_GPR_R10);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R11 :=
                     G (NVMM_X64_GPR_R11);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R12 :=
                     G (NVMM_X64_GPR_R12);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R13 :=
                     G (NVMM_X64_GPR_R13);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R14 :=
                     G (NVMM_X64_GPR_R14);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R15 :=
                     G (NVMM_X64_GPR_R15);
               end;
            end if;

            --  Copy control registers if requested
            if (Flags and VCPU_STATE_CRS) /= 0 then
               --  Apply CR0 fixed bits (e.g., NE must be 1)
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_CR0,
                   Arch.Virtualization.VMX.Apply_CR0_Fixed
                      (State.CRs (NVMM_X64_CR_CR0), True), Dummy_OK);
               --  CR2 not in VMCS, would need to be handled specially
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_CR3,
                   State.CRs (NVMM_X64_CR_CR3), Dummy_OK);
               --  Apply CR4 fixed bits (preserves LA57 for 5-level paging)
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_CR4,
                   Arch.Virtualization.VMX.Apply_CR4_Fixed
                      (State.CRs (NVMM_X64_CR_CR4)), Dummy_OK);
            end if;

            --  Copy debug registers if requested
            if (Flags and VCPU_STATE_DRS) /= 0 then
               Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (0) :=
                  State.DRs (NVMM_X64_DR_DR0);
               Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (1) :=
                  State.DRs (NVMM_X64_DR_DR1);
               Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (2) :=
                  State.DRs (NVMM_X64_DR_DR2);
               Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (3) :=
                  State.DRs (NVMM_X64_DR_DR3);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_DR7,
                   State.DRs (NVMM_X64_DR_DR7), Dummy_OK);
            end if;

            --  Copy MSRs if requested
            if (Flags and VCPU_STATE_MSRS) /= 0 then
               declare
                  M : NVMM_MSR_Array renames State.MSRs;
               begin
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER,
                      M (NVMM_X64_MSR_EFER), Dummy_OK);
                  --  Other MSRs would need MSR load/store areas in VMX
                  --  For now, store locally for potential future use
               end;
            end if;

            --  Copy interrupt state if requested
            if (Flags and VCPU_STATE_INTR) /= 0 then
               Machines (Positive (Mach)).VCPUs (CPU).V_TPR :=
                  Unsigned_8 (State.Intr and 16#0F#);
               Machines (Positive (Mach)).VCPUs (CPU).V_IRQ :=
                  (Shift_Right (State.Intr, 8) and 1) /= 0;
               Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Prio :=
                  Unsigned_8 (Shift_Right (State.Intr, 16) and 16#0F#);
               Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Vector :=
                  Unsigned_8 (Shift_Right (State.Intr, 20) and 16#FF#);
               Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Masking :=
                  (Shift_Right (State.Intr, 24) and 1) /= 0;
            end if;

            --  Copy FPU state if requested
            if (Flags and VCPU_STATE_FPU) /= 0 then
               declare
                  FPU_Addr_Local : constant Integer_Address :=
                     Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
                  type Byte_Array is array (0 .. 511) of Unsigned_8;
                  FPU_Buf : Byte_Array
                     with Import, Address => To_Address (FPU_Addr_Local);
               begin
                  for I in State.FPU'Range loop
                     FPU_Buf (I) := State.FPU (I);
                  end loop;
               end;
            end if;

            Release (Machines (Positive (Mach)).Lock);
            return True;
         end;
      end if;

      --  SVM backend
      --  Get VMCB pointer
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Copy segments if requested
      if (Flags and VCPU_STATE_SEGS) /= 0 then
         declare
            SS : Arch.Virtualization.SVM.VMCB_State_Save_Area renames
               VMCB_Ptr.State_Save;
            SG : NVMM_Seg_Array renames State.Segs;
         begin
            SS.ES.Selector := SG (NVMM_X64_SEG_ES).Selector;
            SS.ES.Attrib := NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_ES).Attrib);
            SS.ES.Limit := SG (NVMM_X64_SEG_ES).Limit;
            SS.ES.Base := SG (NVMM_X64_SEG_ES).Base;

            SS.CS.Selector := SG (NVMM_X64_SEG_CS).Selector;
            SS.CS.Attrib := NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_CS).Attrib);
            SS.CS.Limit := SG (NVMM_X64_SEG_CS).Limit;
            SS.CS.Base := SG (NVMM_X64_SEG_CS).Base;

            SS.SS.Selector := SG (NVMM_X64_SEG_SS).Selector;
            SS.SS.Attrib := NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_SS).Attrib);
            SS.SS.Limit := SG (NVMM_X64_SEG_SS).Limit;
            SS.SS.Base := SG (NVMM_X64_SEG_SS).Base;

            SS.DS.Selector := SG (NVMM_X64_SEG_DS).Selector;
            SS.DS.Attrib := NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_DS).Attrib);
            SS.DS.Limit := SG (NVMM_X64_SEG_DS).Limit;
            SS.DS.Base := SG (NVMM_X64_SEG_DS).Base;

            SS.FS.Selector := SG (NVMM_X64_SEG_FS).Selector;
            SS.FS.Attrib := NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_FS).Attrib);
            SS.FS.Limit := SG (NVMM_X64_SEG_FS).Limit;
            SS.FS.Base := SG (NVMM_X64_SEG_FS).Base;

            SS.GS.Selector := SG (NVMM_X64_SEG_GS).Selector;
            SS.GS.Attrib := NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_GS).Attrib);
            SS.GS.Limit := SG (NVMM_X64_SEG_GS).Limit;
            SS.GS.Base := SG (NVMM_X64_SEG_GS).Base;

            SS.GDTR.Selector := SG (NVMM_X64_SEG_GDT).Selector;
            SS.GDTR.Attrib :=
               NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_GDT).Attrib);
            SS.GDTR.Limit := SG (NVMM_X64_SEG_GDT).Limit;
            SS.GDTR.Base := SG (NVMM_X64_SEG_GDT).Base;

            SS.IDTR.Selector := SG (NVMM_X64_SEG_IDT).Selector;
            SS.IDTR.Attrib :=
               NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_IDT).Attrib);
            SS.IDTR.Limit := SG (NVMM_X64_SEG_IDT).Limit;
            SS.IDTR.Base := SG (NVMM_X64_SEG_IDT).Base;

            SS.LDTR.Selector := SG (NVMM_X64_SEG_LDT).Selector;
            SS.LDTR.Attrib :=
               NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_LDT).Attrib);
            SS.LDTR.Limit := SG (NVMM_X64_SEG_LDT).Limit;
            SS.LDTR.Base := SG (NVMM_X64_SEG_LDT).Base;

            SS.TR.Selector := SG (NVMM_X64_SEG_TR).Selector;
            SS.TR.Attrib := NVMM_To_VMCB_Attrib (SG (NVMM_X64_SEG_TR).Attrib);
            SS.TR.Limit := SG (NVMM_X64_SEG_TR).Limit;
            SS.TR.Base := SG (NVMM_X64_SEG_TR).Base;
         end;
      end if;

      --  Copy GPRs if requested
      if (Flags and VCPU_STATE_GPRS) /= 0 then
         VMCB_Ptr.State_Save.RAX := State.GPRs (NVMM_X64_GPR_RAX);
         VMCB_Ptr.State_Save.RSP := State.GPRs (NVMM_X64_GPR_RSP);
         VMCB_Ptr.State_Save.RIP := State.GPRs (NVMM_X64_GPR_RIP);
         VMCB_Ptr.State_Save.RFLAGS := State.GPRs (NVMM_X64_GPR_RFLAGS);
         --  Copy GPRs not in VMCB to our GPR structure
         declare
            G : NVMM_GPR_Array renames State.GPRs;
         begin
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RBX :=
               G (NVMM_X64_GPR_RBX);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX :=
               G (NVMM_X64_GPR_RCX);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RDX :=
               G (NVMM_X64_GPR_RDX);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RSI :=
               G (NVMM_X64_GPR_RSI);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RDI :=
               G (NVMM_X64_GPR_RDI);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RBP :=
               G (NVMM_X64_GPR_RBP);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R8 :=
               G (NVMM_X64_GPR_R8);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R9 :=
               G (NVMM_X64_GPR_R9);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R10 :=
               G (NVMM_X64_GPR_R10);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R11 :=
               G (NVMM_X64_GPR_R11);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R12 :=
               G (NVMM_X64_GPR_R12);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R13 :=
               G (NVMM_X64_GPR_R13);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R14 :=
               G (NVMM_X64_GPR_R14);
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.R15 :=
               G (NVMM_X64_GPR_R15);
         end;
      end if;

      --  Copy control registers if requested
      if (Flags and VCPU_STATE_CRS) /= 0 then
         VMCB_Ptr.State_Save.CR0 := State.CRs (NVMM_X64_CR_CR0);
         VMCB_Ptr.State_Save.CR2 := State.CRs (NVMM_X64_CR_CR2);
         VMCB_Ptr.State_Save.CR3 := State.CRs (NVMM_X64_CR_CR3);
         VMCB_Ptr.State_Save.CR4 := State.CRs (NVMM_X64_CR_CR4);
      end if;

      --  Copy debug registers if requested
      if (Flags and VCPU_STATE_DRS) /= 0 then
         --  DR0-DR3 are not in VMCB, store in our DRs_0_3 array
         Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (0) :=
            State.DRs (NVMM_X64_DR_DR0);
         Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (1) :=
            State.DRs (NVMM_X64_DR_DR1);
         Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (2) :=
            State.DRs (NVMM_X64_DR_DR2);
         Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (3) :=
            State.DRs (NVMM_X64_DR_DR3);
         --  DR6/DR7 are in VMCB
         VMCB_Ptr.State_Save.DR6 := State.DRs (NVMM_X64_DR_DR6);
         VMCB_Ptr.State_Save.DR7 := State.DRs (NVMM_X64_DR_DR7);
      end if;

      --  Copy MSRs if requested
      if (Flags and VCPU_STATE_MSRS) /= 0 then
         declare
            M : NVMM_MSR_Array renames State.MSRs;
         begin
            --  Always ensure SVME (bit 12) is set for VMRUN to work
            VMCB_Ptr.State_Save.EFER := M (NVMM_X64_MSR_EFER) or 16#1000#;
            VMCB_Ptr.State_Save.STAR := M (NVMM_X64_MSR_STAR);
            VMCB_Ptr.State_Save.LSTAR := M (NVMM_X64_MSR_LSTAR);
            VMCB_Ptr.State_Save.CSTAR := M (NVMM_X64_MSR_CSTAR);
            VMCB_Ptr.State_Save.SFMASK := M (NVMM_X64_MSR_SFMASK);
            VMCB_Ptr.State_Save.Kernel_GS_Base :=
               M (NVMM_X64_MSR_KERNELGSBASE);
            VMCB_Ptr.State_Save.SYSENTER_CS :=
               M (NVMM_X64_MSR_SYSENTER_CS);
            VMCB_Ptr.State_Save.SYSENTER_ESP :=
               M (NVMM_X64_MSR_SYSENTER_ESP);
            VMCB_Ptr.State_Save.SYSENTER_EIP :=
               M (NVMM_X64_MSR_SYSENTER_EIP);
            VMCB_Ptr.State_Save.G_PAT := M (NVMM_X64_MSR_PAT);
         end;
      end if;

      --  Copy interrupt state if requested
      if (Flags and VCPU_STATE_INTR) /= 0 then
         --  State.Intr maps to V_Intr_Control in VMCB
         --  Bits 0-3: V_TPR
         --  Bit 8: V_IRQ
         --  Bits 16-19: V_INTR_PRIO
         --  Bits 20-27: V_INTR_VECTOR (actually 0-7)
         --  Bit 24: V_INTR_MASKING (actually should be at bit 24)
         VMCB_Ptr.Control.V_Intr_Control := State.Intr;
         --  Also store in our local state for reference
         Machines (Positive (Mach)).VCPUs (CPU).V_TPR :=
            Unsigned_8 (State.Intr and 16#0F#);
         Machines (Positive (Mach)).VCPUs (CPU).V_IRQ :=
            (Shift_Right (State.Intr, 8) and 1) /= 0;
         Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Prio :=
            Unsigned_8 (Shift_Right (State.Intr, 16) and 16#0F#);
         Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Vector :=
            Unsigned_8 (Shift_Right (State.Intr, 20) and 16#FF#);
         Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Masking :=
            (Shift_Right (State.Intr, 24) and 1) /= 0;
      end if;

      --  Copy FPU state if requested
      if (Flags and VCPU_STATE_FPU) /= 0 then
         declare
            FPU_Addr_Local : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
            type Byte_Array is array (0 .. 511) of Unsigned_8;
            FPU_Buf : Byte_Array
               with Import, Address => To_Address (FPU_Addr_Local);
         begin
            for I in State.FPU'Range loop
               FPU_Buf (I) := State.FPU (I);
            end loop;
         end;
      end if;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Set_State;

   function VCPU_Get_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
      State    : NVMM_X64_State with Import, Address => Addr;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend - read from VMCS
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK   : Boolean;
         begin
            --  Load VMCS pointer before reading
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            --  Read segments if requested
            if (Flags and VCPU_STATE_SEGS) /= 0 then
               --  ES
               State.Segs (NVMM_X64_SEG_ES).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_SELECTOR));
               State.Segs (NVMM_X64_SEG_ES).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT));
               State.Segs (NVMM_X64_SEG_ES).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_BASE);
               State.Segs (NVMM_X64_SEG_ES).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR));
               --  CS
               State.Segs (NVMM_X64_SEG_CS).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR));
               State.Segs (NVMM_X64_SEG_CS).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT));
               State.Segs (NVMM_X64_SEG_CS).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_BASE);
               State.Segs (NVMM_X64_SEG_CS).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR));
               --  SS
               State.Segs (NVMM_X64_SEG_SS).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR));
               State.Segs (NVMM_X64_SEG_SS).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT));
               State.Segs (NVMM_X64_SEG_SS).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_BASE);
               State.Segs (NVMM_X64_SEG_SS).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR));
               --  DS
               State.Segs (NVMM_X64_SEG_DS).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_SELECTOR));
               State.Segs (NVMM_X64_SEG_DS).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT));
               State.Segs (NVMM_X64_SEG_DS).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_BASE);
               State.Segs (NVMM_X64_SEG_DS).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR));
               --  FS
               State.Segs (NVMM_X64_SEG_FS).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_SELECTOR));
               State.Segs (NVMM_X64_SEG_FS).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT));
               State.Segs (NVMM_X64_SEG_FS).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_BASE);
               State.Segs (NVMM_X64_SEG_FS).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR));
               --  GS
               State.Segs (NVMM_X64_SEG_GS).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_SELECTOR));
               State.Segs (NVMM_X64_SEG_GS).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT));
               State.Segs (NVMM_X64_SEG_GS).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_BASE);
               State.Segs (NVMM_X64_SEG_GS).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR));
               --  GDTR
               State.Segs (NVMM_X64_SEG_GDT).Selector := 0;
               State.Segs (NVMM_X64_SEG_GDT).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_LIMIT));
               State.Segs (NVMM_X64_SEG_GDT).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_BASE);
               State.Segs (NVMM_X64_SEG_GDT).Attrib := 0;
               --  IDTR
               State.Segs (NVMM_X64_SEG_IDT).Selector := 0;
               State.Segs (NVMM_X64_SEG_IDT).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_LIMIT));
               State.Segs (NVMM_X64_SEG_IDT).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_BASE);
               State.Segs (NVMM_X64_SEG_IDT).Attrib := 0;
               --  LDTR
               State.Segs (NVMM_X64_SEG_LDT).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_SELECTOR));
               State.Segs (NVMM_X64_SEG_LDT).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_LIMIT));
               State.Segs (NVMM_X64_SEG_LDT).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_BASE);
               State.Segs (NVMM_X64_SEG_LDT).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR));
               --  TR
               State.Segs (NVMM_X64_SEG_TR).Selector := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_TR_SELECTOR));
               State.Segs (NVMM_X64_SEG_TR).Limit := Unsigned_32
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_TR_LIMIT));
               State.Segs (NVMM_X64_SEG_TR).Base :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_TR_BASE);
               State.Segs (NVMM_X64_SEG_TR).Attrib := Unsigned_16
                  (Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_TR_AR));
            end if;

            --  Read GPRs if requested
            if (Flags and VCPU_STATE_GPRS) /= 0 then
               State.GPRs (NVMM_X64_GPR_RAX) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX;
               State.GPRs (NVMM_X64_GPR_RSP) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_RSP);
               State.GPRs (NVMM_X64_GPR_RIP) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_RIP);
               State.GPRs (NVMM_X64_GPR_RFLAGS) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS);
               State.GPRs (NVMM_X64_GPR_RBX) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBX;
               State.GPRs (NVMM_X64_GPR_RCX) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX;
               State.GPRs (NVMM_X64_GPR_RDX) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX;
               State.GPRs (NVMM_X64_GPR_RSI) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RSI;
               State.GPRs (NVMM_X64_GPR_RDI) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDI;
               State.GPRs (NVMM_X64_GPR_RBP) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBP;
               State.GPRs (NVMM_X64_GPR_R8) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R8;
               State.GPRs (NVMM_X64_GPR_R9) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R9;
               State.GPRs (NVMM_X64_GPR_R10) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R10;
               State.GPRs (NVMM_X64_GPR_R11) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R11;
               State.GPRs (NVMM_X64_GPR_R12) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R12;
               State.GPRs (NVMM_X64_GPR_R13) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R13;
               State.GPRs (NVMM_X64_GPR_R14) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R14;
               State.GPRs (NVMM_X64_GPR_R15) :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R15;
            end if;

            --  Read control registers if requested
            if (Flags and VCPU_STATE_CRS) /= 0 then
               State.CRs (NVMM_X64_CR_CR0) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR0);
               State.CRs (NVMM_X64_CR_CR2) := 0;  --  Not in VMCS
               State.CRs (NVMM_X64_CR_CR3) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR3);
               State.CRs (NVMM_X64_CR_CR4) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR4);
            end if;

            --  Read debug registers if requested
            if (Flags and VCPU_STATE_DRS) /= 0 then
               State.DRs (NVMM_X64_DR_DR0) :=
                  Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (0);
               State.DRs (NVMM_X64_DR_DR1) :=
                  Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (1);
               State.DRs (NVMM_X64_DR_DR2) :=
                  Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (2);
               State.DRs (NVMM_X64_DR_DR3) :=
                  Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (3);
               State.DRs (NVMM_X64_DR_DR6) := 0;  --  Not directly in VMCS
               State.DRs (NVMM_X64_DR_DR7) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_DR7);
            end if;

            --  Read MSRs if requested
            if (Flags and VCPU_STATE_MSRS) /= 0 then
               State.MSRs (NVMM_X64_MSR_EFER) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER);
               --  Other MSRs not directly available in VMCS
               State.MSRs (NVMM_X64_MSR_STAR) := 0;
               State.MSRs (NVMM_X64_MSR_LSTAR) := 0;
               State.MSRs (NVMM_X64_MSR_CSTAR) := 0;
               State.MSRs (NVMM_X64_MSR_SFMASK) := 0;
               State.MSRs (NVMM_X64_MSR_KERNELGSBASE) := 0;
               State.MSRs (NVMM_X64_MSR_SYSENTER_CS) := 0;
               State.MSRs (NVMM_X64_MSR_SYSENTER_ESP) := 0;
               State.MSRs (NVMM_X64_MSR_SYSENTER_EIP) := 0;
               State.MSRs (NVMM_X64_MSR_PAT) :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_PAT);
            end if;

            --  Read interrupt state if requested
            if (Flags and VCPU_STATE_INTR) /= 0 then
               State.Intr :=
                  Unsigned_64 (Machines (Positive (Mach)).VCPUs (CPU).V_TPR) or
                  (if Machines (Positive (Mach)).VCPUs (CPU).V_IRQ then
                     Shift_Left (Unsigned_64'(1), 8) else 0) or
                  Shift_Left (Unsigned_64
                     (Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Prio), 16)
                  or Shift_Left (Unsigned_64
                     (Machines
                        (Positive (Mach)).VCPUs (CPU).V_Intr_Vector), 20) or
                  (if Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Masking
                   then Shift_Left (Unsigned_64'(1), 24) else 0);
            end if;

            --  Read FPU state if requested
            if (Flags and VCPU_STATE_FPU) /= 0 then
               declare
                  FPU_Addr_Local : constant Integer_Address :=
                     Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
                  type Byte_Array is array (0 .. 511) of Unsigned_8;
                  FPU_Buf : Byte_Array
                     with Import, Address => To_Address (FPU_Addr_Local);
               begin
                  for I in State.FPU'Range loop
                     State.FPU (I) := FPU_Buf (I);
                  end loop;
               end;
            end if;

            Release (Machines (Positive (Mach)).Lock);
            return True;
         end;
      end if;

      --  SVM backend
      --  Get VMCB pointer
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Copy segments if requested
      if (Flags and VCPU_STATE_SEGS) /= 0 then
         declare
            SS : Arch.Virtualization.SVM.VMCB_State_Save_Area renames
               VMCB_Ptr.State_Save;
         begin
            State.Segs (NVMM_X64_SEG_ES).Selector := SS.ES.Selector;
            State.Segs (NVMM_X64_SEG_ES).Attrib :=
               VMCB_To_NVMM_Attrib (SS.ES.Attrib);
            State.Segs (NVMM_X64_SEG_ES).Limit := SS.ES.Limit;
            State.Segs (NVMM_X64_SEG_ES).Base := SS.ES.Base;

            State.Segs (NVMM_X64_SEG_CS).Selector := SS.CS.Selector;
            State.Segs (NVMM_X64_SEG_CS).Attrib :=
               VMCB_To_NVMM_Attrib (SS.CS.Attrib);
            State.Segs (NVMM_X64_SEG_CS).Limit := SS.CS.Limit;
            State.Segs (NVMM_X64_SEG_CS).Base := SS.CS.Base;

            State.Segs (NVMM_X64_SEG_SS).Selector := SS.SS.Selector;
            State.Segs (NVMM_X64_SEG_SS).Attrib :=
               VMCB_To_NVMM_Attrib (SS.SS.Attrib);
            State.Segs (NVMM_X64_SEG_SS).Limit := SS.SS.Limit;
            State.Segs (NVMM_X64_SEG_SS).Base := SS.SS.Base;

            State.Segs (NVMM_X64_SEG_DS).Selector := SS.DS.Selector;
            State.Segs (NVMM_X64_SEG_DS).Attrib :=
               VMCB_To_NVMM_Attrib (SS.DS.Attrib);
            State.Segs (NVMM_X64_SEG_DS).Limit := SS.DS.Limit;
            State.Segs (NVMM_X64_SEG_DS).Base := SS.DS.Base;

            State.Segs (NVMM_X64_SEG_FS).Selector := SS.FS.Selector;
            State.Segs (NVMM_X64_SEG_FS).Attrib :=
               VMCB_To_NVMM_Attrib (SS.FS.Attrib);
            State.Segs (NVMM_X64_SEG_FS).Limit := SS.FS.Limit;
            State.Segs (NVMM_X64_SEG_FS).Base := SS.FS.Base;

            State.Segs (NVMM_X64_SEG_GS).Selector := SS.GS.Selector;
            State.Segs (NVMM_X64_SEG_GS).Attrib :=
               VMCB_To_NVMM_Attrib (SS.GS.Attrib);
            State.Segs (NVMM_X64_SEG_GS).Limit := SS.GS.Limit;
            State.Segs (NVMM_X64_SEG_GS).Base := SS.GS.Base;

            State.Segs (NVMM_X64_SEG_GDT).Selector := SS.GDTR.Selector;
            State.Segs (NVMM_X64_SEG_GDT).Attrib :=
               VMCB_To_NVMM_Attrib (SS.GDTR.Attrib);
            State.Segs (NVMM_X64_SEG_GDT).Limit := SS.GDTR.Limit;
            State.Segs (NVMM_X64_SEG_GDT).Base := SS.GDTR.Base;

            State.Segs (NVMM_X64_SEG_IDT).Selector := SS.IDTR.Selector;
            State.Segs (NVMM_X64_SEG_IDT).Attrib :=
               VMCB_To_NVMM_Attrib (SS.IDTR.Attrib);
            State.Segs (NVMM_X64_SEG_IDT).Limit := SS.IDTR.Limit;
            State.Segs (NVMM_X64_SEG_IDT).Base := SS.IDTR.Base;

            State.Segs (NVMM_X64_SEG_LDT).Selector := SS.LDTR.Selector;
            State.Segs (NVMM_X64_SEG_LDT).Attrib :=
               VMCB_To_NVMM_Attrib (SS.LDTR.Attrib);
            State.Segs (NVMM_X64_SEG_LDT).Limit := SS.LDTR.Limit;
            State.Segs (NVMM_X64_SEG_LDT).Base := SS.LDTR.Base;

            State.Segs (NVMM_X64_SEG_TR).Selector := SS.TR.Selector;
            State.Segs (NVMM_X64_SEG_TR).Attrib :=
               VMCB_To_NVMM_Attrib (SS.TR.Attrib);
            State.Segs (NVMM_X64_SEG_TR).Limit := SS.TR.Limit;
            State.Segs (NVMM_X64_SEG_TR).Base := SS.TR.Base;
         end;
      end if;

      --  Copy GPRs if requested
      if (Flags and VCPU_STATE_GPRS) /= 0 then
         State.GPRs (NVMM_X64_GPR_RAX) := VMCB_Ptr.State_Save.RAX;
         State.GPRs (NVMM_X64_GPR_RSP) := VMCB_Ptr.State_Save.RSP;
         State.GPRs (NVMM_X64_GPR_RIP) := VMCB_Ptr.State_Save.RIP;
         State.GPRs (NVMM_X64_GPR_RFLAGS) := VMCB_Ptr.State_Save.RFLAGS;
         --  Copy GPRs from our GPR structure
         declare
            G : Arch.Virtualization.SVM.Guest_GPRs renames
               Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs;
         begin
            State.GPRs (NVMM_X64_GPR_RBX) := G.RBX;
            State.GPRs (NVMM_X64_GPR_RCX) := G.RCX;
            State.GPRs (NVMM_X64_GPR_RDX) := G.RDX;
            State.GPRs (NVMM_X64_GPR_RSI) := G.RSI;
            State.GPRs (NVMM_X64_GPR_RDI) := G.RDI;
            State.GPRs (NVMM_X64_GPR_RBP) := G.RBP;
            State.GPRs (NVMM_X64_GPR_R8) := G.R8;
            State.GPRs (NVMM_X64_GPR_R9) := G.R9;
            State.GPRs (NVMM_X64_GPR_R10) := G.R10;
            State.GPRs (NVMM_X64_GPR_R11) := G.R11;
            State.GPRs (NVMM_X64_GPR_R12) := G.R12;
            State.GPRs (NVMM_X64_GPR_R13) := G.R13;
            State.GPRs (NVMM_X64_GPR_R14) := G.R14;
            State.GPRs (NVMM_X64_GPR_R15) := G.R15;
         end;
      end if;

      --  Copy control registers if requested
      if (Flags and VCPU_STATE_CRS) /= 0 then
         State.CRs (NVMM_X64_CR_CR0) := VMCB_Ptr.State_Save.CR0;
         State.CRs (NVMM_X64_CR_CR2) := VMCB_Ptr.State_Save.CR2;
         State.CRs (NVMM_X64_CR_CR3) := VMCB_Ptr.State_Save.CR3;
         State.CRs (NVMM_X64_CR_CR4) := VMCB_Ptr.State_Save.CR4;
      end if;

      --  Copy debug registers if requested
      if (Flags and VCPU_STATE_DRS) /= 0 then
         --  DR0-DR3 are not in VMCB, read from our DRs_0_3 array
         State.DRs (NVMM_X64_DR_DR0) :=
            Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (0);
         State.DRs (NVMM_X64_DR_DR1) :=
            Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (1);
         State.DRs (NVMM_X64_DR_DR2) :=
            Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (2);
         State.DRs (NVMM_X64_DR_DR3) :=
            Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 (3);
         --  DR6/DR7 are in VMCB
         State.DRs (NVMM_X64_DR_DR6) := VMCB_Ptr.State_Save.DR6;
         State.DRs (NVMM_X64_DR_DR7) := VMCB_Ptr.State_Save.DR7;
      end if;

      --  Copy MSRs if requested
      if (Flags and VCPU_STATE_MSRS) /= 0 then
         declare
            SS : Arch.Virtualization.SVM.VMCB_State_Save_Area renames
               VMCB_Ptr.State_Save;
         begin
            State.MSRs (NVMM_X64_MSR_EFER) := SS.EFER;
            State.MSRs (NVMM_X64_MSR_STAR) := SS.STAR;
            State.MSRs (NVMM_X64_MSR_LSTAR) := SS.LSTAR;
            State.MSRs (NVMM_X64_MSR_CSTAR) := SS.CSTAR;
            State.MSRs (NVMM_X64_MSR_SFMASK) := SS.SFMASK;
            State.MSRs (NVMM_X64_MSR_KERNELGSBASE) := SS.Kernel_GS_Base;
            State.MSRs (NVMM_X64_MSR_SYSENTER_CS) := SS.SYSENTER_CS;
            State.MSRs (NVMM_X64_MSR_SYSENTER_ESP) := SS.SYSENTER_ESP;
            State.MSRs (NVMM_X64_MSR_SYSENTER_EIP) := SS.SYSENTER_EIP;
            State.MSRs (NVMM_X64_MSR_PAT) := SS.G_PAT;
         end;
      end if;

      --  Copy interrupt state if requested
      if (Flags and VCPU_STATE_INTR) /= 0 then
         --  Read V_Intr_Control from VMCB
         State.Intr := VMCB_Ptr.Control.V_Intr_Control;
      end if;

      --  Copy FPU state if requested
      if (Flags and VCPU_STATE_FPU) /= 0 then
         declare
            FPU_Addr_Local : constant Integer_Address :=
               Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
            type Byte_Array is array (0 .. 511) of Unsigned_8;
            FPU_Buf : Byte_Array
               with Import, Address => To_Address (FPU_Addr_Local);
         begin
            for I in State.FPU'Range loop
               State.FPU (I) := FPU_Buf (I);
            end loop;
         end;
      end if;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Get_State;

   function GPA_Map
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      type U64_Array is array (Natural range <>) of Unsigned_64;
      NPT_Addr    : Integer_Address;
      HPA         : Unsigned_64;
      Flags       : Unsigned_64;
      Num_Pages   : Unsigned_64;
      Page_4KB    : constant Unsigned_64 := 16#1000#;     --  4KB
      Page_2MB    : constant Unsigned_64 := 16#20_0000#;  --  2MB
      PT_Phys     : Unsigned_64;
      PT_Idx      : Natural;
      Current_GPA : Unsigned_64;
      Current_HPA : Unsigned_64;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      NPT_Addr := Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;

      --  Validate alignment (must be 4KB aligned)
      if (GPA and (Page_4KB - 1)) /= 0 or
         (HVA and (Page_4KB - 1)) /= 0 or
         (Size and (Page_4KB - 1)) /= 0
      then
         Release (Machines (Positive (Mach)).Lock);
         return False;  --  Must be 4KB aligned
      end if;

      --  Validate GPA range (must fit in first 2MB for now - our PT pool)
      --  We only have PTs pre-allocated for the first 2MB
      if GPA + Size > Page_2MB then
         Release (Machines (Positive (Mach)).Lock);
         return False;  --  Beyond 2MB limit with 4KB pages
      end if;

      --  Convert HVA to physical address
      HPA := Unsigned_64 (Integer_Address (HVA) - Arch.MMU.Memory_Offset);

      --  Build 4KB page flags: Present(0), Writable(1), User(2)
      --  Note: No PS bit for 4KB pages
      Flags := 16#01#;  --  Present
      if (Prot and GPA_PROT_WRITE) /= 0 then
         Flags := Flags or 16#02#;  --  Writable
      end if;
      Flags := Flags or 16#04#;  --  User (always set for guest access)

      --  NPT layout with 4KB support:
      --  PML4 (0), PDPT (4096), PD0 (8192), PD1 (12288), PD2 (16384),
      --  PD3 (20480), PT0 (24576)
      --  PT0 covers GPA 0x000000 - 0x1FFFFF (first 2MB with 4KB pages)
      --
      --  For first 2MB, we use a PT. PD0[0] points to PT0.
      --  Modify PD0[0] to point to PT0 (not a 2MB page anymore)
      declare
         PD0 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 8192);
         PT0 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 24576);
      begin
         --  Ensure PD0[0] points to PT0 (Present, Writable, User, no PS bit)
         PT_Phys := Unsigned_64 (NPT_Addr - Arch.MMU.Memory_Offset) + 24576;
         PD0 (0) := PT_Phys or 16#07#;  --  P + W + U

         Num_Pages := Size / Page_4KB;

         for I in 0 .. Natural (Num_Pages) - 1 loop
            Current_GPA := GPA + Unsigned_64 (I) * Page_4KB;
            Current_HPA := HPA + Unsigned_64 (I) * Page_4KB;

            --  PT index within PT0 (GPA / 4KB, max 511)
            PT_Idx := Natural (Current_GPA / Page_4KB);
            if PT_Idx <= 511 then
               PT0 (PT_Idx) := Current_HPA or Flags;
            end if;
         end loop;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end GPA_Map;

   function GPA_Map_All
      (Mach : Machine_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      Mapped_Any : Boolean := False;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      --  Map to all active VCPUs
      for CPU in VCPU_ID'Range loop
         if Machines (Positive (Mach)).VCPUs (CPU).Active then
            if GPA_Map (Mach, CPU, HVA, GPA, Size, Prot) then
               Mapped_Any := True;
            end if;
         end if;
      end loop;

      return Mapped_Any;
   exception
      when Constraint_Error =>
         return False;
   end GPA_Map_All;

   function GPA_Unmap
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean
   is
      type U64_Array is array (Natural range <>) of Unsigned_64;
      NPT_Addr  : Integer_Address;
      Num_Pages : Unsigned_64;
      Page_2MB  : constant Unsigned_64 := 16#20_0000#;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      NPT_Addr := Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;

      --  Validate alignment
      if (GPA and (Page_2MB - 1)) /= 0 or
         (Size and (Page_2MB - 1)) /= 0
      then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Validate GPA range (must fit in first 4GB)
      if GPA + Size > 16#1_0000_0000# then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Clear the PD entries (handle all 4 PDs for 4GB coverage)
      declare
         PD0 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 8192);
         PD1 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 12288);
         PD2 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 16384);
         PD3 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 20480);
         PD_Num    : Natural;
         Local_Idx : Natural;
         Current_GPA : Unsigned_64;
      begin
         Num_Pages := Size / Page_2MB;

         for I in 0 .. Natural (Num_Pages) - 1 loop
            Current_GPA := GPA + Unsigned_64 (I) * Page_2MB;
            PD_Num := Natural (Current_GPA / 16#4000_0000#);
            Local_Idx := Natural ((Current_GPA mod 16#4000_0000#) / Page_2MB);

            case PD_Num is
               when 0 => PD0 (Local_Idx) := 0;
               when 1 => PD1 (Local_Idx) := 0;
               when 2 => PD2 (Local_Idx) := 0;
               when 3 => PD3 (Local_Idx) := 0;
               when others => null;
            end case;
         end loop;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end GPA_Unmap;

   function GPA_Unmap_All
      (Mach : Machine_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean
   is
      Unmapped_Any : Boolean := False;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      --  Unmap from all active VCPUs
      for CPU in VCPU_ID'Range loop
         if Machines (Positive (Mach)).VCPUs (CPU).Active then
            if GPA_Unmap (Mach, CPU, GPA, Size) then
               Unmapped_Any := True;
            end if;
         end if;
      end loop;

      return Unmapped_Any;
   exception
      when Constraint_Error =>
         return False;
   end GPA_Unmap_All;

   function VCPU_Run_Ex_VMX
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : in out VCPU_Exit_Info) return Boolean
   is
      VMCS_Phys  : Unsigned_64;
      Exit_Reason : Unsigned_64;
      Load_OK    : Boolean;
   begin
      VMCS_Phys := Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;

      --  Load the VMCS pointer
      Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
      if not Load_OK then
         Release (Machines (Positive (Mach)).Lock);
         Exit_Info.Reason := NVMM_EXIT_INVALID;
         Exit_Info.U.Invalid.HW_Code := 16#DEAD_0001#;
         return False;
      end if;

      --  Check if stop was requested BEFORE running
      if Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested then
         Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;
         Exit_Info.Reason := NVMM_EXIT_STOPPED;
         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  FPU/XSAVE save/restore around VM entry
      declare
         FPU_Addr_Local : constant Integer_Address :=
            Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
         Guest_XCR0 : constant Unsigned_64 :=
            Machines (Positive (Mach)).VCPUs (CPU).XCR0_Value;
         Saved_Host_XCR0 : Unsigned_64 := 0;
      begin
         if Has_XSAVE then
            declare
               Guest_XSAVE : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_XSAVE  : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 2048);
            begin
               Saved_Host_XCR0 := Arch.Virtualization.SVM.Get_XCR0;
               Arch.Virtualization.SVM.XSAVE_Save
                  (Host_XSAVE, Saved_Host_XCR0);
               if Guest_XCR0 /= Saved_Host_XCR0 and then Guest_XCR0 /= 0 then
                  Arch.Virtualization.SVM.Set_XCR0 (Guest_XCR0);
               end if;
               Arch.Virtualization.SVM.XSAVE_Restore (Guest_XSAVE, Guest_XCR0);
            end;
         else
            declare
               Guest_FPU : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_FPU  : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 512);
            begin
               Arch.Virtualization.SVM.FPU_Save (Host_FPU);
               Arch.Virtualization.SVM.FPU_Restore (Guest_FPU);
            end;
         end if;

         --  Save host FS and GS bases
         declare
            Host_FS_Base : constant Unsigned_64 := Arch.Snippets.Read_FS;
            Host_GS_Base : constant Unsigned_64 := Arch.Snippets.Read_GS;
            Is_Launch : constant Boolean :=
               not Machines (Positive (Mach)).VCPUs (CPU).VMX_Launched;
            VM_Success : Boolean;
         begin
            --  Run the VCPU using VMLAUNCH or VMRESUME
            Arch.Virtualization.VMX.VMLAUNCH_VMRESUME
               (VMCS_PA   => VMCS_Phys,
                GPRs      => Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs,
                Is_Launch => Is_Launch,
                Success   => VM_Success);

            --  Check if VMLAUNCH/VMRESUME failed
            if not VM_Success then
               --  Restore FS/GS before returning
               Arch.Snippets.Write_FS (Host_FS_Base);
               Arch.Snippets.Write_GS (Host_GS_Base);
               --  Continue to return invalid exit
            end if;

            --  After first successful launch, use VMRESUME for next entries
            if Is_Launch and VM_Success then
               Machines (Positive (Mach)).VCPUs (CPU).VMX_Launched := True;
            end if;

            --  Restore host FS and GS bases
            Arch.Snippets.Write_FS (Host_FS_Base);
            Arch.Snippets.Write_GS (Host_GS_Base);
         end;

         --  Save guest state and restore host state
         if Has_XSAVE then
            declare
               Guest_XSAVE : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_XSAVE  : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 2048);
            begin
               Arch.Virtualization.SVM.XSAVE_Save (Guest_XSAVE, Guest_XCR0);
               if Guest_XCR0 /= Saved_Host_XCR0 and then Guest_XCR0 /= 0 then
                  Arch.Virtualization.SVM.Set_XCR0 (Saved_Host_XCR0);
               end if;
               Arch.Virtualization.SVM.XSAVE_Restore
                  (Host_XSAVE, Saved_Host_XCR0);
            end;
         else
            declare
               Guest_FPU : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_FPU  : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 512);
            begin
               Arch.Virtualization.SVM.FPU_Save (Guest_FPU);
               Arch.Virtualization.SVM.FPU_Restore (Host_FPU);
            end;
         end if;
      end;

      --  Reload IDT after VM exit
      Arch.IDT.Load_IDT;

      --  Check if stop was requested after running
      if Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested then
         Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;
         Exit_Info.Reason := NVMM_EXIT_STOPPED;
         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  Get exit reason from VMCS
      Exit_Reason := Arch.Virtualization.VMX.VMX_Read
         (Arch.Virtualization.VMX.VMCS_EXIT_REASON);

      --  Get exit state - read RFLAGS from VMCS
      Exit_Info.Exit_State.RFLAGS := Arch.Virtualization.VMX.VMX_Read
         (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS);
      Exit_Info.Exit_State.CR8 := 0;
      Exit_Info.Exit_State.Int_Shadow :=
         (Arch.Virtualization.VMX.VMX_Read
            (Arch.Virtualization.VMX.VMCS_GUEST_INTERRUPTIBILITY) and 1) /= 0;

      --  Translate VMX exit reason to NVMM exit code
      --  VMX exit reasons are in the low 16 bits
      case Exit_Reason and 16#FFFF# is
         when Arch.Virtualization.VMX.EXIT_REASON_EXCEPTION_NMI =>
            --  Guest hit an intercepted exception
            --  TODO: Properly handle different exception types
            Exit_Info.Reason := NVMM_EXIT_INVALID;
            Exit_Info.U.Invalid.HW_Code := Exit_Reason;

         when Arch.Virtualization.VMX.EXIT_REASON_EXTERNAL_INTERRUPT =>
            --  External interrupt arrived - just continue guest
            Exit_Info.Reason := NVMM_EXIT_NONE;

         when Arch.Virtualization.VMX.EXIT_REASON_TRIPLE_FAULT =>
            Exit_Info.Reason := NVMM_EXIT_SHUTDOWN;

         when Arch.Virtualization.VMX.EXIT_REASON_CPUID =>
            --  Emulate CPUID (RAX is in VMX_GPRs, not VMCS)
            declare
               Guest_RAX : constant Unsigned_64 :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX;
               Leaf    : constant Unsigned_32 :=
                  Unsigned_32 (Guest_RAX and 16#FFFF_FFFF#);
               Subleaf : constant Unsigned_32 := Unsigned_32
                  (Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX
                               and 16#FFFF_FFFF#);
               Out_EAX, Out_EBX, Out_ECX, Out_EDX : Unsigned_32;
               CPUID_OK : Boolean;
               Inst_Len : Unsigned_64;
               Guest_RIP : Unsigned_64;
            begin
               Arch.Snippets.Get_CPUID
                  (Leaf    => Leaf,
                   Subleaf => Subleaf,
                   EAX     => Out_EAX,
                   EBX     => Out_EBX,
                   ECX     => Out_ECX,
                   EDX     => Out_EDX,
                   Success => CPUID_OK);

               if CPUID_OK then
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX :=
                     Unsigned_64 (Out_EAX);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBX :=
                     Unsigned_64 (Out_EBX);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX :=
                     Unsigned_64 (Out_ECX);
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX :=
                     Unsigned_64 (Out_EDX);
               else
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX := 0;
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBX := 0;
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX := 0;
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX := 0;
               end if;

               --  Advance RIP
               Inst_Len := Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_EXIT_INSTR_LENGTH);
               Guest_RIP := Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_RIP);
               declare
                  Dummy_OK : Boolean;
               begin
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_RIP,
                      Guest_RIP + Inst_Len, Dummy_OK);
               end;

               Exit_Info.Reason := NVMM_EXIT_NONE;
            end;

         when Arch.Virtualization.VMX.EXIT_REASON_HLT =>
            Exit_Info.Reason := NVMM_EXIT_HALTED;

         when Arch.Virtualization.VMX.EXIT_REASON_IO_INSTRUCTION =>
            Exit_Info.Reason := NVMM_EXIT_IO;
            declare
               Qual : constant Unsigned_64 := Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_EXIT_QUALIFICATION);
            begin
               --  Bit 3: Direction (0=OUT, 1=IN)
               Exit_Info.U.IO.Is_In := (Qual and 8) /= 0;
               --  Bit 4: String instruction
               Exit_Info.U.IO.Is_String := (Qual and 16) /= 0;
               --  Bit 5: REP prefix
               Exit_Info.U.IO.Is_Rep := (Qual and 32) /= 0;
               --  Bits 0-2: Size (0=1, 1=2, 3=4)
               case Qual and 7 is
                  when 0 => Exit_Info.U.IO.Operand_Size := 1;
                  when 1 => Exit_Info.U.IO.Operand_Size := 2;
                  when 3 => Exit_Info.U.IO.Operand_Size := 4;
                  when others => Exit_Info.U.IO.Operand_Size := 1;
               end case;
               Exit_Info.U.IO.Address_Size := 32;  --  Default
               Exit_Info.U.IO.Segment := 0;
               --  Bits 16-31: Port number
               Exit_Info.U.IO.Port :=
                  Unsigned_16 (Shift_Right (Qual, 16) and 16#FFFF#);
            end;
            Exit_Info.U.IO.Next_RIP := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RIP) +
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_EXIT_INSTR_LENGTH);

         when Arch.Virtualization.VMX.EXIT_REASON_RDMSR =>
            Exit_Info.Reason := NVMM_EXIT_RDMSR;
            Exit_Info.U.MSR_Read.MSR_Num :=
               Unsigned_32 (Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX
                            and 16#FFFF_FFFF#);
            Exit_Info.U.MSR_Read.Next_RIP := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RIP) +
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_EXIT_INSTR_LENGTH);

         when Arch.Virtualization.VMX.EXIT_REASON_WRMSR =>
            Exit_Info.Reason := NVMM_EXIT_WRMSR;
            Exit_Info.U.MSR_Write.MSR_Num :=
               Unsigned_32 (Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX
                            and 16#FFFF_FFFF#);
            Exit_Info.U.MSR_Write.MSR_Val :=
               Shift_Left (Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX
                            and 16#FFFF_FFFF#, 32) or
               (Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX
                            and 16#FFFF_FFFF#);
            Exit_Info.U.MSR_Write.Next_RIP := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RIP) +
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_EXIT_INSTR_LENGTH);

         when Arch.Virtualization.VMX.EXIT_REASON_EPT_VIOLATION =>
            Exit_Info.Reason := NVMM_EXIT_MEMORY;
            Exit_Info.U.Memory.GPA := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_PHYS_ADDR);
            declare
               Qual : constant Unsigned_64 := Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_EXIT_QUALIFICATION);
               Prot_Val : Integer := 0;
            begin
               --  Bit 0: Read access
               --  Bit 1: Write access
               --  Bit 2: Execute access
               if (Qual and 2) /= 0 then
                  Prot_Val := Prot_Val + 2;  --  PROT_WRITE
               elsif (Qual and 1) /= 0 then
                  Prot_Val := Prot_Val + 1;  --  PROT_READ
               end if;
               if (Qual and 4) /= 0 then
                  Prot_Val := Prot_Val + 4;  --  PROT_EXEC
               end if;
               Exit_Info.U.Memory.Prot := Prot_Val;
            end;
            --  Fetch instruction bytes from guest memory at RIP
            --  Uses same logic as SVM: PT0 for first 2MB, identity elsewhere
            declare
               Guest_RIP   : constant Unsigned_64 :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_RIP);
               Page_4KB    : constant Unsigned_64 := 16#1000#;
               Page_2MB    : constant Unsigned_64 := 16#20_0000#;
               NPT_Addr    : constant Integer_Address :=
                  Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;
               type U64_Array is array (Natural range <>) of Unsigned_64;
               type Byte_Array is array (0 .. 14) of Unsigned_8;
               HPA         : Unsigned_64;
               Page_Off    : Unsigned_64;
               Inst_Addr   : Integer_Address;
            begin
               if Guest_RIP < Page_2MB then
                  --  Look up in PT0 (page 6 of EPT allocation)
                  declare
                     PT0 : U64_Array (0 .. 511)
                        with Import, Address => To_Address (NPT_Addr + 24576);
                     PT_Idx : constant Natural :=
                        Natural (Guest_RIP / Page_4KB);
                  begin
                     HPA := PT0 (PT_Idx) and 16#FFFF_FFFF_FFFF_F000#;
                     Page_Off := Guest_RIP and (Page_4KB - 1);
                     Inst_Addr := Integer_Address (HPA + Page_Off) +
                        Arch.MMU.Memory_Offset;
                  end;
               else
                  --  Identity mapped, GPA = HPA
                  Inst_Addr := Integer_Address (Guest_RIP) +
                     Arch.MMU.Memory_Offset;
               end if;

               declare
                  Inst_Mem : Byte_Array
                     with Import, Address => To_Address (Inst_Addr);
               begin
                  Exit_Info.U.Memory.Inst_Len := 15;  --  Max, userspace decode
                  for I in 0 .. 14 loop
                     Exit_Info.U.Memory.Inst_Bytes (I) := Inst_Mem (I);
                  end loop;
               end;
            end;

         when Arch.Virtualization.VMX.EXIT_REASON_XSETBV =>
            --  XSETBV: Emulate XCR0 internally (same as SVM)
            declare
               XCR_Num : constant Unsigned_32 := Unsigned_32
                  (Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX
                               and 16#FFFF_FFFF#);
               Guest_RAX : constant Unsigned_64 :=
                  Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX;
               XCR_Val : constant Unsigned_64 := Shift_Left
                  (Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX
                               and 16#FFFF_FFFF#, 32) or
                  (Guest_RAX and 16#FFFF_FFFF#);
               XCR0_Valid : Boolean;
               Inst_Len : Unsigned_64;
               Guest_RIP : Unsigned_64;
            begin
               if XCR_Num = 0 then
                  XCR0_Valid := (XCR_Val and 1) /= 0 and then
                     ((XCR_Val and 2) /= 0 or (XCR_Val and 4) = 0) and then
                     (XCR_Val and not Host_XCR0_Max) = 0;

                  if XCR0_Valid then
                     Machines (Positive (Mach)).VCPUs (CPU).XCR0_Value :=
                        XCR_Val;
                     Inst_Len := Arch.Virtualization.VMX.VMX_Read
                        (Arch.Virtualization.VMX.VMCS_EXIT_INSTR_LENGTH);
                     Guest_RIP := Arch.Virtualization.VMX.VMX_Read
                        (Arch.Virtualization.VMX.VMCS_GUEST_RIP);
                     declare
                        Dummy_OK : Boolean;
                     begin
                        Arch.Virtualization.VMX.VMX_Write
                           (Arch.Virtualization.VMX.VMCS_GUEST_RIP,
                            Guest_RIP + Inst_Len, Dummy_OK);
                     end;
                     Exit_Info.Reason := NVMM_EXIT_NONE;
                  else
                     Exit_Info.Reason := NVMM_EXIT_INVALID;
                     Exit_Info.U.Invalid.HW_Code := Exit_Reason;
                  end if;
               else
                  Exit_Info.Reason := NVMM_EXIT_INVALID;
                  Exit_Info.U.Invalid.HW_Code := Exit_Reason;
               end if;
            end;

         when others =>
            --  Unknown/unhandled VMX exit
            Exit_Info.Reason := NVMM_EXIT_INVALID;
            Exit_Info.U.Invalid.HW_Code := Exit_Reason;
      end case;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Run_Ex_VMX;

   function VCPU_Run_Ex
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : out VCPU_Exit_Info) return Boolean
   is
      VMCB_Phys : Unsigned_64;
      VMCB_Ptr  : Arch.Virtualization.SVM.VMCB_Acc;
      Exit_Code : Unsigned_64;
   begin
      --  Initialize exit info
      Exit_Info.Reason := NVMM_EXIT_NONE;
      Exit_Info.U.Insn.Next_RIP := 0;  --  Zero out union via one variant
      Exit_Info.Exit_State := (RFLAGS          => 0,
                               CR8             => 0,
                               Int_Shadow      => False,
                               Int_Window_Exit => False,
                               NMI_Window_Exit => False,
                               Evt_Pending     => False);

      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Dispatch based on backend
      if Current_Backend = Backend_VMX then
         return VCPU_Run_Ex_VMX (Mach, CPU, Exit_Info);
      end if;

      VMCB_Phys := Machines (Positive (Mach)).VCPUs (CPU).VMCB_Phys;

      --  Get VMCB pointer
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Clear TLB control after first run (only flush on first entry)
      VMCB_Ptr.Control.TLB_Control :=
         Arch.Virtualization.SVM.TLB_CONTROL_DO_NOTHING;

      --  Set up event injection if pending
      if Machines (Positive (Mach)).VCPUs (CPU).Event_Pending then
         declare
            Evt   : constant NVMM_Event_Info :=
               Machines (Positive (Mach)).VCPUs (CPU).Pending_Event;
            Inject_Val : Unsigned_64;
            SVM_Type   : Unsigned_64;
         begin
            --  Convert NVMM event type to SVM EVENTINJ type:
            --  NVMM: 0=HW_INT, 1=SW_INT, 2=EXCEPTION, 3=NMI
            --  SVM:  0=INTR, 2=NMI, 3=EXCEPTION, 4=SW_INT
            case Evt.Event_Type is
               when NVMM_EVENT_INTERRUPT_HW => SVM_Type := 0;
               when NVMM_EVENT_INTERRUPT_SW => SVM_Type := 4;
               when NVMM_EVENT_EXCEPTION    => SVM_Type := 3;
               when NVMM_EVENT_NMI          => SVM_Type := 2;
               when others                  => SVM_Type := 0;
            end case;

            --  Build EVENTINJ value:
            --  Bits 0-7: Vector
            --  Bits 8-10: Type
            --  Bit 11: EV (error code valid)
            --  Bit 31: V (valid)
            --  Bits 32-63: Error code
            Inject_Val := Unsigned_64 (Evt.Vector) or
                          Shift_Left (SVM_Type, 8) or
                          Shift_Left (Unsigned_64'(1), 31);  --  Valid bit
            if Evt.Has_Error then
               Inject_Val := Inject_Val or
                             Shift_Left (Unsigned_64'(1), 11) or  --  EV bit
                             Shift_Left (Evt.Error_Code, 32);
            end if;

            VMCB_Ptr.Control.Event_Inject := Inject_Val;
            Machines (Positive (Mach)).VCPUs (CPU).Event_Pending := False;
         end;
      else
         VMCB_Ptr.Control.Event_Inject := 0;
      end if;

      --  Check if stop was requested BEFORE running (for early exit)
      if Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested then
         Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;
         Exit_Info.Reason := NVMM_EXIT_STOPPED;
         Exit_Info.Exit_State.RFLAGS := VMCB_Ptr.State_Save.RFLAGS;
         Exit_Info.Exit_State.CR8 := 0;
         Exit_Info.Exit_State.Int_Shadow :=
            (VMCB_Ptr.Control.Interrupt_Shadow and 1) /= 0;
         Exit_Info.Exit_State.Int_Window_Exit := False;
         Exit_Info.Exit_State.NMI_Window_Exit := False;
         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  FPU/XSAVE save/restore around VMRUN using page-aligned buffer.
      --  The FPU_Addr buffer is 4KB (1 page), 64-byte aligned.
      --  - With XSAVE: Guest at offset 0 (2KB), Host at offset 2048 (2KB)
      --  - With FXSAVE: Guest at offset 0 (512B), Host at offset 512 (512B)
      declare
         FPU_Addr_Local : constant Integer_Address :=
            Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
         Guest_XCR0 : constant Unsigned_64 :=
            Machines (Positive (Mach)).VCPUs (CPU).XCR0_Value;
         --  Save host's XCR0 value to restore after VMRUN
         Saved_Host_XCR0 : Unsigned_64 := 0;
      begin
         if Has_XSAVE then
            --  Use XSAVE for extended state (AVX, etc.)
            declare
               Guest_XSAVE : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_XSAVE  : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 2048);
            begin
               --  Read and save host's current XCR0 value
               Saved_Host_XCR0 := Arch.Virtualization.SVM.Get_XCR0;
               --  Save host extended state with host's XCR0
               Arch.Virtualization.SVM.XSAVE_Save
                  (Host_XSAVE, Saved_Host_XCR0);
               --  Set XCR0 to guest value if different and valid
               if Guest_XCR0 /= Saved_Host_XCR0 and then Guest_XCR0 /= 0 then
                  Arch.Virtualization.SVM.Set_XCR0 (Guest_XCR0);
               end if;
               --  Load guest extended state
               Arch.Virtualization.SVM.XSAVE_Restore (Guest_XSAVE, Guest_XCR0);
            end;
         else
            --  Use legacy FXSAVE for x87/SSE only
            declare
               Guest_FPU : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_FPU  : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 512);
            begin
               Arch.Virtualization.SVM.FPU_Save (Host_FPU);
               Arch.Virtualization.SVM.FPU_Restore (Guest_FPU);
            end;
         end if;

         --  Save host FS and GS bases.
         --  VMRUN may corrupt FS.BASE and GS.BASE, so we save and restore.
         --  FS.BASE is used by userland for TLS (thread-local storage).
         --  GS.BASE is used for per-CPU data via %gs:0.
         declare
            Host_FS_Base : constant Unsigned_64 := Arch.Snippets.Read_FS;
            Host_GS_Base : constant Unsigned_64 := Arch.Snippets.Read_GS;
         begin
            --  Run the VCPU
            Arch.Virtualization.SVM.VMRUN
               (VMCB_PA => VMCB_Phys,
                GPRs    => Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs);

            --  Restore host FS and GS bases
            Arch.Snippets.Write_FS (Host_FS_Base);
            Arch.Snippets.Write_GS (Host_GS_Base);
         end;

         --  Save guest state and restore host state
         if Has_XSAVE then
            declare
               Guest_XSAVE : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_XSAVE  : Arch.Virtualization.SVM.XSAVE_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 2048);
            begin
               --  Save guest extended state with guest's XCR0
               Arch.Virtualization.SVM.XSAVE_Save (Guest_XSAVE, Guest_XCR0);
               --  Restore host XCR0 if it was changed
               if Guest_XCR0 /= Saved_Host_XCR0 and then Guest_XCR0 /= 0 then
                  Arch.Virtualization.SVM.Set_XCR0 (Saved_Host_XCR0);
               end if;
               --  Restore host extended state with host's XCR0
               Arch.Virtualization.SVM.XSAVE_Restore
                  (Host_XSAVE, Saved_Host_XCR0);
            end;
         else
            declare
               Guest_FPU : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local);
               Host_FPU  : Arch.Virtualization.SVM.FPU_State_Area
                  with Import, Address => To_Address (FPU_Addr_Local + 512);
            begin
               Arch.Virtualization.SVM.FPU_Save (Guest_FPU);
               Arch.Virtualization.SVM.FPU_Restore (Host_FPU);
            end;
         end if;
      end;

      --  Reload IDT after VMRUN (may not be fully restored)
      Arch.IDT.Load_IDT;

      --  Check if stop was requested
      if Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested then
         Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;
         Exit_Info.Reason := NVMM_EXIT_STOPPED;
         Exit_Info.Exit_State.RFLAGS := VMCB_Ptr.State_Save.RFLAGS;
         Exit_Info.Exit_State.CR8 := 0;
         Exit_Info.Exit_State.Int_Shadow :=
            (VMCB_Ptr.Control.Interrupt_Shadow and 1) /= 0;
         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  Get raw exit code from VMCB
      Exit_Code := VMCB_Ptr.Control.Exit_Code;

      --  Populate exit state from VMCB
      Exit_Info.Exit_State.RFLAGS := VMCB_Ptr.State_Save.RFLAGS;
      --  CR8 would come from TPR, currently not implemented
      Exit_Info.Exit_State.CR8 := 0;
      --  Interrupt shadow from VMCB
      Exit_Info.Exit_State.Int_Shadow :=
         (VMCB_Ptr.Control.Interrupt_Shadow and 1) /= 0;

      --  Translate SVM exit code to NVMM exit code
      case Exit_Code is
         when Arch.Virtualization.SVM.VMEXIT_SHUTDOWN =>
            Exit_Info.Reason := NVMM_EXIT_SHUTDOWN;

         when 16#0040# .. 16#005F# =>
            --  Exception intercept (VMEXIT codes 0x40-0x5F)
            Exit_Info.Reason := NVMM_EXIT_INVALID;
            Exit_Info.U.Invalid.HW_Code := Exit_Code;

         when Arch.Virtualization.SVM.VMEXIT_HLT =>
            Exit_Info.Reason := NVMM_EXIT_HALTED;

         when Arch.Virtualization.SVM.VMEXIT_CPUID =>
            --  Emulate CPUID by executing it on the host
            declare
               Leaf    : constant Unsigned_32 :=
                  Unsigned_32 (VMCB_Ptr.State_Save.RAX and 16#FFFF_FFFF#);
               Subleaf : constant Unsigned_32 :=
                  Unsigned_32
                     (Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX
                      and 16#FFFF_FFFF#);
               Out_EAX, Out_EBX, Out_ECX, Out_EDX : Unsigned_32;
               CPUID_OK : Boolean;
            begin
               Arch.Snippets.Get_CPUID
                  (Leaf    => Leaf,
                   Subleaf => Subleaf,
                   EAX     => Out_EAX,
                   EBX     => Out_EBX,
                   ECX     => Out_ECX,
                   EDX     => Out_EDX,
                   Success => CPUID_OK);

               if CPUID_OK then
                  --  Set guest registers with CPUID results
                  VMCB_Ptr.State_Save.RAX := Unsigned_64 (Out_EAX);
                  Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RBX :=
                     Unsigned_64 (Out_EBX);
                  Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX :=
                     Unsigned_64 (Out_ECX);
                  Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RDX :=
                     Unsigned_64 (Out_EDX);
               else
                  --  CPUID failed - return zeros
                  VMCB_Ptr.State_Save.RAX := 0;
                  Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RBX := 0;
                  Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX := 0;
                  Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RDX := 0;
               end if;

               --  Advance RIP past CPUID instruction
               --  Use Next_RIP if NRIP Save is supported, else add 2 bytes
               if VMCB_Ptr.Control.Next_RIP /= 0 then
                  VMCB_Ptr.State_Save.RIP := VMCB_Ptr.Control.Next_RIP;
               else
                  VMCB_Ptr.State_Save.RIP :=
                     VMCB_Ptr.State_Save.RIP + 2;  -- CPUID is 2 bytes
               end if;

               --  Return NONE so guest continues running
               Exit_Info.Reason := NVMM_EXIT_NONE;
            end;

         when Arch.Virtualization.SVM.VMEXIT_IOIO =>
            Exit_Info.Reason := NVMM_EXIT_IO;
            --  Parse Exit_Info_1 for I/O details (AMD APM format)
            declare
               Info1 : constant Unsigned_64 := VMCB_Ptr.Control.Exit_Info_1;
            begin
               Exit_Info.U.IO.Is_In := (Info1 and 1) /= 0;
               Exit_Info.U.IO.Is_String := (Shift_Right (Info1, 2) and 1) /= 0;
               Exit_Info.U.IO.Is_Rep := (Shift_Right (Info1, 3) and 1) /= 0;
               --  Operand size: bits 4-6
               case Shift_Right (Info1, 4) and 7 is
                  when 1 => Exit_Info.U.IO.Operand_Size := 1;
                  when 2 => Exit_Info.U.IO.Operand_Size := 2;
                  when 4 => Exit_Info.U.IO.Operand_Size := 4;
                  when others => Exit_Info.U.IO.Operand_Size := 1;
               end case;
               --  Address size: bits 7-9
               case Shift_Right (Info1, 7) and 7 is
                  when 1 => Exit_Info.U.IO.Address_Size := 16;
                  when 2 => Exit_Info.U.IO.Address_Size := 32;
                  when 4 => Exit_Info.U.IO.Address_Size := 64;
                  when others => Exit_Info.U.IO.Address_Size := 32;
               end case;
               --  Segment: bits 10-12
               Exit_Info.U.IO.Segment :=
                  Integer_8 (Shift_Right (Info1, 10) and 7);
               --  Port: bits 16-31
               Exit_Info.U.IO.Port :=
                  Unsigned_16 (Shift_Right (Info1, 16) and 16#FFFF#);
            end;
            Exit_Info.U.IO.Next_RIP := VMCB_Ptr.Control.Next_RIP;

         when Arch.Virtualization.SVM.VMEXIT_MSR =>
            --  Exit_Info_1 bit 0: 0=read, 1=write
            if (VMCB_Ptr.Control.Exit_Info_1 and 1) = 0 then
               Exit_Info.Reason := NVMM_EXIT_RDMSR;
               --  MSR number is in RCX
               Exit_Info.U.MSR_Read.MSR_Num := Unsigned_32
                  (Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX
                               and 16#FFFF_FFFF#);
            else
               Exit_Info.Reason := NVMM_EXIT_WRMSR;
               Exit_Info.U.MSR_Write.MSR_Num := Unsigned_32
                  (Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX
                               and 16#FFFF_FFFF#);
               --  Value is in EDX:EAX
               Exit_Info.U.MSR_Write.MSR_Val := (Shift_Left
                  (Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RDX
                               and 16#FFFF_FFFF#, 32)) or
                  (VMCB_Ptr.State_Save.RAX and 16#FFFF_FFFF#);
            end if;
            Exit_Info.U.MSR_Read.Next_RIP := VMCB_Ptr.Control.Next_RIP;
            Exit_Info.U.MSR_Write.Next_RIP := VMCB_Ptr.Control.Next_RIP;

         when Arch.Virtualization.SVM.VMEXIT_NPF =>
            Exit_Info.Reason := NVMM_EXIT_MEMORY;
            Exit_Info.U.Memory.GPA := VMCB_Ptr.Control.Exit_Info_2;
            --  Exit_Info_1 contains page fault error code
            declare
               Info1 : constant Unsigned_64 := VMCB_Ptr.Control.Exit_Info_1;
               Prot_Val : Integer := 0;
            begin
               if (Info1 and 2) /= 0 then  --  Write access
                  Prot_Val := Prot_Val + 2;  --  PROT_WRITE
               else
                  Prot_Val := Prot_Val + 1;  --  PROT_READ
               end if;
               if (Info1 and 4) /= 0 then  --  User mode
                  Prot_Val := Prot_Val + 8;  --  PROT_USER
               end if;
               if (Info1 and 16) /= 0 then  --  Execute
                  Prot_Val := Prot_Val + 4;  --  PROT_EXEC
               end if;
               Exit_Info.U.Memory.Prot := Prot_Val;
            end;
            --  Fetch instruction bytes from guest memory at RIP
            --  Guest has no paging (CR0.PG=0), so GVA=GPA.
            --  NPT uses PT0 (4KB pages) for first 2MB, 2MB pages elsewhere.
            --  Must look up PT0 to get actual HPA for addresses < 2MB.
            declare
               Guest_RIP   : constant Unsigned_64 := VMCB_Ptr.State_Save.RIP;
               Page_4KB    : constant Unsigned_64 := 16#1000#;
               Page_2MB    : constant Unsigned_64 := 16#20_0000#;
               NPT_Addr    : constant Integer_Address :=
                  Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;
               type U64_Array is array (Natural range <>) of Unsigned_64;
               type Byte_Array is array (0 .. 14) of Unsigned_8;
               HPA         : Unsigned_64;
               Page_Off    : Unsigned_64;
               Inst_Addr   : Integer_Address;
            begin
               if Guest_RIP < Page_2MB then
                  --  Look up in PT0 (page 6 of NPT allocation)
                  declare
                     PT0 : U64_Array (0 .. 511)
                        with Import, Address => To_Address (NPT_Addr + 24576);
                     PT_Idx : constant Natural :=
                        Natural (Guest_RIP / Page_4KB);
                  begin
                     HPA := PT0 (PT_Idx) and 16#FFFF_FFFF_FFFF_F000#;
                     Page_Off := Guest_RIP and (Page_4KB - 1);
                     Inst_Addr := Integer_Address (HPA + Page_Off) +
                        Arch.MMU.Memory_Offset;
                  end;
               else
                  --  Identity mapped, GPA = HPA
                  Inst_Addr := Integer_Address (Guest_RIP) +
                     Arch.MMU.Memory_Offset;
               end if;

               declare
                  Inst_Mem : Byte_Array
                     with Import, Address => To_Address (Inst_Addr);
               begin
                  Exit_Info.U.Memory.Inst_Len := 15;  --  Max, userspace decode
                  for I in 0 .. 14 loop
                     Exit_Info.U.Memory.Inst_Bytes (I) := Inst_Mem (I);
                  end loop;
               end;
            end;

         when Arch.Virtualization.SVM.VMEXIT_VINTR =>
            Exit_Info.Reason := NVMM_EXIT_INT_READY;

         when Arch.Virtualization.SVM.VMEXIT_INVLPG =>
            --  INVLPG: Guest TLB invalidation - NPT handles this, just skip
            if VMCB_Ptr.Control.Next_RIP /= 0 then
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.Control.Next_RIP;
            else
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.State_Save.RIP + 3;
            end if;
            Exit_Info.Reason := NVMM_EXIT_NONE;

         when Arch.Virtualization.SVM.VMEXIT_INVLPGA =>
            --  INVLPGA: Guest TLB invalidation with ASID - just skip
            if VMCB_Ptr.Control.Next_RIP /= 0 then
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.Control.Next_RIP;
            else
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.State_Save.RIP + 3;
            end if;
            Exit_Info.Reason := NVMM_EXIT_NONE;

         when Arch.Virtualization.SVM.VMEXIT_PAUSE =>
            --  PAUSE: Yield hint - just skip and continue
            if VMCB_Ptr.Control.Next_RIP /= 0 then
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.Control.Next_RIP;
            else
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.State_Save.RIP + 2;
            end if;
            Exit_Info.Reason := NVMM_EXIT_NONE;

         when Arch.Virtualization.SVM.VMEXIT_WBINVD =>
            --  WBINVD: Cache writeback - skip and continue
            if VMCB_Ptr.Control.Next_RIP /= 0 then
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.Control.Next_RIP;
            else
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.State_Save.RIP + 2;
            end if;
            Exit_Info.Reason := NVMM_EXIT_NONE;

         when Arch.Virtualization.SVM.VMEXIT_MONITOR =>
            --  MONITOR: Report to userspace
            Exit_Info.Reason := NVMM_EXIT_MONITOR;
            Exit_Info.U.Insn.Next_RIP := VMCB_Ptr.Control.Next_RIP;

         when Arch.Virtualization.SVM.VMEXIT_MWAIT =>
            --  MWAIT: Report to userspace (may want to halt)
            Exit_Info.Reason := NVMM_EXIT_MWAIT;
            Exit_Info.U.Insn.Next_RIP := VMCB_Ptr.Control.Next_RIP;

         when Arch.Virtualization.SVM.VMEXIT_XSETBV =>
            --  XSETBV: XCR write - emulate XCR0 internally
            declare
               XCR_Num : constant Unsigned_32 := Unsigned_32
                  (Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX
                               and 16#FFFF_FFFF#);
               XCR_Val : constant Unsigned_64 := (Shift_Left
                  (Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RDX
                               and 16#FFFF_FFFF#, 32)) or
                  (VMCB_Ptr.State_Save.RAX and 16#FFFF_FFFF#);
               XCR0_Valid : Boolean;
            begin
               if XCR_Num = 0 then
                  --  XCR0 write - validate according to Intel SDM rules:
                  --  1. Bit 0 (x87) must always be set
                  --  2. If SSE (bit 1) is clear, AVX (bit 2) must also be 0
                  --  3. Value must not exceed host's supported XCR0
                  XCR0_Valid := (XCR_Val and 1) /= 0 and then  --  x87 required
                     ((XCR_Val and 2) /= 0 or (XCR_Val and 4) = 0) and then
                     (XCR_Val and not Host_XCR0_Max) = 0;  --  subset of host

                  if XCR0_Valid then
                     --  Store XCR0 value for use in XSAVE/XRSTOR
                     Machines (Positive (Mach)).VCPUs (CPU).XCR0_Value :=
                        XCR_Val;
                     if VMCB_Ptr.Control.Next_RIP /= 0 then
                        VMCB_Ptr.State_Save.RIP := VMCB_Ptr.Control.Next_RIP;
                     else
                        --  Fallback: XSETBV is 3 bytes (0F 01 D1)
                        VMCB_Ptr.State_Save.RIP :=
                           VMCB_Ptr.State_Save.RIP + 3;
                     end if;
                     Exit_Info.Reason := NVMM_EXIT_NONE;
                  else
                     --  Invalid XCR0 value - report as INVALID exit
                     Exit_Info.Reason := NVMM_EXIT_INVALID;
                     Exit_Info.U.Invalid.HW_Code := Exit_Code;
                  end if;
               else
                  --  Unknown XCR - report as INVALID exit
                  Exit_Info.Reason := NVMM_EXIT_INVALID;
                  Exit_Info.U.Invalid.HW_Code := Exit_Code;
               end if;
            end;

         when Arch.Virtualization.SVM.VMEXIT_VMMCALL =>
            --  VMMCALL: Hypercall - report as INVALID (no standard handling)
            Exit_Info.Reason := NVMM_EXIT_INVALID;
            Exit_Info.U.Invalid.HW_Code := Exit_Code;

         when Arch.Virtualization.SVM.VMEXIT_RDTSCP =>
            --  RDTSCP: Emulate by reading host TSC + offset and processor ID
            declare
               TSC_Val : constant Unsigned_64 :=
                  Arch.Snippets.Read_TSC +
                  Machines (Positive (Mach)).VCPUs (CPU).TSC_Offset_Val;
            begin
               --  RAX = low 32 bits, RDX = high 32 bits
               VMCB_Ptr.State_Save.RAX := TSC_Val and 16#FFFF_FFFF#;
               Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RDX :=
                  Shift_Right (TSC_Val, 32) and 16#FFFF_FFFF#;
               --  RCX = processor ID (IA32_TSC_AUX MSR, use VCPU ID)
               Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs.RCX :=
                  Unsigned_64 (CPU);
            end;
            if VMCB_Ptr.Control.Next_RIP /= 0 then
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.Control.Next_RIP;
            else
               VMCB_Ptr.State_Save.RIP := VMCB_Ptr.State_Save.RIP + 3;
            end if;
            Exit_Info.Reason := NVMM_EXIT_NONE;

         when Unsigned_64'Last =>  --  VMEXIT_INVALID (-1)
            Exit_Info.Reason := NVMM_EXIT_INVALID;
            Exit_Info.U.Invalid.HW_Code := Exit_Code;

         when others =>
            --  Unknown/unhandled exit
            Exit_Info.Reason := NVMM_EXIT_INVALID;
            Exit_Info.U.Invalid.HW_Code := Exit_Code;
      end case;

      Synchronization.Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Run_Ex;

   function GPA_To_HVA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       HVA  : out Unsigned_64) return Boolean
   is
      type U64_Array is array (Natural range <>) of Unsigned_64;
      NPT_Addr   : Integer_Address;
      PML4_Idx   : Natural;
      PDPT_Idx   : Natural;
      PD_Idx     : Natural;
      PT_Idx     : Natural;
      Offset     : Unsigned_64;
      PML4E      : Unsigned_64;
      PDPTE      : Unsigned_64;
      PDE        : Unsigned_64;
      PTE        : Unsigned_64;
      Page_Base  : Unsigned_64;
   begin
      HVA := 0;

      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;
      if not Machines (Positive (Mach)).Active then
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         return False;
      end if;

      NPT_Addr := Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;
      if NPT_Addr = 0 then
         return False;
      end if;

      --  Extract page table indices from GPA
      PML4_Idx := Natural (Shift_Right (GPA, 39) and 16#1FF#);
      PDPT_Idx := Natural (Shift_Right (GPA, 30) and 16#1FF#);
      PD_Idx   := Natural (Shift_Right (GPA, 21) and 16#1FF#);
      PT_Idx   := Natural (Shift_Right (GPA, 12) and 16#1FF#);
      Offset   := GPA and 16#FFF#;

      --  Read PML4 entry
      declare
         PML4 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr);
      begin
         PML4E := PML4 (PML4_Idx);
         if (PML4E and 1) = 0 then
            return False;  --  Not present
         end if;
      end;

      --  Read PDPT entry (page 1 of NPT allocation)
      declare
         PDPT : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 4096);
      begin
         PDPTE := PDPT (PDPT_Idx);
         if (PDPTE and 1) = 0 then
            return False;  --  Not present
         end if;
         --  Check for 1GB page (PS bit = 0x80)
         if (PDPTE and 16#80#) /= 0 then
            Page_Base := PDPTE and 16#FFFFFC0000000#;  --  1GB aligned
            HVA := Page_Base + (GPA and 16#3FFF_FFFF#) +
                   Unsigned_64 (Arch.MMU.Memory_Offset);
            return True;
         end if;
      end;

      --  Read PD entry (pages 2-5 of NPT allocation, one per GB)
      --  PD0 is at offset 8192, PD1 at 12288, etc.
      declare
         PD_Offset : constant Integer_Address :=
            Integer_Address (8192 + PDPT_Idx * 4096);
         PD : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + PD_Offset);
      begin
         PDE := PD (PD_Idx);
         if (PDE and 1) = 0 then
            return False;  --  Not present
         end if;
         --  Check for 2MB page (PS bit = 0x80)
         if (PDE and 16#80#) /= 0 then
            Page_Base := PDE and 16#FFFFFFFE00000#;  --  2MB aligned
            HVA := Page_Base + (GPA and 16#1F_FFFF#) +
                   Unsigned_64 (Arch.MMU.Memory_Offset);
            return True;
         end if;
      end;

      --  Read PT entry (page 6 of NPT allocation, PT0 for first 2MB)
      --  Note: NPT only has PT0 for first 2MB, using 2MB pages elsewhere
      if PDPT_Idx = 0 and PD_Idx = 0 then
         declare
            PT0 : U64_Array (0 .. 511)
               with Import, Address => To_Address (NPT_Addr + 24576);
         begin
            PTE := PT0 (PT_Idx);
            if (PTE and 1) = 0 then
               return False;  --  Not present
            end if;
            Page_Base := PTE and 16#FFFFFFFFFF000#;  --  4KB aligned
            HVA := Page_Base + Offset +
                   Unsigned_64 (Arch.MMU.Memory_Offset);
            return True;
         end;
      else
         --  No PT for this region (should have been handled by 2MB page above)
         return False;
      end if;
   exception
      when Constraint_Error =>
         return False;
   end GPA_To_HVA;

   function GVA_To_GPA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GVA  : Unsigned_64;
       GPA  : out Unsigned_64) return Boolean
   is
      VMCB_Ptr  : Arch.Virtualization.SVM.VMCB_Acc;
      CR0       : Unsigned_64;
      CR3       : Unsigned_64;
      CR4       : Unsigned_64;
      EFER      : Unsigned_64;
   begin
      GPA := 0;

      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;
      if not Machines (Positive (Mach)).Active then
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         return False;
      end if;

      --  VMX backend - read CRs from VMCS
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               return False;
            end if;

            CR0  := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR0);
            CR3  := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR3);
            CR4  := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR4);
            EFER := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER);
         end;
      else
         --  SVM backend - get VMCB pointer
         declare
            VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
               with Import, Address => To_Address
                  (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
         begin
            VMCB_Ptr := VMCB_Obj'Unchecked_Access;
         end;

         CR0  := VMCB_Ptr.State_Save.CR0;
         CR3  := VMCB_Ptr.State_Save.CR3;
         CR4  := VMCB_Ptr.State_Save.CR4;
         EFER := VMCB_Ptr.State_Save.EFER;
      end if;

      --  Check if paging is enabled (CR0.PG bit 31)
      if (CR0 and 16#8000_0000#) = 0 then
         --  No paging: GVA = GPA
         GPA := GVA;
         return True;
      end if;

      --  Check for 64-bit long mode (EFER.LMA bit 10)
      if (EFER and 16#400#) /= 0 then
         --  Check for 5-level paging (CR4.LA57 bit 12)
         if (CR4 and 16#1000#) /= 0 then
            --  5-level paging (57-bit virtual addresses)
            declare
               PML5_Base : constant Unsigned_64 := CR3 and 16#FFFFFFFFFF000#;
               PML5_Idx  : constant Unsigned_64 :=
                  Shift_Right (GVA, 48) and 16#1FF#;
               PML4_Idx  : constant Unsigned_64 :=
                  Shift_Right (GVA, 39) and 16#1FF#;
               PDPT_Idx  : constant Unsigned_64 :=
                  Shift_Right (GVA, 30) and 16#1FF#;
               PD_Idx    : constant Unsigned_64 :=
                  Shift_Right (GVA, 21) and 16#1FF#;
               PT_Idx    : constant Unsigned_64 :=
                  Shift_Right (GVA, 12) and 16#1FF#;
               Offset    : constant Unsigned_64 := GVA and 16#FFF#;

               PML5E, PML4E, PDPTE, PDE, PTE : Unsigned_64;
               HVA                           : Unsigned_64;
               Entry_Ptr                     : System.Address;
            begin
               --  Read PML5 entry
               if not GPA_To_HVA
                  (Mach, CPU, PML5_Base + PML5_Idx * 8, HVA)
               then
                  return False;
               end if;
               Entry_Ptr := To_Address (Integer_Address (HVA));
               declare
                  Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
               begin
                  PML5E := Entry_Val;
               end;
               if (PML5E and 1) = 0 then return False; end if;

               --  Read PML4 entry
               if not GPA_To_HVA
                  (Mach, CPU,
                   (PML5E and 16#FFFFFFFFFF000#) + PML4_Idx * 8, HVA)
               then
                  return False;
               end if;
               Entry_Ptr := To_Address (Integer_Address (HVA));
               declare
                  Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
               begin
                  PML4E := Entry_Val;
               end;
               if (PML4E and 1) = 0 then return False; end if;

               --  Read PDPT entry
               if not GPA_To_HVA
                  (Mach, CPU,
                   (PML4E and 16#FFFFFFFFFF000#) + PDPT_Idx * 8, HVA)
               then
                  return False;
               end if;
               Entry_Ptr := To_Address (Integer_Address (HVA));
               declare
                  Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
               begin
                  PDPTE := Entry_Val;
               end;
               if (PDPTE and 1) = 0 then return False; end if;
               if (PDPTE and 16#80#) /= 0 then  --  1GB page
                  GPA := (PDPTE and 16#FFFFFC0000000#) or
                         (GVA and 16#3FFFFFFF#);
                  return True;
               end if;

               --  Read PD entry
               if not GPA_To_HVA
                  (Mach, CPU,
                   (PDPTE and 16#FFFFFFFFFF000#) + PD_Idx * 8, HVA)
               then
                  return False;
               end if;
               Entry_Ptr := To_Address (Integer_Address (HVA));
               declare
                  Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
               begin
                  PDE := Entry_Val;
               end;
               if (PDE and 1) = 0 then return False; end if;
               if (PDE and 16#80#) /= 0 then  --  2MB page
                  GPA := (PDE and 16#FFFFFFFE00000#) or (GVA and 16#1FFFFF#);
                  return True;
               end if;

               --  Read PT entry
               if not GPA_To_HVA
                  (Mach, CPU,
                   (PDE and 16#FFFFFFFFFF000#) + PT_Idx * 8, HVA)
               then
                  return False;
               end if;
               Entry_Ptr := To_Address (Integer_Address (HVA));
               declare
                  Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
               begin
                  PTE := Entry_Val;
               end;
               if (PTE and 1) = 0 then return False; end if;

               GPA := (PTE and 16#FFFFFFFFFF000#) or Offset;
               return True;
            end;
         end if;

         --  4-level paging (64-bit mode, standard)
         declare
            PML4_Base : constant Unsigned_64 := CR3 and 16#FFFFFFFFFF000#;
            PML4_Idx  : constant Unsigned_64 :=
               Shift_Right (GVA, 39) and 16#1FF#;
            PDPT_Idx  : constant Unsigned_64 :=
               Shift_Right (GVA, 30) and 16#1FF#;
            PD_Idx    : constant Unsigned_64 :=
               Shift_Right (GVA, 21) and 16#1FF#;
            PT_Idx    : constant Unsigned_64 :=
               Shift_Right (GVA, 12) and 16#1FF#;
            Offset    : constant Unsigned_64 := GVA and 16#FFF#;

            PML4E, PDPTE, PDE, PTE : Unsigned_64;
            HVA                    : Unsigned_64;
            Entry_Ptr              : System.Address;
         begin
            --  Read PML4 entry
            if not GPA_To_HVA (Mach, CPU, PML4_Base + PML4_Idx * 8, HVA) then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
            begin
               PML4E := Entry_Val;
            end;
            if (PML4E and 1) = 0 then return False; end if;  --  Not present

            --  Read PDPT entry
            if not GPA_To_HVA
               (Mach, CPU,
                (PML4E and 16#FFFFFFFFFF000#) + PDPT_Idx * 8, HVA)
            then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
            begin
               PDPTE := Entry_Val;
            end;
            if (PDPTE and 1) = 0 then return False; end if;
            if (PDPTE and 16#80#) /= 0 then  --  1GB page
               GPA := (PDPTE and 16#FFFFFC0000000#) or (GVA and 16#3FFFFFFF#);
               return True;
            end if;

            --  Read PD entry
            if not GPA_To_HVA
               (Mach, CPU,
                (PDPTE and 16#FFFFFFFFFF000#) + PD_Idx * 8, HVA)
            then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
            begin
               PDE := Entry_Val;
            end;
            if (PDE and 1) = 0 then return False; end if;
            if (PDE and 16#80#) /= 0 then  --  2MB page
               GPA := (PDE and 16#FFFFFFFE00000#) or (GVA and 16#1FFFFF#);
               return True;
            end if;

            --  Read PT entry
            if not GPA_To_HVA
               (Mach, CPU,
                (PDE and 16#FFFFFFFFFF000#) + PT_Idx * 8, HVA)
            then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
            begin
               PTE := Entry_Val;
            end;
            if (PTE and 1) = 0 then return False; end if;

            GPA := (PTE and 16#FFFFFFFFFF000#) or Offset;
            return True;
         end;
      elsif (CR4 and 16#20#) /= 0 then
         --  PAE paging (32-bit with PAE)
         --  PDPT has 4 entries (bits 31-30), PD has 512 (bits 29-21),
         --  PT has 512 (bits 20-12), offset is bits 11-0
         declare
            PDPT_Base : constant Unsigned_64 := CR3 and 16#FFFFFFE0#;
            PDPT_Idx  : constant Unsigned_64 :=
               Shift_Right (GVA, 30) and 16#3#;
            PD_Idx    : constant Unsigned_64 :=
               Shift_Right (GVA, 21) and 16#1FF#;
            PT_Idx    : constant Unsigned_64 :=
               Shift_Right (GVA, 12) and 16#1FF#;
            Offset    : constant Unsigned_64 := GVA and 16#FFF#;

            PDPTE, PDE, PTE : Unsigned_64;
            HVA             : Unsigned_64;
            Entry_Ptr       : System.Address;
         begin
            --  Read PDPT entry (8 bytes each)
            if not GPA_To_HVA (Mach, CPU, PDPT_Base + PDPT_Idx * 8, HVA) then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
            begin
               PDPTE := Entry_Val;
            end;
            if (PDPTE and 1) = 0 then return False; end if;  --  Not present

            --  Read PD entry (8 bytes each)
            if not GPA_To_HVA
               (Mach, CPU,
                (PDPTE and 16#FFFFFFFFFF000#) + PD_Idx * 8, HVA)
            then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
            begin
               PDE := Entry_Val;
            end;
            if (PDE and 1) = 0 then return False; end if;
            if (PDE and 16#80#) /= 0 then  --  2MB page (PS bit)
               GPA := (PDE and 16#FFFFFFFE00000#) or (GVA and 16#1FFFFF#);
               return True;
            end if;

            --  Read PT entry (8 bytes each)
            if not GPA_To_HVA
               (Mach, CPU,
                (PDE and 16#FFFFFFFFFF000#) + PT_Idx * 8, HVA)
            then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_64 with Import, Address => Entry_Ptr;
            begin
               PTE := Entry_Val;
            end;
            if (PTE and 1) = 0 then return False; end if;

            GPA := (PTE and 16#FFFFFFFFFF000#) or Offset;
            return True;
         end;
      else
         --  32-bit paging (legacy, no PAE)
         --  PD has 1024 entries (bits 31-22), PT has 1024 (bits 21-12),
         --  offset is bits 11-0. Entries are 4 bytes each.
         declare
            PD_Base : constant Unsigned_64 := CR3 and 16#FFFFF000#;
            PD_Idx  : constant Unsigned_64 :=
               Shift_Right (GVA, 22) and 16#3FF#;
            PT_Idx  : constant Unsigned_64 :=
               Shift_Right (GVA, 12) and 16#3FF#;
            Offset  : constant Unsigned_64 := GVA and 16#FFF#;

            PDE, PTE : Unsigned_32;
            HVA      : Unsigned_64;
            Entry_Ptr : System.Address;
         begin
            --  Read PD entry (4 bytes each)
            if not GPA_To_HVA (Mach, CPU, PD_Base + PD_Idx * 4, HVA) then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_32 with Import, Address => Entry_Ptr;
            begin
               PDE := Entry_Val;
            end;
            if (PDE and 1) = 0 then return False; end if;  --  Not present
            --  Check for 4MB page (PS bit, requires CR4.PSE)
            if (PDE and 16#80#) /= 0 and (CR4 and 16#10#) /= 0 then
               --  4MB page: bits 31-22 of PDE are bits 31-22 of phys addr
               GPA := Unsigned_64 (PDE and 16#FFC00000#) or
                      (GVA and 16#3FFFFF#);
               return True;
            end if;

            --  Read PT entry (4 bytes each)
            if not GPA_To_HVA
               (Mach, CPU,
                Unsigned_64 (PDE and 16#FFFFF000#) + PT_Idx * 4, HVA)
            then
               return False;
            end if;
            Entry_Ptr := To_Address (Integer_Address (HVA));
            declare
               Entry_Val : Unsigned_32 with Import, Address => Entry_Ptr;
            begin
               PTE := Entry_Val;
            end;
            if (PTE and 1) = 0 then return False; end if;

            GPA := Unsigned_64 (PTE and 16#FFFFF000#) or Offset;
            return True;
         end;
      end if;
   exception
      when Constraint_Error =>
         return False;
   end GVA_To_GPA;

   function VCPU_Stop
      (Mach : Machine_ID;
       CPU  : VCPU_ID) return Boolean
   is
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;
      if not Machines (Positive (Mach)).Active then
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         return False;
      end if;

      Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := True;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Stop;

   function VCPU_Set_Mode
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Mode : Unsigned_32) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK  : Boolean;
            Dummy_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            case Mode is
               when VCPU_MODE_64BIT =>
                  --  64-bit Long Mode
                  --  CS: L=1 (bit 13 in VMX AR), D=0
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR,
                      16#209B#, Dummy_OK);  --  L=1, D=0
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR,
                      16#0093#, Dummy_OK);
                  --  EFER: LME (bit 8) + LMA (bit 10)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER,
                      16#500#, Dummy_OK);
                  --  CR0: PG + PE + ET + NE (apply fixed bits)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR0,
                      Arch.Virtualization.VMX.Apply_CR0_Fixed
                         (16#8001_0031#, False), Dummy_OK);
                  --  CR4: PAE (bit 5) required for long mode (apply fixed)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR4,
                      Arch.Virtualization.VMX.Apply_CR4_Fixed (16#20#),
                      Dummy_OK);
                  --  Set VM-entry control IA-32e mode guest bit
                  declare
                     Entry_Ctls : Unsigned_64 :=
                        Arch.Virtualization.VMX.VMX_Read
                           (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS);
                  begin
                     Entry_Ctls := Entry_Ctls or
                        Arch.Virtualization.VMX.ENTRY_IA32E_MODE_GUEST;
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS,
                         Entry_Ctls, Dummy_OK);
                  end;

               when VCPU_MODE_REAL =>
                  --  16-bit Real Mode
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR,
                      16#009B#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR,
                      16#0093#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR,
                      16#0093#, Dummy_OK);
                  --  Set 64KB limits
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  --  EFER: 0 (no LME)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER,
                      0, Dummy_OK);
                  --  CR0: ET only, no PE (unrestricted guest)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR0,
                      Arch.Virtualization.VMX.Apply_CR0_Fixed
                         (16#10#, True), Dummy_OK);
                  --  CR4: apply fixed bits
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR4,
                      Arch.Virtualization.VMX.Apply_CR4_Fixed (0), Dummy_OK);
                  --  Clear IA-32e mode guest bit in entry controls
                  declare
                     Entry_Ctls : Unsigned_64 :=
                        Arch.Virtualization.VMX.VMX_Read
                           (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS);
                  begin
                     Entry_Ctls := Entry_Ctls and
                        not Arch.Virtualization.VMX.ENTRY_IA32E_MODE_GUEST;
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS,
                         Entry_Ctls, Dummy_OK);
                  end;

               when VCPU_MODE_VM86 =>
                  --  Virtual 8086 Mode
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR,
                      16#00F3#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR,
                      16#00F3#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR,
                      16#00F3#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR,
                      16#00F3#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR,
                      16#00F3#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR,
                      16#00F3#, Dummy_OK);
                  --  Set 64KB limits
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT,
                      16#FFFF#, Dummy_OK);
                  --  EFER: 0
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER,
                      0, Dummy_OK);
                  --  CR0: PE + ET (apply fixed bits, unrestricted)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR0,
                      Arch.Virtualization.VMX.Apply_CR0_Fixed
                         (16#11#, True), Dummy_OK);
                  --  CR4: apply fixed bits
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR4,
                      Arch.Virtualization.VMX.Apply_CR4_Fixed (0), Dummy_OK);
                  --  RFLAGS: Set VM bit (bit 17) and IOPL=3
                  declare
                     Cur_Rflags : Unsigned_64 :=
                        Arch.Virtualization.VMX.VMX_Read
                           (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS);
                  begin
                     Cur_Rflags := Cur_Rflags or 16#23002#;
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS,
                         Cur_Rflags, Dummy_OK);
                  end;
                  --  Clear IA-32e mode guest bit
                  declare
                     Entry_Ctls : Unsigned_64 :=
                        Arch.Virtualization.VMX.VMX_Read
                           (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS);
                  begin
                     Entry_Ctls := Entry_Ctls and
                        not Arch.Virtualization.VMX.ENTRY_IA32E_MODE_GUEST;
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS,
                         Entry_Ctls, Dummy_OK);
                  end;

               when others =>
                  --  32-bit Protected Mode (default)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR,
                      16#0C9B#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR,
                      16#0C93#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR,
                      16#0C93#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR,
                      16#0C93#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR,
                      16#0C93#, Dummy_OK);
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR,
                      16#0C93#, Dummy_OK);
                  --  EFER: 0
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER,
                      0, Dummy_OK);
                  --  CR0: PE + ET (apply fixed bits, unrestricted)
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR0,
                      Arch.Virtualization.VMX.Apply_CR0_Fixed
                         (16#11#, True), Dummy_OK);
                  --  CR4: apply fixed bits
                  Arch.Virtualization.VMX.VMX_Write
                     (Arch.Virtualization.VMX.VMCS_GUEST_CR4,
                      Arch.Virtualization.VMX.Apply_CR4_Fixed (0), Dummy_OK);
                  --  Clear IA-32e mode guest bit
                  declare
                     Entry_Ctls : Unsigned_64 :=
                        Arch.Virtualization.VMX.VMX_Read
                           (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS);
                  begin
                     Entry_Ctls := Entry_Ctls and
                        not Arch.Virtualization.VMX.ENTRY_IA32E_MODE_GUEST;
                     Arch.Virtualization.VMX.VMX_Write
                        (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS,
                         Entry_Ctls, Dummy_OK);
                  end;
            end case;
         end;

         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend - get VMCB pointer
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      case Mode is
         when VCPU_MODE_64BIT =>
            --  64-bit Long Mode
            --  CS: L=1 (bit 9), D/B=0 (bit 10), rest same as 32-bit
            --  Attrib: 0-3=Type,4=S,5-6=DPL,7=P,8=AVL,9=L,10=D/B,11=G
            VMCB_Ptr.State_Save.CS.Attrib := 16#0A9B#;  --  L=1, D/B=0
            VMCB_Ptr.State_Save.DS.Attrib := 16#0A93#;
            VMCB_Ptr.State_Save.ES.Attrib := 16#0A93#;
            VMCB_Ptr.State_Save.SS.Attrib := 16#0A93#;
            VMCB_Ptr.State_Save.FS.Attrib := 16#0A93#;
            VMCB_Ptr.State_Save.GS.Attrib := 16#0A93#;
            --  EFER: SVME (bit 12) + LME (bit 8) + LMA (bit 10)
            VMCB_Ptr.State_Save.EFER := 16#1500#;
            --  CR0: PG (bit 31) + PE (bit 0) + ET (bit 4) + WP (bit 16)
            VMCB_Ptr.State_Save.CR0 := 16#8001_0031#;
            --  CR4: PAE (bit 5) required for long mode
            VMCB_Ptr.State_Save.CR4 := 16#20#;
            --  Note: CR3 must be set by caller with valid page tables

         when VCPU_MODE_REAL =>
            --  16-bit Real Mode
            --  Segment attributes: 16-bit, present, DPL=0
            --  Type=3 for data (r/w), Type=B for code (exec/read)
            --  No granularity (G=0), no D/B (16-bit operands)
            VMCB_Ptr.State_Save.CS.Attrib := 16#009B#;  --  16-bit code
            VMCB_Ptr.State_Save.DS.Attrib := 16#0093#;  --  16-bit data
            VMCB_Ptr.State_Save.ES.Attrib := 16#0093#;
            VMCB_Ptr.State_Save.SS.Attrib := 16#0093#;
            VMCB_Ptr.State_Save.FS.Attrib := 16#0093#;
            VMCB_Ptr.State_Save.GS.Attrib := 16#0093#;
            --  Set 64KB limits for all segments
            VMCB_Ptr.State_Save.CS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.DS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.ES.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.SS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.FS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.GS.Limit := 16#FFFF#;
            --  EFER: SVME only (no LME/LMA)
            VMCB_Ptr.State_Save.EFER := 16#1000#;
            --  CR0: ET only, no PE (real mode), no PG
            VMCB_Ptr.State_Save.CR0 := 16#10#;
            --  CR4: 0
            VMCB_Ptr.State_Save.CR4 := 0;

         when VCPU_MODE_VM86 =>
            --  Virtual 8086 Mode
            --  Similar to real mode but runs in protected mode with VM flag
            --  Segment attributes: 16-bit, present, DPL=3 (user mode)
            VMCB_Ptr.State_Save.CS.Attrib := 16#00F3#;  --  16-bit, DPL=3
            VMCB_Ptr.State_Save.DS.Attrib := 16#00F3#;
            VMCB_Ptr.State_Save.ES.Attrib := 16#00F3#;
            VMCB_Ptr.State_Save.SS.Attrib := 16#00F3#;
            VMCB_Ptr.State_Save.FS.Attrib := 16#00F3#;
            VMCB_Ptr.State_Save.GS.Attrib := 16#00F3#;
            --  Set 64KB limits for all segments
            VMCB_Ptr.State_Save.CS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.DS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.ES.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.SS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.FS.Limit := 16#FFFF#;
            VMCB_Ptr.State_Save.GS.Limit := 16#FFFF#;
            --  EFER: SVME only
            VMCB_Ptr.State_Save.EFER := 16#1000#;
            --  CR0: PE + ET (protected mode, but VM86)
            VMCB_Ptr.State_Save.CR0 := 16#11#;
            --  CR4: 0
            VMCB_Ptr.State_Save.CR4 := 0;
            --  RFLAGS: Set VM bit (bit 17) and IOPL=3 (bits 12-13)
            --  IOPL=3 allows I/O in VM86 mode without trapping
            VMCB_Ptr.State_Save.RFLAGS :=
               VMCB_Ptr.State_Save.RFLAGS or 16#23002#;  --  VM + IOPL=3 + bit1
            --  CPL must be 3 for VM86 mode
            VMCB_Ptr.State_Save.CPL := 3;

         when others =>
            --  32-bit Protected Mode (default)
            VMCB_Ptr.State_Save.CS.Attrib := 16#0C9B#;  --  D/B=1, G=1
            VMCB_Ptr.State_Save.DS.Attrib := 16#0C93#;
            VMCB_Ptr.State_Save.ES.Attrib := 16#0C93#;
            VMCB_Ptr.State_Save.SS.Attrib := 16#0C93#;
            VMCB_Ptr.State_Save.FS.Attrib := 16#0C93#;
            VMCB_Ptr.State_Save.GS.Attrib := 16#0C93#;
            --  EFER: SVME only
            VMCB_Ptr.State_Save.EFER := 16#1000#;
            --  CR0: PE + ET (no paging)
            VMCB_Ptr.State_Save.CR0 := 16#11#;
            --  CR4: 0 (no PAE needed without paging)
            VMCB_Ptr.State_Save.CR4 := 0;
      end case;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Set_Mode;

   function Set_IO_Port_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Port      : Unsigned_16;
       Intercept : Boolean) return Boolean
   is
      IOPM_Addr : Integer_Address;
      Byte_Idx  : Natural;
      Bit_Idx   : Natural;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      IOPM_Addr := Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr;
      if IOPM_Addr = 0 then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  IOPM is a 12KB bitmap: each bit corresponds to a port
      --  Bit N corresponds to port N: 0 = passthrough, 1 = intercept
      Byte_Idx := Natural (Port) / 8;
      Bit_Idx := Natural (Port) mod 8;

      declare
         type Byte_Array is array (0 .. 12287) of Unsigned_8;
         IOPM : Byte_Array with Import, Address => To_Address (IOPM_Addr);
         Mask : constant Unsigned_8 := Shift_Left (Unsigned_8'(1), Bit_Idx);
      begin
         if Intercept then
            IOPM (Byte_Idx) := IOPM (Byte_Idx) or Mask;
         else
            IOPM (Byte_Idx) := IOPM (Byte_Idx) and not Mask;
         end if;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Set_IO_Port_Intercept;

   function Set_MSR_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       MSR_Num   : Unsigned_32;
       Intercept : Boolean;
       Is_Write  : Boolean) return Boolean
   is
      MSRPM_Addr : Integer_Address;
      Base_Offset : Natural;
      Byte_Offset : Natural;
      Bit_Idx    : Natural;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      MSRPM_Addr := Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr;
      if MSRPM_Addr = 0 then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  MSRPM is 8KB (2 pages), organized as:
      --    0x0000-0x07FF: MSRs 0x00000000-0x00001FFF (8192 MSRs, 2 bits each)
      --    0x0800-0x0FFF: MSRs 0xC0000000-0xC0001FFF (8192 MSRs, 2 bits each)
      --    0x1000-0x17FF: MSRs 0xC0010000-0xC0011FFF (8192 MSRs, 2 bits each)
      --  Each MSR has 2 bits: bit 0 = read intercept, bit 1 = write intercept

      --  Determine which 2KB region based on MSR range
      if MSR_Num <= 16#1FFF# then
         Base_Offset := 0;
         Byte_Offset := Natural (MSR_Num) / 4;  --  4 MSRs per byte.
         Bit_Idx := (Natural (MSR_Num) mod 4) * 2;  --  2 bits per MSR
      elsif MSR_Num >= 16#C000_0000# and MSR_Num <= 16#C000_1FFF# then
         Base_Offset := 16#800#;
         Byte_Offset := Natural (MSR_Num - 16#C000_0000#) / 4;
         Bit_Idx := (Natural (MSR_Num - 16#C000_0000#) mod 4) * 2;
      elsif MSR_Num >= 16#C001_0000# and MSR_Num <= 16#C001_1FFF# then
         Base_Offset := 16#1000#;
         Byte_Offset := Natural (MSR_Num - 16#C001_0000#) / 4;
         Bit_Idx := (Natural (MSR_Num - 16#C001_0000#) mod 4) * 2;
      else
         --  MSR not in permission map - always intercepted
         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  Bit 0 = read intercept, Bit 1 = write intercept
      if Is_Write then
         Bit_Idx := Bit_Idx + 1;
      end if;

      declare
         type Byte_Array is array (0 .. 8191) of Unsigned_8;
         MSRPM : Byte_Array with Import, Address => To_Address (MSRPM_Addr);
         Offset : constant Natural := Base_Offset + Byte_Offset;
         Mask : constant Unsigned_8 := Shift_Left (Unsigned_8'(1), Bit_Idx);
      begin
         if Offset <= 8191 then
            if Intercept then
               MSRPM (Offset) := MSRPM (Offset) or Mask;
            else
               MSRPM (Offset) := MSRPM (Offset) and not Mask;
            end if;
         end if;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Set_MSR_Intercept;

   function Set_TLB_Control
      (Mach    : Machine_ID;
       CPU     : VCPU_ID;
       Control : Unsigned_32) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend: TLB control is SVM-specific, no-op for VMX
      if Current_Backend = Backend_VMX then
         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend - get VMCB pointer
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Set TLB control value
      VMCB_Ptr.Control.TLB_Control := Control;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Set_TLB_Control;

   function Set_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : Unsigned_64) return Boolean
   is
      VMCB_Ptr : Arch.Virtualization.SVM.VMCB_Acc;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  VMX backend - write to VMCS
      if Current_Backend = Backend_VMX then
         declare
            VMCS_Phys : constant Unsigned_64 :=
               Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;
            Load_OK  : Boolean;
            Dummy_OK : Boolean;
         begin
            Arch.Virtualization.VMX.VMPTRLD (VMCS_Phys, Load_OK);
            if not Load_OK then
               Release (Machines (Positive (Mach)).Lock);
               return False;
            end if;

            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_TSC_OFFSET, Offset, Dummy_OK);
            Machines (Positive (Mach)).VCPUs (CPU).TSC_Offset_Val := Offset;
         end;

         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  SVM backend - get VMCB pointer
      declare
         VMCB_Obj : aliased Arch.Virtualization.SVM.VMCB
            with Import, Address => To_Address
               (Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr);
      begin
         VMCB_Ptr := VMCB_Obj'Unchecked_Access;
      end;

      --  Set TSC offset in VMCB
      VMCB_Ptr.Control.TSC_Offset := Offset;
      --  Also store in our local state
      Machines (Positive (Mach)).VCPUs (CPU).TSC_Offset_Val := Offset;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Set_TSC_Offset;

   function Get_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : out Unsigned_64) return Boolean
   is
   begin
      Offset := 0;

      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Return stored TSC offset
      Offset := Machines (Positive (Mach)).VCPUs (CPU).TSC_Offset_Val;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Get_TSC_Offset;

   function GPA_Map_1GB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      type U64_Array is array (Natural range <>) of Unsigned_64;
      NPT_Addr  : Integer_Address;
      HPA       : Unsigned_64;
      Flags     : Unsigned_64;
      PDPT_Idx  : Natural;
      Page_1GB  : constant Unsigned_64 := 16#4000_0000#;  --  1GB
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      NPT_Addr := Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;

      --  Validate 1GB alignment
      if (GPA and (Page_1GB - 1)) /= 0 or
         (HVA and (Page_1GB - 1)) /= 0
      then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Only support first 4GB (PDPT indices 0-3)
      if GPA >= 16#1_0000_0000# then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Convert HVA to physical address
      HPA := Unsigned_64 (Integer_Address (HVA) - Arch.MMU.Memory_Offset);

      --  Build 1GB page flags:
      --  Present(0), Writable(1), User(2), PS(7)=1GB page
      Flags := 16#81#;  --  Present + PS
      if (Prot and GPA_PROT_WRITE) /= 0 then
         Flags := Flags or 16#02#;  --  Writable
      end if;
      Flags := Flags or 16#04#;  --  User (always set for guest access)

      --  Get PDPT index
      PDPT_Idx := Natural (GPA / Page_1GB);

      declare
         PDPT : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 4096);
      begin
         --  Set PDPT entry as a 1GB page (PS bit set, no PD pointer)
         --  HPA must be 1GB-aligned (bits 29:0 are offset)
         PDPT (PDPT_Idx) := (HPA and 16#FFFFFC0000000#) or Flags;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end GPA_Map_1GB;

   function GPA_Map_2MB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      type U64_Array is array (Natural range <>) of Unsigned_64;
      NPT_Addr  : Integer_Address;
      HPA       : Unsigned_64;
      Flags     : Unsigned_64;
      PDPT_Idx  : Natural;
      PD_Idx    : Natural;
      Page_2MB  : constant Unsigned_64 := 16#20_0000#;  --  2MB
      Page_1GB  : constant Unsigned_64 := 16#4000_0000#;  --  1GB
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      Seize (Machines (Positive (Mach)).Lock);

      if not Machines (Positive (Mach)).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;
      if not Machines (Positive (Mach)).VCPUs (CPU).Active then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      NPT_Addr := Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr;

      --  Validate 2MB alignment
      if (GPA and (Page_2MB - 1)) /= 0 or (HVA and (Page_2MB - 1)) /= 0 then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Only support first 4GB
      if GPA >= 16#1_0000_0000# then
         Release (Machines (Positive (Mach)).Lock);
         return False;
      end if;

      --  Convert HVA to physical address
      HPA := Unsigned_64 (Integer_Address (HVA) - Arch.MMU.Memory_Offset);

      --  Build 2MB page flags:
      --  Present(0), Writable(1), User(2), PS(7)=2MB page
      Flags := 16#81#;  --  Present + PS
      if (Prot and GPA_PROT_WRITE) /= 0 then
         Flags := Flags or 16#02#;  --  Writable
      end if;
      Flags := Flags or 16#04#;  --  User (always set for guest access)

      --  Get PDPT and PD indices
      PDPT_Idx := Natural (GPA / Page_1GB);
      PD_Idx := Natural ((GPA mod Page_1GB) / Page_2MB);

      --  PD tables are at offsets:
      --  8192 (PD0), 12288 (PD1), 16384 (PD2), 20480 (PD3)
      declare
         PD_Offset : constant Integer_Address :=
            Integer_Address (8192 + PDPT_Idx * 4096);
         PD : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + PD_Offset);
      begin
         --  Set PD entry as a 2MB page
         --  HPA must be 2MB-aligned (bits 20:0 are offset)
         PD (PD_Idx) := (HPA and 16#FFFFFFFE00000#) or Flags;
      end;

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end GPA_Map_2MB;
   ----------------------------------------------------------------------------
   function Allocate_ASID return Unsigned_32 is
      Result : Unsigned_32 := 0;
   begin
      Seize (ASID_Lock);
      for I in 1 .. Max_ASID loop
         declare
            Idx : constant Unsigned_32 :=
               ((Next_ASID_Hint - 1 + Unsigned_32 (I) - 1) mod Max_ASID) + 1;
         begin
            if not ASID_In_Use (Positive (Idx)) then
               ASID_In_Use (Positive (Idx)) := True;
               Result := Idx;
               Next_ASID_Hint := (Idx mod Max_ASID) + 1;
               exit;
            end if;
         end;
      end loop;
      Release (ASID_Lock);
      return Result;
   exception
      when Constraint_Error =>
         return 0;
   end Allocate_ASID;

   procedure Free_ASID (ASID : Unsigned_32) is
   begin
      if ASID >= 1 and ASID <= Max_ASID then
         Seize (ASID_Lock);
         ASID_In_Use (Positive (ASID)) := False;
         Release (ASID_Lock);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Free_ASID;

   --  Convert VMCB segment attribute to NVMM format
   --  VMCB and NVMM use the SAME format:
   --    bits 0-3=Type, bit 4=S, bits 5-6=DPL, bit 7=P,
   --    bit 8=AVL, bit 9=L, bit 10=D/B, bit 11=G
   --  So this is an identity function.
   function VMCB_To_NVMM_Attrib (A : Unsigned_16) return Unsigned_16 is
   begin
      return A and 16#0FFF#;  --  Mask off reserved bits 12-15
   end VMCB_To_NVMM_Attrib;

   --  Convert NVMM segment attribute to VMCB format
   --  VMCB and NVMM use the SAME format - identity function.
   function NVMM_To_VMCB_Attrib (A : Unsigned_16) return Unsigned_16 is
   begin
      return A and 16#0FFF#;  --  Mask off reserved bits 12-15
   end NVMM_To_VMCB_Attrib;

   --  Convert NVMM segment attributes to VMX access rights format
   --  NVMM: bits 0-7 = type/S/DPL/P, bits 8-11 = AVL/L/D/G
   --  VMX:  bits 0-7 = type/S/DPL/P, bits 8-11 = reserved,
   --        bits 12-15 = AVL/L/D/G, bit 16 = unusable
   function To_VMX_AR
      (NVMM_Attrib : Unsigned_16;
       Limit       : Unsigned_32) return Unsigned_64
   is
      Low  : constant Unsigned_16 := NVMM_Attrib and 16#FF#;
      High : constant Unsigned_16 := Shift_Left (NVMM_Attrib and 16#F00#, 4);
      Result : Unsigned_64 := Unsigned_64 (Low or High);
   begin
      --  If P=0 (bit 7 clear), set VMX unusable bit
      if (Low and 16#80#) = 0 then
         Result := Result or 16#1_0000#;
      end if;

      --  VMX requires: if G=0, bits 31:20 of limit must be 0.
      --  If limit > 0xFFFFF and G=0, set G=1 to satisfy VMX check.
      --  G is bit 15 in VMX format (bit 11 in NVMM format).
      if Limit > 16#F_FFFF# and then (Result and 16#8000#) = 0 then
         Result := Result or 16#8000#;
      end if;
      return Result;
   end To_VMX_AR;

   function Get_Attrib_Raw (S : NVMM_Segment) return Unsigned_16 is
      Raw : Seg_Bytes with Import, Address => S'Address;
   begin
      return Unsigned_16 (Raw (2)) or Shift_Left (Unsigned_16 (Raw (3)), 8);
   end Get_Attrib_Raw;
end Virtualization;
