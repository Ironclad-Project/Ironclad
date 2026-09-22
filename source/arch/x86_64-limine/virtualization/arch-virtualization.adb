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

with Alignment;
with Arch.Virtualization.SVM;
with Arch.Virtualization.VMX;
with Arch.CPU;
with Arch.IDT;
with Arch.MMU;
with Arch.Snippets;
with Interfaces.C;
with Memory.Physical;
with Memory.MMU;
with Synchronization; use Synchronization;
with System;

package body Arch.Virtualization with SPARK_Mode => Off is
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

      --  The core the VCPU last entered the guest on, Natural'Last before
      --  its first entry, and whether its nested table has lost or changed a
      --  translation since: either has what the processor cached for the
      --  guest flushed before the next entry (see VCPU_Run_Ex).
      Last_Host_CPU  : Natural;
      Flush_Wanted   : Boolean;

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

   --  The size of each of a VCPU's two FPU save areas, the guest's and then
   --  the host's: with XSAVE, CPUID.(EAX=0DH,ECX=0):ECX rounded up to the 64
   --  bytes XSAVE aligns to, which holds every user state component the
   --  processor supports and so any XCR0 a guest may be given; without it,
   --  the 512-byte FXSAVE image.
   FPU_Area_Size   : Integer_Address := 512;

   --  ASID management (ASIDs 1-255, 0 is invalid)
   Max_ASID        : constant := 255;
   type ASID_Bitmap is array (1 .. Max_ASID) of Boolean;
   ASID_In_Use     : ASID_Bitmap := [others => False];
   ASID_Lock       : aliased Binary_Semaphore := Unlocked_Semaphore;
   Next_ASID_Hint  : Unsigned_32 := 1;  --  Hint for next free ASID

   --  The ASID a VCPU without one of its own uses, which Allocate_ASID never
   --  hands out: the highest the processor has, "The maximum ASID value
   --  supported by a processor is implementation specific" and CPUID
   --  Fn8000_000A_EBX giving the number supported, the host's ASID 0
   --  included (AMD APM 40332 rev 4.10, Vol. 2 15.5.1). Every entry with it
   --  flushes. Under VMX it is the VPID, which may not be 0 (Intel SDM
   --  325462-092US, Vol. 3C 29.2.1.1) and which EPT makes safe to share
   --  (Vol. 3C 31.4.3.3).
   Shared_ASID : Unsigned_32 := Max_ASID;

   --  The TLB_CONTROL command that flushes one guest's translations: "Flush
   --  this guest's TLB entries" (3) where CPUID Fn8000_000A_EDX[FlushByAsid]
   --  says the processor has it, else "Flush entire TLB" (1), which every SVM
   --  processor has (AMD APM 40332 rev 4.10, Vol. 2 15.16.1).
   SVM_Flush_Command : Unsigned_32 :=
      Arch.Virtualization.SVM.TLB_CONTROL_FLUSH_ALL;

   --  Data for serialization.
   type Seg_Bytes is array (0 .. 15) of Unsigned_8;

   function Is_Supported return Boolean is
   begin
      return Has_Initialized;
   end Is_Supported;

   procedure Enable_For_This_Core is
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
   end Enable_For_This_Core;

   procedure Initialize is
   begin
      Enable_For_This_Core;

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

            --  A processor with XSAVE describes it in leaf 0DH; without the
            --  size there would be nothing to lay the areas out by.
            declare
               package Align is new Alignment (Integer_Address);
               Size : constant Integer_Address :=
                  Integer_Address (Arch.Virtualization.SVM.Get_XSAVE_Size);
            begin
               if Size = 0 then
                  Has_XSAVE := False;
               else
                  FPU_Area_Size := Align.Align_Up (Size, 64);
               end if;
            end;
         end if;

         --  How many ASIDs the processor has (EBX, which counts the host's
         --  ASID 0) and whether it can flush one guest's alone (EDX bit 6,
         --  FlushByAsid), CPUID Fn8000_000A.
         if Current_Backend = Backend_SVM then
            declare
               EAX, EBX, ECX, EDX : Unsigned_32;
               CPUID_OK           : Boolean;
            begin
               Arch.Snippets.Get_CPUID
                  (16#8000_000A#, 0, EAX, EBX, ECX, EDX, CPUID_OK);
               if CPUID_OK then
                  if EBX > Max_ASID then
                     Shared_ASID := Max_ASID;
                  elsif EBX >= 2 then
                     Shared_ASID := EBX - 1;
                  end if;
                  if (EDX and Shift_Left (Unsigned_32'(1), 6)) /= 0 then
                     SVM_Flush_Command :=
                        Arch.Virtualization.SVM.TLB_CONTROL_FLUSH_GUEST;
                  end if;
               end if;
            end;
         end if;
      end if;
   end Initialize;
   ----------------------------------------------------------------------------
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
            for J in VCPU_ID loop
               Reset_VCPU (Machine_ID (I), J);
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
            Teardown_VCPU (ID, J);
         end if;
      end loop;

      Machines (Positive (ID)).Active := False;
      Release (Machines_Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Machine_Destroy;
   ----------------------------------------------------------------------------
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

      --  Allocate the nested page tables: PML4, PDPT, four PDs and PT0, the
      --  one page table, which covers the first 2 MiB of guest physical memory
      --  and so everything a guest can be given.
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

         --  Page 0: PML4, page 1: PDPT, pages 2-5: PD0-3, page 6: PT0. Every
         --  leaf starts empty, so a guest has what GPA_Map gave it and nothing
         --  else, and an access to a guest physical address the VMM did not
         --  map is a memory exit. Only the path down to PT0 is linked; PD1-3
         --  stay in the layout, zeroed and unlinked.
         declare
            type U64_Array is array (Natural range <>) of Unsigned_64;
            NPT_PA : constant Unsigned_64 :=
               Unsigned_64 (NPT_Addr_Local - Arch.MMU.Memory_Offset);
            Tables : U64_Array (0 .. 7 * 512 - 1)
               with Import, Address => To_Address (NPT_Addr_Local);
         begin
            Tables := [others => 0];

            --  PML4[0] -> PDPT, PDPT[0] -> PD0, PD0[0] -> PT0, each Present,
            --  Writable and User (Read, Write and Execute read as EPT bits).
            Tables (0)        := (NPT_PA + 4096) or 16#07#;
            Tables (512)      := (NPT_PA + 8192) or 16#07#;
            Tables (2 * 512)  := (NPT_PA + 24576) or 16#07#;
         end;
      end;

      --  Allocate the FPU buffer, the guest's save area and then the host's,
      --  FPU_Area_Size each. It is page-aligned, which both the 64 bytes
      --  XSAVE and the 16 FXSAVE ask of an area then are too.
      declare
         FPU_Addr_Local : Integer_Address;
         FPU_Size : constant Integer_Address := 2 * FPU_Area_Size;
         IOPM_Tmp : Integer_Address;
         MSRPM_Tmp : Integer_Address;
         NPT_Tmp : Integer_Address;
      begin
         Memory.Physical.Alloc
            (Interfaces.C.size_t (FPU_Size), FPU_Addr_Local);
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

         --  Initialize FPU buffer with default x87/SSE state. The whole
         --  buffer is zeroed, the XSAVE header past the legacy region
         --  included, which XRSTOR checks.
         declare
            type Byte_Array is array (Integer_Address range <>) of Unsigned_8;
            FPU_Buf : Byte_Array (0 .. FPU_Size - 1)
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
               --  None free: the shared one, which every entry flushes.
               VMCB_Ptr.Control.Guest_ASID := Shared_ASID;
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

         --  Nothing is flushed from here: VCPU_Run_Ex decides before every
         --  entry, and the first one always flushes.
         VMCB_Ptr.Control.TLB_Control :=
            Arch.Virtualization.SVM.TLB_CONTROL_DO_NOTHING;

         --  VMCB Clean = 0 means reload all state from VMCB on VMRUN
         VMCB_Ptr.Control.VMCB_Clean := 0;

         --  Enable nested paging (NPT), through the tables set up above.
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
            VPID_Got  : constant Unsigned_32 := Allocate_ASID;
            VPID_Val  : constant Unsigned_16 := Unsigned_16
               (if VPID_Got = 0 then Shared_ASID else VPID_Got);
            Clear_OK  : Boolean;
            Setup_OK  : Boolean;
         begin
            --  Store VMCS addresses
            Machines (Positive (Mach)).VCPUs (CPU).VMCS_Addr := CS_Addr;
            Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys := VMCS_PA;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_Launched := False;
            Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID := VPID_Got;

            --  Write VMCS revision ID before VMCLEAR
            Arch.Virtualization.VMX.Write_VMCS_Revision (CS_Addr);

            --  Clear the VMCS before its first use, load it, fill it, and
            --  clear it again before the lock goes: no VMCS may be active on
            --  more than one core, and the next call may load it on another
            --  (Intel SDM 325462-092US, Vol. 3C 27.11.1); and a page freed
            --  below while it is active could still be written back by the
            --  processor.
            Arch.Virtualization.VMX.VMCLEAR (VMCS_PA, Clear_OK);
            if Clear_OK then
               Arch.Virtualization.VMX.VMPTRLD (VMCS_PA, Setup_OK);
               if Setup_OK then
                  Arch.Virtualization.VMX.VMCS_Setup
                     (VMCS_VA   => CS_Addr,
                      EPT_PA    => EPT_PA,
                      IOPM_PA   => IOPM_PA,
                      MSRPM_PA  => MSRPM_PA,
                      VPID_Val  => VPID_Val,
                      Success   => Setup_OK);
               end if;
               Arch.Virtualization.VMX.VMCLEAR (VMCS_PA, Clear_OK);
               Setup_OK := Setup_OK and Clear_OK;
            else
               Setup_OK := False;
            end if;

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
               if VPID_Got /= 0 then
                  Free_ASID (VPID_Got);
               end if;
               Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID := 0;
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

      Teardown_VCPU (Mach, CPU);

      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Destroy;

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
            GPRs.RAX := G.RAX;
            --  RSP, RIP, RFLAGS from VMCS
            GPRs.RSP := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RSP);
            GPRs.RIP := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RIP);
            GPRs.RFLAGS := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS);
            --  Other GPRs from VMX_GPRs
            GPRs.RBX := G.RBX;
            GPRs.RCX := G.RCX;
            GPRs.RDX := G.RDX;
            GPRs.RBP := G.RBP;
            GPRs.RSI := G.RSI;
            GPRs.RDI := G.RDI;
            GPRs.R8  := G.R8;
            GPRs.R9  := G.R9;
            GPRs.R10 := G.R10;
            GPRs.R11 := G.R11;
            GPRs.R12 := G.R12;
            GPRs.R13 := G.R13;
            GPRs.R14 := G.R14;
            GPRs.R15 := G.R15;
         end;

         Unload_VMCS (Mach, CPU);
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
      GPRs.RAX := VMCB_Ptr.State_Save.RAX;
      GPRs.RSP := VMCB_Ptr.State_Save.RSP;
      GPRs.RIP := VMCB_Ptr.State_Save.RIP;
      GPRs.RFLAGS := VMCB_Ptr.State_Save.RFLAGS;

      --  GPRs stored separately (not in VMCB, saved/restored around VMRUN)
      declare
         G : Arch.Virtualization.SVM.Guest_GPRs renames
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs;
      begin
         GPRs.RBX := G.RBX;
         GPRs.RCX := G.RCX;
         GPRs.RDX := G.RDX;
         GPRs.RBP := G.RBP;
         GPRs.RSI := G.RSI;
         GPRs.RDI := G.RDI;
         GPRs.R8  := G.R8;
         GPRs.R9  := G.R9;
         GPRs.R10 := G.R10;
         GPRs.R11 := G.R11;
         GPRs.R12 := G.R12;
         GPRs.R13 := G.R13;
         GPRs.R14 := G.R14;
         GPRs.R15 := G.R15;
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
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RAX := GPRs.RAX;
            --  RSP, RIP, RFLAGS go to VMCS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_RSP, GPRs.RSP, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_RIP, GPRs.RIP, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_RFLAGS,
                GPRs.RFLAGS, Dummy_OK);
            --  Other GPRs go to VMX_GPRs
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBX := GPRs.RBX;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RCX := GPRs.RCX;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDX := GPRs.RDX;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RBP := GPRs.RBP;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RSI := GPRs.RSI;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.RDI := GPRs.RDI;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R8 := GPRs.R8;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R9 := GPRs.R9;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R10 := GPRs.R10;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R11 := GPRs.R11;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R12 := GPRs.R12;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R13 := GPRs.R13;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R14 := GPRs.R14;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_GPRs.R15 := GPRs.R15;
            Unload_VMCS (Mach, CPU);
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
      VMCB_Ptr.State_Save.RAX := GPRs.RAX;
      VMCB_Ptr.State_Save.RSP := GPRs.RSP;
      VMCB_Ptr.State_Save.RIP := GPRs.RIP;
      VMCB_Ptr.State_Save.RFLAGS := GPRs.RFLAGS;

      --  GPRs stored separately (not in VMCB, saved/restored around VMRUN)
      declare
         G : Arch.Virtualization.SVM.Guest_GPRs renames
            Machines (Positive (Mach)).VCPUs (CPU).SVM_GPRs;
      begin
         G.RBX := GPRs.RBX;
         G.RCX := GPRs.RCX;
         G.RDX := GPRs.RDX;
         G.RBP := GPRs.RBP;
         G.RSI := GPRs.RSI;
         G.RDI := GPRs.RDI;
         G.R8  := GPRs.R8;
         G.R9  := GPRs.R9;
         G.R10 := GPRs.R10;
         G.R11 := GPRs.R11;
         G.R12 := GPRs.R12;
         G.R13 := GPRs.R13;
         G.R14 := GPRs.R14;
         G.R15 := GPRs.R15;
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

            Segs.ES :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_ES_BASE));
            Segs.CS :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_BASE));
            Segs.SS :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_BASE));
            Segs.DS :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_DS_BASE));
            Segs.FS :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_FS_BASE));
            Segs.GS :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GS_BASE));
            Segs.GDT :=
               (0, 0,
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_BASE));
            Segs.IDT :=
               (0, 0,
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_BASE));
            Segs.LDT :=
               (Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_SELECTOR)
                   and 16#FFFF#),
                Unsigned_16 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR) and 16#FFFF#),
                Unsigned_32 (Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_LIMIT)),
                Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_BASE));
            Segs.TR :=
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

         Unload_VMCS (Mach, CPU);
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

      Segs.ES :=
         (VMCB_Ptr.State_Save.ES.Selector, VMCB_Ptr.State_Save.ES.Attrib,
          VMCB_Ptr.State_Save.ES.Limit, VMCB_Ptr.State_Save.ES.Base);
      Segs.CS :=
         (VMCB_Ptr.State_Save.CS.Selector, VMCB_Ptr.State_Save.CS.Attrib,
          VMCB_Ptr.State_Save.CS.Limit, VMCB_Ptr.State_Save.CS.Base);
      Segs.SS :=
         (VMCB_Ptr.State_Save.SS.Selector, VMCB_Ptr.State_Save.SS.Attrib,
          VMCB_Ptr.State_Save.SS.Limit, VMCB_Ptr.State_Save.SS.Base);
      Segs.DS :=
         (VMCB_Ptr.State_Save.DS.Selector, VMCB_Ptr.State_Save.DS.Attrib,
          VMCB_Ptr.State_Save.DS.Limit, VMCB_Ptr.State_Save.DS.Base);
      Segs.FS :=
         (VMCB_Ptr.State_Save.FS.Selector, VMCB_Ptr.State_Save.FS.Attrib,
          VMCB_Ptr.State_Save.FS.Limit, VMCB_Ptr.State_Save.FS.Base);
      Segs.GS :=
         (VMCB_Ptr.State_Save.GS.Selector, VMCB_Ptr.State_Save.GS.Attrib,
          VMCB_Ptr.State_Save.GS.Limit, VMCB_Ptr.State_Save.GS.Base);
      Segs.GDT :=
         (0, 0, VMCB_Ptr.State_Save.GDTR.Limit, VMCB_Ptr.State_Save.GDTR.Base);
      Segs.IDT :=
         (0, 0, VMCB_Ptr.State_Save.IDTR.Limit, VMCB_Ptr.State_Save.IDTR.Base);
      Segs.LDT :=
         (VMCB_Ptr.State_Save.LDTR.Selector, VMCB_Ptr.State_Save.LDTR.Attrib,
          VMCB_Ptr.State_Save.LDTR.Limit, VMCB_Ptr.State_Save.LDTR.Base);
      Segs.TR :=
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
                Unsigned_64 (Segs.ES.Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_ES_LIMIT,
                Unsigned_64 (Segs.ES.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_ES_BASE,
                Segs.ES.Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_ES_AR,
                To_VMX_AR (Segs.ES.Attrib, Segs.ES.Limit), Dummy_OK);
            --  CS
            --  VMX requires CS selector to be non-null even in unrestricted
            --  guest mode. Use selector 8 if selector is 0.
            if Segs.CS.Selector = 0 then
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR,
                   8, Dummy_OK);
            else
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_CS_SELECTOR,
                   Unsigned_64 (Segs.CS.Selector), Dummy_OK);
            end if;
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CS_LIMIT,
                Unsigned_64 (Segs.CS.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CS_BASE,
                Segs.CS.Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR,
                To_VMX_AR (Segs.CS.Attrib, Segs.CS.Limit), Dummy_OK);
            --  SS
            --  VMX requires SS selector to be non-null if SS is usable.
            --  Use selector 10h if selector is 0 and SS is usable.
            if Segs.SS.Selector = 0 and then
               (Segs.SS.Attrib and 16#80#) /= 0
            then
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR,
                   16#10#, Dummy_OK);
            else
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_SS_SELECTOR,
                   Unsigned_64 (Segs.SS.Selector), Dummy_OK);
            end if;
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SS_LIMIT,
                Unsigned_64 (Segs.SS.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SS_BASE,
                Segs.SS.Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SS_AR,
                To_VMX_AR (Segs.SS.Attrib, Segs.SS.Limit), Dummy_OK);
            --  DS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_SELECTOR,
                Unsigned_64 (Segs.DS.Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_LIMIT,
                Unsigned_64 (Segs.DS.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_BASE,
                Segs.DS.Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_DS_AR,
                To_VMX_AR (Segs.DS.Attrib, Segs.DS.Limit), Dummy_OK);
            --  FS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_SELECTOR,
                Unsigned_64 (Segs.FS.Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_LIMIT,
                Unsigned_64 (Segs.FS.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_BASE,
                Segs.FS.Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_FS_AR,
                To_VMX_AR (Segs.FS.Attrib, Segs.FS.Limit), Dummy_OK);
            --  GS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_SELECTOR,
                Unsigned_64 (Segs.GS.Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_LIMIT,
                Unsigned_64 (Segs.GS.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_BASE,
                Segs.GS.Base, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GS_AR,
                To_VMX_AR (Segs.GS.Attrib, Segs.GS.Limit), Dummy_OK);
            --  GDTR
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_LIMIT,
                Unsigned_64 (Segs.GDT.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_GDTR_BASE,
                Segs.GDT.Base, Dummy_OK);
            --  IDTR
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_LIMIT,
                Unsigned_64 (Segs.IDT.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_IDTR_BASE,
                Segs.IDT.Base, Dummy_OK);
            --  LDTR: if selector is 0, set VMX unusable bit
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_SELECTOR,
                Unsigned_64 (Segs.LDT.Selector), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_LIMIT,
                Unsigned_64 (Segs.LDT.Limit), Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_BASE,
                Segs.LDT.Base, Dummy_OK);
            if Segs.LDT.Selector = 0 then
               --  Null LDTR selector: set VMX unusable bit
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR,
                   16#1_0000#, Dummy_OK);  --  Just unusable bit
            else
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_LDTR_AR,
                   To_VMX_AR (Segs.LDT.Attrib, Segs.LDT.Limit), Dummy_OK);
            end if;
            --  TR
            --  TR: VMX requires TR to always be usable (unlike SVM)
            --  If selector is 0, keep the VMCS_Setup defaults
            if Segs.TR.Selector /= 0 then
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_SELECTOR,
                   Unsigned_64 (Segs.TR.Selector), Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_LIMIT,
                   Unsigned_64 (Segs.TR.Limit), Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_BASE,
                   Segs.TR.Base, Dummy_OK);
               Arch.Virtualization.VMX.VMX_Write
                  (Arch.Virtualization.VMX.VMCS_GUEST_TR_AR,
                   To_VMX_AR (Segs.TR.Attrib, Segs.TR.Limit), Dummy_OK);
            end if;
            Unload_VMCS (Mach, CPU);
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
      VMCB_Ptr.State_Save.ES.Selector := Segs.ES.Selector;
      VMCB_Ptr.State_Save.ES.Attrib := Get_Attrib_Raw (Segs.ES);
      VMCB_Ptr.State_Save.ES.Limit := Segs.ES.Limit;
      VMCB_Ptr.State_Save.ES.Base := Segs.ES.Base;
      VMCB_Ptr.State_Save.CS.Selector := Segs.CS.Selector;
      VMCB_Ptr.State_Save.CS.Attrib := Get_Attrib_Raw (Segs.CS);
      VMCB_Ptr.State_Save.CS.Limit := Segs.CS.Limit;
      VMCB_Ptr.State_Save.CS.Base := Segs.CS.Base;
      VMCB_Ptr.State_Save.SS.Selector := Segs.SS.Selector;
      VMCB_Ptr.State_Save.SS.Attrib := Get_Attrib_Raw (Segs.SS);
      VMCB_Ptr.State_Save.SS.Limit := Segs.SS.Limit;
      VMCB_Ptr.State_Save.SS.Base := Segs.SS.Base;
      VMCB_Ptr.State_Save.DS.Selector := Segs.DS.Selector;
      VMCB_Ptr.State_Save.DS.Attrib := Get_Attrib_Raw (Segs.DS);
      VMCB_Ptr.State_Save.DS.Limit := Segs.DS.Limit;
      VMCB_Ptr.State_Save.DS.Base := Segs.DS.Base;
      VMCB_Ptr.State_Save.FS.Selector := Segs.FS.Selector;
      VMCB_Ptr.State_Save.FS.Attrib := Get_Attrib_Raw (Segs.FS);
      VMCB_Ptr.State_Save.FS.Limit := Segs.FS.Limit;
      VMCB_Ptr.State_Save.FS.Base := Segs.FS.Base;
      VMCB_Ptr.State_Save.GS.Selector := Segs.GS.Selector;
      VMCB_Ptr.State_Save.GS.Attrib := Get_Attrib_Raw (Segs.GS);
      VMCB_Ptr.State_Save.GS.Limit := Segs.GS.Limit;
      VMCB_Ptr.State_Save.GS.Base := Segs.GS.Base;
      VMCB_Ptr.State_Save.GDTR.Limit := Segs.GDT.Limit;
      VMCB_Ptr.State_Save.GDTR.Base := Segs.GDT.Base;
      VMCB_Ptr.State_Save.IDTR.Limit := Segs.IDT.Limit;
      VMCB_Ptr.State_Save.IDTR.Base := Segs.IDT.Base;
      VMCB_Ptr.State_Save.LDTR.Selector := Segs.LDT.Selector;
      VMCB_Ptr.State_Save.LDTR.Attrib := Get_Attrib_Raw (Segs.LDT);
      VMCB_Ptr.State_Save.LDTR.Limit := Segs.LDT.Limit;
      VMCB_Ptr.State_Save.LDTR.Base := Segs.LDT.Base;
      VMCB_Ptr.State_Save.TR.Selector := Segs.TR.Selector;
      VMCB_Ptr.State_Save.TR.Attrib := Get_Attrib_Raw (Segs.TR);
      VMCB_Ptr.State_Save.TR.Limit := Segs.TR.Limit;
      VMCB_Ptr.State_Save.TR.Base := Segs.TR.Base;

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

            CRs := (others => 0);
            CRs.CR0 := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR0);
            --  CR2 not in VMCS, would need to track separately
            CRs.CR2 := 0;
            CRs.CR3 := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR3);
            CRs.CR4 := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_CR4);
         end;

         Unload_VMCS (Mach, CPU);
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

      CRs :=
         (CR0 => VMCB_Ptr.State_Save.CR0,
          CR2 => VMCB_Ptr.State_Save.CR2,
          CR3 => VMCB_Ptr.State_Save.CR3,
          CR4 => VMCB_Ptr.State_Save.CR4,
          others => 0);

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
                Arch.Virtualization.VMX.Apply_CR0_Fixed (CRs.CR0, True),
                Dummy_OK);
            --  CR2 not in VMCS
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CR3, CRs.CR3, Dummy_OK);
            --  Apply CR4 fixed bits (preserves LA57 for 5-level paging)
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_CR4,
                Arch.Virtualization.VMX.Apply_CR4_Fixed (CRs.CR4), Dummy_OK);
            Unload_VMCS (Mach, CPU);
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

      VMCB_Ptr.State_Save.CR0 := CRs.CR0;
      VMCB_Ptr.State_Save.CR2 := CRs.CR2;
      VMCB_Ptr.State_Save.CR3 := CRs.CR3;
      VMCB_Ptr.State_Save.CR4 := CRs.CR4;

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

            MSRs := (others => 0);

            --  Read EFER and SYSENTER from VMCS
            MSRs.EFER := Arch.Virtualization.VMX.VMX_Read
               (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER);
            MSRs.SYSENTER_CS :=
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_IA32_SYSENTER_CS);
            MSRs.SYSENTER_ESP :=
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_ESP);
            MSRs.SYSENTER_EIP :=
               Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_EIP);

            --  Read syscall MSRs from VMX_MSRs
            MSRs.STAR := Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.STAR;
            MSRs.LSTAR :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.LSTAR;
            MSRs.CSTAR :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.CSTAR;
            MSRs.SFMASK :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.SFMASK;
            MSRs.KERNELGSBASE :=
               Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.Kernel_GS_Base;
         end;

         Unload_VMCS (Mach, CPU);
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

      MSRs :=
         (EFER         => VMCB_Ptr.State_Save.EFER,
          STAR         => VMCB_Ptr.State_Save.STAR,
          LSTAR        => VMCB_Ptr.State_Save.LSTAR,
          CSTAR        => VMCB_Ptr.State_Save.CSTAR,
          SFMASK       => VMCB_Ptr.State_Save.SFMASK,
          KERNELGSBASE => VMCB_Ptr.State_Save.Kernel_GS_Base,
          SYSENTER_CS  => VMCB_Ptr.State_Save.SYSENTER_CS,
          SYSENTER_ESP => VMCB_Ptr.State_Save.SYSENTER_ESP,
          SYSENTER_EIP => VMCB_Ptr.State_Save.SYSENTER_EIP,
          PAT          => 0);

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
                MSRs.EFER, Dummy_OK);

            --  Update entry controls based on EFER.LMA (bit 10)
            --  IA-32e mode guest bit must match EFER.LMA
            declare
               Entry_Ctls : Unsigned_64 := Arch.Virtualization.VMX.VMX_Read
                  (Arch.Virtualization.VMX.VMCS_ENTRY_CONTROLS);
               LMA_Set : constant Boolean := (MSRs.EFER and 16#400#) /= 0;
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
                MSRs.SYSENTER_CS, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_ESP,
                MSRs.SYSENTER_ESP, Dummy_OK);
            Arch.Virtualization.VMX.VMX_Write
               (Arch.Virtualization.VMX.VMCS_GUEST_SYSENTER_EIP,
                MSRs.SYSENTER_EIP, Dummy_OK);

            --  Syscall MSRs go to VMX_MSRs (loaded manually before VM entry)
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.STAR := MSRs.STAR;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.LSTAR :=
               MSRs.LSTAR;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.CSTAR :=
               MSRs.CSTAR;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.SFMASK :=
               MSRs.SFMASK;
            Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs.Kernel_GS_Base :=
               MSRs.KERNELGSBASE;
         end;

         Unload_VMCS (Mach, CPU);
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


      VMCB_Ptr.State_Save.EFER := MSRs.EFER or 16#1000#; --  Ensure SVME = 1
      VMCB_Ptr.State_Save.STAR := MSRs.STAR;
      VMCB_Ptr.State_Save.LSTAR := MSRs.LSTAR;
      VMCB_Ptr.State_Save.CSTAR := MSRs.CSTAR;
      VMCB_Ptr.State_Save.SFMASK := MSRs.SFMASK;
      VMCB_Ptr.State_Save.Kernel_GS_Base := MSRs.KERNELGSBASE;
      VMCB_Ptr.State_Save.SYSENTER_CS := MSRs.SYSENTER_CS;
      VMCB_Ptr.State_Save.SYSENTER_ESP := MSRs.SYSENTER_ESP;
      VMCB_Ptr.State_Save.SYSENTER_EIP := MSRs.SYSENTER_EIP;

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

   function VCPU_Run_Ex
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : out VCPU_Exit_Info) return Boolean
   is
      VMCB_Phys : Unsigned_64;
      VMCB_Ptr  : Arch.Virtualization.SVM.VMCB_Acc;
      Exit_Code : Unsigned_64;
      Host_XCR0 : Unsigned_64;
      Here      : Natural;
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

      --  Flush what the processor may have cached for this guest whenever it
      --  may be stale: on a core the VCPU did not last enter on, where its
      --  ASID may since have served another VCPU and its own entries predate
      --  its latest changes; when its nested table lost or changed a
      --  translation since the last entry (Flush_Wanted); and on every entry
      --  with the shared ASID. After such a change a VMM returning to the same
      --  ASID "should use either TLB command 011b or 001b", and VMRUN "reads,
      --  but does not change" the field, so it is cleared once an entry has
      --  carried it out (AMD APM 40332 rev 4.10, Vol. 2 15.16.1). The machine
      --  lock keeps host interrupts out from here to the exit, so the core
      --  cannot change under the decision, and it keeps GPA changes and runs
      --  apart, so no other core needs to be told.
      Here := Arch.CPU.Get_Local.Number;
      if Machines (Positive (Mach)).VCPUs (CPU).Last_Host_CPU /= Here or
         Machines (Positive (Mach)).VCPUs (CPU).Flush_Wanted or
         Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID = 0
      then
         VMCB_Ptr.Control.TLB_Control := SVM_Flush_Command;
      else
         VMCB_Ptr.Control.TLB_Control :=
            Arch.Virtualization.SVM.TLB_CONTROL_DO_NOTHING;
      end if;

      --  The guest's FPU state goes in for the entry and comes out after
      --  it, the host's kept aside meanwhile (Enter_Guest_FPU).
      Enter_Guest_FPU (Mach, CPU, Host_XCR0);

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

      Leave_Guest_FPU (Mach, CPU, Host_XCR0);

      --  Reload IDT after VMRUN (may not be fully restored)
      Arch.IDT.Load_IDT;

      --  An entry that happened carried out the flush it asked for. One that
      --  VMRUN refused (VMEXIT_INVALID, its consistency checks) entered
      --  nothing, so the flush stays wanted.
      if VMCB_Ptr.Control.Exit_Code /= Unsigned_64'Last then
         Machines (Positive (Mach)).VCPUs (CPU).Last_Host_CPU := Here;
         Machines (Positive (Mach)).VCPUs (CPU).Flush_Wanted := False;
         VMCB_Ptr.Control.TLB_Control :=
            Arch.Virtualization.SVM.TLB_CONTROL_DO_NOTHING;
      end if;

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
            --  The bytes of the instruction that faulted, read as the guest
            --  reads them: see Fetch_Instruction.  EFER.LMA with CS.L (bit 9
            --  of a VMCB attribute) is 64-bit mode, where CS's base is 0.
            declare
               Long_64 : constant Boolean :=
                  (VMCB_Ptr.State_Save.EFER and 16#400#) /= 0 and
                  (VMCB_Ptr.State_Save.CS.Attrib and 16#200#) /= 0;
            begin
               Fetch_Instruction
                  (Mach    => Mach,
                   CPU     => CPU,
                   CR0     => VMCB_Ptr.State_Save.CR0,
                   CR3     => VMCB_Ptr.State_Save.CR3,
                   CR4     => VMCB_Ptr.State_Save.CR4,
                   EFER    => VMCB_Ptr.State_Save.EFER,
                   Linear  =>
                      (if Long_64 then VMCB_Ptr.State_Save.RIP
                       else VMCB_Ptr.State_Save.CS.Base +
                            VMCB_Ptr.State_Save.RIP),
                   Wrap_32 => not Long_64,
                   Info    => Exit_Info.U.Memory);
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
                  --  XCR0 write: the kernel loads it for every entry, so it
                  --  is taken only as a value XSETBV itself would accept.
                  XCR0_Valid := Guest_XCR0_Valid (XCR_Val);

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
   ----------------------------------------------------------------------------
   function GPA_Map
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : GPA_Flags) return Boolean
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
      --  We only have PTs pre-allocated for the first 2MB. Written so that
      --  it cannot wrap.
      if GPA > Page_2MB or else Size > Page_2MB - GPA then
         Release (Machines (Positive (Mach)).Lock);
         return False;  --  Beyond 2MB limit with 4KB pages
      end if;

      --  Convert HVA to physical address
      HPA := Unsigned_64 (Integer_Address (HVA) - Arch.MMU.Memory_Offset);

      --  Build 4KB page flags: Present(0), Writable(1), User(2)
      --  Note: No PS bit for 4KB pages
      Flags := 16#01#;  --  Present
      if Prot.Can_Write then
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

            --  PT index within PT0 (GPA / 4KB, max 511). Replacing an
            --  entry that was present changes a translation the guest may
            --  have cached, so its next entry flushes (see VCPU_Run_Ex).
            PT_Idx := Natural (Current_GPA / Page_4KB);
            if PT_Idx <= 511 then
               if (PT0 (PT_Idx) and 1) /= 0 then
                  Machines (Positive (Mach)).VCPUs (CPU).Flush_Wanted := True;
               end if;
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
       Prot : GPA_Flags) return Boolean
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
      NPT_Addr : Integer_Address;
      Page_4KB : constant Unsigned_64 := 16#1000#;
      Page_2MB : constant Unsigned_64 := 16#20_0000#;
   begin
      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      --  4 KiB pages inside PT0, which is all a guest can be given, as
      --  GPA_Map maps them. Checked before the lock, so that nothing under it
      --  can raise.
      if (GPA and (Page_4KB - 1)) /= 0 or
         (Size and (Page_4KB - 1)) /= 0 or
         GPA > Page_2MB or
         Size > Page_2MB - GPA
      then
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

      --  Clearing an entry that was present takes away a translation the
      --  guest may have cached, so its next entry flushes (see VCPU_Run_Ex).
      declare
         PT0 : U64_Array (0 .. 511)
            with Import, Address => To_Address (NPT_Addr + 24576);
         First : constant Integer := Integer (GPA / Page_4KB);
         Last  : constant Integer := Integer ((GPA + Size) / Page_4KB) - 1;
      begin
         for I in First .. Last loop
            if (PT0 (I) and 1) /= 0 then
               Machines (Positive (Mach)).VCPUs (CPU).Flush_Wanted := True;
            end if;
            PT0 (I) := 0;
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
      Result    : Boolean;
   begin
      GPA := 0;

      if not Has_Initialized or Mach = Invalid_Machine then
         return False;
      end if;

      --  The walk holds the machine: it reads the guest's tables through the
      --  nested ones, which GPA_Map and GPA_Unmap change under this lock, and
      --  under VMX it makes the VMCS current, which must never happen on two
      --  cores at once.
      Seize (Machines (Positive (Mach)).Lock);
      if not Machines (Positive (Mach)).Active or else
         not Machines (Positive (Mach)).VCPUs (CPU).Active
      then
         Release (Machines (Positive (Mach)).Lock);
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
               Release (Machines (Positive (Mach)).Lock);
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
            Unload_VMCS (Mach, CPU);
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

      Result := Walk_Guest (Mach, CPU, CR0, CR3, CR4, EFER, GVA, GPA);
      Release (Machines (Positive (Mach)).Lock);
      return Result;
   exception
      when Constraint_Error =>
         return False;
   end GVA_To_GPA;

   function Walk_Guest
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CR0  : Unsigned_64;
       CR3  : Unsigned_64;
       CR4  : Unsigned_64;
       EFER : Unsigned_64;
       GVA  : Unsigned_64;
       GPA  : out Unsigned_64) return Boolean
   is
   begin
      GPA := 0;

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
   end Walk_Guest;

   --  Fill a memory exit's instruction bytes as the guest reads them: each
   --  page is translated through the guest's own page tables (Walk_Guest)
   --  and then the nested ones (GPA_To_HVA), and the fetch stops at the first
   --  page the guest does not have, Inst_Len saying how many bytes were read.
   --  Called with the machine held and, under VMX, its VMCS current.
   procedure Fetch_Instruction
      (Mach    : Machine_ID;
       CPU     : VCPU_ID;
       CR0     : Unsigned_64;
       CR3     : Unsigned_64;
       CR4     : Unsigned_64;
       EFER    : Unsigned_64;
       Linear  : Unsigned_64;
       Wrap_32 : Boolean;
       Info    : in out Exit_Memory_Info)
   is
      Page_4KB : constant Unsigned_64 := 16#1000#;
      Addr     : Unsigned_64;
      GPA      : Unsigned_64;
      HVA      : Unsigned_64 := 0;
   begin
      Info.Inst_Len   := 0;
      Info.Inst_Bytes := [others => 0];
      for I in 0 .. 14 loop
         Addr := Linear + Unsigned_64 (I);
         if Wrap_32 then
            Addr := Addr and 16#FFFF_FFFF#;
         end if;

         if I = 0 or (Addr and (Page_4KB - 1)) = 0 then
            exit when not Walk_Guest
               (Mach, CPU, CR0, CR3, CR4, EFER, Addr, GPA)
               or else not GPA_To_HVA (Mach, CPU, GPA, HVA);
         else
            HVA := HVA + 1;
         end if;

         declare
            Byte : Unsigned_8
               with Import, Address => To_Address (Integer_Address (HVA));
         begin
            Info.Inst_Bytes (I) := Byte;
         end;
         Info.Inst_Len := Unsigned_8 (I + 1);
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Fetch_Instruction;

   procedure Unload_VMCS (Mach : Machine_ID; CPU : VCPU_ID) is
      Dummy_OK : Boolean;
   begin
      Arch.Virtualization.VMX.VMCLEAR
         (Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys, Dummy_OK);
      Machines (Positive (Mach)).VCPUs (CPU).VMX_Launched := False;
   exception
      when Constraint_Error =>
         null;
   end Unload_VMCS;

   function Guest_XCR0_Valid (Value : Unsigned_64) return Boolean is
      SSE_AVX : constant Unsigned_64 := 16#6#;
      MPX     : constant Unsigned_64 := 16#18#;
      AVX_512 : constant Unsigned_64 := 16#E0#;
      AMX     : constant Unsigned_64 := 16#6_0000#;
   begin
      --  Nothing XSETBV would refuse with #GP, the stricter vendor's rule
      --  where the two differ (Intel SDM 325462-092US, Vol. 1 13.3 and
      --  Vol. 2D XSETBV; AMD APM 40332 rev 4.10, Vol. 4 XSETBV): only
      --  components the processor supports, x87 always, AVX only beside SSE,
      --  MPX's two components together, AVX-512's three together and only
      --  beside SSE and AVX, and AMX's two together.
      return (Value and not Host_XCR0_Max) = 0 and then
             (Value and 1) /= 0 and then
             (Value and SSE_AVX) /= 16#4# and then
             (Value and MPX) in 0 | MPX and then
             ((Value and AVX_512) = 0 or else
              ((Value and AVX_512) = AVX_512 and then
               (Value and SSE_AVX) = SSE_AVX)) and then
             (Value and AMX) in 0 | AMX;
   end Guest_XCR0_Valid;

   procedure Enter_Guest_FPU
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Host_XCR0 : out Unsigned_64)
   is
      Guest_Area, Host_Area : Integer_Address;
      Guest_XCR0            : Unsigned_64;
   begin
      Host_XCR0  := 0;
      Guest_Area := Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
      Host_Area  := Guest_Area + FPU_Area_Size;
      Guest_XCR0 := Machines (Positive (Mach)).VCPUs (CPU).XCR0_Value;

      if Has_XSAVE then
         declare
            --  The XSAVE header's XSTATE_BV, the components the area holds.
            XSTATE_BV : Unsigned_64
               with Import, Address => To_Address (Guest_Area + 512);
         begin
            Host_XCR0 := Arch.Virtualization.SVM.Get_XCR0;
            Arch.Virtualization.SVM.XSAVE_Save
               (To_Address (Host_Area), Host_XCR0);
            if Guest_XCR0 /= Host_XCR0 and then Guest_XCR0 /= 0 then
               Arch.Virtualization.SVM.Set_XCR0 (Guest_XCR0);
            end if;

            --  Only what XCR0 has for the restore may be marked in the area:
            --  XRSTOR raises #GP for an XSTATE_BV bit that XCR0 does not have
            --  (Intel SDM 325462-092US, Vol. 1 13.8.1), and XSAVE leaves the
            --  bits outside its mask as they were. A component a guest
            --  disables starts from its initial state if the guest enables it
            --  again.
            XSTATE_BV := XSTATE_BV and Guest_XCR0;
            Arch.Virtualization.SVM.XSAVE_Restore
               (To_Address (Guest_Area), Guest_XCR0);
         end;
      else
         declare
            Guest_FPU : Arch.Virtualization.SVM.FPU_State_Area
               with Import, Address => To_Address (Guest_Area);
            Host_FPU  : Arch.Virtualization.SVM.FPU_State_Area
               with Import, Address => To_Address (Host_Area);
         begin
            Arch.Virtualization.SVM.FPU_Save (Host_FPU);
            Arch.Virtualization.SVM.FPU_Restore (Guest_FPU);
         end;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Enter_Guest_FPU;

   procedure Leave_Guest_FPU
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Host_XCR0 : Unsigned_64)
   is
      Guest_Area, Host_Area : Integer_Address;
      Guest_XCR0            : Unsigned_64;
   begin
      Guest_Area := Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr;
      Host_Area  := Guest_Area + FPU_Area_Size;
      Guest_XCR0 := Machines (Positive (Mach)).VCPUs (CPU).XCR0_Value;

      if Has_XSAVE then
         Arch.Virtualization.SVM.XSAVE_Save
            (To_Address (Guest_Area), Guest_XCR0);
         if Guest_XCR0 /= Host_XCR0 and then Guest_XCR0 /= 0 then
            Arch.Virtualization.SVM.Set_XCR0 (Host_XCR0);
         end if;
         Arch.Virtualization.SVM.XSAVE_Restore
            (To_Address (Host_Area), Host_XCR0);
      else
         declare
            Guest_FPU : Arch.Virtualization.SVM.FPU_State_Area
               with Import, Address => To_Address (Guest_Area);
            Host_FPU  : Arch.Virtualization.SVM.FPU_State_Area
               with Import, Address => To_Address (Host_Area);
         begin
            Arch.Virtualization.SVM.FPU_Save (Guest_FPU);
            Arch.Virtualization.SVM.FPU_Restore (Host_FPU);
         end;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Leave_Guest_FPU;
   ----------------------------------------------------------------------------
   function Allocate_ASID return Unsigned_32 is
      --  1 .. Shared_ASID - 1, never the shared one (see Shared_ASID).
      Limit  : constant Unsigned_32 := Shared_ASID - 1;
      Result : Unsigned_32 := 0;
   begin
      if Limit = 0 then
         return 0;
      end if;

      Seize (ASID_Lock);
      for I in 1 .. Limit loop
         declare
            Idx : constant Unsigned_32 :=
               ((Next_ASID_Hint - 1 + I - 1) mod Limit) + 1;
         begin
            if not ASID_In_Use (Positive (Idx)) then
               ASID_In_Use (Positive (Idx)) := True;
               Result := Idx;
               Next_ASID_Hint := (Idx mod Limit) + 1;
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

   procedure Reset_VCPU (Mach : Machine_ID; CPU : VCPU_ID) is
   begin
      Machines (Positive (Mach)).VCPUs (CPU).Active := False;
      --  SVM-specific fields
      Machines (Positive (Mach)).VCPUs (CPU).VMCB_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).VMCB_Phys := 0;
      --  VMX-specific fields
      Machines (Positive (Mach)).VCPUs (CPU).VMCS_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys := 0;
      Machines (Positive (Mach)).VCPUs (CPU).VMX_Launched := False;
      --  The guest's syscall MSRs, which VCPU_Create leaves as they are.
      Machines (Positive (Mach)).VCPUs (CPU).VMX_MSRs :=
         (STAR   => 0, LSTAR          => 0, CSTAR => 0,
          SFMASK => 0, Kernel_GS_Base => 0);
      --  Common fields
      Machines (Positive (Mach)).VCPUs (CPU).IOPM_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).MSRPM_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).FPU_Addr := 0;
      Machines (Positive (Mach)).VCPUs (CPU).Event_Pending := False;
      Machines (Positive (Mach)).VCPUs (CPU).Pending_Event :=
         (Event_Type => 0, Vector => 0, Has_Error => False, Error_Code => 0);
      Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;
      Machines (Positive (Mach)).VCPUs (CPU).DRs_0_3 := [others => 0];
      Machines (Positive (Mach)).VCPUs (CPU).Assigned_ASID := 0;
      Machines (Positive (Mach)).VCPUs (CPU).TSC_Offset_Val := 0;
      Machines (Positive (Mach)).VCPUs (CPU).V_TPR := 0;
      Machines (Positive (Mach)).VCPUs (CPU).V_IRQ := False;
      Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Prio := 0;
      Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Vector := 0;
      Machines (Positive (Mach)).VCPUs (CPU).V_Intr_Masking := False;
      --  XCR0 defaults to 1 (x87 FPU only)
      Machines (Positive (Mach)).VCPUs (CPU).XCR0_Value := 1;
      Machines (Positive (Mach)).VCPUs (CPU).Last_Host_CPU := Natural'Last;
      Machines (Positive (Mach)).VCPUs (CPU).Flush_Wanted := False;
   exception
      when Constraint_Error =>
         null;
   end Reset_VCPU;

   procedure Teardown_VCPU (Mach : Machine_ID; CPU : VCPU_ID) is
   begin
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

      Reset_VCPU (Mach, CPU);
   exception
      when Constraint_Error =>
         null;
   end Teardown_VCPU;

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

   function VCPU_Run_Ex_VMX
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : in out VCPU_Exit_Info) return Boolean
   is
      VMCS_Phys  : Unsigned_64;
      Exit_Reason : Unsigned_64;
      Load_OK    : Boolean;
      Here       : Positive;
      Host_XCR0  : Unsigned_64;
   begin
      VMCS_Phys := Machines (Positive (Mach)).VCPUs (CPU).VMCS_Phys;

      --  Load the VMCS pointer. From here on every way out makes it inactive
      --  again (Unload_VMCS), so that it is never active on a core this
      --  thread may leave, and every entry is therefore a VMLAUNCH.
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
         Unload_VMCS (Mach, CPU);
         Release (Machines (Positive (Mach)).Lock);
         return True;
      end if;

      --  An exit loads the host's TR, FS and GS bases and CR3 from the VMCS,
      --  and a VCPU runs on whichever core its thread is on, so they are this
      --  core's and this thread's, written before every entry.
      Arch.Virtualization.VMX.VMCS_Write_Unchecked
         (Arch.Virtualization.VMX.VMCS_HOST_TR_BASE,
          Unsigned_64 (To_Integer (Arch.CPU.Get_Local.Core_TSS'Address)));
      Arch.Virtualization.VMX.VMCS_Write_Unchecked
         (Arch.Virtualization.VMX.VMCS_HOST_FS_BASE, Arch.Snippets.Read_FS);
      Arch.Virtualization.VMX.VMCS_Write_Unchecked
         (Arch.Virtualization.VMX.VMCS_HOST_GS_BASE, Arch.Snippets.Read_GS);
      Arch.Virtualization.VMX.VMCS_Write_Unchecked
         (Arch.Virtualization.VMX.VMCS_HOST_CR3, Arch.Snippets.Read_CR3);

      --  Invalidate what this core may have cached through the VCPU's EPT
      --  whenever it may be stale, as VCPU_Run_Ex does for SVM: on a core the
      --  VCPU did not last enter on, and after its table lost or changed a
      --  translation. Software "should use the INVEPT instruction with the
      --  'single-context' INVEPT type after making any" change that clears a
      --  permission or changes an address (Intel SDM 325462-092US, Vol. 3C
      --  31.4.3.4), and cached mappings are tagged by the EPT root, which a
      --  freed table's page can become for another VCPU. With EPT a shared
      --  VPID needs nothing (Vol. 3C 31.4.3.3).
      Here := Arch.CPU.Get_Local.Number;
      if Machines (Positive (Mach)).VCPUs (CPU).Last_Host_CPU /= Here or
         Machines (Positive (Mach)).VCPUs (CPU).Flush_Wanted
      then
         declare
            EPTP : constant Unsigned_64 := Unsigned_64
               (Machines (Positive (Mach)).VCPUs (CPU).NPT_Addr -
                Arch.MMU.Memory_Offset) or
               Arch.Virtualization.VMX.EPT_POINTER_WB_4LEVEL;
            Flushed : Boolean;
         begin
            Arch.Virtualization.VMX.INVEPT (EPTP, Flushed);
            if not Flushed then
               Unload_VMCS (Mach, CPU);
               Release (Machines (Positive (Mach)).Lock);
               Exit_Info.Reason := NVMM_EXIT_INVALID;
               Exit_Info.U.Invalid.HW_Code := 16#DEAD_0002#;
               return False;
            end if;
         end;
      end if;

      --  The guest's FPU state goes in for the entry and comes out after
      --  it, the host's kept aside meanwhile (Enter_Guest_FPU).
      Enter_Guest_FPU (Mach, CPU, Host_XCR0);

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

         --  An entry that happened did so with what this core cached for
         --  the VCPU invalidated, above.
         if VM_Success then
            Machines (Positive (Mach)).VCPUs (CPU).Last_Host_CPU := Here;
            Machines (Positive (Mach)).VCPUs (CPU).Flush_Wanted := False;
         end if;

         --  Restore host FS and GS bases
         Arch.Snippets.Write_FS (Host_FS_Base);
         Arch.Snippets.Write_GS (Host_GS_Base);
      end;

      Leave_Guest_FPU (Mach, CPU, Host_XCR0);

      --  Reload IDT after VM exit
      Arch.IDT.Load_IDT;

      --  Check if stop was requested after running
      if Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested then
         Machines (Positive (Mach)).VCPUs (CPU).Stop_Requested := False;
         Exit_Info.Reason := NVMM_EXIT_STOPPED;
         Unload_VMCS (Mach, CPU);
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
            --  The bytes of the instruction that faulted, read as the guest
            --  reads them: see Fetch_Instruction.  EFER.LMA with CS.L (bit 13
            --  of VMX access rights) is 64-bit mode, where CS's base is 0.
            declare
               EFER    : constant Unsigned_64 :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_IA32_EFER);
               CS_AR   : constant Unsigned_64 :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_CS_AR);
               RIP     : constant Unsigned_64 :=
                  Arch.Virtualization.VMX.VMX_Read
                     (Arch.Virtualization.VMX.VMCS_GUEST_RIP);
               Long_64 : constant Boolean :=
                  (EFER and 16#400#) /= 0 and (CS_AR and 16#2000#) /= 0;
            begin
               Fetch_Instruction
                  (Mach    => Mach,
                   CPU     => CPU,
                   CR0     => Arch.Virtualization.VMX.VMX_Read
                                 (Arch.Virtualization.VMX.VMCS_GUEST_CR0),
                   CR3     => Arch.Virtualization.VMX.VMX_Read
                                 (Arch.Virtualization.VMX.VMCS_GUEST_CR3),
                   CR4     => Arch.Virtualization.VMX.VMX_Read
                                 (Arch.Virtualization.VMX.VMCS_GUEST_CR4),
                   EFER    => EFER,
                   Linear  =>
                      (if Long_64 then RIP
                       else Arch.Virtualization.VMX.VMX_Read
                               (Arch.Virtualization.VMX.VMCS_GUEST_CS_BASE) +
                            RIP),
                   Wrap_32 => not Long_64,
                   Info    => Exit_Info.U.Memory);
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
                  XCR0_Valid := Guest_XCR0_Valid (XCR_Val);

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

      Unload_VMCS (Mach, CPU);
      Release (Machines (Positive (Mach)).Lock);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end VCPU_Run_Ex_VMX;
end Arch.Virtualization;
