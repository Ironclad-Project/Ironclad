--  virtualization.ads: Virtualization module of the kernel.
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

with Interfaces; use Interfaces;
with System;

package Virtualization with SPARK_Mode => Off is
   --  This module implements a KVM compatible interface, KVM's specification
   --  can be found at https://docs.kernel.org/virt/kvm/api.html

   --  KVM version that this module implements, we implement version 2, which
   --  means we provide VCPU-stop.
   NVMM_Version : constant := 2;

   --  Capabilities of this implementation.
   State_Size           : constant := 0; --  TODO.
   Max_Virtual_Machines : constant := 128;
   Max_CPUs_Per_VM      : constant := 4;
   Max_RAM_Per_VM       : constant := Unsigned_64'Last;

   --  Machine ID type (0 = invalid)
   subtype Machine_ID is Unsigned_32 range 0 .. Max_Virtual_Machines;
   Invalid_Machine : constant Machine_ID := 0;

   --  VCPU ID type
   subtype VCPU_ID is Unsigned_32 range 0 .. Max_CPUs_Per_VM - 1;
   ----------------------------------------------------------------------------
   --  Returns True if virtualization is supported.
   function Is_Supported return Boolean;

   procedure Initialize;

   ----------------------------------------------------------------------------
   --  Machine management
   ----------------------------------------------------------------------------

   --  Create a new virtual machine.
   --  @return Machine ID on success, Invalid_Machine on failure.
   function Machine_Create return Machine_ID;

   --  Destroy a virtual machine.
   --  @param ID  The machine ID to destroy.
   --  @return True on success, False if ID is invalid.
   function Machine_Destroy (ID : Machine_ID) return Boolean;

   ----------------------------------------------------------------------------
   --  VCPU management
   ----------------------------------------------------------------------------

   --  Create a new VCPU for a machine.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID to create.
   --  @return True on success.
   function VCPU_Create (Mach : Machine_ID; CPU : VCPU_ID) return Boolean;

   --  Destroy a VCPU.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID to destroy.
   --  @return True on success.
   function VCPU_Destroy (Mach : Machine_ID; CPU : VCPU_ID) return Boolean;

   --  Run a VCPU until VMEXIT.
   --  @param Mach       The machine ID.
   --  @param CPU        The VCPU ID to run.
   --  @param Exit_Code  Set to the VMEXIT reason on return.
   --  @return True on success.
   function VCPU_Run
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Code : out Unsigned_64) return Boolean;

   --  Get VCPU state flags (must match nvmm_x86.h NVMM_X64_STATE_* values)
   VCPU_STATE_SEGS   : constant := 16#0001#;  --  Segment registers
   VCPU_STATE_GPRS   : constant := 16#0002#;  --  General purpose registers
   VCPU_STATE_CRS    : constant := 16#0004#;  --  Control registers
   VCPU_STATE_DRS    : constant := 16#0008#;  --  Debug registers
   VCPU_STATE_MSRS   : constant := 16#0010#;  --  MSRs
   VCPU_STATE_INTR   : constant := 16#0020#;  --  Interrupt state
   VCPU_STATE_FPU    : constant := 16#0040#;  --  FPU state
   VCPU_STATE_ALL    : constant := 16#007F#;

   --  GPR array type for userland transfer (18 64-bit registers)
   type NVMM_GPR_Array is array (0 .. 17) of Unsigned_64;

   --  Segment register structure (matches nvmm_x64_state_seg, 16 bytes)
   type NVMM_Segment is record
      Selector : Unsigned_16;
      Attrib   : Unsigned_16;  --  Packed bitfield
      Limit    : Unsigned_32;
      Base     : Unsigned_64;
   end record;
   for NVMM_Segment use record
      Selector at 0 range 0 .. 15;
      Attrib   at 2 range 0 .. 15;
      Limit    at 4 range 0 .. 31;
      Base     at 8 range 0 .. 63;
   end record;
   for NVMM_Segment'Size use 128;

   --  Segment array (10 segments: ES,CS,SS,DS,FS,GS,GDT,IDT,LDT,TR)
   type NVMM_Seg_Array is array (0 .. 9) of NVMM_Segment;

   --  Control register array (6 CRs to match libnvmm)
   type NVMM_CR_Array is array (0 .. 5) of Unsigned_64;

   --  MSR array (11 MSRs)
   type NVMM_MSR_Array is array (0 .. 10) of Unsigned_64;

   --  DR array (6 debug registers to match libnvmm)
   type NVMM_DR_Array is array (0 .. 5) of Unsigned_64;

   --  FPU state type for userland transfer (512 bytes, fxsave format)
   type NVMM_FPU_State is array (0 .. 511) of Unsigned_8
      with Alignment => 16;

   --  Event types for injection (matches nvmm_x86.h)
   NVMM_EVENT_INTERRUPT_HW : constant := 0;  --  Hardware interrupt
   NVMM_EVENT_INTERRUPT_SW : constant := 1;  --  Software interrupt (INT n)
   NVMM_EVENT_EXCEPTION    : constant := 2;  --  Exception
   NVMM_EVENT_NMI          : constant := 3;  --  Non-maskable interrupt

   --  Event info for injection
   type NVMM_Event_Info is record
      Event_Type : Unsigned_32;  --  NVMM_EVENT_* constant
      Vector     : Unsigned_8;   --  Interrupt/exception vector
      Has_Error  : Boolean;      --  True if error code is valid
      Error_Code : Unsigned_64;  --  Error code (for exceptions)
   end record;

   --  GPR indices
   NVMM_X64_GPR_RAX : constant := 0;
   NVMM_X64_GPR_RCX : constant := 1;
   NVMM_X64_GPR_RDX : constant := 2;
   NVMM_X64_GPR_RBX : constant := 3;
   NVMM_X64_GPR_RSP : constant := 4;
   NVMM_X64_GPR_RBP : constant := 5;
   NVMM_X64_GPR_RSI : constant := 6;
   NVMM_X64_GPR_RDI : constant := 7;
   NVMM_X64_GPR_R8  : constant := 8;
   NVMM_X64_GPR_R9  : constant := 9;
   NVMM_X64_GPR_R10 : constant := 10;
   NVMM_X64_GPR_R11 : constant := 11;
   NVMM_X64_GPR_R12 : constant := 12;
   NVMM_X64_GPR_R13 : constant := 13;
   NVMM_X64_GPR_R14 : constant := 14;
   NVMM_X64_GPR_R15 : constant := 15;
   NVMM_X64_GPR_RIP : constant := 16;
   NVMM_X64_GPR_RFLAGS : constant := 17;

   --  Get VCPU GPRs to kernel buffer (safe for userland transfer)
   function VCPU_Get_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : out NVMM_GPR_Array) return Boolean;

   --  Set VCPU GPRs from kernel buffer (safe for userland transfer)
   function VCPU_Set_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : NVMM_GPR_Array) return Boolean;

   --  Get VCPU FPU state to kernel buffer
   function VCPU_Get_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : out NVMM_FPU_State) return Boolean;

   --  Set VCPU FPU state from kernel buffer
   function VCPU_Set_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : NVMM_FPU_State) return Boolean;

   --  Get VCPU segments to kernel buffer
   function VCPU_Get_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : out NVMM_Seg_Array) return Boolean;

   --  Set VCPU segments from kernel buffer
   function VCPU_Set_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : NVMM_Seg_Array) return Boolean;

   --  Get VCPU CRs to kernel buffer
   function VCPU_Get_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : out NVMM_CR_Array) return Boolean;

   --  Set VCPU CRs from kernel buffer
   function VCPU_Set_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : NVMM_CR_Array) return Boolean;

   --  Get VCPU MSRs to kernel buffer
   function VCPU_Get_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : out NVMM_MSR_Array) return Boolean;

   --  Set VCPU MSRs from kernel buffer
   function VCPU_Set_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : NVMM_MSR_Array) return Boolean;

   --  Inject an event (interrupt/exception) into the VCPU
   --  The event will be delivered on the next VCPU_Run
   function VCPU_Inject_Event
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Event : NVMM_Event_Info) return Boolean;

   --  Set VCPU state from userspace buffer.
   --  @param Mach   The machine ID.
   --  @param CPU    The VCPU ID.
   --  @param Flags  Which state to set.
   --  @param Addr   Address of state buffer.
   --  @return True on success.
   function VCPU_Set_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean;

   --  Get VCPU state to userspace buffer.
   --  @param Mach   The machine ID.
   --  @param CPU    The VCPU ID.
   --  @param Flags  Which state to get.
   --  @param Addr   Address of state buffer.
   --  @return True on success.
   function VCPU_Get_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean;

   ----------------------------------------------------------------------------
   --  Memory management
   ----------------------------------------------------------------------------

   --  Protection flags for GPA mapping
   GPA_PROT_READ  : constant := 16#01#;
   GPA_PROT_WRITE : constant := 16#02#;
   GPA_PROT_EXEC  : constant := 16#04#;
   GPA_PROT_USER  : constant := 16#08#;

   ----------------------------------------------------------------------------
   --  NVMM Exit Codes (translated from hardware-specific codes)
   ----------------------------------------------------------------------------
   NVMM_EXIT_NONE       : constant := 16#0000_0000_0000_0000#;
   NVMM_EXIT_STOPPED    : constant := 16#FFFF_FFFF_FFFF_FFFE#;
   NVMM_EXIT_INVALID    : constant := 16#FFFF_FFFF_FFFF_FFFF#;
   NVMM_EXIT_MEMORY     : constant := 16#0000_0000_0000_0001#;  --  NPF
   NVMM_EXIT_IO         : constant := 16#0000_0000_0000_0002#;  --  I/O
   NVMM_EXIT_SHUTDOWN   : constant := 16#0000_0000_0000_1000#;
   NVMM_EXIT_INT_READY  : constant := 16#0000_0000_0000_1001#;
   NVMM_EXIT_NMI_READY  : constant := 16#0000_0000_0000_1002#;
   NVMM_EXIT_HALTED     : constant := 16#0000_0000_0000_1003#;
   NVMM_EXIT_RDMSR      : constant := 16#0000_0000_0000_2000#;
   NVMM_EXIT_WRMSR      : constant := 16#0000_0000_0000_2001#;
   NVMM_EXIT_MONITOR    : constant := 16#0000_0000_0000_2002#;
   NVMM_EXIT_MWAIT      : constant := 16#0000_0000_0000_2003#;
   NVMM_EXIT_CPUID      : constant := 16#0000_0000_0000_2004#;

   ----------------------------------------------------------------------------
   --  Exit information structure (matches userspace nvmm_x86_exit)
   ----------------------------------------------------------------------------
   type Exit_IO_Info is record
      Is_In        : Boolean;
      Port         : Unsigned_16;
      Segment      : Integer_8;
      Address_Size : Unsigned_8;
      Operand_Size : Unsigned_8;
      Is_Rep       : Boolean;
      Is_String    : Boolean;
      Next_RIP     : Unsigned_64;
   end record;

   type Exit_MSR_Read_Info is record
      MSR_Num  : Unsigned_32;
      Next_RIP : Unsigned_64;
   end record;

   type Exit_MSR_Write_Info is record
      MSR_Num  : Unsigned_32;
      MSR_Val  : Unsigned_64;
      Next_RIP : Unsigned_64;
   end record;

   --  Instruction bytes array for memory exit (must match C uint8_t[15])
   type Inst_Bytes_Array is array (0 .. 14) of Unsigned_8
      with Pack;

   type Exit_Memory_Info is record
      Prot       : Integer;
      GPA        : Unsigned_64;
      Inst_Len   : Unsigned_8;
      Inst_Bytes : Inst_Bytes_Array;  --  15 bytes of instruction data
   end record
      with Pack;

   type Exit_Insn_Info is record
      Next_RIP : Unsigned_64;
   end record;

   type Exit_Invalid_Info is record
      HW_Code : Unsigned_64;
   end record;

   type Exit_State_Info is record
      RFLAGS           : Unsigned_64;
      CR8              : Unsigned_64;
      Int_Shadow       : Boolean;
      Int_Window_Exit  : Boolean;
      NMI_Window_Exit  : Boolean;
      Evt_Pending      : Boolean;
   end record;

   --  Exit union - all variants overlap in memory like C union
   --  The largest variant is Exit_Memory_Info at about 32 bytes
   type Exit_Union (Variant : Unsigned_64 := 0) is record
      case Variant is
         when NVMM_EXIT_MEMORY =>
            Memory : Exit_Memory_Info;
         when NVMM_EXIT_IO =>
            IO : Exit_IO_Info;
         when NVMM_EXIT_RDMSR =>
            MSR_Read : Exit_MSR_Read_Info;
         when NVMM_EXIT_WRMSR =>
            MSR_Write : Exit_MSR_Write_Info;
         when NVMM_EXIT_INVALID =>
            Invalid : Exit_Invalid_Info;
         when others =>
            Insn : Exit_Insn_Info;
      end case;
   end record
      with Unchecked_Union, Size => 32 * 8;  --  32 bytes to match C union

   type VCPU_Exit_Info is record
      Reason     : Unsigned_64;
      U          : Exit_Union;
      Exit_State : Exit_State_Info;
   end record;

   --  Enhanced VCPU_Run that populates exit info
   function VCPU_Run_Ex
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : out VCPU_Exit_Info) return Boolean;

   function VCPU_Run_Ex_VMX
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : in out VCPU_Exit_Info) return Boolean;

   --  Map a host virtual address to a guest physical address.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID (for NPT access).
   --  @param HVA   Host virtual address to map.
   --  @param GPA   Guest physical address to map to.
   --  @param Size  Size of the mapping in bytes.
   --  @param Prot  Protection flags (GPA_PROT_*).
   --  @return True on success.
   function GPA_Map
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

   --  Map a guest physical address range for ALL active VCPUs.
   --  This should be used when mapping memory shared by multiple VCPUs.
   --  @param Mach  The machine ID.
   --  @param HVA   Host virtual address.
   --  @param GPA   Guest physical address.
   --  @param Size  Size in bytes.
   --  @param Prot  Protection flags.
   --  @return True if at least one VCPU was mapped, False otherwise.
   function GPA_Map_All
      (Mach : Machine_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

   --  Unmap a guest physical address range.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID.
   --  @param GPA   Guest physical address to unmap.
   --  @param Size  Size of the mapping in bytes.
   --  @return True on success.
   function GPA_Unmap
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean;

   --  Unmap a guest physical address range for ALL active VCPUs.
   function GPA_Unmap_All
      (Mach : Machine_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean;

   ----------------------------------------------------------------------------
   --  Address translation
   ----------------------------------------------------------------------------

   --  Translate guest physical address to host virtual address.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID (for NPT access).
   --  @param GPA   Guest physical address to translate.
   --  @param HVA   Output: Host virtual address.
   --  @return True on success, False if GPA is not mapped.
   function GPA_To_HVA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       HVA  : out Unsigned_64) return Boolean;

   --  Translate guest virtual address to guest physical address.
   --  Walks guest page tables to perform translation.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID.
   --  @param GVA   Guest virtual address to translate.
   --  @param GPA   Output: Guest physical address.
   --  @return True on success, False if translation fails.
   function GVA_To_GPA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GVA  : Unsigned_64;
       GPA  : out Unsigned_64) return Boolean;

   ----------------------------------------------------------------------------
   --  VCPU control
   ----------------------------------------------------------------------------

   --  Request a running VCPU to stop.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID.
   --  @return True on success.
   function VCPU_Stop
      (Mach : Machine_ID;
       CPU  : VCPU_ID) return Boolean;

   --  VCPU mode values for VCPU_Set_Mode
   VCPU_MODE_32BIT : constant := 0;  --  32-bit protected mode
   VCPU_MODE_64BIT : constant := 1;  --  64-bit long mode
   VCPU_MODE_REAL  : constant := 2;  --  16-bit real mode
   VCPU_MODE_VM86  : constant := 3;  --  Virtual 8086 mode

   --  Set VCPU execution mode.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID.
   --  @param Mode  VCPU_MODE_32BIT, VCPU_MODE_64BIT, VCPU_MODE_REAL, or
   --               VCPU_MODE_VM86.
   --  @return True on success.
   function VCPU_Set_Mode
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Mode : Unsigned_32) return Boolean;

   ----------------------------------------------------------------------------
   --  I/O port passthrough control
   ----------------------------------------------------------------------------

   --  Set I/O port interception.
   --  @param Mach       The machine ID.
   --  @param CPU        The VCPU ID.
   --  @param Port       The I/O port number (0-65535).
   --  @param Intercept  True to intercept (VMEXIT), False for passthrough.
   --  @return True on success.
   function Set_IO_Port_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Port      : Unsigned_16;
       Intercept : Boolean) return Boolean;

   ----------------------------------------------------------------------------
   --  MSR passthrough control
   ----------------------------------------------------------------------------

   --  Set MSR interception.
   --  @param Mach       The machine ID.
   --  @param CPU        The VCPU ID.
   --  @param MSR_Num    The MSR number.
   --  @param Intercept  True to intercept (VMEXIT), False for passthrough.
   --  @param Is_Write   Whether to do read or white interception.
   --  @return True on success.
   function Set_MSR_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       MSR_Num   : Unsigned_32;
       Intercept : Boolean;
       Is_Write  : Boolean) return Boolean;

   ----------------------------------------------------------------------------
   --  TLB flush control
   ----------------------------------------------------------------------------

   --  TLB control values
   TLB_CONTROL_DO_NOTHING    : constant := 0;  --  No TLB flush
   TLB_CONTROL_FLUSH_ALL     : constant := 1;  --  Flush all TLB entries
   TLB_CONTROL_FLUSH_GUEST   : constant := 3;  --  Flush guest TLB entries
   TLB_CONTROL_FLUSH_NONGLOBAL : constant := 7;  --  Flush non-global entries

   --  Set TLB control for next VCPU run.
   --  @param Mach     The machine ID.
   --  @param CPU      The VCPU ID.
   --  @param Control  TLB_CONTROL_* value.
   --  @return True on success.
   function Set_TLB_Control
      (Mach    : Machine_ID;
       CPU     : VCPU_ID;
       Control : Unsigned_32) return Boolean;

   ----------------------------------------------------------------------------
   --  TSC offsetting
   ----------------------------------------------------------------------------

   --  Set TSC offset for a VCPU.
   --  @param Mach    The machine ID.
   --  @param CPU     The VCPU ID.
   --  @param Offset  The TSC offset value (signed).
   --  @return True on success.
   function Set_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : Unsigned_64) return Boolean;

   --  Get TSC offset for a VCPU.
   --  @param Mach    The machine ID.
   --  @param CPU     The VCPU ID.
   --  @param Offset  Output: The current TSC offset value.
   --  @return True on success.
   function Get_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : out Unsigned_64) return Boolean;

   ----------------------------------------------------------------------------
   --  Large page NPT mapping (1GB pages)
   ----------------------------------------------------------------------------

   --  Map a 1GB region using a single NPT entry.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID.
   --  @param HVA   Host virtual address (must be 1GB aligned).
   --  @param GPA   Guest physical address (must be 1GB aligned).
   --  @param Prot  Protection flags (GPA_PROT_*).
   --  @return True on success.
   function GPA_Map_1GB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

   --  Map a 2MB region using a single NPT entry.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID.
   --  @param HVA   Host virtual address (must be 2MB aligned).
   --  @param GPA   Guest physical address (must be 2MB aligned).
   --  @param Prot  Protection flags (GPA_PROT_*).
   --  @return True on success.
   function GPA_Map_2MB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

private

   function Allocate_ASID return Unsigned_32;
   procedure Free_ASID (ASID : Unsigned_32);

   function VMCB_To_NVMM_Attrib (A : Unsigned_16) return Unsigned_16;
   function NVMM_To_VMCB_Attrib (A : Unsigned_16) return Unsigned_16;

   function To_VMX_AR
      (NVMM_Attrib : Unsigned_16;
       Limit       : Unsigned_32) return Unsigned_64;

   function Get_Attrib_Raw (S : NVMM_Segment) return Unsigned_16;
end Virtualization;
