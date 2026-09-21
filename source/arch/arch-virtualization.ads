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

package Arch.Virtualization with SPARK_Mode => Off is
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

   --  Initialize virtualization if available, otherwise return silently.
   procedure Initialize;

   --  Enable hardware virtualization on the calling core, which every core
   --  that may run a VCPU needs. A core already enabled is left alone.
   procedure Enable_For_This_Core;
   ----------------------------------------------------------------------------
   --  Create a new virtual machine.
   --  @return Machine ID on success, Invalid_Machine on failure.
   function Machine_Create return Machine_ID;

   --  Destroy a virtual machine.
   --  @param ID  The machine ID to destroy.
   --  @return True on success, False if ID is invalid.
   function Machine_Destroy (ID : Machine_ID) return Boolean;
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

   --  GPR array type for userland transfer (18 64-bit registers)
   --  This is user ABI, dont change structure!
   type NVMM_GPR_Array is record
      RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9 : Unsigned_64;
      R10, R11, R12, R13, R14, R15, RIP, RFLAGS : Unsigned_64;
   end record;

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

   --  Segment array, this is user ABI.
   type NVMM_Seg_Array is record
      ES, CS, SS, DS, FS, GS, GDT, IDT, LDT, TR : NVMM_Segment;
   end record;

   --  Control register array, this is user ABI.
   type NVMM_CR_Array is record
      CR0, CR2, CR3, CR4 : Unsigned_64;
      Placeholder1, Placeholder2 : Unsigned_64;
   end record;

   --  MSR array, this is user ABI.
   type NVMM_MSR_Array is record
      EFER, STAR, LSTAR, CSTAR, SFMASK, KERNELGSBASE : Unsigned_64;
      SYSENTER_CS, SYSENTER_ESP, SYSENTER_EIP, PAT : Unsigned_64;
   end record;

   --  Debug Register array, this is user ABI.
   type NVMM_DR_Array is record
      DR0, DR1, DR2, DR3, DR6, DR7 : Unsigned_64;
   end record;

   --  FPU state type for userland transfer (512 bytes, fxsave format)
   type NVMM_FPU_State is array (0 .. 511) of Unsigned_8 with Alignment => 16;

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

   --  Event types for injection (is user ABI).
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

   --  Inject an event (interrupt/exception) into the VCPU
   --  The event will be delivered on the next VCPU_Run
   function VCPU_Inject_Event
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Event : NVMM_Event_Info) return Boolean;

   --  NVMM Exit Codes (translated from hardware-specific codes)
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

   --  Exit information structure (matches userspace nvmm_x86_exit)
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
   type Inst_Bytes_Array is array (0 .. 14) of Unsigned_8 with Pack;

   type Exit_Memory_Info is record
      Prot       : Integer;
      GPA        : Unsigned_64;
      Inst_Len   : Unsigned_8;
      Inst_Bytes : Inst_Bytes_Array;  --  15 bytes of instruction data
   end record with Pack;

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

   --  Exit C-style union.
   type Exit_Union (Variant : Unsigned_64 := 0) is record
      case Variant is
         when NVMM_EXIT_MEMORY =>  Memory    : Exit_Memory_Info;
         when NVMM_EXIT_IO =>      IO        : Exit_IO_Info;
         when NVMM_EXIT_RDMSR =>   MSR_Read  : Exit_MSR_Read_Info;
         when NVMM_EXIT_WRMSR =>   MSR_Write : Exit_MSR_Write_Info;
         when NVMM_EXIT_INVALID => Invalid   : Exit_Invalid_Info;
         when others =>            Insn      : Exit_Insn_Info;
      end case;
   end record with Unchecked_Union, Size => 32 * 8;

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

   --  Request a running VCPU to stop.
   --  @param Mach  The machine ID.
   --  @param CPU   The VCPU ID.
   --  @return True on success.
   function VCPU_Stop
      (Mach : Machine_ID;
       CPU  : VCPU_ID) return Boolean;
   ----------------------------------------------------------------------------
   --  Protection flags for GPA mapping
   type GPA_Flags is record
      Can_Read : Boolean;
      Can_Write : Boolean;
      Can_Exec : Boolean;
      Is_User_Accessible : Boolean;
   end record;

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
       Prot : GPA_Flags) return Boolean;

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
       Prot : GPA_Flags) return Boolean;

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

private

   #if ArchName = """x86_64-limine"""
      function Allocate_ASID return Unsigned_32;
      procedure Free_ASID (ASID : Unsigned_32);

      function VMCB_To_NVMM_Attrib (A : Unsigned_16) return Unsigned_16;
      function NVMM_To_VMCB_Attrib (A : Unsigned_16) return Unsigned_16;

      function To_VMX_AR
         (NVMM_Attrib : Unsigned_16;
          Limit       : Unsigned_32) return Unsigned_64;

      function Get_Attrib_Raw (S : NVMM_Segment) return Unsigned_16;

      function GPA_To_HVA
         (Mach : Machine_ID;
          CPU  : VCPU_ID;
          GPA  : Unsigned_64;
          HVA  : out Unsigned_64) return Boolean;

      --  True if a guest may have Value in XCR0: the kernel loads it with
      --  XSETBV on the guest's behalf, so no value may be one that XSETBV
      --  refuses with #GP.
      function Guest_XCR0_Valid (Value : Unsigned_64) return Boolean;

      --  Save the host's FPU state and load the guest's, just before an
      --  entry, answering in Host_XCR0 the XCR0 Leave_Guest_FPU puts back.
      --  Called with the machine's lock held, as is Leave_Guest_FPU, which
      --  saves the guest's state and loads the host's again after the exit.
      procedure Enter_Guest_FPU
         (Mach      : Machine_ID;
          CPU       : VCPU_ID;
          Host_XCR0 : out Unsigned_64);
      procedure Leave_Guest_FPU
         (Mach      : Machine_ID;
          CPU       : VCPU_ID;
          Host_XCR0 : Unsigned_64);

      function VCPU_Run_Ex_VMX
         (Mach      : Machine_ID;
          CPU       : VCPU_ID;
          Exit_Info : in out VCPU_Exit_Info) return Boolean;
   #end if;
end Arch.Virtualization;
