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
with Arch.Virtualization;

package Virtualization with SPARK_Mode => Off is
   --  This module implements a NVMM compatible interface, NVMM's spec
   --  can be found at the kernel's docs, or
   --  https://www.dragonflybsd.org/docs/docs/howtos/nvmm/

   --  NVMM version that this module implements. The meaning of these version
   --  numbers is provided at ... TODO: Add link.
   NVMM_Version : constant := 2;

   --  Capabilities of this implementation.
   State_Size           : constant := Arch.Virtualization.State_Size;
   Max_Virtual_Machines : constant := Arch.Virtualization.Max_Virtual_Machines;
   Max_CPUs_Per_VM      : constant := Arch.Virtualization.Max_CPUs_Per_VM;
   Max_RAM_Per_VM       : constant := Arch.Virtualization.Max_RAM_Per_VM;

   --  Machine ID type (0 = invalid)
   subtype Machine_ID is Arch.Virtualization.Machine_ID;
   Invalid_Machine : constant Machine_ID :=
      Arch.Virtualization.Invalid_Machine;

   --  VCPU ID type
   subtype VCPU_ID is Arch.Virtualization.VCPU_ID;
   ----------------------------------------------------------------------------
   --  Returns True if virtualization is supported.
   function Is_Supported return Boolean;

   --  Initialize virtualization if available, otherwise return silently.
   procedure Initialize;
   ----------------------------------------------------------------------------
   --  Create a new virtual machine, owned by the calling process; see
   --  Arch.Virtualization for what owning one means.
   --  @return Machine ID on success, Invalid_Machine on failure.
   function Machine_Create return Machine_ID;

   --  Destroy a virtual machine of the calling process.
   --  @param ID  The machine ID to destroy.
   --  @return True on success, False if ID is invalid or not the caller's.
   function Machine_Destroy (ID : Machine_ID) return Boolean;

   --  True if Mach exists and another process owns it.
   function Owned_By_Another (Mach : Machine_ID) return Boolean;

   --  Destroy every machine Owner has, for its exit or its exec.
   --  @param Owner The owner, as Userland.Process.Convert gives its PID.
   procedure Destroy_Owned (Owner : Natural);
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
   subtype NVMM_GPR_Array is Arch.Virtualization.NVMM_GPR_Array;

   --  Segment register structure (matches nvmm_x64_state_seg, 16 bytes)
   subtype NVMM_Segment is Arch.Virtualization.NVMM_Segment;

   --  Segment array, this is user ABI.
   subtype NVMM_Seg_Array is Arch.Virtualization.NVMM_Seg_Array;

   --  Control register array, this is user ABI.
   subtype NVMM_CR_Array is Arch.Virtualization.NVMM_CR_Array;

   --  MSR array, this is user ABI.
   subtype NVMM_MSR_Array is Arch.Virtualization.NVMM_MSR_Array;

   --  Debug Register array, this is user ABI.
   subtype NVMM_DR_Array is Arch.Virtualization.NVMM_DR_Array;

   --  FPU state type for userland transfer (512 bytes, fxsave format)
   subtype NVMM_FPU_State is Arch.Virtualization.NVMM_FPU_State;

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

   --  Event info for injection.
   NVMM_EXIT_NONE       : constant := Arch.Virtualization.NVMM_EXIT_NONE;
   NVMM_EXIT_STOPPED    : constant := Arch.Virtualization.NVMM_EXIT_STOPPED;
   NVMM_EXIT_INVALID    : constant := Arch.Virtualization.NVMM_EXIT_INVALID;
   NVMM_EXIT_MEMORY     : constant := Arch.Virtualization.NVMM_EXIT_MEMORY;
   NVMM_EXIT_IO         : constant := Arch.Virtualization.NVMM_EXIT_IO;
   NVMM_EXIT_SHUTDOWN   : constant := Arch.Virtualization.NVMM_EXIT_SHUTDOWN;
   NVMM_EXIT_INT_READY  : constant := Arch.Virtualization.NVMM_EXIT_INT_READY;
   NVMM_EXIT_NMI_READY  : constant := Arch.Virtualization.NVMM_EXIT_NMI_READY;
   NVMM_EXIT_HALTED     : constant := Arch.Virtualization.NVMM_EXIT_HALTED;
   NVMM_EXIT_RDMSR      : constant := Arch.Virtualization.NVMM_EXIT_RDMSR;
   NVMM_EXIT_WRMSR      : constant := Arch.Virtualization.NVMM_EXIT_WRMSR;
   NVMM_EXIT_MONITOR    : constant := Arch.Virtualization.NVMM_EXIT_MONITOR;
   NVMM_EXIT_MWAIT      : constant := Arch.Virtualization.NVMM_EXIT_MWAIT;
   NVMM_EXIT_CPUID      : constant := Arch.Virtualization.NVMM_EXIT_CPUID;

   subtype NVMM_Event_Info is Arch.Virtualization.NVMM_Event_Info;

   --  Inject an event (interrupt/exception) into the VCPU
   --  The event will be delivered on the next VCPU_Run
   function VCPU_Inject_Event
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Event : NVMM_Event_Info) return Boolean;

   --  NVMM Exit Codes (translated from hardware-specific codes)
   subtype VCPU_Exit_Info is Arch.Virtualization.VCPU_Exit_Info;

   --  Run a VCPU until VMEXIT.
   --  @param Mach       The machine ID.
   --  @param CPU        The VCPU ID to run.
   --  @param Exit_Info  VMEXIT reason on return, among other info.
   --  @return True on success.
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
   subtype GPA_Flags is Arch.Virtualization.GPA_Flags;

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
end Virtualization;
