--  virtualization.ads: Virtualization stub for RISC-V (not supported).
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

with Interfaces; use Interfaces;
with System;

package Virtualization with SPARK_Mode => Off is
   --  RISC-V stub: virtualization not supported on this architecture.

   NVMM_Version : constant := 0;

   State_Size           : constant := 0;
   Max_Virtual_Machines : constant := 0;
   Max_CPUs_Per_VM      : constant := 1;
   Max_RAM_Per_VM       : constant := 0;

   subtype Machine_ID is Unsigned_32 range 0 .. 0;
   Invalid_Machine : constant Machine_ID := 0;

   subtype VCPU_ID is Unsigned_32 range 0 .. 0;

   function Is_Supported return Boolean;

   procedure Initialize;

   function Machine_Create return Machine_ID;
   function Machine_Destroy (ID : Machine_ID) return Boolean;

   function VCPU_Create (Mach : Machine_ID; CPU : VCPU_ID) return Boolean;
   function VCPU_Destroy (Mach : Machine_ID; CPU : VCPU_ID) return Boolean;

   function VCPU_Run
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Code : out Unsigned_64) return Boolean;

   VCPU_STATE_SEGS   : constant := 16#0001#;
   VCPU_STATE_GPRS   : constant := 16#0002#;
   VCPU_STATE_CRS    : constant := 16#0004#;
   VCPU_STATE_DRS    : constant := 16#0008#;
   VCPU_STATE_MSRS   : constant := 16#0010#;
   VCPU_STATE_INTR   : constant := 16#0020#;
   VCPU_STATE_FPU    : constant := 16#0040#;
   VCPU_STATE_ALL    : constant := 16#007F#;

   type NVMM_GPR_Array is array (0 .. 17) of Unsigned_64;

   type NVMM_Segment is record
      Selector : Unsigned_16;
      Attrib   : Unsigned_16;
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

   type NVMM_Seg_Array is array (0 .. 9) of NVMM_Segment;
   type NVMM_CR_Array is array (0 .. 5) of Unsigned_64;
   type NVMM_MSR_Array is array (0 .. 10) of Unsigned_64;
   type NVMM_DR_Array is array (0 .. 5) of Unsigned_64;

   type NVMM_FPU_State is array (0 .. 511) of Unsigned_8
      with Alignment => 16;

   NVMM_EVENT_INTERRUPT_HW : constant := 0;
   NVMM_EVENT_INTERRUPT_SW : constant := 1;
   NVMM_EVENT_EXCEPTION    : constant := 2;
   NVMM_EVENT_NMI          : constant := 3;

   type NVMM_Event_Info is record
      Event_Type : Unsigned_32;
      Vector     : Unsigned_8;
      Has_Error  : Boolean;
      Error_Code : Unsigned_64;
   end record;

   NVMM_X64_GPR_RAX    : constant := 0;
   NVMM_X64_GPR_RCX    : constant := 1;
   NVMM_X64_GPR_RDX    : constant := 2;
   NVMM_X64_GPR_RBX    : constant := 3;
   NVMM_X64_GPR_RSP    : constant := 4;
   NVMM_X64_GPR_RBP    : constant := 5;
   NVMM_X64_GPR_RSI    : constant := 6;
   NVMM_X64_GPR_RDI    : constant := 7;
   NVMM_X64_GPR_R8     : constant := 8;
   NVMM_X64_GPR_R9     : constant := 9;
   NVMM_X64_GPR_R10    : constant := 10;
   NVMM_X64_GPR_R11    : constant := 11;
   NVMM_X64_GPR_R12    : constant := 12;
   NVMM_X64_GPR_R13    : constant := 13;
   NVMM_X64_GPR_R14    : constant := 14;
   NVMM_X64_GPR_R15    : constant := 15;
   NVMM_X64_GPR_RIP    : constant := 16;
   NVMM_X64_GPR_RFLAGS : constant := 17;

   function VCPU_Get_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : out NVMM_GPR_Array) return Boolean;

   function VCPU_Set_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : NVMM_GPR_Array) return Boolean;

   function VCPU_Get_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : out NVMM_FPU_State) return Boolean;

   function VCPU_Set_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : NVMM_FPU_State) return Boolean;

   function VCPU_Get_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : out NVMM_Seg_Array) return Boolean;

   function VCPU_Set_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : NVMM_Seg_Array) return Boolean;

   function VCPU_Get_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : out NVMM_CR_Array) return Boolean;

   function VCPU_Set_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : NVMM_CR_Array) return Boolean;

   function VCPU_Get_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : out NVMM_MSR_Array) return Boolean;

   function VCPU_Set_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : NVMM_MSR_Array) return Boolean;

   function VCPU_Inject_Event
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Event : NVMM_Event_Info) return Boolean;

   function VCPU_Set_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean;

   function VCPU_Get_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean;

   GPA_PROT_READ  : constant := 16#01#;
   GPA_PROT_WRITE : constant := 16#02#;
   GPA_PROT_EXEC  : constant := 16#04#;
   GPA_PROT_USER  : constant := 16#08#;

   NVMM_EXIT_NONE      : constant := 16#0000_0000_0000_0000#;
   NVMM_EXIT_STOPPED   : constant := 16#FFFF_FFFF_FFFF_FFFE#;
   NVMM_EXIT_INVALID   : constant := 16#FFFF_FFFF_FFFF_FFFF#;
   NVMM_EXIT_MEMORY    : constant := 16#0000_0000_0000_0001#;
   NVMM_EXIT_IO        : constant := 16#0000_0000_0000_0002#;
   NVMM_EXIT_SHUTDOWN  : constant := 16#0000_0000_0000_1000#;
   NVMM_EXIT_INT_READY : constant := 16#0000_0000_0000_1001#;
   NVMM_EXIT_NMI_READY : constant := 16#0000_0000_0000_1002#;
   NVMM_EXIT_HALTED    : constant := 16#0000_0000_0000_1003#;
   NVMM_EXIT_RDMSR     : constant := 16#0000_0000_0000_2000#;
   NVMM_EXIT_WRMSR     : constant := 16#0000_0000_0000_2001#;
   NVMM_EXIT_MONITOR   : constant := 16#0000_0000_0000_2002#;
   NVMM_EXIT_MWAIT     : constant := 16#0000_0000_0000_2003#;
   NVMM_EXIT_CPUID     : constant := 16#0000_0000_0000_2004#;

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

   type Inst_Bytes_Array is array (0 .. 14) of Unsigned_8
      with Pack;

   type Exit_Memory_Info is record
      Prot       : Integer;
      GPA        : Unsigned_64;
      Inst_Len   : Unsigned_8;
      Inst_Bytes : Inst_Bytes_Array;
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
      with Unchecked_Union, Size => 32 * 8;

   type VCPU_Exit_Info is record
      Reason     : Unsigned_64;
      U          : Exit_Union;
      Exit_State : Exit_State_Info;
   end record;

   function VCPU_Run_Ex
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : out VCPU_Exit_Info) return Boolean;

   function GPA_Map
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

   function GPA_Map_All
      (Mach : Machine_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

   function GPA_Unmap
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean;

   function GPA_Unmap_All
      (Mach : Machine_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean;

   function GPA_To_HVA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       HVA  : out Unsigned_64) return Boolean;

   function GVA_To_GPA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GVA  : Unsigned_64;
       GPA  : out Unsigned_64) return Boolean;

   function VCPU_Stop
      (Mach : Machine_ID;
       CPU  : VCPU_ID) return Boolean;

   VCPU_MODE_32BIT : constant := 0;
   VCPU_MODE_64BIT : constant := 1;
   VCPU_MODE_REAL  : constant := 2;
   VCPU_MODE_VM86  : constant := 3;

   function VCPU_Set_Mode
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Mode : Unsigned_32) return Boolean;

   --  I/O port passthrough control
   function Set_IO_Port_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Port      : Unsigned_16;
       Intercept : Boolean) return Boolean;

   --  MSR passthrough control
   function Set_MSR_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       MSR_Num   : Unsigned_32;
       Intercept : Boolean;
       Is_Write  : Boolean) return Boolean;

   --  TLB control values
   TLB_CONTROL_DO_NOTHING    : constant := 0;
   TLB_CONTROL_FLUSH_ALL     : constant := 1;
   TLB_CONTROL_FLUSH_GUEST   : constant := 3;
   TLB_CONTROL_FLUSH_NONGLOBAL : constant := 7;

   function Set_TLB_Control
      (Mach    : Machine_ID;
       CPU     : VCPU_ID;
       Control : Unsigned_32) return Boolean;

   --  TSC offsetting
   function Set_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : Unsigned_64) return Boolean;

   function Get_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : out Unsigned_64) return Boolean;

   --  Large page NPT mapping
   function GPA_Map_1GB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

   function GPA_Map_2MB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean;

end Virtualization;
