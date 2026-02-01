--  virtualization.adb: Virtualization stub for RISC-V (not supported).
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

package body Virtualization is
   --  RISC-V stub: all virtualization functions return failure.

   function Is_Supported return Boolean is
   begin
      return False;
   end Is_Supported;

   procedure Initialize is
   begin
      null;
   end Initialize;

   function Machine_Create return Machine_ID is
   begin
      return Invalid_Machine;
   end Machine_Create;

   function Machine_Destroy (ID : Machine_ID) return Boolean is
      pragma Unreferenced (ID);
   begin
      return False;
   end Machine_Destroy;

   function VCPU_Create (Mach : Machine_ID; CPU : VCPU_ID) return Boolean is
      pragma Unreferenced (Mach, CPU);
   begin
      return False;
   end VCPU_Create;

   function VCPU_Destroy (Mach : Machine_ID; CPU : VCPU_ID) return Boolean is
      pragma Unreferenced (Mach, CPU);
   begin
      return False;
   end VCPU_Destroy;

   function VCPU_Run
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Code : out Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      Exit_Code := NVMM_EXIT_INVALID;
      return False;
   end VCPU_Run;

   function VCPU_Get_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : out NVMM_GPR_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      GPRs := [others => 0];
      return False;
   end VCPU_Get_GPRs;

   function VCPU_Set_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : NVMM_GPR_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU, GPRs);
   begin
      return False;
   end VCPU_Set_GPRs;

   function VCPU_Get_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : out NVMM_FPU_State) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      FPU := [others => 0];
      return False;
   end VCPU_Get_FPU;

   function VCPU_Set_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : NVMM_FPU_State) return Boolean
   is
      pragma Unreferenced (Mach, CPU, FPU);
   begin
      return False;
   end VCPU_Set_FPU;

   function VCPU_Get_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : out NVMM_Seg_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      Segs := [others => (Selector => 0, Attrib => 0, Limit => 0, Base => 0)];
      return False;
   end VCPU_Get_Segs;

   function VCPU_Set_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : NVMM_Seg_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Segs);
   begin
      return False;
   end VCPU_Set_Segs;

   function VCPU_Get_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : out NVMM_CR_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      CRs := [others => 0];
      return False;
   end VCPU_Get_CRs;

   function VCPU_Set_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : NVMM_CR_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU, CRs);
   begin
      return False;
   end VCPU_Set_CRs;

   function VCPU_Get_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : out NVMM_MSR_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      MSRs := [others => 0];
      return False;
   end VCPU_Get_MSRs;

   function VCPU_Set_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : NVMM_MSR_Array) return Boolean
   is
      pragma Unreferenced (Mach, CPU, MSRs);
   begin
      return False;
   end VCPU_Set_MSRs;

   function VCPU_Inject_Event
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Event : NVMM_Event_Info) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Event);
   begin
      return False;
   end VCPU_Inject_Event;

   function VCPU_Set_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Flags, Addr);
   begin
      return False;
   end VCPU_Set_State;

   function VCPU_Get_State
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Flags : Unsigned_64;
       Addr  : System.Address) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Flags, Addr);
   begin
      return False;
   end VCPU_Get_State;

   function VCPU_Run_Ex
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : out VCPU_Exit_Info) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      Exit_Info := (Reason     => NVMM_EXIT_INVALID,
                    U          => (Variant => 0,
                                   Insn    => (Next_RIP => 0)),
                    Exit_State => (RFLAGS          => 0,
                                   CR8             => 0,
                                   Int_Shadow      => False,
                                   Int_Window_Exit => False,
                                   NMI_Window_Exit => False,
                                   Evt_Pending     => False));
      return False;
   end VCPU_Run_Ex;

   function GPA_Map
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU, HVA, GPA, Size, Prot);
   begin
      return False;
   end GPA_Map;

   function GPA_Map_All
      (Mach : Machine_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, HVA, GPA, Size, Prot);
   begin
      return False;
   end GPA_Map_All;

   function GPA_Unmap
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU, GPA, Size);
   begin
      return False;
   end GPA_Unmap;

   function GPA_Unmap_All
      (Mach : Machine_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, GPA, Size);
   begin
      return False;
   end GPA_Unmap_All;

   function GPA_To_HVA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       HVA  : out Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU, GPA);
   begin
      HVA := 0;
      return False;
   end GPA_To_HVA;

   function GVA_To_GPA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GVA  : Unsigned_64;
       GPA  : out Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU, GVA);
   begin
      GPA := 0;
      return False;
   end GVA_To_GPA;

   function VCPU_Stop
      (Mach : Machine_ID;
       CPU  : VCPU_ID) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      return False;
   end VCPU_Stop;

   function VCPU_Set_Mode
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Mode : Unsigned_32) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Mode);
   begin
      return False;
   end VCPU_Set_Mode;

   function Set_IO_Port_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Port      : Unsigned_16;
       Intercept : Boolean) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Port, Intercept);
   begin
      return False;
   end Set_IO_Port_Intercept;

   function Set_MSR_Intercept
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       MSR_Num   : Unsigned_32;
       Intercept : Boolean;
       Is_Write  : Boolean) return Boolean
   is
      pragma Unreferenced (Mach, CPU, MSR_Num, Intercept, Is_Write);
   begin
      return False;
   end Set_MSR_Intercept;

   function Set_TLB_Control
      (Mach    : Machine_ID;
       CPU     : VCPU_ID;
       Control : Unsigned_32) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Control);
   begin
      return False;
   end Set_TLB_Control;

   function Set_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU, Offset);
   begin
      return False;
   end Set_TSC_Offset;

   function Get_TSC_Offset
      (Mach   : Machine_ID;
       CPU    : VCPU_ID;
       Offset : out Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU);
   begin
      Offset := 0;
      return False;
   end Get_TSC_Offset;

   function GPA_Map_1GB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU, HVA, GPA, Prot);
   begin
      return False;
   end GPA_Map_1GB;

   function GPA_Map_2MB
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Prot : Unsigned_64) return Boolean
   is
      pragma Unreferenced (Mach, CPU, HVA, GPA, Prot);
   begin
      return False;
   end GPA_Map_2MB;

end Virtualization;
