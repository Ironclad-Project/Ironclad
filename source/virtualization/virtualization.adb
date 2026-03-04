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

package body Virtualization with SPARK_Mode => Off is
   function Is_Supported return Boolean is
   begin
      return Arch.Virtualization.Is_Supported;
   end Is_Supported;

   procedure Initialize is
   begin
      Arch.Virtualization.Initialize;
   end Initialize;
   ----------------------------------------------------------------------------
   function Machine_Create return Machine_ID is
   begin
      return Arch.Virtualization.Machine_Create;
   end Machine_Create;

   function Machine_Destroy (ID : Machine_ID) return Boolean is
   begin
      return Arch.Virtualization.Machine_Destroy (ID);
   end Machine_Destroy;
   ----------------------------------------------------------------------------
   function VCPU_Create (Mach : Machine_ID; CPU : VCPU_ID) return Boolean is
   begin
      return Arch.Virtualization.VCPU_Create (Mach, CPU);
   end VCPU_Create;

   function VCPU_Destroy (Mach : Machine_ID; CPU : VCPU_ID) return Boolean is
   begin
      return Arch.Virtualization.VCPU_Destroy (Mach, CPU);
   end VCPU_Destroy;

   function VCPU_Run
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Code : out Unsigned_64) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Run (Mach, CPU, Exit_Code);
   end VCPU_Run;

   function VCPU_Get_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : out NVMM_GPR_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Get_GPRs (Mach, CPU, GPRs);
   end VCPU_Get_GPRs;

   function VCPU_Set_GPRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPRs : NVMM_GPR_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Set_GPRs (Mach, CPU, GPRs);
   end VCPU_Set_GPRs;

   function VCPU_Get_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : out NVMM_FPU_State) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Get_FPU (Mach, CPU, FPU);
   end VCPU_Get_FPU;

   function VCPU_Set_FPU
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       FPU  : NVMM_FPU_State) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Set_FPU (Mach, CPU, FPU);
   end VCPU_Set_FPU;

   function VCPU_Get_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : out NVMM_Seg_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Get_Segs (Mach, CPU, Segs);
   end VCPU_Get_Segs;

   function VCPU_Set_Segs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       Segs : NVMM_Seg_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Set_Segs (Mach, CPU, Segs);
   end VCPU_Set_Segs;

   function VCPU_Get_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : out NVMM_CR_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Get_CRs (Mach, CPU, CRs);
   end VCPU_Get_CRs;

   --  Set VCPU CRs from kernel buffer
   function VCPU_Set_CRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       CRs  : NVMM_CR_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Set_CRs (Mach, CPU, CRs);
   end VCPU_Set_CRs;

   --  Get VCPU MSRs to kernel buffer
   function VCPU_Get_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : out NVMM_MSR_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Get_MSRs (Mach, CPU, MSRs);
   end VCPU_Get_MSRs;

   function VCPU_Set_MSRs
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       MSRs : NVMM_MSR_Array) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Set_MSRs (Mach, CPU, MSRs);
   end VCPU_Set_MSRs;

   function VCPU_Inject_Event
      (Mach  : Machine_ID;
       CPU   : VCPU_ID;
       Event : NVMM_Event_Info) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Inject_Event (Mach, CPU, Event);
   end VCPU_Inject_Event;

   function VCPU_Run_Ex
      (Mach      : Machine_ID;
       CPU       : VCPU_ID;
       Exit_Info : out VCPU_Exit_Info) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Run_Ex (Mach, CPU, Exit_Info);
   end VCPU_Run_Ex;

   function VCPU_Stop
      (Mach : Machine_ID;
       CPU  : VCPU_ID) return Boolean
   is
   begin
      return Arch.Virtualization.VCPU_Stop (Mach, CPU);
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
   begin
      return Arch.Virtualization.GPA_Map (Mach, CPU, HVA, GPA, Size, Prot);
   end GPA_Map;

   function GPA_Map_All
      (Mach : Machine_ID;
       HVA  : Unsigned_64;
       GPA  : Unsigned_64;
       Size : Unsigned_64;
       Prot : GPA_Flags) return Boolean
   is
   begin
      return Arch.Virtualization.GPA_Map_All (Mach, HVA, GPA, Size, Prot);
   end GPA_Map_All;

   function GPA_Unmap
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean
   is
   begin
      return Arch.Virtualization.GPA_Unmap (Mach, CPU, GPA, Size);
   end GPA_Unmap;

   function GPA_Unmap_All
      (Mach : Machine_ID;
       GPA  : Unsigned_64;
       Size : Unsigned_64) return Boolean
   is
   begin
      return Arch.Virtualization.GPA_Unmap_All (Mach, GPA, Size);
   end GPA_Unmap_All;

   function GPA_To_HVA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GPA  : Unsigned_64;
       HVA  : out Unsigned_64) return Boolean
   is
   begin
      return Arch.Virtualization.GPA_To_HVA (Mach, CPU, GPA, HVA);
   end GPA_To_HVA;

   function GVA_To_GPA
      (Mach : Machine_ID;
       CPU  : VCPU_ID;
       GVA  : Unsigned_64;
       GPA  : out Unsigned_64) return Boolean
   is
   begin
      return Arch.Virtualization.GVA_To_GPA (Mach, CPU, GVA, GPA);
   end GVA_To_GPA;
end Virtualization;
