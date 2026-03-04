--  arch-virtualization-svm.ads: AMD-V virtualization code.
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

package Arch.Virtualization.SVM with SPARK_Mode => Off is
   procedure Initialize (Success : out Boolean);
   function Is_Initialized return Boolean;

   --  VMCB Segment Descriptor (16 bytes each)
   type Segment_Descriptor is record
      Selector   : Unsigned_16;
      Attrib     : Unsigned_16;
      Limit      : Unsigned_32;
      Base       : Unsigned_64;
   end record with Size => 128;
   for Segment_Descriptor use record
      Selector   at 0  range 0 .. 15;
      Attrib     at 2  range 0 .. 15;
      Limit      at 4  range 0 .. 31;
      Base       at 8  range 0 .. 63;
   end record;

   pragma Warnings (Off, "bits of *** unused");

   --  VMCB Control Area (offset 0x000 - 0x3FF, 1024 bytes)
   --  Simplified to key fields only; unused areas left as padding
   type VMCB_Control_Area is record
      Intercept_CR_Reads     : Unsigned_16;
      Intercept_CR_Writes    : Unsigned_16;
      Intercept_DR_Reads     : Unsigned_16;
      Intercept_DR_Writes    : Unsigned_16;
      Intercept_Exceptions   : Unsigned_32;
      Intercept_Misc_1       : Unsigned_32;
      Intercept_Misc_2       : Unsigned_32;
      Intercept_Misc_3       : Unsigned_32;
      Pause_Filter_Threshold : Unsigned_16;
      Pause_Filter_Count     : Unsigned_16;
      IOPM_Base_PA           : Unsigned_64;
      MSRPM_Base_PA          : Unsigned_64;
      TSC_Offset             : Unsigned_64;
      Guest_ASID             : Unsigned_32;
      TLB_Control            : Unsigned_32;
      V_Intr_Control         : Unsigned_64;
      Interrupt_Shadow       : Unsigned_64;
      Exit_Code              : Unsigned_64;
      Exit_Info_1            : Unsigned_64;
      Exit_Info_2            : Unsigned_64;
      Exit_Int_Info          : Unsigned_64;
      NP_Enable              : Unsigned_64;
      AVIC_APIC_Bar          : Unsigned_64;
      GHCB_GPA               : Unsigned_64;
      Event_Inject           : Unsigned_64;
      N_CR3                  : Unsigned_64;
      LBR_Virt_Enable        : Unsigned_64;
      VMCB_Clean             : Unsigned_32;
      Reserved_0C4           : Unsigned_32;
      Next_RIP               : Unsigned_64;
   end record with Size => 8192;
   for VMCB_Control_Area use record
      Intercept_CR_Reads     at 16#000# range 0 .. 15;
      Intercept_CR_Writes    at 16#002# range 0 .. 15;
      Intercept_DR_Reads     at 16#004# range 0 .. 15;
      Intercept_DR_Writes    at 16#006# range 0 .. 15;
      Intercept_Exceptions   at 16#008# range 0 .. 31;
      Intercept_Misc_1       at 16#00C# range 0 .. 31;
      Intercept_Misc_2       at 16#010# range 0 .. 31;
      Intercept_Misc_3       at 16#014# range 0 .. 31;
      Pause_Filter_Threshold at 16#03C# range 0 .. 15;
      Pause_Filter_Count     at 16#03E# range 0 .. 15;
      IOPM_Base_PA           at 16#040# range 0 .. 63;
      MSRPM_Base_PA          at 16#048# range 0 .. 63;
      TSC_Offset             at 16#050# range 0 .. 63;
      Guest_ASID             at 16#058# range 0 .. 31;
      TLB_Control            at 16#05C# range 0 .. 31;
      V_Intr_Control         at 16#060# range 0 .. 63;
      Interrupt_Shadow       at 16#068# range 0 .. 63;
      Exit_Code              at 16#070# range 0 .. 63;
      Exit_Info_1            at 16#078# range 0 .. 63;
      Exit_Info_2            at 16#080# range 0 .. 63;
      Exit_Int_Info          at 16#088# range 0 .. 63;
      NP_Enable              at 16#090# range 0 .. 63;
      AVIC_APIC_Bar          at 16#098# range 0 .. 63;
      GHCB_GPA               at 16#0A0# range 0 .. 63;
      Event_Inject           at 16#0A8# range 0 .. 63;
      N_CR3                  at 16#0B0# range 0 .. 63;
      LBR_Virt_Enable        at 16#0B8# range 0 .. 63;
      VMCB_Clean             at 16#0C0# range 0 .. 31;
      Reserved_0C4           at 16#0C4# range 0 .. 31;
      Next_RIP               at 16#0C8# range 0 .. 63;
   end record;

   --  VMCB State Save Area
   type VMCB_State_Save_Area is record
      ES             : Segment_Descriptor;
      CS             : Segment_Descriptor;
      SS             : Segment_Descriptor;
      DS             : Segment_Descriptor;
      FS             : Segment_Descriptor;
      GS             : Segment_Descriptor;
      GDTR           : Segment_Descriptor;
      LDTR           : Segment_Descriptor;
      IDTR           : Segment_Descriptor;
      TR             : Segment_Descriptor;
      CPL            : Unsigned_8;
      EFER           : Unsigned_64;
      CR4            : Unsigned_64;
      CR3            : Unsigned_64;
      CR0            : Unsigned_64;
      DR7            : Unsigned_64;
      DR6            : Unsigned_64;
      RFLAGS         : Unsigned_64;
      RIP            : Unsigned_64;
      RSP            : Unsigned_64;
      RAX            : Unsigned_64;
      STAR           : Unsigned_64;
      LSTAR          : Unsigned_64;
      CSTAR          : Unsigned_64;
      SFMASK         : Unsigned_64;
      Kernel_GS_Base : Unsigned_64;
      SYSENTER_CS    : Unsigned_64;
      SYSENTER_ESP   : Unsigned_64;
      SYSENTER_EIP   : Unsigned_64;
      CR2            : Unsigned_64;
      G_PAT          : Unsigned_64;
      DBGCTL         : Unsigned_64;
      BR_FROM        : Unsigned_64;
      BR_TO          : Unsigned_64;
      Last_Excp_From : Unsigned_64;
      Last_Excp_To   : Unsigned_64;
   end record with Size => 24576;
   for VMCB_State_Save_Area use record
      ES             at 16#000# range 0 .. 127;
      CS             at 16#010# range 0 .. 127;
      SS             at 16#020# range 0 .. 127;
      DS             at 16#030# range 0 .. 127;
      FS             at 16#040# range 0 .. 127;
      GS             at 16#050# range 0 .. 127;
      GDTR           at 16#060# range 0 .. 127;
      LDTR           at 16#070# range 0 .. 127;
      IDTR           at 16#080# range 0 .. 127;
      TR             at 16#090# range 0 .. 127;
      CPL            at 16#0CB# range 0 .. 7;
      EFER           at 16#0D0# range 0 .. 63;
      CR4            at 16#148# range 0 .. 63;
      CR3            at 16#150# range 0 .. 63;
      CR0            at 16#158# range 0 .. 63;
      DR7            at 16#160# range 0 .. 63;
      DR6            at 16#168# range 0 .. 63;
      RFLAGS         at 16#170# range 0 .. 63;
      RIP            at 16#178# range 0 .. 63;
      RSP            at 16#1D8# range 0 .. 63;
      RAX            at 16#1F8# range 0 .. 63;
      STAR           at 16#200# range 0 .. 63;
      LSTAR          at 16#208# range 0 .. 63;
      CSTAR          at 16#210# range 0 .. 63;
      SFMASK         at 16#218# range 0 .. 63;
      Kernel_GS_Base at 16#220# range 0 .. 63;
      SYSENTER_CS    at 16#228# range 0 .. 63;
      SYSENTER_ESP   at 16#230# range 0 .. 63;
      SYSENTER_EIP   at 16#238# range 0 .. 63;
      CR2            at 16#240# range 0 .. 63;
      G_PAT          at 16#268# range 0 .. 63;
      DBGCTL         at 16#270# range 0 .. 63;
      BR_FROM        at 16#278# range 0 .. 63;
      BR_TO          at 16#280# range 0 .. 63;
      Last_Excp_From at 16#288# range 0 .. 63;
      Last_Excp_To   at 16#290# range 0 .. 63;
   end record;

   pragma Warnings (On, "bits of *** unused");

   type VMCB is record
      Control    : VMCB_Control_Area;
      State_Save : VMCB_State_Save_Area;
   end record with Size => 32768;  --  4096 bytes
   for VMCB use record
      Control    at 16#000# range 0 .. 8191;
      State_Save at 16#400# range 0 .. 24575;
   end record;

   type VMCB_Acc is access all VMCB;

   --  Guest GPR state (registers not auto-saved by VMRUN)
   --  RAX is in VMCB, so we track the others here
   type Guest_GPRs is record
      RBX : Unsigned_64;
      RCX : Unsigned_64;
      RDX : Unsigned_64;
      RSI : Unsigned_64;
      RDI : Unsigned_64;
      RBP : Unsigned_64;
      R8  : Unsigned_64;
      R9  : Unsigned_64;
      R10 : Unsigned_64;
      R11 : Unsigned_64;
      R12 : Unsigned_64;
      R13 : Unsigned_64;
      R14 : Unsigned_64;
      R15 : Unsigned_64;
   end record with Size => 896;  --  14 * 64 bits
   for Guest_GPRs use record
      RBX at 16#00# range 0 .. 63;
      RCX at 16#08# range 0 .. 63;
      RDX at 16#10# range 0 .. 63;
      RSI at 16#18# range 0 .. 63;
      RDI at 16#20# range 0 .. 63;
      RBP at 16#28# range 0 .. 63;
      R8  at 16#30# range 0 .. 63;
      R9  at 16#38# range 0 .. 63;
      R10 at 16#40# range 0 .. 63;
      R11 at 16#48# range 0 .. 63;
      R12 at 16#50# range 0 .. 63;
      R13 at 16#58# range 0 .. 63;
      R14 at 16#60# range 0 .. 63;
      R15 at 16#68# range 0 .. 63;
   end record;

   --  Run guest with VMCB
   --  @param VMCB_PA     Physical address of VMCB (page-aligned)
   --  @param GPRs        Guest GPR state (in/out)
   procedure VMRUN (VMCB_PA : Unsigned_64; GPRs : in out Guest_GPRs);

   --  FPU State Type (16-byte aligned for fxsave/fxrstor)
   type FPU_State_Area is array (0 .. 511) of Unsigned_8 with Alignment => 16;

   --  XSAVE State Type (2048 bytes, 64-byte aligned for xsave/xrstor)
   --  This is enough for x87 + SSE + AVX + AVX-512 state
   type XSAVE_Area is array (0 .. 2047) of Unsigned_8 with Alignment => 64;

   --  FPU save/restore procedures (legacy FXSAVE)
   procedure FPU_Save (Area : out FPU_State_Area);
   procedure FPU_Restore (Area : FPU_State_Area);

   --  XSAVE support detection and save/restore
   function XSAVE_Supported return Boolean;

   --  Get the size needed for XSAVE area with given XCR0 mask
   function Get_XSAVE_Size (XCR0_Mask : Unsigned_64) return Unsigned_32;

   --  Get the maximum supported XCR0 value
   function Get_XCR0_Max return Unsigned_64;

   --  Get the current XCR0 value (via XGETBV)
   function Get_XCR0 return Unsigned_64;

   --  Set the XCR0 value (via XSETBV)
   procedure Set_XCR0 (Value : Unsigned_64);

   --  XSAVE/XRSTOR procedures
   procedure XSAVE_Save (Area : out XSAVE_Area; XCR0_Mask : Unsigned_64);
   procedure XSAVE_Restore (Area : XSAVE_Area; XCR0_Mask : Unsigned_64);

   --  Intercept bits for Misc_1 (offset 0x00C)
   INTERCEPT_INTR      : constant := 16#0000_0001#;
   INTERCEPT_NMI       : constant := 16#0000_0002#;
   INTERCEPT_SMI       : constant := 16#0000_0004#;
   INTERCEPT_INIT      : constant := 16#0000_0008#;
   INTERCEPT_VINTR     : constant := 16#0000_0010#;
   INTERCEPT_CR0_SEL   : constant := 16#0000_0020#;
   INTERCEPT_IDTR_RD   : constant := 16#0000_0040#;
   INTERCEPT_GDTR_RD   : constant := 16#0000_0080#;
   INTERCEPT_LDTR_RD   : constant := 16#0000_0100#;
   INTERCEPT_TR_RD     : constant := 16#0000_0200#;
   INTERCEPT_IDTR_WR   : constant := 16#0000_0400#;
   INTERCEPT_GDTR_WR   : constant := 16#0000_0800#;
   INTERCEPT_LDTR_WR   : constant := 16#0000_1000#;
   INTERCEPT_TR_WR     : constant := 16#0000_2000#;
   INTERCEPT_RDTSC     : constant := 16#0000_4000#;
   INTERCEPT_RDPMC     : constant := 16#0000_8000#;
   INTERCEPT_PUSHF     : constant := 16#0001_0000#;
   INTERCEPT_POPF      : constant := 16#0002_0000#;
   INTERCEPT_CPUID     : constant := 16#0004_0000#;
   INTERCEPT_RSM       : constant := 16#0008_0000#;
   INTERCEPT_IRET      : constant := 16#0010_0000#;
   INTERCEPT_INTN      : constant := 16#0020_0000#;
   INTERCEPT_INVD      : constant := 16#0040_0000#;
   INTERCEPT_PAUSE     : constant := 16#0080_0000#;
   INTERCEPT_HLT       : constant := 16#0100_0000#;
   INTERCEPT_INVLPG    : constant := 16#0200_0000#;
   INTERCEPT_INVLPGA   : constant := 16#0400_0000#;
   INTERCEPT_IOIO      : constant := 16#0800_0000#;
   INTERCEPT_MSR       : constant := 16#1000_0000#;
   INTERCEPT_TASK_SW   : constant := 16#2000_0000#;
   INTERCEPT_FERR_FRZ  : constant := 16#4000_0000#;
   INTERCEPT_SHUTDOWN  : constant := 16#8000_0000#;

   --  Intercept bits for Misc_2 (offset 0x010) - 32 bits
   INTERCEPT_VMRUN     : constant := 16#0000_0001#;
   INTERCEPT_VMMCALL   : constant := 16#0000_0002#;
   INTERCEPT_VMLOAD    : constant := 16#0000_0004#;
   INTERCEPT_VMSAVE    : constant := 16#0000_0008#;
   INTERCEPT_STGI      : constant := 16#0000_0010#;
   INTERCEPT_CLGI      : constant := 16#0000_0020#;
   INTERCEPT_SKINIT    : constant := 16#0000_0040#;
   INTERCEPT_RDTSCP    : constant := 16#0000_0080#;
   INTERCEPT_ICEBP     : constant := 16#0000_0100#;
   INTERCEPT_WBINVD    : constant := 16#0000_0200#;
   INTERCEPT_MONITOR   : constant := 16#0000_0400#;
   INTERCEPT_MWAIT     : constant := 16#0000_0800#;
   INTERCEPT_MWAIT_ARM : constant := 16#0000_1000#;
   INTERCEPT_XSETBV    : constant := 16#0000_2000#;
   INTERCEPT_RDPRU     : constant := 16#0000_4000#;

   --  VMEXIT codes
   VMEXIT_CR0_READ     : constant := 16#0000#;
   VMEXIT_CR0_WRITE    : constant := 16#0010#;
   VMEXIT_DR0_READ     : constant := 16#0020#;
   VMEXIT_DR0_WRITE    : constant := 16#0030#;
   VMEXIT_EXCP_BASE    : constant := 16#0040#;
   VMEXIT_INTR         : constant := 16#0060#;
   VMEXIT_NMI          : constant := 16#0061#;
   VMEXIT_SMI          : constant := 16#0062#;
   VMEXIT_INIT         : constant := 16#0063#;
   VMEXIT_VINTR        : constant := 16#0064#;
   VMEXIT_CR0_SEL_WR   : constant := 16#0065#;
   VMEXIT_TASK_SWITCH  : constant := 16#0066#;
   VMEXIT_PUSHF        : constant := 16#0070#;
   VMEXIT_POPF         : constant := 16#0071#;
   VMEXIT_CPUID        : constant := 16#0072#;
   VMEXIT_INVLPG       : constant := 16#0073#;
   VMEXIT_INVLPGA      : constant := 16#0074#;
   VMEXIT_PAUSE        : constant := 16#0077#;
   VMEXIT_HLT          : constant := 16#0078#;
   VMEXIT_INVD         : constant := 16#0079#;
   VMEXIT_IOIO         : constant := 16#007B#;
   VMEXIT_MSR          : constant := 16#007C#;
   VMEXIT_SHUTDOWN     : constant := 16#007F#;
   VMEXIT_VMRUN        : constant := 16#0080#;
   VMEXIT_VMMCALL      : constant := 16#0081#;
   VMEXIT_VMLOAD       : constant := 16#0082#;
   VMEXIT_VMSAVE       : constant := 16#0083#;
   VMEXIT_STGI         : constant := 16#0084#;
   VMEXIT_CLGI         : constant := 16#0085#;
   VMEXIT_SKINIT       : constant := 16#0086#;
   VMEXIT_RDTSCP       : constant := 16#0087#;
   VMEXIT_ICEBP        : constant := 16#0088#;
   VMEXIT_WBINVD       : constant := 16#0089#;
   VMEXIT_MONITOR      : constant := 16#008A#;
   VMEXIT_MWAIT        : constant := 16#008B#;
   VMEXIT_MWAIT_ARMED  : constant := 16#008C#;
   VMEXIT_XSETBV       : constant := 16#008D#;
   VMEXIT_RDPRU        : constant := 16#008E#;
   VMEXIT_NPF          : constant := 16#0400#;
   VMEXIT_AVIC_NOACCEL : constant := 16#0402#;
   VMEXIT_INVALID      : constant := -1;

   --  TLB Control values
   TLB_CONTROL_DO_NOTHING    : constant := 0;
   TLB_CONTROL_FLUSH_ALL     : constant := 1;
   TLB_CONTROL_FLUSH_GUEST   : constant := 3;
   TLB_CONTROL_FLUSH_GUEST_NONGLOBAL : constant := 7;
end Arch.Virtualization.SVM;
