--  arch-vmx.ads: Intel VT-x virtualization code.
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

package Arch.Virtualization.VMX with SPARK_Mode => Off is
   procedure Initialize (Success : out Boolean);
   function Is_Initialized return Boolean;

   ---------------------------------------------------------------------------
   --  Guest GPR state (registers not auto-saved by VMLAUNCH/VMRESUME)
   --  VMX auto-saves RSP, RIP, RFLAGS to VMCS, but NOT RAX or other GPRs
   ---------------------------------------------------------------------------
   type Guest_GPRs is record
      RAX : Unsigned_64;
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
   end record with Size => 960;  --  15 * 64 bits
   for Guest_GPRs use record
      RAX at 16#00# range 0 .. 63;
      RBX at 16#08# range 0 .. 63;
      RCX at 16#10# range 0 .. 63;
      RDX at 16#18# range 0 .. 63;
      RSI at 16#20# range 0 .. 63;
      RDI at 16#28# range 0 .. 63;
      RBP at 16#30# range 0 .. 63;
      R8  at 16#38# range 0 .. 63;
      R9  at 16#40# range 0 .. 63;
      R10 at 16#48# range 0 .. 63;
      R11 at 16#50# range 0 .. 63;
      R12 at 16#58# range 0 .. 63;
      R13 at 16#60# range 0 .. 63;
      R14 at 16#68# range 0 .. 63;
      R15 at 16#70# range 0 .. 63;
   end record;

   ---------------------------------------------------------------------------
   --  Guest MSR state (MSRs not in VMCS that need manual save/restore)
   --  EFER, SYSENTER_CS/ESP/EIP are in VMCS; these are for syscall MSRs
   ---------------------------------------------------------------------------
   type Guest_MSRs is record
      STAR            : Unsigned_64;  --  Syscall target address
      LSTAR           : Unsigned_64;  --  Long mode syscall target
      CSTAR           : Unsigned_64;  --  Compat mode syscall target
      SFMASK          : Unsigned_64;  --  Syscall flag mask
      Kernel_GS_Base  : Unsigned_64;  --  Kernel GS base (swapgs)
   end record;

   ---------------------------------------------------------------------------
   --  VMCS Management Operations
   ---------------------------------------------------------------------------

   --  Write VMCS revision ID (must be done before VMCLEAR)
   procedure Write_VMCS_Revision (VMCS_VA : Integer_Address);

   --  Clear VMCS (must be done before first VMPTRLD)
   procedure VMCLEAR (VMCS_PA : Unsigned_64; Success : out Boolean);

   --  Load VMCS pointer (makes VMCS current for this processor)
   procedure VMPTRLD (VMCS_PA : Unsigned_64; Success : out Boolean);

   --  Store current VMCS pointer
   function VMPTRST return Unsigned_64;

   --  Invalidate what this core cached through the EPT that EPTP names:
   --  single-context INVEPT, or all-context where the processor has only
   --  that (Initialize refuses VMX with neither).
   procedure INVEPT (EPTP : Unsigned_64; Success : out Boolean);

   --  Read a VMCS field
   function VMX_Read (Encoding : Unsigned_64) return Unsigned_64;

   --  Write a VMCS field
   procedure VMX_Write (Encoding, Value : Unsigned_64; Success : out Boolean);

   --  Initialize a VMCS with default guest state (32-bit protected mode)
   procedure VMCS_Setup
      (VMCS_VA   : Integer_Address;
       EPT_PA    : Unsigned_64;
       IOPM_PA   : Unsigned_64;
       MSRPM_PA  : Unsigned_64;
       VPID_Val  : Unsigned_16;
       Success   : out Boolean);

   --  Run guest - VMLAUNCH for first entry, VMRESUME for subsequent
   --  @param VMCS_PA     Physical address of VMCS
   --  @param GPRs        Guest GPR state (in/out)
   --  @param Is_Launch   True for VMLAUNCH (first entry), False for VMRESUME
   procedure VMLAUNCH_VMRESUME
      (VMCS_PA   : Unsigned_64;
       GPRs      : in out Guest_GPRs;
       Is_Launch : Boolean;
       Success   : out Boolean);

   --  Apply CR0 fixed bits for the given mode
   --  If for_unrestricted_paging is True, PE and PG bits are NOT forced to 1
   function Apply_CR0_Fixed
      (Value : Unsigned_64;
       For_Unrestricted_Paging : Boolean) return Unsigned_64;

   --  Apply CR4 fixed bits
   function Apply_CR4_Fixed (Value : Unsigned_64) return Unsigned_64;

   procedure VMCS_Write_Unchecked (Encoding, Value : Unsigned_64);

   function Adjust_Controls
      (Value : Unsigned_32;
       MSR   : Unsigned_64) return Unsigned_32;
   ---------------------------------------------------------------------------
   --  VMCS Field Encodings - Intel SDM Vol 3D, Appendix B
   --
   --  Encoding format (bits):
   --    0     - Access type: 0=full, 1=high (for 64-bit fields)
   --    1-9   - Index
   --    10-11 - Type: 0=control, 1=VM-exit info (RO), 2=guest state, 3=host
   --    12    - Reserved (0)
   --    13-14 - Width: 0=16-bit, 1=64-bit, 2=32-bit, 3=natural-width
   ---------------------------------------------------------------------------

   --  16-bit Control Fields
   VMCS_VPID                      : constant := 16#0000#;
   VMCS_POSTED_INT_NOTIF_VECTOR   : constant := 16#0002#;
   VMCS_EPTP_INDEX                : constant := 16#0004#;

   --  16-bit Guest-State Fields
   VMCS_GUEST_ES_SELECTOR         : constant := 16#0800#;
   VMCS_GUEST_CS_SELECTOR         : constant := 16#0802#;
   VMCS_GUEST_SS_SELECTOR         : constant := 16#0804#;
   VMCS_GUEST_DS_SELECTOR         : constant := 16#0806#;
   VMCS_GUEST_FS_SELECTOR         : constant := 16#0808#;
   VMCS_GUEST_GS_SELECTOR         : constant := 16#080A#;
   VMCS_GUEST_LDTR_SELECTOR       : constant := 16#080C#;
   VMCS_GUEST_TR_SELECTOR         : constant := 16#080E#;
   VMCS_GUEST_INTR_STATUS         : constant := 16#0810#;
   VMCS_GUEST_PML_INDEX           : constant := 16#0812#;

   --  16-bit Host-State Fields
   VMCS_HOST_ES_SELECTOR          : constant := 16#0C00#;
   VMCS_HOST_CS_SELECTOR          : constant := 16#0C02#;
   VMCS_HOST_SS_SELECTOR          : constant := 16#0C04#;
   VMCS_HOST_DS_SELECTOR          : constant := 16#0C06#;
   VMCS_HOST_FS_SELECTOR          : constant := 16#0C08#;
   VMCS_HOST_GS_SELECTOR          : constant := 16#0C0A#;
   VMCS_HOST_TR_SELECTOR          : constant := 16#0C0C#;

   --  64-bit Control Fields
   VMCS_IO_BITMAP_A               : constant := 16#2000#;
   VMCS_IO_BITMAP_A_HIGH          : constant := 16#2001#;
   VMCS_IO_BITMAP_B               : constant := 16#2002#;
   VMCS_IO_BITMAP_B_HIGH          : constant := 16#2003#;
   VMCS_MSR_BITMAP                : constant := 16#2004#;
   VMCS_MSR_BITMAP_HIGH           : constant := 16#2005#;
   VMCS_EXIT_MSR_STORE_ADDR       : constant := 16#2006#;
   VMCS_EXIT_MSR_STORE_ADDR_HIGH  : constant := 16#2007#;
   VMCS_EXIT_MSR_LOAD_ADDR        : constant := 16#2008#;
   VMCS_EXIT_MSR_LOAD_ADDR_HIGH   : constant := 16#2009#;
   VMCS_ENTRY_MSR_LOAD_ADDR       : constant := 16#200A#;
   VMCS_ENTRY_MSR_LOAD_ADDR_HIGH  : constant := 16#200B#;
   VMCS_EXECUTIVE_VMCS_PTR        : constant := 16#200C#;
   VMCS_PML_ADDRESS               : constant := 16#200E#;
   VMCS_TSC_OFFSET                : constant := 16#2010#;
   VMCS_TSC_OFFSET_HIGH           : constant := 16#2011#;
   VMCS_VIRTUAL_APIC_ADDR         : constant := 16#2012#;
   VMCS_APIC_ACCESS_ADDR          : constant := 16#2014#;
   VMCS_POSTED_INT_DESC_ADDR      : constant := 16#2016#;
   VMCS_VM_FUNCTION_CONTROLS      : constant := 16#2018#;
   VMCS_EPT_POINTER               : constant := 16#201A#;
   VMCS_EPT_POINTER_HIGH          : constant := 16#201B#;
   VMCS_EOI_EXIT_BITMAP_0         : constant := 16#201C#;
   VMCS_EOI_EXIT_BITMAP_1         : constant := 16#201E#;
   VMCS_EOI_EXIT_BITMAP_2         : constant := 16#2020#;
   VMCS_EOI_EXIT_BITMAP_3         : constant := 16#2022#;
   VMCS_EPTP_LIST_ADDRESS         : constant := 16#2024#;
   VMCS_VMREAD_BITMAP             : constant := 16#2026#;
   VMCS_VMWRITE_BITMAP            : constant := 16#2028#;
   VMCS_XSS_EXIT_BITMAP           : constant := 16#202C#;
   VMCS_ENCLS_EXIT_BITMAP         : constant := 16#202E#;
   VMCS_TSC_MULTIPLIER            : constant := 16#2032#;

   --  64-bit Read-Only Data Fields
   VMCS_GUEST_PHYS_ADDR           : constant := 16#2400#;
   VMCS_GUEST_PHYS_ADDR_HIGH      : constant := 16#2401#;

   --  64-bit Guest-State Fields
   VMCS_GUEST_VMCS_LINK_PTR       : constant := 16#2800#;
   VMCS_GUEST_VMCS_LINK_PTR_HIGH  : constant := 16#2801#;
   VMCS_GUEST_IA32_DEBUGCTL       : constant := 16#2802#;
   VMCS_GUEST_IA32_DEBUGCTL_HIGH  : constant := 16#2803#;
   VMCS_GUEST_IA32_PAT            : constant := 16#2804#;
   VMCS_GUEST_IA32_PAT_HIGH       : constant := 16#2805#;
   VMCS_GUEST_IA32_EFER           : constant := 16#2806#;
   VMCS_GUEST_IA32_EFER_HIGH      : constant := 16#2807#;
   VMCS_GUEST_IA32_PERF_GLOBAL    : constant := 16#2808#;
   VMCS_GUEST_PDPTE0              : constant := 16#280A#;
   VMCS_GUEST_PDPTE1              : constant := 16#280C#;
   VMCS_GUEST_PDPTE2              : constant := 16#280E#;
   VMCS_GUEST_PDPTE3              : constant := 16#2810#;
   VMCS_GUEST_IA32_BNDCFGS        : constant := 16#2812#;

   --  64-bit Host-State Fields
   VMCS_HOST_IA32_PAT             : constant := 16#2C00#;
   VMCS_HOST_IA32_PAT_HIGH        : constant := 16#2C01#;
   VMCS_HOST_IA32_EFER            : constant := 16#2C02#;
   VMCS_HOST_IA32_EFER_HIGH       : constant := 16#2C03#;
   VMCS_HOST_IA32_PERF_GLOBAL     : constant := 16#2C04#;

   --  32-bit Control Fields
   VMCS_PIN_BASED_EXEC_CTRL       : constant := 16#4000#;
   VMCS_PRIMARY_EXEC_CTRL         : constant := 16#4002#;
   VMCS_EXCEPTION_BITMAP          : constant := 16#4004#;
   VMCS_PAGE_FAULT_ERR_MASK       : constant := 16#4006#;
   VMCS_PAGE_FAULT_ERR_MATCH      : constant := 16#4008#;
   VMCS_CR3_TARGET_COUNT          : constant := 16#400A#;
   VMCS_EXIT_CONTROLS             : constant := 16#400C#;
   VMCS_EXIT_MSR_STORE_COUNT      : constant := 16#400E#;
   VMCS_EXIT_MSR_LOAD_COUNT       : constant := 16#4010#;
   VMCS_ENTRY_CONTROLS            : constant := 16#4012#;
   VMCS_ENTRY_MSR_LOAD_COUNT      : constant := 16#4014#;
   VMCS_ENTRY_INTR_INFO           : constant := 16#4016#;
   VMCS_ENTRY_EXCEPTION_ERR_CODE  : constant := 16#4018#;
   VMCS_ENTRY_INSTR_LENGTH        : constant := 16#401A#;
   VMCS_TPR_THRESHOLD             : constant := 16#401C#;
   VMCS_SECONDARY_EXEC_CTRL       : constant := 16#401E#;
   VMCS_PLE_GAP                   : constant := 16#4020#;
   VMCS_PLE_WINDOW                : constant := 16#4022#;

   --  32-bit Read-Only Data Fields
   VMCS_VM_INSTR_ERROR            : constant := 16#4400#;
   VMCS_EXIT_REASON               : constant := 16#4402#;
   VMCS_EXIT_INTR_INFO            : constant := 16#4404#;
   VMCS_EXIT_INTR_ERR_CODE        : constant := 16#4406#;
   VMCS_IDT_VECTORING_INFO        : constant := 16#4408#;
   VMCS_IDT_VECTORING_ERR_CODE    : constant := 16#440A#;
   VMCS_EXIT_INSTR_LENGTH         : constant := 16#440C#;
   VMCS_EXIT_INSTR_INFO           : constant := 16#440E#;

   --  32-bit Guest-State Fields
   VMCS_GUEST_ES_LIMIT            : constant := 16#4800#;
   VMCS_GUEST_CS_LIMIT            : constant := 16#4802#;
   VMCS_GUEST_SS_LIMIT            : constant := 16#4804#;
   VMCS_GUEST_DS_LIMIT            : constant := 16#4806#;
   VMCS_GUEST_FS_LIMIT            : constant := 16#4808#;
   VMCS_GUEST_GS_LIMIT            : constant := 16#480A#;
   VMCS_GUEST_LDTR_LIMIT          : constant := 16#480C#;
   VMCS_GUEST_TR_LIMIT            : constant := 16#480E#;
   VMCS_GUEST_GDTR_LIMIT          : constant := 16#4810#;
   VMCS_GUEST_IDTR_LIMIT          : constant := 16#4812#;
   VMCS_GUEST_ES_AR               : constant := 16#4814#;
   VMCS_GUEST_CS_AR               : constant := 16#4816#;
   VMCS_GUEST_SS_AR               : constant := 16#4818#;
   VMCS_GUEST_DS_AR               : constant := 16#481A#;
   VMCS_GUEST_FS_AR               : constant := 16#481C#;
   VMCS_GUEST_GS_AR               : constant := 16#481E#;
   VMCS_GUEST_LDTR_AR             : constant := 16#4820#;
   VMCS_GUEST_TR_AR               : constant := 16#4822#;
   VMCS_GUEST_INTERRUPTIBILITY    : constant := 16#4824#;
   VMCS_GUEST_ACTIVITY_STATE      : constant := 16#4826#;
   VMCS_GUEST_SMBASE              : constant := 16#4828#;
   VMCS_GUEST_IA32_SYSENTER_CS    : constant := 16#482A#;
   VMCS_GUEST_VMX_PREEMPT_TIMER   : constant := 16#482E#;

   --  32-bit Host-State Fields
   VMCS_HOST_IA32_SYSENTER_CS     : constant := 16#4C00#;

   --  Natural-Width Control Fields
   VMCS_CR0_GUEST_HOST_MASK       : constant := 16#6000#;
   VMCS_CR4_GUEST_HOST_MASK       : constant := 16#6002#;
   VMCS_CR0_READ_SHADOW           : constant := 16#6004#;
   VMCS_CR4_READ_SHADOW           : constant := 16#6006#;
   VMCS_CR3_TARGET_VALUE0         : constant := 16#6008#;
   VMCS_CR3_TARGET_VALUE1         : constant := 16#600A#;
   VMCS_CR3_TARGET_VALUE2         : constant := 16#600C#;
   VMCS_CR3_TARGET_VALUE3         : constant := 16#600E#;

   --  Natural-Width Read-Only Data Fields
   VMCS_EXIT_QUALIFICATION        : constant := 16#6400#;
   VMCS_IO_RCX                    : constant := 16#6402#;
   VMCS_IO_RSI                    : constant := 16#6404#;
   VMCS_IO_RDI                    : constant := 16#6406#;
   VMCS_IO_RIP                    : constant := 16#6408#;
   VMCS_GUEST_LINEAR_ADDR         : constant := 16#640A#;

   --  Natural-Width Guest-State Fields
   VMCS_GUEST_CR0                 : constant := 16#6800#;
   VMCS_GUEST_CR3                 : constant := 16#6802#;
   VMCS_GUEST_CR4                 : constant := 16#6804#;
   VMCS_GUEST_ES_BASE             : constant := 16#6806#;
   VMCS_GUEST_CS_BASE             : constant := 16#6808#;
   VMCS_GUEST_SS_BASE             : constant := 16#680A#;
   VMCS_GUEST_DS_BASE             : constant := 16#680C#;
   VMCS_GUEST_FS_BASE             : constant := 16#680E#;
   VMCS_GUEST_GS_BASE             : constant := 16#6810#;
   VMCS_GUEST_LDTR_BASE           : constant := 16#6812#;
   VMCS_GUEST_TR_BASE             : constant := 16#6814#;
   VMCS_GUEST_GDTR_BASE           : constant := 16#6816#;
   VMCS_GUEST_IDTR_BASE           : constant := 16#6818#;
   VMCS_GUEST_DR7                 : constant := 16#681A#;
   VMCS_GUEST_RSP                 : constant := 16#681C#;
   VMCS_GUEST_RIP                 : constant := 16#681E#;
   VMCS_GUEST_RFLAGS              : constant := 16#6820#;
   VMCS_GUEST_PENDING_DBG_EXCEP   : constant := 16#6822#;
   VMCS_GUEST_SYSENTER_ESP        : constant := 16#6824#;
   VMCS_GUEST_SYSENTER_EIP        : constant := 16#6826#;

   --  Natural-Width Host-State Fields
   VMCS_HOST_CR0                  : constant := 16#6C00#;
   VMCS_HOST_CR3                  : constant := 16#6C02#;
   VMCS_HOST_CR4                  : constant := 16#6C04#;
   VMCS_HOST_FS_BASE              : constant := 16#6C06#;
   VMCS_HOST_GS_BASE              : constant := 16#6C08#;
   VMCS_HOST_TR_BASE              : constant := 16#6C0A#;
   VMCS_HOST_GDTR_BASE            : constant := 16#6C0C#;
   VMCS_HOST_IDTR_BASE            : constant := 16#6C0E#;
   VMCS_HOST_SYSENTER_ESP         : constant := 16#6C10#;
   VMCS_HOST_SYSENTER_EIP         : constant := 16#6C12#;
   VMCS_HOST_RSP                  : constant := 16#6C14#;
   VMCS_HOST_RIP                  : constant := 16#6C16#;

   ---------------------------------------------------------------------------
   --  Pin-Based VM-Execution Controls (VMCS offset 4000h)
   ---------------------------------------------------------------------------
   PIN_EXTERNAL_INTERRUPT_EXIT    : constant := 16#0000_0001#;
   PIN_NMI_EXIT                   : constant := 16#0000_0008#;
   PIN_VIRTUAL_NMI                : constant := 16#0000_0020#;
   PIN_VMX_PREEMPTION_TIMER       : constant := 16#0000_0040#;
   PIN_POSTED_INTERRUPTS          : constant := 16#0000_0080#;

   ---------------------------------------------------------------------------
   --  Primary Processor-Based VM-Execution Controls (VMCS offset 4002h)
   ---------------------------------------------------------------------------
   CPU_INTERRUPT_WINDOW_EXIT      : constant := 16#0000_0004#;
   CPU_USE_TSC_OFFSETTING         : constant := 16#0000_0008#;
   CPU_HLT_EXIT                   : constant := 16#0000_0080#;
   CPU_INVLPG_EXIT                : constant := 16#0000_0200#;
   CPU_MWAIT_EXIT                 : constant := 16#0000_0400#;
   CPU_RDPMC_EXIT                 : constant := 16#0000_0800#;
   CPU_RDTSC_EXIT                 : constant := 16#0000_1000#;
   CPU_CR3_LOAD_EXIT              : constant := 16#0000_8000#;
   CPU_CR3_STORE_EXIT             : constant := 16#0001_0000#;
   CPU_CR8_LOAD_EXIT              : constant := 16#0008_0000#;
   CPU_CR8_STORE_EXIT             : constant := 16#0010_0000#;
   CPU_USE_TPR_SHADOW             : constant := 16#0020_0000#;
   CPU_NMI_WINDOW_EXIT            : constant := 16#0040_0000#;
   CPU_MOV_DR_EXIT                : constant := 16#0080_0000#;
   CPU_UNCONDITIONAL_IO_EXIT      : constant := 16#0100_0000#;
   CPU_USE_IO_BITMAPS             : constant := 16#0200_0000#;
   CPU_MONITOR_TRAP_FLAG          : constant := 16#0800_0000#;
   CPU_USE_MSR_BITMAPS            : constant := 16#1000_0000#;
   CPU_MONITOR_EXIT               : constant := 16#2000_0000#;
   CPU_PAUSE_EXIT                 : constant := 16#4000_0000#;
   CPU_SECONDARY_CONTROLS         : constant := 16#8000_0000#;

   ---------------------------------------------------------------------------
   --  Secondary Processor-Based VM-Execution Controls (VMCS offset 401Eh)
   ---------------------------------------------------------------------------
   CPU2_VIRTUALIZE_APIC           : constant := 16#0000_0001#;
   CPU2_ENABLE_EPT                : constant := 16#0000_0002#;
   CPU2_DESCRIPTOR_TABLE_EXIT     : constant := 16#0000_0004#;
   CPU2_ENABLE_RDTSCP             : constant := 16#0000_0008#;
   CPU2_VIRTUALIZE_X2APIC         : constant := 16#0000_0010#;
   CPU2_ENABLE_VPID               : constant := 16#0000_0020#;
   CPU2_WBINVD_EXIT               : constant := 16#0000_0040#;
   CPU2_UNRESTRICTED_GUEST        : constant := 16#0000_0080#;
   CPU2_APIC_REGISTER_VIRT        : constant := 16#0000_0100#;
   CPU2_VIRTUAL_INT_DELIVERY      : constant := 16#0000_0200#;
   CPU2_PAUSE_LOOP_EXIT           : constant := 16#0000_0400#;
   CPU2_RDRAND_EXIT               : constant := 16#0000_0800#;
   CPU2_ENABLE_INVPCID            : constant := 16#0000_1000#;
   CPU2_ENABLE_VMFUNC             : constant := 16#0000_2000#;
   CPU2_VMCS_SHADOWING            : constant := 16#0000_4000#;
   CPU2_ENCLS_EXIT                : constant := 16#0000_8000#;
   CPU2_RDSEED_EXIT               : constant := 16#0001_0000#;
   CPU2_ENABLE_PML                : constant := 16#0002_0000#;
   CPU2_EPT_VIOLATION_VE          : constant := 16#0004_0000#;
   CPU2_CONCEAL_VMX_FROM_PT       : constant := 16#0008_0000#;
   CPU2_ENABLE_XSAVES             : constant := 16#0010_0000#;
   CPU2_MODE_BASED_EPT            : constant := 16#0040_0000#;
   CPU2_TSC_SCALING               : constant := 16#0200_0000#;

   ---------------------------------------------------------------------------
   --  VM-Exit Controls (VMCS offset 400Ch)
   ---------------------------------------------------------------------------
   EXIT_SAVE_DEBUG_CONTROLS       : constant := 16#0000_0004#;
   EXIT_HOST_ADDR_SPACE_SIZE      : constant := 16#0000_0200#;
   EXIT_LOAD_IA32_PERF_GLOBAL     : constant := 16#0000_1000#;
   EXIT_ACK_INTR_ON_EXIT          : constant := 16#0000_8000#;
   EXIT_SAVE_IA32_PAT             : constant := 16#0004_0000#;
   EXIT_LOAD_IA32_PAT             : constant := 16#0008_0000#;
   EXIT_SAVE_IA32_EFER            : constant := 16#0010_0000#;
   EXIT_LOAD_IA32_EFER            : constant := 16#0020_0000#;
   EXIT_SAVE_VMX_PREEMPT_TIMER    : constant := 16#0040_0000#;
   EXIT_CLEAR_IA32_BNDCFGS        : constant := 16#0080_0000#;

   ---------------------------------------------------------------------------
   --  VM-Entry Controls (VMCS offset 4012h)
   ---------------------------------------------------------------------------
   ENTRY_LOAD_DEBUG_CONTROLS      : constant := 16#0000_0004#;
   ENTRY_IA32E_MODE_GUEST         : constant := 16#0000_0200#;
   ENTRY_SMM                      : constant := 16#0000_0400#;
   ENTRY_DEACTIVATE_DUAL_MONITOR  : constant := 16#0000_0800#;
   ENTRY_LOAD_IA32_PERF_GLOBAL    : constant := 16#0000_2000#;
   ENTRY_LOAD_IA32_PAT            : constant := 16#0000_4000#;
   ENTRY_LOAD_IA32_EFER           : constant := 16#0000_8000#;
   ENTRY_LOAD_IA32_BNDCFGS        : constant := 16#0001_0000#;

   ---------------------------------------------------------------------------
   --  VMX Exit Reasons (Basic exit reason in bits 15:0 of exit reason field)
   ---------------------------------------------------------------------------
   EXIT_REASON_EXCEPTION_NMI       : constant := 0;
   EXIT_REASON_EXTERNAL_INTERRUPT  : constant := 1;
   EXIT_REASON_TRIPLE_FAULT        : constant := 2;
   EXIT_REASON_INIT_SIGNAL         : constant := 3;
   EXIT_REASON_SIPI                : constant := 4;
   EXIT_REASON_IO_SMI              : constant := 5;
   EXIT_REASON_OTHER_SMI           : constant := 6;
   EXIT_REASON_INTERRUPT_WINDOW    : constant := 7;
   EXIT_REASON_NMI_WINDOW          : constant := 8;
   EXIT_REASON_TASK_SWITCH         : constant := 9;
   EXIT_REASON_CPUID               : constant := 10;
   EXIT_REASON_GETSEC              : constant := 11;
   EXIT_REASON_HLT                 : constant := 12;
   EXIT_REASON_INVD                : constant := 13;
   EXIT_REASON_INVLPG              : constant := 14;
   EXIT_REASON_RDPMC               : constant := 15;
   EXIT_REASON_RDTSC               : constant := 16;
   EXIT_REASON_RSM                 : constant := 17;
   EXIT_REASON_VMCALL              : constant := 18;
   EXIT_REASON_VMCLEAR             : constant := 19;
   EXIT_REASON_VMLAUNCH            : constant := 20;
   EXIT_REASON_VMPTRLD             : constant := 21;
   EXIT_REASON_VMPTRST             : constant := 22;
   EXIT_REASON_VMREAD              : constant := 23;
   EXIT_REASON_VMRESUME            : constant := 24;
   EXIT_REASON_VMWRITE             : constant := 25;
   EXIT_REASON_VMXOFF              : constant := 26;
   EXIT_REASON_VMXON               : constant := 27;
   EXIT_REASON_CR_ACCESS           : constant := 28;
   EXIT_REASON_DR_ACCESS           : constant := 29;
   EXIT_REASON_IO_INSTRUCTION      : constant := 30;
   EXIT_REASON_RDMSR               : constant := 31;
   EXIT_REASON_WRMSR               : constant := 32;
   EXIT_REASON_ENTRY_FAIL_GUEST    : constant := 33;
   EXIT_REASON_ENTRY_FAIL_MSR      : constant := 34;
   EXIT_REASON_MWAIT               : constant := 36;
   EXIT_REASON_MONITOR_TRAP_FLAG   : constant := 37;
   EXIT_REASON_MONITOR             : constant := 39;
   EXIT_REASON_PAUSE               : constant := 40;
   EXIT_REASON_ENTRY_FAIL_MACHINE  : constant := 41;
   EXIT_REASON_TPR_BELOW_THRESHOLD : constant := 43;
   EXIT_REASON_APIC_ACCESS         : constant := 44;
   EXIT_REASON_VIRTUALIZED_EOI     : constant := 45;
   EXIT_REASON_ACCESS_GDTR_IDTR    : constant := 46;
   EXIT_REASON_ACCESS_LDTR_TR      : constant := 47;
   EXIT_REASON_EPT_VIOLATION       : constant := 48;
   EXIT_REASON_EPT_MISCONFIG       : constant := 49;
   EXIT_REASON_INVEPT              : constant := 50;
   EXIT_REASON_RDTSCP              : constant := 51;
   EXIT_REASON_VMX_PREEMPT_TIMER   : constant := 52;
   EXIT_REASON_INVVPID             : constant := 53;
   EXIT_REASON_WBINVD              : constant := 54;
   EXIT_REASON_XSETBV              : constant := 55;
   EXIT_REASON_APIC_WRITE          : constant := 56;
   EXIT_REASON_RDRAND              : constant := 57;
   EXIT_REASON_INVPCID             : constant := 58;
   EXIT_REASON_VMFUNC              : constant := 59;
   EXIT_REASON_ENCLS               : constant := 60;
   EXIT_REASON_RDSEED              : constant := 61;
   EXIT_REASON_PML_FULL            : constant := 62;
   EXIT_REASON_XSAVES              : constant := 63;
   EXIT_REASON_XRSTORS             : constant := 64;

   ---------------------------------------------------------------------------
   --  Guest Activity States
   ---------------------------------------------------------------------------
   ACTIVITY_STATE_ACTIVE           : constant := 0;
   ACTIVITY_STATE_HLT              : constant := 1;
   ACTIVITY_STATE_SHUTDOWN         : constant := 2;
   ACTIVITY_STATE_WAIT_FOR_SIPI    : constant := 3;

   ---------------------------------------------------------------------------
   --  Interruptibility State Bits
   ---------------------------------------------------------------------------
   INTERRUPTIBILITY_STI_BLOCKING   : constant := 16#0000_0001#;
   INTERRUPTIBILITY_MOV_SS_BLOCKING : constant := 16#0000_0002#;
   INTERRUPTIBILITY_SMI_BLOCKING   : constant := 16#0000_0004#;
   INTERRUPTIBILITY_NMI_BLOCKING   : constant := 16#0000_0008#;

   ---------------------------------------------------------------------------
   --  VMX Capability MSRs
   ---------------------------------------------------------------------------
   IA32_VMX_BASIC            : constant := 16#480#;
   IA32_VMX_PINBASED_CTLS    : constant := 16#481#;
   IA32_VMX_PROCBASED_CTLS   : constant := 16#482#;
   IA32_VMX_EXIT_CTLS        : constant := 16#483#;
   IA32_VMX_ENTRY_CTLS       : constant := 16#484#;
   IA32_VMX_MISC             : constant := 16#485#;
   IA32_VMX_CR0_FIXED0       : constant := 16#486#;
   IA32_VMX_CR0_FIXED1       : constant := 16#487#;
   IA32_VMX_CR4_FIXED0       : constant := 16#488#;
   IA32_VMX_CR4_FIXED1       : constant := 16#489#;
   IA32_VMX_VMCS_ENUM        : constant := 16#48A#;
   IA32_VMX_PROCBASED_CTLS2  : constant := 16#48B#;
   IA32_VMX_EPT_VPID_CAP     : constant := 16#48C#;
   IA32_VMX_TRUE_PINBASED    : constant := 16#48D#;
   IA32_VMX_TRUE_PROCBASED   : constant := 16#48E#;
   IA32_VMX_TRUE_EXIT        : constant := 16#48F#;
   IA32_VMX_TRUE_ENTRY       : constant := 16#490#;
   IA32_VMX_VMFUNC           : constant := 16#491#;

   ---------------------------------------------------------------------------
   --  EPT Memory Types (bits 2:0 of EPT pointer and EPT entries)
   ---------------------------------------------------------------------------
   EPT_MEMORY_TYPE_UC              : constant := 0;  --  Uncacheable
   EPT_MEMORY_TYPE_WC              : constant := 1;  --  Write-combining
   EPT_MEMORY_TYPE_WT              : constant := 4;  --  Write-through
   EPT_MEMORY_TYPE_WP              : constant := 5;  --  Write-protected
   EPT_MEMORY_TYPE_WB              : constant := 6;  --  Write-back

   ---------------------------------------------------------------------------
   --  EPT Entry Bits
   ---------------------------------------------------------------------------
   EPT_READ                        : constant := 16#01#;
   EPT_WRITE                       : constant := 16#02#;
   EPT_EXECUTE                     : constant := 16#04#;
   EPT_MEMORY_TYPE_SHIFT           : constant := 3;
   EPT_IGNORE_PAT                  : constant := 16#40#;
   EPT_LARGE_PAGE                  : constant := 16#80#;  --  2MB or 1GB page
   EPT_ACCESSED                    : constant := 16#100#;
   EPT_DIRTY                       : constant := 16#200#;
   EPT_EXECUTE_USER                : constant := 16#400#;

   ---------------------------------------------------------------------------
   --  EPT Pointer Format (bits in VMCS_EPT_POINTER)
   --  Bits 2:0   = Memory type (0=UC, 6=WB)
   --  Bits 5:3   = Page-walk length minus 1 (3 = 4-level)
   --  Bit 6      = Enable accessed/dirty flags
   --  Bits 11:7  = Reserved
   --  Bits N-1:12 = Physical address of EPT PML4
   ---------------------------------------------------------------------------
   EPT_POINTER_WB_4LEVEL : constant := 16#1E#;  --  WB + 4-level (6 | (3 << 3))

   ---------------------------------------------------------------------------
   --  Segment Access Rights Format (for VMX, slightly different from SVM)
   --  Bits 3:0   = Type
   --  Bit 4      = S (descriptor type: 0=system, 1=code/data)
   --  Bits 6:5   = DPL
   --  Bit 7      = P (present)
   --  Bits 11:8  = Reserved (0)
   --  Bit 12     = AVL
   --  Bit 13     = L (64-bit mode for CS)
   --  Bit 14     = D/B
   --  Bit 15     = G
   --  Bit 16     = Unusable (1 = segment unusable)
   ---------------------------------------------------------------------------
   SEG_AR_TYPE_MASK                : constant := 16#000F#;
   SEG_AR_S                        : constant := 16#0010#;
   SEG_AR_DPL_SHIFT                : constant := 5;
   SEG_AR_DPL_MASK                 : constant := 16#0060#;
   SEG_AR_P                        : constant := 16#0080#;
   SEG_AR_AVL                      : constant := 16#1000#;
   SEG_AR_L                        : constant := 16#2000#;
   SEG_AR_DB                       : constant := 16#4000#;
   SEG_AR_G                        : constant := 16#8000#;
   SEG_AR_UNUSABLE                 : constant := 16#1_0000#;

   --  Common segment AR values
   SEG_AR_CODE_32_DPL0 : constant := 16#C09B#;  --  G=1,D/B=1,P=1,S=1,Type=B
   SEG_AR_DATA_32_DPL0 : constant := 16#C093#;  --  G=1,D/B=1,P=1,S=1,Type=3
   SEG_AR_CODE_64_DPL0 : constant := 16#A09B#;  --  G=1,L=1,D/B=0,P=1,S=1,Typ=B
   SEG_AR_TSS_BUSY     : constant := 16#008B#;  --  P=1,Type=B (busy TSS)
   SEG_AR_LDT          : constant := 16#0082#;  --  P=1,Type=2 (LDT)
end Arch.Virtualization.VMX;
