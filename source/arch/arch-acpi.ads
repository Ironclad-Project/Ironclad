--  arch-acpi.ads: ACPI driver, when present.
--  Copyright (C) 2025 streaksu
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

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Memory;       use Memory;

package Arch.ACPI is
   --  This package exposes architecture-independent ACPI behaviour.
   --
   --  ACPI requires interpreters when present, since it uses some bytecode we
   --  have to parse. In Ironclad we use uACPI for this. This unit concerns
   --  itself with porting it to the kernel as well.

   --  Check whether ACPI is supported.
   function Is_Supported return Boolean;

   --  Get ACPI major revision.
   function Get_Revision return Natural;

   --  Initialize ACPI, which usually means initializing the underlying uACPI.
   procedure Initialize (Success : out Boolean);
   ----------------------------------------------------------------------------
   --  Architecture specific tables and table searching functions.
   --  SDT header, which leads all ACPI tables.
   subtype SDT_Signature is String (1 .. 4);
   type SDT_Header is record
      Signature        : SDT_Signature;
      Length           : Interfaces.Unsigned_32;
      Revision         : Interfaces.Unsigned_8;
      Checksum         : Interfaces.Unsigned_8;
      OEM_ID           : String (1 .. 6);
      OEM_Table_ID     : String (1 .. 8);
      OEM_Revision     : Interfaces.Unsigned_32;
      Creator_ID       : Interfaces.Unsigned_32;
      Creator_Revision : Interfaces.Unsigned_32;
   end record;
   for SDT_Header use record
      Signature        at 0 range   0 ..  31;
      Length           at 0 range  32 ..  63;
      Revision         at 0 range  64 ..  71;
      Checksum         at 0 range  72 ..  79;
      OEM_ID           at 0 range  80 .. 127;
      OEM_Table_ID     at 0 range 128 .. 191;
      OEM_Revision     at 0 range 192 .. 223;
      Creator_ID       at 0 range 224 .. 255;
      Creator_Revision at 0 range 256 .. 287;
   end record;
   for SDT_Header'Size use 288;

   --  Table detailing DMA related shenanigans.
   DMAR_Signature : constant SDT_Signature := "DMAR";
   type DMAR is record
      Header             : SDT_Header;
      Host_Address_Width : Interfaces.Unsigned_8;
      Flags              : Interfaces.Unsigned_8;
   end record;
   for DMAR use record
      Header             at 0 range   0 .. 287;
      Host_Address_Width at 0 range 288 .. 295;
      Flags              at 0 range 296 .. 303;
   end record;
   for DMAR'Size use 304;

   --  Multiple APIC Description Table, it features an array in memory of
   --  different entries for each hardware piece.
   MADT_Signature : constant SDT_Signature := "APIC";
   type MADT is record
      Header        : SDT_Header;
      LAPIC_Address : Interfaces.Unsigned_32;
      Flags         : Interfaces.Unsigned_32;
      Entries_Start : Interfaces.Unsigned_32; --  Array of entries.
   end record;
   for MADT use record
      Header        at 0 range   0 .. 287;
      LAPIC_Address at 0 range 288 .. 319;
      Flags         at 0 range 320 .. 351;
      Entries_Start at 0 range 352 .. 383;
   end record;
   for MADT'Size use 384;

   --  Header of each MADT hardware entry.
   type MADT_Header is record
      Entry_Type : Interfaces.Unsigned_8;
      Length     : Interfaces.Unsigned_8;
   end record;
   for MADT_Header use record
      Entry_Type at 0 range 0 ..  7;
      Length     at 0 range 8 .. 15;
   end record;
   for MADT_Header'Size use 16;

   --  Local APIC MADT entry.
   MADT_LAPIC_Type : constant := 0;
   type MADT_LAPIC is record
      Header       : MADT_Header;
      Processor_ID : Interfaces.Unsigned_8;
      LAPIC_ID     : Interfaces.Unsigned_8;
      Flags        : Interfaces.Unsigned_32;
   end record;
   for MADT_LAPIC use record
      Header       at 0 range  0 .. 15;
      Processor_ID at 0 range 16 .. 23;
      LAPIC_ID     at 0 range 24 .. 31;
      Flags        at 0 range 32 .. 63;
   end record;
   for MADT_LAPIC'Size use 64;

   --  IO APIC MADT entry.
   MADT_IOAPIC_Type : constant := 1;
   type MADT_IOAPIC is record
      Header   : MADT_Header;
      APIC_ID  : Interfaces.Unsigned_8;
      Reserved : Interfaces.Unsigned_8;
      Address  : Interfaces.Unsigned_32;
      GSIB     : Interfaces.Unsigned_32;
   end record;
   for MADT_IOAPIC use record
      Header   at 0 range  0 .. 15;
      APIC_ID  at 0 range 16 .. 23;
      Reserved at 0 range 24 .. 31;
      Address  at 0 range 32 .. 63;
      GSIB     at 0 range 64 .. 95;
   end record;
   for MADT_IOAPIC'Size use 96;

   --  ISO MADT entry.
   MADT_ISO_Type : constant := 2;
   type MADT_ISO is record
      Header     : MADT_Header;
      Bus_Source : Interfaces.Unsigned_8;
      IRQ_Source : Interfaces.Unsigned_8;
      GSI        : Interfaces.Unsigned_32;
      Flags      : Interfaces.Unsigned_16;
   end record;
   for MADT_ISO use record
      Header     at 0 range  0 .. 15;
      Bus_Source at 0 range 16 .. 23;
      IRQ_Source at 0 range 24 .. 31;
      GSI        at 0 range 32 .. 63;
      Flags      at 0 range 64 .. 79;
   end record;
   for MADT_ISO'Size use 80;

   --  Non-Maskable Interrupt MADT entry.
   MADT_NMI_Type : constant := 4;
   type MADT_NMI is record
      Header                : MADT_Header;
      Processor_ID          : Interfaces.Unsigned_8;
      Flags                 : Interfaces.Unsigned_16;
      Local_Interrupt_Index : Interfaces.Unsigned_8;
   end record;
   for MADT_NMI use record
      Header                at 0 range  0 .. 15;
      Processor_ID          at 0 range 16 .. 23;
      Flags                 at 0 range 24 .. 39;
      Local_Interrupt_Index at 0 range 40 .. 47;
   end record;
   for MADT_NMI'Size use 48;

   --  x2APIC MADT entry.
   MADT_x2APIC_Type : constant := 9;
   type MADT_x2APIC is record
      Header       : MADT_Header;
      Reserved_1   : Unsigned_8;
      Reserved_2   : Unsigned_8;
      x2APIC_ID    : Interfaces.Unsigned_32;
      Flags        : Interfaces.Unsigned_32;
      Processor_ID : Interfaces.Unsigned_32;
   end record;
   for MADT_x2APIC use record
      Header       at 0 range  0 .. 15;
      Reserved_1   at 0 range 16 .. 23;
      Reserved_2   at 0 range 24 .. 31;
      x2APIC_ID    at 0 range 32 .. 63;
      Flags        at 0 range 64 .. 95;
      Processor_ID at 0 range 96 .. 127;
   end record;
   for MADT_x2APIC'Size use 128;

   --  FADT table.
   FADT_Signature : constant SDT_Signature := "FACP";
   type FADT is record
      Header              : SDT_Header;
      Firmware_Control    : Unsigned_32;
      DSDT                : Unsigned_32;
      Reserved_1          : Unsigned_8;
      Preferred_PM_Profile : Unsigned_8;
      SCI_Interrupt       : Unsigned_16;
      SMI_Command         : Unsigned_32;
      ACPI_Enable         : Unsigned_8;
      ACPI_Disable        : Unsigned_8;
      S4BIOS_REQ          : Unsigned_8;
      PSTATE_CNT          : Unsigned_8;
      PM1a_EVT_BLK        : Unsigned_32;
      PM1a_CNT_BLK        : Unsigned_32;
      PM1b_CNT_BLK        : Unsigned_32;
      PM2_CNT_BLK         : Unsigned_32;
      PM_TMR_BLK          : Unsigned_32;
      GPE0_BLK            : Unsigned_32;
      GPE1_BLK            : Unsigned_32;
      PM1_EVT_LEN         : Unsigned_8;
      PM1_CNT_LEN         : Unsigned_8;
      PM2_CNT_LEN         : Unsigned_8;
      PM_TMR_LEN          : Unsigned_8;
      GPE0_BLK_LEN        : Unsigned_8;
      GPE1_BLK_LEN        : Unsigned_8;
      GPE1_Base           : Unsigned_8;
      CST_CNT             : Unsigned_8;
      P_LVL2_LAT          : Unsigned_16;
      P_LVL3_LAT          : Unsigned_16;
      Flush_Size          : Unsigned_16;
      Flush_Stride        : Unsigned_16;
      Duty_Offset         : Unsigned_8;
      Duty_Width          : Unsigned_8;
      Day_Alarm           : Unsigned_8;
      Mon_Alarm           : Unsigned_8;
      Century             : Unsigned_8;
      IAPC_Architecture   : Unsigned_16;
   end record with Pack;

   --  HPET table.
   HPET_Signature : constant SDT_Signature := "HPET";
   type HPET is record
      Header              : SDT_Header;
      Hardware_Revision   : Interfaces.Unsigned_8;
      Information         : Interfaces.Unsigned_8;
      PCI_Vendor_ID       : Interfaces.Unsigned_16;
      Address_Space_ID    : Interfaces.Unsigned_8;
      Register_Bit_Width  : Interfaces.Unsigned_8;
      Register_Bit_Offset : Interfaces.Unsigned_8;
      Reserved1           : Interfaces.Unsigned_8;
      Address             : Physical_Address; -- Pointer to HPET_Contents.
      HPET_Number         : Interfaces.Unsigned_8;
      Minimum_Tick        : Interfaces.Unsigned_16;
      Page_Protection     : Interfaces.Unsigned_8;
   end record;
   for HPET use record
      Header              at 0 range   0 .. 287;
      Hardware_Revision   at 0 range 288 .. 295;
      Information         at 0 range 296 .. 303;
      PCI_Vendor_ID       at 0 range 304 .. 319;
      Address_Space_ID    at 0 range 320 .. 327;
      Register_Bit_Width  at 0 range 328 .. 335;
      Register_Bit_Offset at 0 range 336 .. 343;
      Reserved1           at 0 range 344 .. 351;
      Address             at 0 range 352 .. 415;
      HPET_Number         at 0 range 416 .. 423;
      Minimum_Tick        at 0 range 424 .. 439;
      Page_Protection     at 0 range 440 .. 447;
   end record;
   for HPET'Size use 448;
   type HPET_Padding is array (1 .. 25) of Interfaces.Unsigned_64;
   type HPET_Contents is record
      General_Capabilities  : Interfaces.Unsigned_64;
      Unused0               : Interfaces.Unsigned_64;
      General_Configuration : Interfaces.Unsigned_64;
      Unused1               : Interfaces.Unsigned_64;
      General_Int_Status    : Interfaces.Unsigned_64;
      Unused2               : HPET_Padding;
      Main_Counter_Value    : Interfaces.Unsigned_64;
      Unused3               : Interfaces.Unsigned_64;
   end record with Volatile;
   for HPET_Contents use record
      General_Capabilities  at 0 range    0 ..   63;
      Unused0               at 0 range   64 ..  127;
      General_Configuration at 0 range  128 ..  191;
      Unused1               at 0 range  192 ..  255;
      General_Int_Status    at 0 range  256 ..  319;
      Unused2               at 0 range  320 .. 1919;
      Main_Counter_Value    at 0 range 1920 .. 1983;
      Unused3               at 0 range 1984 .. 2047;
   end record;
   for HPET_Contents'Size use 2048;

   --  RSDP table in memory, and its 2.0 version.
   type RSDP_Padding is array (1 .. 3) of Unsigned_8;
   type RSDP is record
      Signature    : String (1 .. 8);
      Checksum     : Unsigned_8;
      OEM_ID       : String (1 .. 6);
      Revision     : Unsigned_8;
      RSDT_Address : Unsigned_32;
      --  Version 2.0 onwards.
      Length       : Unsigned_32;
      XSDT_Address : Unsigned_64;
      Checksum_2   : Unsigned_8;
      Reserved     : RSDP_Padding;
   end record;
   for RSDP use record
      Signature    at 0 range   0 ..  63;
      Checksum     at 0 range  64 ..  71;
      OEM_ID       at 0 range  72 .. 119;
      Revision     at 0 range 120 .. 127;
      RSDT_Address at 0 range 128 .. 159;
      Length       at 0 range 160 .. 191;
      XSDT_Address at 0 range 192 .. 255;
      Checksum_2   at 0 range 256 .. 263;
      Reserved     at 0 range 264 .. 287;
   end record;
   for RSDP'Size use 288;

   --  Root System Descriptor Table and entries for itself and the XSDT
   --  (The XSDT is just an RSDT with 64 bit entries).
   type RSDT_Entries is array (Natural range <>) of Unsigned_32;
   type XSDT_Entries is array (Natural range <>) of Unsigned_64;
   type RSDT is record
      Header  : SDT_Header;
      Entries : Unsigned_32; --  Actually the start of the RSDT/XSDT entries.
   end record;
   for RSDT use record
      Header at 0 range 0 .. 287;
   end record;

   --  Search for an ACPI table and return its address, null if not found.
   --  The table will not necessarily be mapped.
   type Table_Record is record
      Virt_Addr : Integer_Address;
      Index     : Unsigned_64;
   end record;
   procedure FindTable (Signature : SDT_Signature; Table : out Table_Record);
   procedure Unref_Table (Table : Table_Record);
   ----------------------------------------------------------------------------
   --  Power management functions.

   --  Sleep levels supported.
   --  https://uefi.org/htmlspecs/ACPI_Spec_6_4_html/
   --  16_Waking_and_Sleeping/sleeping-states.html
   type Sleep_Level is (S0, S1, S2, S3, S4, S5);

   --  Enter sleep, execution will stay in this function until woken up.
   procedure Enter_Sleep (Level : Sleep_Level; Success : out Boolean);

   --  Reboot the system.
   procedure Do_Reboot;

   #if ArchName = """x86_64-limine"""
      Has_Power_Button : Boolean := False;
      Has_Sleep_Button : Boolean := False;
   #end if;

private

   --  uACPI is incompatible with the concept of SPARK.
   pragma SPARK_Mode (Off);

   --  Actual uACPI bindings.
   --  These are in an architecture if-block for architectures that dont
   --  support ACPI to not have to make 30 quintillion stubs.
   #if ArchName = """x86_64-limine"""
      type Status is
         (Status_OK,
          Status_Mapping_Failed,
          Status_Out_Of_Memory,
          Status_Bad_Checksum,
          Status_Invalid_Signature,
          Status_Invalid_Table_Length,
          Status_Not_Found,
          Status_Invalid_Argument,
          Status_Unimplemented,
          Status_Already_Exists,
          Status_Internal_Error,
          Status_Type_Mismatch,
          Status_Init_Level_Mismatch,
          Status_Namespace_Node_Dangling,
          Status_No_Handler,
          Status_No_Resource_End_Tag,
          Status_Compiled_Out,
          Status_Hardware_Timeout,
          Status_Timeout,
          Status_Overridden,
          Status_Denied);
      for Status use
         (Status_OK => 0,
          Status_Mapping_Failed => 1,
          Status_Out_Of_Memory => 2,
          Status_Bad_Checksum => 3,
          Status_Invalid_Signature => 4,
          Status_Invalid_Table_Length => 5,
          Status_Not_Found => 6,
          Status_Invalid_Argument => 7,
          Status_Unimplemented => 8,
          Status_Already_Exists => 9,
          Status_Internal_Error => 10,
          Status_Type_Mismatch => 11,
          Status_Init_Level_Mismatch => 12,
          Status_Namespace_Node_Dangling => 13,
          Status_No_Handler => 14,
          Status_No_Resource_End_Tag => 15,
          Status_Compiled_Out => 16,
          Status_Hardware_Timeout => 17,
          Status_Timeout => 18,
          Status_Overridden => 19,
          Status_Denied => 20);
      for Status'Size use 32;

      type Sleep_State is
         (Sleep_S0,
          Sleep_S1,
          Sleep_S2,
          Sleep_S3,
          Sleep_S4,
          Sleep_S5);
      for Sleep_State use
         (Sleep_S0 => 0,
          Sleep_S1 => 1,
          Sleep_S2 => 2,
          Sleep_S3 => 3,
          Sleep_S4 => 4,
          Sleep_S5 => 5);
      for Sleep_State'Size use 32;

      type Log_Level is
         (Log_Error,
          Log_Warn,
          Log_Info,
          Log_Trace,
          Log_Debug);
      for Log_Level use
         (Log_Error => 1,
          Log_Warn  => 2,
          Log_Info  => 3,
          Log_Trace => 4,
          Log_Debug => 5);
      for Log_Level'Size use 32;

      type Fixed_Event is
         (Fixed_Event_Timer_Status,
          Fixed_Event_Power_Button,
          Fixed_Event_Sleep_Button,
          Fixed_Event_RTC);
      for Fixed_Event use
         (Fixed_Event_Timer_Status => 1,
          Fixed_Event_Power_Button => 2,
          Fixed_Event_Sleep_Button => 3,
          Fixed_Event_RTC          => 4);
      for Fixed_Event'Size use 32;

      type Interrupt_Model is
         (Interrupt_Model_PIC,
          Interrupt_Model_IOAPIC,
          Interrupt_Model_IOSAPIC);
      for Interrupt_Model use
         (Interrupt_Model_PIC     => 0,
          Interrupt_Model_IOAPIC  => 1,
          Interrupt_Model_IOSAPIC => 2);
      for Interrupt_Model'Size use 32;
      -------------------------------------------------------------------------
      --  Bindings we provide.
      function Get_RSDP (Addr : access Unsigned_64) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_get_rsdp";

      procedure Stall (USec : Unsigned_8)
         with Export, Convention => C, External_Name => "uacpi_kernel_stall";

      procedure Sleep (MSec : Unsigned_64)
         with Export, Convention => C, External_Name => "uacpi_kernel_sleep";

      function Create_Event return System.Address
         with Export, Convention => C,
              External_Name => "uacpi_kernel_create_event";

      procedure Free_Event (Handle : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_free_event";

      function Create_Spinlock return System.Address
         with Export, Convention => C,
              External_Name => "uacpi_kernel_create_spinlock";

      procedure Free_Spinlock (Handle : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_free_spinlock";

      function Lock_Spinlock (Handle : System.Address) return Unsigned_64
         with Export, Convention => C,
              External_Name => "uacpi_kernel_lock_spinlock";

      procedure Unlock_Spinlock (Handle : System.Address; Flags : Unsigned_64)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_unlock_spinlock";

      function Alloc (Size : size_t) return System.Address
         with Export, Convention => C, External_Name => "uacpi_kernel_alloc";

      procedure Free (Ptr : System.Address)
         with Export, Convention => C, External_Name => "uacpi_kernel_free";

      type PCI_Address is record
         Segment : Unsigned_16;
         Bus     : Unsigned_8;
         Device  : Unsigned_8;
         Func    : Unsigned_8;
      end record with Convention => C_Pass_By_Copy;

      function PCI_Device_Open
         (Address : PCI_Address;
          Handle  : out System.Address) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_device_open";

      procedure PCI_Device_Close (Handle : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_device_close";

      function PCI_Read8
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : out Unsigned_8) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_read8";

      function PCI_Write8
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : Unsigned_8) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_write8";

      function PCI_Read16
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : out Unsigned_16) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_read16";

      function PCI_Write16
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : Unsigned_16) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_write16";

      function PCI_Read32
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : out Unsigned_32) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_read32";

      function PCI_Write32
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : Unsigned_32) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_pci_write32";

      function IO_Map
         (Base   : Unsigned_64;
          Len    : size_t;
          Handle : out System.Address) return Status
         with Export, Convention => C, External_Name => "uacpi_kernel_io_map";

      procedure IO_Unmap (Handle : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_io_unmap";

      function IO_Read8
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : out Unsigned_8) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_io_read8";

      function IO_Write8
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : Unsigned_8) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_io_write8";

      function IO_Read16
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : out Unsigned_16) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_io_read16";

      function IO_Write16
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : Unsigned_16) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_io_write16";

      function IO_Read32
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : out Unsigned_32) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_io_read32";

      function IO_Write32
         (Handle     : System.Address;
          Offset     : size_t;
          Value      : Unsigned_32) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_io_write32";

      function Map
         (Phys_Addr : Unsigned_64;
          Length    : size_t) return System.Address
         with Export, Convention => C, External_Name => "uacpi_kernel_map";

      procedure Unmap (Address : System.Address; Length : size_t)
         with Export, Convention => C, External_Name => "uacpi_kernel_unmap";

      function Wait_For_Work_Completion return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_wait_for_work_completion";

      function Install_Interrupt_Handler
         (IRQ     : Unsigned_32;
          Handler : System.Address;
          Context : System.Address;
          Handle  : out System.Address) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_install_interrupt_handler";

      function Uninstall_Interrupt_Handler
         (Handler : System.Address;
          Handle  : System.Address) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_uninstall_interrupt_handler";

      function Schedule_Work
         (Work_Type    : int;
          Work_Handler : System.Address;
          Context      : System.Address) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_schedule_work";

      function Handle_Firmware_Request (Request : System.Address) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_handle_firmware_request";

      procedure Signal_Event (Event : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_signal_event";

      procedure Reset_Event (Event : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_reset_event";

      function Wait_For_Event
         (Handle  : System.Address;
          Timeout : Unsigned_16) return int
         with Export, Convention => C,
              External_Name => "uacpi_kernel_wait_for_event";

      function Get_Nanoseconds_Since_Boot return Unsigned_64
         with Export, Convention => C,
              External_Name => "uacpi_kernel_get_nanoseconds_since_boot";

      function Create_Mutex return System.Address
         with Export, Convention => C,
              External_Name => "uacpi_kernel_create_mutex";

      procedure Free_Mutex (Handle : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_free_mutex";

      function Acquire_Mutex
         (Handle  : System.Address;
          Timeout : Unsigned_16) return Status
         with Export, Convention => C,
              External_Name => "uacpi_kernel_acquire_mutex";

      procedure Release_Mutex (Handle : System.Address)
         with Export, Convention => C,
              External_Name => "uacpi_kernel_release_mutex";

      function Get_Thread_ID return Unsigned_64
         with Export, Convention => C,
              External_Name => "uacpi_kernel_get_thread_id";

      procedure Kernel_Log (Level : int; Str_Addr : System.Address)
         with Export, Convention => C, External_Name => "uacpi_kernel_log";
      -------------------------------------------------------------------------
      --  Provided API.
      function Initialize (Flags : Unsigned_64) return Status
         with Import, Convention => C, External_Name => "uacpi_initialize";

      function Namespace_Load return Status
         with Import, Convention => C, External_Name => "uacpi_namespace_load";

      function Namespace_Init return Status
         with Import, Convention => C,
              External_Name => "uacpi_namespace_initialize";

      procedure Context_Set_Log_Level (Level : Log_Level)
         with Import, Convention => C,
              External_Name => "uacpi_context_set_log_level";

      function Reboot return Status
         with Import, Convention => C, External_Name => "uacpi_reboot";

      function Prepare_For_Sleep (State : Sleep_State) return Status
         with Import, Convention => C,
              External_Name => "uacpi_prepare_for_sleep_state";

      function Enter_Sleep (State : Sleep_State) return Status
         with Import, Convention => C,
              External_Name => "uacpi_enter_sleep_state";

      function Setup_Early_Table_Access
         (Buffer_Addr : System.Address;
          Size        : Unsigned_64) return Status
         with Import, Convention => C,
              External_Name => "uacpi_setup_early_table_access";

      function Find_Table_By_Signature
         (Signature_Addr    : System.Address;
          Table_Record_Addr : System.Address) return Status
         with Import, Convention => C,
              External_Name => "uacpi_table_find_by_signature";

      function Unref_Table (Table_Record_Addr : System.Address) return Status
         with Import, Convention => C, External_Name => "uacpi_table_unref";

      function Power_Button_Handler return Unsigned_32 with Convention => C;
      function Sleep_Button_Handler return Unsigned_32 with Convention => C;

      procedure Generic_uACPI_Handler (Num : Integer);

      function Set_Interrupt_Model
         (Model : Interrupt_Model) return Status
         with Import, Convention => C,
              External_Name => "uacpi_set_interrupt_model";

      function Install_Fixed_Event_Handler
         (Event   : Fixed_Event;
          Handler : System.Address;
          User    : System.Address) return Status
         with Import, Convention => C,
              External_Name => "uacpi_install_fixed_event_handler";
   #end if;
end Arch.ACPI;
