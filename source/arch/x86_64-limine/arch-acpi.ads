--  arch-acpi.ads: ACPI parsing and scanning.
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
with Memory; use Memory;

package Arch.ACPI is
   --  SDT header, which leads all ACPI tables.
   type SDT_Signature is new String (1 .. 4);
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
   end record;
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

   --  Scan the ACPI tables, true on success, false on failure.
   function ScanTables return Boolean;

   --  Search for an ACPI table and return its address, null if not found.
   function FindTable (Signature : SDT_Signature) return Virtual_Address;
end Arch.ACPI;
