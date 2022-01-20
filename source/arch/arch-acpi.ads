--  arch-acpi.ads: Specification of the ACPI parsing and scanning.
--  Copyright (C) 2021 streaksu
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

with System;
with Interfaces;

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

   --  Scan the ACPI tables from the RSDP, true on success, false on failure.
   function ScanTables (RSDP_Address : System.Address) return Boolean;

   --  Search for an ACPI table and return its address, null if not found.
   function FindTable (Signature : SDT_Signature) return System.Address;
end Arch.ACPI;
