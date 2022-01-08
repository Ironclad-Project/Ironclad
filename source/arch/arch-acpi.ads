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

   --  Scan the ACPI tables from the RSDP, true on success, false on failure.
   function ScanTables (RSDP_Address : System.Address) return Boolean;

   --  Search for an ACPI table and return a pointer to its SDT.
   function FindTable (Signature : SDT_Signature) return access SDT_Header;
end Arch.ACPI;
