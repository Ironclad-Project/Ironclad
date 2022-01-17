--  arch-acpi.adb: ACPI parsing and scanning.
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

with System;     use System;
with Interfaces; use Interfaces;

package body Arch.ACPI is
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

   --  Globals to keep track of scanned tables.
   Use_XSDT     : Boolean        := False;
   Root_Address : System.Address := Null_Address;

   function ScanTables (RSDP_Address : System.Address) return Boolean is
      Table : RSDP;
      for Table'Address use RSDP_Address;
   begin
      if RSDP_Address = Null_Address or Table.Signature /= "RSD PTR " then
         return False;
      end if;

      if Table.Revision >= 2 and Table.XSDT_Address /= 0 then
         Use_XSDT     := True;
         Root_Address := System'To_Address (Table.XSDT_Address);
      else
         Use_XSDT     := False;
         Root_Address := System'To_Address (Table.RSDT_Address);
      end if;

      return True;
   end ScanTables;

   function FindTable (Signature : SDT_Signature) return System.Address is
      Root : RSDT;
      for Root'Address use Root_Address;

      Limit : constant Natural := (Natural (Root.Header.Length)
         - Root.Header'Size / 8) / (if Use_XSDT then 8 else 4);
      Returned : System.Address := System.Null_Address;
   begin
      for I in 1 .. Limit loop
         if Use_XSDT then
            declare
               Entries : XSDT_Entries (1 .. Limit);
               for Entries'Address use Root.Entries'Address;
            begin
               Returned := System'To_Address (Entries (I));
            end;
         else
            declare
               Entries : RSDT_Entries (1 .. Limit);
               for Entries'Address use Root.Entries'Address;
            begin
               Returned := System'To_Address (Entries (I));
            end;
         end if;

         declare
            Test_Header : SDT_Header;
            for Test_Header'Address use Returned;
         begin
            if Test_Header.Signature = Signature then
               return Returned;
            end if;
         end;
      end loop;

      return System.Null_Address;
   end FindTable;
end Arch.ACPI;
