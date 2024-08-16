--  arch-gdt.ads: Specification of the GDT utilities.
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
with Interfaces; use Interfaces;

package Arch.GDT is
   --  Indexes of the corresponding segments in the GDT.
   Kernel_Code64_Segment : constant := 16#28#;
   Kernel_Data64_Segment : constant := 16#30#;
   User_Data64_Segment   : constant := 16#38#;
   User_Code64_Segment   : constant := 16#40#;
   TSS_Segment           : constant := 16#48#;

   --  Initialize the global GDT and load it on the callee core.
   procedure Init;

   --  Load the GDT in the callee core.
   procedure Load_GDT;

   --  Load an address on the active TSS of the GDT.
   type TSS is record
      Unused0     : Unsigned_32;
      Stack_Ring0 : System.Address;
      Stack_Ring1 : System.Address;
      Stack_Ring2 : System.Address;
      Unused1     : Unsigned_64;
      IST1        : System.Address;
      IST2        : System.Address;
      IST3        : System.Address;
      IST4        : System.Address;
      IST5        : System.Address;
      IST6        : System.Address;
      IST7        : System.Address;
      Unused2     : Unsigned_64;
      IOPB        : Unsigned_32;
   end record;
   for TSS use record
      Unused0     at 0 range   0 .. 31;
      Stack_Ring0 at 0 range  32 .. 95;
      Stack_Ring1 at 0 range  96 .. 159;
      Stack_Ring2 at 0 range 160 .. 223;
      Unused1     at 0 range 224 .. 287;
      IST1        at 0 range 288 .. 351;
      IST2        at 0 range 352 .. 415;
      IST3        at 0 range 416 .. 479;
      IST4        at 0 range 480 .. 543;
      IST5        at 0 range 544 .. 607;
      IST6        at 0 range 608 .. 671;
      IST7        at 0 range 672 .. 735;
      Unused2     at 0 range 736 .. 799;
      IOPB        at 0 range 800 .. 831;
   end record;
   for TSS'Size use 832;

   procedure Load_TSS (Address : System.Address);
end Arch.GDT;
