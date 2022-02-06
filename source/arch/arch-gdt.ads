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

with Memory; use Memory;

package Arch.GDT is
   --  Indexes of the corresponding segments in the GDT.
   Kernel_Code64_Segment : constant := 16#28#;
   Kernel_Data64_Segment : constant := 16#30#;
   User_Data64_Segment   : constant := 16#38#;
   User_Code64_Segment   : constant := 16#42#;
   TSS_Segment           : constant := 16#48#;

   --  Initialize the global GDT and load it on the callee core.
   procedure Init;

   --  Load the GDT in the callee core.
   procedure Load_GDT;

   --  Load an address on the active TSS of the GDT.
   procedure Load_TSS (Address : Virtual_Address);
end Arch.GDT;
