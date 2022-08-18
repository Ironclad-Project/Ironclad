--  arch-wrappers.ads: Specification of some ASM wrappers.
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

with Interfaces; use Interfaces;
with Memory; use Memory;

package Arch.Wrappers with SPARK_Mode => Off is
   --  IO port wrappers.
   procedure Port_Out (Port : Unsigned_16; Value : Unsigned_8)
      with Inline_Always;
   function  Port_In (Port : Unsigned_16) return Unsigned_8
      with Inline_Always;
   ----------------------------------------------------------------------------
   --  Invalidate page.
   procedure Invalidate_Page (Value : Virtual_Address) with Inline_Always;
   ----------------------------------------------------------------------------
   --  Read an write MSRs.
   function Read_MSR (MSR : Unsigned_32) return Unsigned_64;
   procedure Write_MSR (MSR : Unsigned_32; Value : Unsigned_64);
   ----------------------------------------------------------------------------
   --  Read and write control registers.
   function Read_CR0 return Unsigned_64;
   procedure Write_CR0 (Value : Unsigned_64);
   function Read_CR3 return Unsigned_64;
   procedure Write_CR3 (Value : Unsigned_64);
   function Read_CR4 return Unsigned_64;
   procedure Write_CR4 (Value : Unsigned_64);
   ----------------------------------------------------------------------------
   --  Read and write userland and kernel FS and GS.
   function Read_FS return Unsigned_64;
   procedure Write_FS (Value : Unsigned_64);
   function Read_GS return Unsigned_64;
   procedure Write_GS (Value : Unsigned_64);
   function Read_Kernel_GS return Unsigned_64;
   procedure Write_Kernel_GS (Value : Unsigned_64);
   procedure Swap_GS;
end Arch.Wrappers;
