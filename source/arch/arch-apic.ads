--  arch-apic.ads: Specification of the IO/LAPIC driver.
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

package Arch.APIC is
   --  Entry of the IDT for LAPIC spurious fires.
   LAPIC_Spurious_Entry : constant := 255;

   --  Initialize the Local APIC for the caller core.
   procedure Init_LAPIC;

   --  LAPIC End Of Interrupt routine, that is to be called at the end of
   --  an interrupt.
   procedure LAPIC_EOI;

private
   function Get_LAPIC_Base return System.Address;
   function LAPIC_Read (Register : Unsigned_32) return Unsigned_32;
   procedure LAPIC_Write (Register : Unsigned_32; Value : Unsigned_32);
end Arch.APIC;
