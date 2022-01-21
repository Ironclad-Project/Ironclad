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
with Arch.IDT;

package Arch.APIC is
   --  Entry of the IDT for LAPIC spurious fires.
   LAPIC_Spurious_Entry : constant := 255;

   --  Initialize the Local APIC for the caller core.
   procedure Init_LAPIC;

   --  LAPIC End Of Interrupt routine, that is to be called at the end of
   --  an interrupt.
   procedure LAPIC_EOI;

   ----------------------------------------------------------------------------

   --  Initialize the IOAPICs, return True in success or False on failure.
   function Init_IOAPIC return Boolean;

   --  Use the IOAPIC to redirect an IRQ to an IDT entry for a given LAPIC.
   --  Set enable to true or false to enable or disable it.
   --  Return true on success.
   function IOAPIC_Set_Redirect
      (LAPIC_ID  : Unsigned_32;
       IRQ       : IDT.IRQ_Index;
       IDT_Entry : IDT.IDT_Index;
       Enable    : Boolean) return Boolean;

   --  Use the IOAPIC to redirect a GSI to an IDT entry for a given LAPIC.
   --  Use ISO flags for setting specific flags for the GSI, and set enable to
   --  true or false to enable or disable it. Return true on success.
   IOAPIC_ISO_Flag_Polarity : constant Unsigned_16 := Shift_Left (1, 1);
   IOAPIC_ISO_Flag_Trigger  : constant Unsigned_16 := Shift_Left (1, 3);
   function IOAPIC_Set_Redirect
      (LAPIC_ID  : Unsigned_32;
       GSI       : Unsigned_32;
       IDT_Entry : IDT.IDT_Index;
       Flags     : Unsigned_16;
       Enable    : Boolean) return Boolean;
private
   function Get_LAPIC_Base return System.Address;
   function LAPIC_Read (Register : Unsigned_32) return Unsigned_32;
   procedure LAPIC_Write (Register : Unsigned_32; Value : Unsigned_32);

   function Get_IOAPIC_From_GSI
      (GSI  : Unsigned_32;
       GSIB : out Unsigned_32) return System.Address;
   function Get_IOAPIC_GSI_Count (MMIO : System.Address) return Unsigned_32;
   function IOAPIC_Read
      (IOAPIC_MMIO : System.Address;
       Register    : Unsigned_32) return Unsigned_32;
   procedure IOAPIC_Write
      (IOAPIC_MMIO : System.Address;
       Register    : Unsigned_32;
       Value       : Unsigned_32);
end Arch.APIC;
