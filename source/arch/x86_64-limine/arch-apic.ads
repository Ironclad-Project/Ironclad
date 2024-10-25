--  arch-apic.ads: IO/LAPIC driver.
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
with Arch.IDT;
with Memory; use Memory;

package Arch.APIC is
   --  Entry of the IDT for LAPIC spurious fires.
   LAPIC_Spurious_Entry : constant := 255;

   --  Initialize the Local APIC facilities.
   procedure Init_LAPIC;

   --  Initialize the LAPIC for the caller core.
   procedure Init_Core_LAPIC;

   --  Send an InterProcessor Interrupt to another LAPIC.
   procedure LAPIC_Send_IPI_Raw (LAPIC_ID : Unsigned_32; Code : Unsigned_32);
   procedure LAPIC_Send_IPI (LAPIC_ID : Unsigned_32; Vector : IDT.IDT_Index);

   --  Find out the LAPIC timer frequency, in Hz.
   function LAPIC_Timer_Calibrate return Unsigned_64;

   --  Stop the timer.
   procedure LAPIC_Timer_Stop;

   --  Setup the LAPIC timer to wait for some given time and then call the
   --  vector.
   procedure LAPIC_Timer_Oneshot
      (Vector       : IDT.IDT_Index;
       Hz           : Unsigned_64;
       Microseconds : Unsigned_64);

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

   function LAPIC_Read (Register : Unsigned_32) return Unsigned_32;
   procedure LAPIC_Write (Register : Unsigned_32; Value : Unsigned_32);

   function x2APIC_Read (Register : Unsigned_32) return Unsigned_64;
   procedure x2APIC_Write (Register : Unsigned_32; Value : Unsigned_64);

   procedure Get_IOAPIC_From_GSI
      (GSI    : Unsigned_32;
       GSIB   : out Unsigned_32;
       Result : out Virtual_Address);
   function Get_IOAPIC_GSI_Count (MMIO : Virtual_Address) return Unsigned_32;
   function IOAPIC_Read
      (MMIO     : Virtual_Address;
       Register : Unsigned_32) return Unsigned_32;
   procedure IOAPIC_Write
      (MMIO     : Virtual_Address;
       Register : Unsigned_32;
       To_Write : Unsigned_32);
end Arch.APIC;
