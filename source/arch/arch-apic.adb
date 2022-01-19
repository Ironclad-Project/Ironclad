--  arch-apic.adb: IO/LAPIC driver.
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

with Arch.MSR;
with System.Storage_Elements; use System.Storage_Elements;

package body Arch.APIC is
   --  TODO: Allocate the LAPIC base in a core-specific way instead of fetching
   --  it each time.

   LAPIC_MSR               : constant := 16#01B#;
   LAPIC_EOI_Register      : constant := 16#0B0#;
   LAPIC_Spurious_Register : constant := 16#0F0#;

   procedure Init_LAPIC is
      Value    : constant Unsigned_32 := LAPIC_Read (LAPIC_Spurious_Register);
      To_Write : constant Unsigned_32 := Value or LAPIC_Spurious_Entry;
   begin
      --  Enable the LAPIC by setting the spurious interrupt vector and
      --  ORing the enable bit.
      LAPIC_Write (LAPIC_Spurious_Register, To_Write or Shift_Right (1, 8));
   end Init_LAPIC;

   procedure LAPIC_EOI is
   begin
      LAPIC_Write (LAPIC_EOI_Register, 0);
   end LAPIC_EOI;

   function Get_LAPIC_Base return System.Address is
      Value : constant Unsigned_64 := Arch.MSR.Read (LAPIC_MSR);
   begin
      return System'To_Address (Value and 16#FFFFF000#);
   end Get_LAPIC_Base;

   function LAPIC_Read (Register : Unsigned_32) return Unsigned_32 is
      Base       : constant System.Address := Get_LAPIC_Base;
      Value_Addr : constant System.Address := Base + Storage_Offset (Register);
      Value_Mem  : Unsigned_32 with Address => Value_Addr, Volatile;
   begin
      return Value_Mem;
   end LAPIC_Read;

   procedure LAPIC_Write (Register : Unsigned_32; Value : Unsigned_32) is
      Base       : constant System.Address := Get_LAPIC_Base;
      Value_Addr : constant System.Address := Base + Storage_Offset (Register);
      Value_Mem  : Unsigned_32 with Address => Value_Addr, Volatile;
   begin
      Value_Mem := Value;
   end LAPIC_Write;
end Arch.APIC;
