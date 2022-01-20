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

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.ACPI;
with Arch.MSR;

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
   begin
      return System'To_Address (Arch.MSR.Read (LAPIC_MSR) and 16#FFFFF000#);
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

   ----------------------------------------------------------------------------
   IOAPIC_VER_Register : constant := 1;

   IOAPIC_IOREDTBL_Pin_Polarity : constant Unsigned_64 := Shift_Left (1, 13);
   IOAPIC_IOREDTBL_Trigger_Mode : constant Unsigned_64 := Shift_Left (1, 15);
   IOAPIC_IOREDTBL_Mask         : constant Unsigned_64 := Shift_Left (1, 16);

   MADT_Address : System.Address := System.Null_Address;

   function Init_IOAPIC return Boolean is
   begin
      MADT_Address := ACPI.FindTable (ACPI.MADT_Signature);
      return MADT_Address /= System.Null_Address;
   end Init_IOAPIC;

   function IOAPIC_Set_Redirect
      (LAPIC_ID  : Unsigned_32;
       IRQ       : IDT.IDT_Index;
       IDT_Entry : IDT.IDT_Index;
       Enable    : Boolean) return Boolean is
      MADT         : ACPI.MADT with Address => MADT_Address;
      MADT_Length  : constant Unsigned_32 := MADT.Header.Length;
      Current_Byte : Unsigned_32          := 0;
   begin
      while (Current_Byte + (MADT'Size - 1)) < MADT_Length loop
         declare
            ISO : ACPI.MADT_ISO;
            for ISO'Address use
               MADT.Entries_Start'Address + Storage_Offset (Current_Byte);
         begin
            if ISO.Header.Entry_Type = ACPI.MADT_ISO_Type and
               ISO.IRQ_Source        = Unsigned_8 (IRQ)
            then
               return IOAPIC_Set_Redirect (LAPIC_ID, ISO.GSI, IDT_Entry,
                                           ISO.Flags, Enable);
            end if;
            Current_Byte := Current_Byte + Unsigned_32 (ISO.Header.Length);
         end;
      end loop;
      return IOAPIC_Set_Redirect (LAPIC_ID, Unsigned_32 (IRQ), IDT_Entry, 0,
                                  Enable);
   end IOAPIC_Set_Redirect;

   function IOAPIC_Set_Redirect
      (LAPIC_ID  : Unsigned_32;
       GSI       : Unsigned_32;
       IDT_Entry : IDT.IDT_Index;
       Flags     : Unsigned_16;
       Enable    : Boolean) return Boolean is
      GSIB        :          Unsigned_32    := 0;
      IOAPIC_MMIO : constant System.Address := Get_IOAPIC_From_GSI (GSI, GSIB);
      Redirect    :          Unsigned_64    := Unsigned_64 (IDT_Entry);
      IOREDTBL    : constant Unsigned_32    := (GSI - GSIB) * 2 + 16;
   begin
      --  Check if the IOAPIC could be found.
      if IOAPIC_MMIO = System.Null_Address then
         return False;
      end if;

      --  Build the redirect value by translating the ISO flags into IOREDTBL
      --  flags and the enable.
      if (Flags and IOAPIC_ISO_Flag_Polarity) /= 0 then
         Redirect := Redirect or IOAPIC_IOREDTBL_Pin_Polarity;
      end if;
      if (Flags and IOAPIC_ISO_Flag_Trigger) /= 0 then
         Redirect := Redirect or IOAPIC_IOREDTBL_Trigger_Mode;
      end if;
      if Enable = False then
         Redirect := Redirect or IOAPIC_IOREDTBL_Mask;
      end if;
      Redirect := Redirect or Shift_Left (Unsigned_64 (LAPIC_ID), 56);

      declare
         Mask    : constant Unsigned_64 := 16#FFFFFFFF#;
         Lower32 : constant Unsigned_64 := Redirect and Mask;
         Upper32 : constant Unsigned_64 := Shift_Right (Redirect, 32) and Mask;
      begin
         IOAPIC_Write (IOAPIC_MMIO, IOREDTBL,     Unsigned_32 (Lower32));
         IOAPIC_Write (IOAPIC_MMIO, IOREDTBL + 1, Unsigned_32 (Upper32));
      end;
      return True;
   end IOAPIC_Set_Redirect;

   function Get_IOAPIC_From_GSI
      (GSI  : Unsigned_32;
       GSIB : out Unsigned_32) return System.Address is
      MADT         : ACPI.MADT with Address => MADT_Address;
      MADT_Length  : constant Unsigned_32 := MADT.Header.Length;
      Current_Byte : Unsigned_32          := 0;
   begin
      while (Current_Byte + ((MADT'Size / 8) - 1)) < MADT_Length loop
         declare
            IOAPIC : ACPI.MADT_IOAPIC;
            for IOAPIC'Address use
               MADT.Entries_Start'Address + Storage_Offset (Current_Byte);
            IOAPIC_MMIO : constant System.Address :=
               System'To_Address (IOAPIC.Address);
         begin
            if IOAPIC.Header.Entry_Type = ACPI.MADT_IOAPIC_Type          and
               IOAPIC.GSIB <= GSI                                        and
               IOAPIC.GSIB + Get_IOAPIC_GSI_Count (IOAPIC_MMIO) > GSI
            then
               GSIB := IOAPIC.GSIB;
               return IOAPIC_MMIO;
            end if;
            Current_Byte := Current_Byte + Unsigned_32 (IOAPIC.Header.Length);
         end;
      end loop;

      GSIB := 0;
      return System.Null_Address;
   end Get_IOAPIC_From_GSI;

   function Get_IOAPIC_GSI_Count (MMIO : System.Address) return Unsigned_32 is
      Read : constant Unsigned_32 := IOAPIC_Read (MMIO, IOAPIC_VER_Register);
   begin
      --  The number of GSIs handled by the IOAPIC is in its IOAPICVER register
      --  in bits 16 - 23;
      return Shift_Right (Read and 16#FF0000#, 16);
   end Get_IOAPIC_GSI_Count;

   function IOAPIC_Read
      (IOAPIC_MMIO : System.Address;
       Register    : Unsigned_32) return Unsigned_32 is
      Value_Register : Unsigned_32 with Address => IOAPIC_MMIO,      Volatile;
      Value_Final    : Unsigned_32 with Address => IOAPIC_MMIO + 16, Volatile;
   begin
      Value_Register := Register;
      return Value_Final;
   end IOAPIC_Read;

   procedure IOAPIC_Write
      (IOAPIC_MMIO : System.Address;
       Register    : Unsigned_32;
       Value       : Unsigned_32) is
      Value_Register : Unsigned_32 with Address => IOAPIC_MMIO,      Volatile;
      Value_Final    : Unsigned_32 with Address => IOAPIC_MMIO + 16, Volatile;
   begin
      Value_Register := Register;
      Value_Final    := Value;
   end IOAPIC_Write;
end Arch.APIC;
