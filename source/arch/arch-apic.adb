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

with System.Storage_Elements; use System.Storage_Elements;
with Arch.ACPI;
with Arch.Wrappers;

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

   function Get_LAPIC_Base return Virtual_Address is
      MSR_Read : constant Unsigned_64 := Arch.Wrappers.Read_MSR (LAPIC_MSR);
   begin
      return Virtual_Address ((MSR_Read and 16#FFFFF000#) + Memory_Offset);
   end Get_LAPIC_Base;

   function LAPIC_Read (Register : Unsigned_32) return Unsigned_32 is
      Base       : constant System.Address := To_Address (Get_LAPIC_Base);
      Value_Addr : constant System.Address := Base + Storage_Offset (Register);
      Value_Mem  : Unsigned_32 with Address => Value_Addr, Volatile;
   begin
      return Value_Mem;
   end LAPIC_Read;

   procedure LAPIC_Write (Register : Unsigned_32; Value : Unsigned_32) is
      Base       : constant System.Address := To_Address (Get_LAPIC_Base);
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

   type IOAPIC_Array is array (Natural range <>) of ACPI.MADT_IOAPIC;
   type ISO_Array    is array (Natural range <>) of ACPI.MADT_ISO;

   MADT_IOAPICs : access IOAPIC_Array;
   MADT_ISOs    : access ISO_Array;

   function Init_IOAPIC return Boolean is
      Addr : constant Virtual_Address := ACPI.FindTable (ACPI.MADT_Signature);
      MADT           : ACPI.MADT with Address => To_Address (Addr);
      MADT_Length    : constant Unsigned_32 := MADT.Header.Length;
      Current_Byte   : Unsigned_32          := 0;
      Current_IOAPIC : Natural              := 1;
      IOAPIC_Count   : Natural              := 0;
      Current_ISO    : Natural              := 1;
      ISO_Count      : Natural              := 0;
   begin
      if Addr = Null_Address then
         return False;
      end if;

      --  Check how many entries do we need to allocate.
      while (Current_Byte + ((MADT'Size / 8) - 1)) < MADT_Length loop
         declare
            Header : ACPI.MADT_Header;
            for Header'Address use
               MADT.Entries_Start'Address + Storage_Offset (Current_Byte);
         begin
            case Header.Entry_Type is
               when ACPI.MADT_IOAPIC_Type => IOAPIC_Count := IOAPIC_Count + 1;
               when ACPI.MADT_ISO_Type    => ISO_Count    := ISO_Count    + 1;
               when others                => null;
            end case;
            Current_Byte := Current_Byte + Unsigned_32 (Header.Length);
         end;
      end loop;

      --  Allocate and fill the entries.
      Current_Byte := 0;
      MADT_IOAPICs := new IOAPIC_Array (1 .. IOAPIC_Count);
      MADT_ISOs    := new ISO_Array    (1 .. ISO_Count);
      while (Current_Byte + ((MADT'Size / 8) - 1)) < MADT_Length loop
         declare
            IOAPIC : ACPI.MADT_IOAPIC;
            ISO    : ACPI.MADT_ISO;
            for IOAPIC'Address use
               MADT.Entries_Start'Address + Storage_Offset (Current_Byte);
            for ISO'Address use
               MADT.Entries_Start'Address + Storage_Offset (Current_Byte);
         begin
            case IOAPIC.Header.Entry_Type is
               when ACPI.MADT_IOAPIC_Type =>
                  MADT_IOAPICs (Current_IOAPIC) := IOAPIC;
                  Current_IOAPIC := Current_IOAPIC + 1;
               when ACPI.MADT_ISO_Type =>
                  MADT_ISOs (Current_ISO) := ISO;
                  Current_ISO := Current_ISO + 1;
               when others => null;
            end case;
            Current_Byte := Current_Byte + Unsigned_32 (IOAPIC.Header.Length);
         end;
      end loop;

      return True;
   end Init_IOAPIC;

   function IOAPIC_Set_Redirect
      (LAPIC_ID  : Unsigned_32;
       IRQ       : IDT.IRQ_Index;
       IDT_Entry : IDT.IDT_Index;
       Enable    : Boolean) return Boolean is
      Actual_IRQ : constant Unsigned_8  :=
         Unsigned_8 (IRQ) - Unsigned_8 (IDT.IRQ_Index'First);
   begin
      for ISO of MADT_ISOs.all loop
         if ISO.IRQ_Source = Actual_IRQ then
            return IOAPIC_Set_Redirect (LAPIC_ID, ISO.GSI, IDT_Entry,
                                        ISO.Flags, Enable);
         end if;
      end loop;
      return IOAPIC_Set_Redirect (LAPIC_ID, Unsigned_32 (Actual_IRQ),
                                  IDT_Entry, 0, Enable);
   end IOAPIC_Set_Redirect;

   function IOAPIC_Set_Redirect
      (LAPIC_ID  : Unsigned_32;
       GSI       : Unsigned_32;
       IDT_Entry : IDT.IDT_Index;
       Flags     : Unsigned_16;
       Enable    : Boolean) return Boolean is
      GSIB        :          Unsigned_32     := 0;
      Redirect    :          Unsigned_64     := Unsigned_64 (IDT_Entry) - 1;
      IOREDTBL    : constant Unsigned_32     := (GSI - GSIB) * 2 + 16;
      IOAPIC_MMIO : constant Virtual_Address :=
         Get_IOAPIC_From_GSI (GSI, GSIB);
   begin
      --  Check if the IOAPIC could be found.
      if IOAPIC_MMIO = Null_Address then
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
       GSIB : out Unsigned_32) return Virtual_Address is
   begin
      for IOAPIC of MADT_IOAPICs.all loop
         if IOAPIC.GSIB <= GSI and
            IOAPIC.GSIB + Get_IOAPIC_GSI_Count
               (Virtual_Address (IOAPIC.Address) + Memory_Offset) > GSI
         then
            GSIB := IOAPIC.GSIB;
            return Virtual_Address (IOAPIC.Address) + Memory_Offset;
         end if;
      end loop;
      GSIB := 0;
      return Null_Address;
   end Get_IOAPIC_From_GSI;

   function Get_IOAPIC_GSI_Count (MMIO : Virtual_Address) return Unsigned_32 is
      Read : constant Unsigned_32 := IOAPIC_Read (MMIO, IOAPIC_VER_Register);
   begin
      --  The number of GSIs handled by the IOAPIC is in its IOAPICVER register
      --  in bits 16 - 23;
      return Shift_Right (Read and 16#FF0000#, 16);
   end Get_IOAPIC_GSI_Count;

   function IOAPIC_Read
      (MMIO     : Virtual_Address;
       Register : Unsigned_32) return Unsigned_32 is
      Value_Reg : Unsigned_32 with Address => To_Address (MMIO),      Volatile;
      Value     : Unsigned_32 with Address => To_Address (MMIO + 16), Volatile;
   begin
      Value_Reg := Register;
      return Value;
   end IOAPIC_Read;

   procedure IOAPIC_Write
      (MMIO     : Virtual_Address;
       Register : Unsigned_32;
       To_Write : Unsigned_32) is
      Value_Reg : Unsigned_32 with Address => To_Address (MMIO),      Volatile;
      Value     : Unsigned_32 with Address => To_Address (MMIO + 16), Volatile;
   begin
      Value_Reg := Register;
      Value     := To_Write;
   end IOAPIC_Write;
end Arch.APIC;