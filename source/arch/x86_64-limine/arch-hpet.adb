--  arch-hpet.adb: High Precision Event Timer driver.
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
with Memory;     use Memory;
with Arch.ACPI;
with Arch.MMU;

package body Arch.HPET with SPARK_Mode => Off is
   HPET_Contents : Virtual_Address;
   HPET_Period   : Unsigned_64; --  Time in femtoseconds to increment by 1.

   procedure Init is
      ACPI_Address : Virtual_Address;
   begin
      Is_Initialized := False;

      ACPI_Address := ACPI.FindTable (ACPI.HPET_Signature);
      if ACPI_Address = Null_Address then
         return;
      end if;

      declare
         Table : ACPI.HPET
            with Import, Address => To_Address (ACPI_Address);
         HPET : ACPI.HPET_Contents with Import,
            Address => To_Address (Table.Address + Memory_Offset);
         pragma Volatile (HPET);
      begin
         if not MMU.Map_Range
            (Map            => MMU.Kernel_Table,
             Physical_Start => To_Address (Table.Address),
             Virtual_Start  => To_Address (Table.Address + Memory_Offset),
             Length         => MMU.Page_Size,
             Permissions    =>
              (Is_User_Accesible => False,
               Can_Read          => True,
               Can_Write         => True,
               Can_Execute       => False,
               Is_Global         => True),
             Caching        => MMU.Uncacheable)
         then
            return;
         end if;

         HPET_Contents := Table.Address + Memory_Offset;
         HPET_Period   := Shift_Right (HPET.General_Capabilities, 32);

         --  TODO: Check if the HPET is 64 bits, if so, enable 64 bit mode.
         --  So far we are mode-agnostic, but we could use a timer upgrade.
         --  HPET_Is64 := Shift_Right (HPET.General_Capabilities, 13) and 1;

         --  Disable the HPET by writting 0 the Enable CNF, so we can reset the
         --  counter, and then enable again.
         HPET.General_Configuration := 0;
         HPET.Main_Counter_Value    := 0;
         HPET.General_Configuration := 1;
      end;
      Is_Initialized := True;
   end Init;

   procedure USleep (Microseconds : Positive) is
   begin
      NSleep (Microseconds * 1000);
   end USleep;

   procedure NSleep (Nanoseconds : Positive) is
      --  Reads must be atomic according to spec in 64-bit mode, for 32-bit
      --  mode it doesnt hurt either.
      HPET    : ACPI.HPET_Contents with Address => To_Address (HPET_Contents);
      Counter : Unsigned_64
         with Atomic, Address => HPET.Main_Counter_Value'Address;

      FemtoSec : constant Unsigned_64 := Unsigned_64 (Nanoseconds * 1000000);
      To_Add   : constant Unsigned_64 := FemtoSec / HPET_Period;
      Target   : constant Unsigned_64 := Counter + To_Add;
   begin
      if not Is_Initialized then
         return;
      end if;

      loop
         if Counter > Target then
            exit;
         end if;
      end loop;
   end NSleep;
end Arch.HPET;
