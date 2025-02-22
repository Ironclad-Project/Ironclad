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

with Memory;     use Memory;
with Arch.ACPI;
with Arch.MMU;
with Arch.Snippets;

package body Arch.HPET with SPARK_Mode => Off is
   Is_Initialized : Boolean := False;
   HPET_Contents  : Virtual_Address;
   HPET_Period    : Unsigned_64; --  Time in femtoseconds to increment by 1.
   HPET_Frequency : Unsigned_64 := 0;

   function Init return Boolean is
      ACPI_Address : Virtual_Address;
   begin
      ACPI_Address := ACPI.FindTable (ACPI.HPET_Signature);
      if ACPI_Address = Null_Address then
         return False;
      end if;

      declare
         Success : Boolean;
         Table : ACPI.HPET
            with Import, Address => To_Address (ACPI_Address);
         HPET : ACPI.HPET_Contents with Import,
            Address => To_Address (Table.Address + Memory_Offset);
         pragma Volatile (HPET);
      begin
         MMU.Map_Range
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
             Success        => Success,
             Caching        => MMU.Uncacheable);
         if not Success then
            return False;
         end if;

         --  TODO: Check if the HPET is 64 bits, if so, enable 64 bit mode.
         --  So far we are mode-agnostic, but we could use a timer upgrade.
         if (Shift_Right (HPET.General_Capabilities, 13) and 1) = 0 then
            return False;
         end if;

         HPET_Contents  := Table.Address + Memory_Offset;
         HPET_Period    := Shift_Right (HPET.General_Capabilities, 32);
         HPET_Frequency := 1_000_000_000_000_000 / HPET_Period;

         --  Disable the HPET by writting 0 the Enable CNF, so we can reset the
         --  counter, and then enable again.
         HPET.General_Configuration := 0;
         HPET.Main_Counter_Value    := 0;
         HPET.General_Configuration := 1;
      end;
      Is_Initialized := True;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Init;

   procedure Get_Frequency (Freq : out Unsigned_64) is
   begin
      Freq := HPET_Frequency;
   end Get_Frequency;

   procedure Get_Counter (Counter : out Unsigned_64) is
      HPET : ACPI.HPET_Contents
         with Import, Address => To_Address (HPET_Contents);
   begin
      if Is_Initialized then
         Counter := HPET.Main_Counter_Value;
      else
         Counter := 0;
      end if;
   end Get_Counter;

   procedure NSleep (Nanoseconds : Positive) is
      --  Reads must be atomic according to spec in 64-bit mode, for 32-bit
      --  mode it doesnt hurt either.
      HPET : ACPI.HPET_Contents
         with Import, Address => To_Address (HPET_Contents);
      Target : Unsigned_64;
   begin
      if not Is_Initialized then
         return;
      end if;

      Target := HPET.Main_Counter_Value + (Unsigned_64 (Nanoseconds) *
         (1_000_000 / HPET_Period));
      while HPET.Main_Counter_Value < Target loop
         Snippets.Pause;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end NSleep;
end Arch.HPET;
