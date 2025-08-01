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

with Memory; use Memory;
with Arch.ACPI;
with Arch.MMU;
with Arch.Snippets;

package body Arch.HPET with SPARK_Mode => Off is
   HPET_Contents   : Virtual_Address;
   HPET_Resolution : Unsigned_64; --  Time in nanoseconds to increment by 1.

   function Init return Boolean is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
      ACPI_Address : ACPI.Table_Record;
   begin
      ACPI.FindTable (ACPI.HPET_Signature, ACPI_Address);
      if ACPI_Address.Virt_Addr = Null_Address then
         return False;
      end if;

      declare
         Success : Boolean;
         Table : ACPI.HPET
            with Import, Address => To_Address (ACPI_Address.Virt_Addr);
         HPET : ACPI.HPET_Contents with Import, Volatile,
            Address => To_Address (Table.Address + Memory_Offset);
      begin
         ACPI.Unref_Table (ACPI_Address);

         MMU.Map_Range
            (Map            => MMU.Kernel_Table,
             Physical_Start => To_Address (Table.Address),
             Virtual_Start  => To_Address (Table.Address + Memory_Offset),
             Length         => MMU.Page_Size,
             Permissions    =>
              (Is_User_Accessible => False,
               Can_Read          => True,
               Can_Write         => True,
               Can_Execute       => False,
               Is_Global         => True),
             Success        => Success,
             Caching        => MMU.Uncacheable);
         if not Success then
            return False;
         end if;

         HPET_Contents   := Table.Address + Memory_Offset;
         HPET_Resolution := Shift_Right (HPET.General_Capabilities, 32);

         --  Get the resolution from the femtoseconds, if sub 1, round up.
         HPET_Resolution := HPET_Resolution / 1_000_000;
         if HPET_Resolution = 0 then
            HPET_Resolution := 1;
         end if;

         --  Disable the HPET by writing 0 the Enable CNF, so we can reset the
         --  counter, and then enable again.
         HPET.General_Configuration := 0;
         HPET.Main_Counter_Value    := 0;
         HPET.General_Configuration := 1;
      end;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Init;

   procedure Get_Resolution (Resolution : out Unsigned_64) is
   begin
      Resolution := HPET_Resolution;
   end Get_Resolution;

   procedure NSleep (Nanoseconds : Unsigned_64) is
      --  Reads must be atomic according to spec in 64-bit mode, for 32-bit
      --  mode it doesnt hurt either.
      HPET : ACPI.HPET_Contents
         with Import, Address => To_Address (HPET_Contents);
      Target : Unsigned_64;
   begin
      Target := HPET.Main_Counter_Value + (Nanoseconds / HPET_Resolution);
      while HPET.Main_Counter_Value < Target loop
         Snippets.Pause;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end NSleep;
end Arch.HPET;
