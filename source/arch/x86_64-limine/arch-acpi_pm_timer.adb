--  arch-acpi_pm_timer.adb: ACPI power management timer support.
--  Copyright (C) 2025 streaksu
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

package body Arch.ACPI_PM_Timer with SPARK_Mode => Off is
   procedure Find
      (Port    : out Unsigned_16;
       Mask    : out Unsigned_32;
       Success : out Boolean)
   is
      --  The FADT has to reach past the fields read, as older ones are
      --  shorter.
      PM_TMR_LEN_End   : constant := 92;
      Flags_End        : constant := 116;
      X_PM_TMR_BLK_End : constant := 220;

      --  Set where the count is 32 bits wide.
      TMR_VAL_EXT : constant := 16#100#;

      ACPI_Address : ACPI.Table_Record;
      Address      : Unsigned_64 := 0;
      Flags        : Unsigned_32 := 0;
   begin
      Port    := 0;
      Mask    := 0;
      Success := False;

      if not ACPI.Is_Supported then
         return;
      end if;

      ACPI.FindTable (ACPI.FADT_Signature, ACPI_Address);
      if ACPI_Address.Virt_Addr = Null_Address then
         return;
      end if;

      declare
         FADT   : ACPI.FADT
            with Import, Address => To_Address (ACPI_Address.Virt_Addr);
         Length : constant Unsigned_32 := FADT.Header.Length;
      begin
         --  PM_TMR_LEN is 4 where there is a timer, and 0 where there is not.
         if Length >= PM_TMR_LEN_End and then FADT.PM_TMR_LEN = 4 then
            --  The extended block only takes over where it is in I/O space.
            if Length >= X_PM_TMR_BLK_End and then
               FADT.X_PMTImerBlock.Address_Space = 1 and then
               FADT.X_PMTImerBlock.Address /= 0
            then
               Address := FADT.X_PMTImerBlock.Address;
            else
               Address := Unsigned_64 (FADT.PM_TMR_BLK);
            end if;
            if Length >= Flags_End then
               Flags := FADT.Flags;
            end if;
         end if;
      end;
      ACPI.Unref_Table (ACPI_Address);

      --  A 32-bit read reaches four ports, and those under 100h are fixed
      --  registers of the chipset, so the timer is not trusted to be there.
      if Address >= 16#100# and Address <= 16#FFFC# then
         Port    := Unsigned_16'Mod (Address);
         Mask    := (if (Flags and TMR_VAL_EXT) /= 0 then 16#FFFF_FFFF#
                     else 16#FF_FFFF#);
         Success := True;
      end if;
   end Find;
end Arch.ACPI_PM_Timer;
