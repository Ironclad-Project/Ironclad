--  arch-pci.adb: Architecture-specific PCI code.
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

with Arch.ACPI;

package body Arch.PCI with SPARK_Mode => Off is
   procedure Fetch_ECAM_Address (ECAM : out System.Address) is
      ACPI_Address : ACPI.Table_Record;
   begin
      ECAM := System.Null_Address;

      if not ACPI.Is_Supported then
         return;
      end if;

      ACPI.FindTable (ACPI.MCFG_Signature, ACPI_Address);
      if ACPI_Address.Virt_Addr = 0 then
         return;
      end if;

      declare
         Table : Arch.ACPI.MCFG
            with Import, Address => To_Address (ACPI_Address.Virt_Addr);
      begin
         ECAM := To_Address (Integer_Address (Table.Root_ECAM_Addr));
         Arch.ACPI.Unref_Table (ACPI_Address);
      end;
   end Fetch_ECAM_Address;
end Arch.PCI;
