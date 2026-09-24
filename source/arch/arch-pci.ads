--  arch-pci.ads: Architecture-specific PCI code.
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

with System;
with Interfaces; use Interfaces;

package Arch.PCI is
   --  Fetch the system's ECAM address.
   procedure Fetch_ECAM_Address (ECAM : out System.Address);

   --  A set of PCI bus numbers.
   type Bus_Set is array (Unsigned_8) of Boolean;

   --  Find the root buses of the PCI host bridges ACPI describes on segment 0.
   --  @param Roots Where to mark the buses found.
   --  @param Count Number of buses found, 0 if ACPI describes no bridges.
   procedure Find_Root_Buses (Roots : out Bus_Set; Count : out Natural);
end Arch.PCI;
