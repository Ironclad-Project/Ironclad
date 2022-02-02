--  arch-cpu.ads: Specification of CPU management routines.
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
with Arch.Stivale2;

package Arch.CPU is
   --  Core count, always equal or larger than 1.
   Core_Count : Positive;

   --  Array of LAPIC IDs, the index is the core owning said LAPIC (1-based).
   type LAPIC_Array is array (Positive range <>) of Unsigned_32;
   type LAPIC_Arr_Acc is access LAPIC_Array;
   Core_LAPICs : LAPIC_Arr_Acc;

   --  Tweak the Bootstrap Processor.
   procedure Init_BSP;

   --  Init the rest of cores.
   procedure Init_Cores (SMP_Info : access Arch.Stivale2.SMP_Tag);

   --  Get core number (1 based) for array indexes on modules or reporting.
   function Get_Core_Number return Positive;

private
   procedure Init_Core (Core_Info : access Arch.Stivale2.SMP_Core)
      with Convention => C;
   procedure Init_Common;
end Arch.CPU;
