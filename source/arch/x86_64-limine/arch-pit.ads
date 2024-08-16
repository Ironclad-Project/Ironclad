--  arch-pit.ads: Specification of the PIT driver.
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

package Arch.PIT is
   --  Initialize by doing a first configure of the PIT, true on success.
   function Init return Boolean;

   --  Manage interrupt-free PIT counters.
   --  Just set a value and it goes down until 0.
   function Get_Current_Count return Unsigned_16;
   procedure Set_Current_Count (Value : Unsigned_16);

   --  Interrupt-less sleep.
   procedure Sleep_1MS;
end Arch.PIT;
