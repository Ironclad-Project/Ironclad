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

package Arch.PIT with SPARK_Mode => Off is
   --  The total uptime kept by the PIT.
   Uptime : Unsigned_64 with Volatile;
   pragma Atomic (Uptime);

   --  Initialize by doing a first configure of the PIT, true on success.
   function Init return Boolean;

   --  Loop for the passed ms.
   procedure Sleep (Milliseconds : Positive);

private
   procedure IRQ_Handler;
end Arch.PIT;
