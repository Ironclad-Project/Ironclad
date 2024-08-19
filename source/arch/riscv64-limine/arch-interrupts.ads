--  arch-exceptions.ads: Specification of interrupt utilities.
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

package Arch.Interrupts with SPARK_Mode => Off is
   --  Passed to every interrupt called ever as an access.
   type Frame is record
      R15  : Unsigned_32;
      R14  : Unsigned_32;
      R13  : Unsigned_32;
      R12  : Unsigned_32;
      R11  : Unsigned_32;
      R10  : Unsigned_32;
      R9   : Unsigned_32;
      R8   : Unsigned_32;
      R7   : Unsigned_32;
      R6   : Unsigned_32;
      R5   : Unsigned_32;
      R4   : Unsigned_32;
      R3   : Unsigned_32;
      R2   : Unsigned_32;
      R1   : Unsigned_32;
      R0   : Unsigned_32;
   end record with Pack;
end Arch.Interrupts;
