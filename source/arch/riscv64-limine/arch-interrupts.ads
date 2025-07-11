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
      R15  : Unsigned_64;
      R14  : Unsigned_64;
      R13  : Unsigned_64;
      R12  : Unsigned_64;
      R11  : Unsigned_64;
      R10  : Unsigned_64;
      R9   : Unsigned_64;
      R8   : Unsigned_64;
      R7   : Unsigned_64;
      R6   : Unsigned_64;
      R5   : Unsigned_64;
      R4   : Unsigned_64;
      R3   : Unsigned_64;
      R2   : Unsigned_64;
      R1   : Unsigned_64;
      R0   : Unsigned_64;
   end record with Pack;
end Arch.Interrupts;
