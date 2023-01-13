--  lib-alignment.ads: Generic alignment functions.
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

generic
   type T is mod <>;
package Lib.Alignment with Pure is
   --  This functions are meant for powers of 2, using this for non powers of
   --  two is bogus, and broken.

   --  Align up a value.
   --  @param Value The value to align to the passed alignment
   --  @param Alignment Power-of-2 alignment to align to.
   --  @return The aligned value.
   function Align_Up (Value, Alignment : T) return T
      with Pre  => Alignment /= 0 and (Alignment and (Alignment - 1)) = 0,
           Post => Align_Up'Result rem Alignment = 0;

   --  Align down a value.
   --  @param Value The value to align to the passed alignment
   --  @param Alignment Power-of-2 alignment to align to.
   --  @return The aligned value.
   function Align_Down (Value, Alignment : T) return T
      with Pre  => Alignment /= 0 and (Alignment and (Alignment - 1)) = 0,
           Post => Align_Down'Result rem Alignment = 0;

   --  Divide and round up.
   function Divide_Round_Up (Dividend, Divisor : T) return T
      with Pre  => (Dividend + (Divisor - 1) <= T'Last) and (Divisor > 1),
           Post => Divide_Round_Up'Result mod Divisor = 0;
end Lib.Alignment;
