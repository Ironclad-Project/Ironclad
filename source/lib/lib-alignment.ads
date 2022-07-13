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

--  This functions are designed to be used with pow 2 alignments.
--  They will fail else.

generic
   type T is mod <>;
package Lib.Alignment with Pure is
   function Align_Up (Value, Alignment : T) return T
      with Pre  => Alignment /= 0 and (Alignment and (Alignment - 1)) = 0,
           Post => Align_Up'Result rem Alignment = 0;

   function Align_Down (Value, Alignment : T) return T
      with Pre  => Alignment /= 0 and (Alignment and (Alignment - 1)) = 0,
           Post => Align_Down'Result rem Alignment = 0;
end Lib.Alignment;
