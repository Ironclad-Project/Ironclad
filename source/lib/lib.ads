--  lib.ads: Specification of generic library functions.
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

with System; use System;
with Interfaces; use Interfaces;

package Lib with SPARK_Mode, Pure is
   --  Math alignment functions.
   function Align_Up (Value, Alignment : Unsigned_64) return Unsigned_64
      with Pre  => Alignment /= 0 and then (Alignment and (Alignment - 1)) = 0,
           Post => Align_Up'Result rem Alignment = 0;

   function Align_Down (Value, Alignment : Unsigned_64) return Unsigned_64
      with Pre  => Alignment /= 0 and then (Alignment and (Alignment - 1)) = 0,
           Post => Align_Down'Result rem Alignment = 0;
   ----------------------------------------------------------------------------
   --  Report the length of a NUL-Terminated C string.
   function C_String_Length (Addr : Address) return Natural
      with Pre => Addr /= System.Null_Address;
end Lib;
