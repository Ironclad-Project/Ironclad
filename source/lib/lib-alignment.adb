--  lib-alignment.adb: Generic alignment functions.
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

with System.Storage_Elements; use System.Storage_Elements;

package body Lib.Alignment is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   function Align_Up (Value, Alignment : T) return T is
      Value2 : constant Integer_Address := Integer_Address (Value);
      Align2 : constant Integer_Address := Integer_Address (Alignment);
   begin
      return T ((Value2 + Align2 - 1) and not (Align2 - 1));
   end Align_Up;

   function Align_Down (Value, Alignment : T) return T is
      Value2 : constant Integer_Address := Integer_Address (Value);
      Align2 : constant Integer_Address := Integer_Address (Alignment);
   begin
      return T (Value2 and not (Align2 - 1));
   end Align_Down;

   function Divide_Round_Up (Dividend, Divisor : T) return T is
   begin
      return (Dividend + (Divisor - 1)) / Divisor;
   end Divide_Round_Up;
end Lib.Alignment;
