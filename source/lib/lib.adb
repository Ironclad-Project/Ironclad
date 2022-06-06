--  lib.ads: Generic library functions.
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

with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;

package body Lib with SPARK_Mode is
   function Align_Up (Value, Alignment : Unsigned_64) return Unsigned_64 is
   begin
      return (Value + Alignment - 1) and not (Alignment - 1);
   end Align_Up;

   function Align_Down (Value, Alignment : Unsigned_64) return Unsigned_64 is
   begin
      return Value and not (Alignment - 1);
   end Align_Down;

   function Div_Round_Up (LHS, RHS : Unsigned_64) return Unsigned_64 is
   begin
      return (LHS + (RHS - 1)) / LHS;
   end Div_Round_Up;
   ----------------------------------------------------------------------------
   function C_String_Length (Addr : Address) return Natural is
      Length : Natural := 0;
   begin
      loop
         declare
            C : Character with Address => Addr + Storage_Offset (Length);
         begin
            exit when C = Ada.Characters.Latin_1.NUL or Length = Natural'Last;
            Length := Length + 1;
         end;
      end loop;
      return Length;
   end C_String_Length;
end Lib;
