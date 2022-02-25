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

package body Lib is
   function C_String_Length (Address : System.Address) return Natural is
      Length : Integer := 0;
   begin
      loop
         declare
            C : Character with Address => Address + Storage_Offset (Length);
         begin
            exit when C = Ada.Characters.Latin_1.NUL;
            Length := Length + 1;
         end;
      end loop;
      return Length;
   end C_String_Length;
end Lib;
