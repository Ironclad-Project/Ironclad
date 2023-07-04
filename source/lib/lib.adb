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
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);
   pragma Warnings (
      GNATprove,
      Off,
      "indirect writes to ""C"" through a potential alias are ignored",
      Reason => "No alias are taken"
   );

   function C_String_Length (Addr : Address) return Natural is
      Length : Natural := 0;
   begin
      loop
         declare
            C : constant Character
               with Address => Addr + Storage_Offset (Length), Import;
         begin
            exit when C = Ada.Characters.Latin_1.NUL or Length = Natural'Last;
            Length := Length + 1;
         end;
      end loop;
      return Length;
   end C_String_Length;
   ----------------------------------------------------------------------------
   function Least_Common_Multiple (Val1, Val2 : Integer) return Integer is
   begin
      if Val1 = 0 or Val2 = 0 then
         return 0;
      end if;
      return (abs (Val1 * Val2)) / Greatest_Common_Divisor (Val1, Val2);
   end Least_Common_Multiple;

   function Greatest_Common_Divisor (Val1, Val2 : Integer) return Integer is
      M : Integer := Val1;
      N : Integer := Val2;
      T : Integer;
   begin
      while N /= 0 loop
         T := M;
         M := N;
         N := T mod N;
      end loop;
      return M;
   end Greatest_Common_Divisor;
end Lib;
