--  lib-time.adb: Time-related functions.
--  Copyright (C) 2023 streaksu
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

package body Lib.Time is
   procedure Normalize (Seconds, Nanoseconds : in out Unsigned_64) is
   begin
      Seconds := Seconds + (Nanoseconds / USec_Per_Sec);
      Nanoseconds := Nanoseconds mod USec_Per_Sec;
   end Normalize;

   procedure Increment
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
   is
   begin
      Seconds1 := Seconds1 + Seconds2;
      Nanoseconds1 := Nanoseconds1 + Nanoseconds2;
      Normalize (Seconds1, Nanoseconds1);
   end Increment;

   procedure Substract
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
   is
      Final : constant Unsigned_64 :=
         ((Seconds1 * USec_Per_Sec) + Nanoseconds1) -
         ((Seconds2 * USec_Per_Sec) + Nanoseconds2);
   begin
      Seconds1 := Final / USec_Per_Sec;
      Nanoseconds1 := Final mod USec_Per_Sec;
   end Substract;

   function Is_Greater_Equal (S1, NS1, S2, NS2 : Unsigned_64) return Boolean is
   begin
      return (S1 > S2) or ((S1 = S2) and (NS1 >= NS2));
   end Is_Greater_Equal;
end Lib.Time;
