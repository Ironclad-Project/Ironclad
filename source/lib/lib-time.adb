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
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Normalize (Seconds, Nanoseconds : in out Unsigned_64) is
      Nanosecond_Seconds : constant Unsigned_64 := Nanoseconds / USec_Per_Sec;
   begin
      if Seconds <= Unsigned_64'Last - Nanosecond_Seconds then
         Seconds := Seconds + Nanosecond_Seconds;
      else
         Seconds := Unsigned_64'Last;
      end if;
      Nanoseconds := Nanoseconds mod USec_Per_Sec;
   end Normalize;

   procedure Increment
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
   is
   begin
      if Seconds1 > Unsigned_64'Last - Seconds2 then
         Seconds1 := Unsigned_64'Last;
      else
         Seconds1 := Seconds1 + Seconds2;
      end if;
      Nanoseconds1 := Nanoseconds1 + Nanoseconds2;
      Normalize (Seconds1, Nanoseconds1);
   end Increment;

   procedure Substract
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
   is
   begin
      Seconds1 := Seconds1 - Seconds2;
      if Nanoseconds1 < Nanoseconds2 then
         Seconds1 := Seconds1 - 1;
         Nanoseconds1 := USec_Per_Sec + Nanoseconds1;
      end if;
      Nanoseconds1 := Nanoseconds1 - Nanoseconds2;
   end Substract;

   function Is_Greater_Equal (S1, NS1, S2, NS2 : Unsigned_64) return Boolean is
   begin
      return (S1 > S2) or ((S1 = S2) and (NS1 >= NS2));
   end Is_Greater_Equal;
end Lib.Time;
