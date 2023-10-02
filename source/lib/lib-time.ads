--  lib-time.ads: Time-related functions.
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

with Interfaces; use Interfaces;

package Lib.Time is
   --  Inside Ironclad, time is often refered to as a tuple of seconds and
   --  nanoseconds. These functions handle manipulating those values easily.

   --  Normalize a number of seconds and nanoseconds, by reassigning the values
   --  for nanoseconds to not have seconds inside them.
   procedure Normalize (Seconds, Nanoseconds : in out Unsigned_64);

   --  Add two sets of seconds and nanoseconds and normalize at the end.
   procedure Increment
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64);

   --  Substract 2 timestamps.
   procedure Substract
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
      with Pre => Is_Greater_Equal
         (Seconds1, Nanoseconds1, Seconds2, Nanoseconds2);

   --  Compare two timestamps, and return true if the first one is greater.
   function Is_Greater_Equal (S1, NS1, S2, NS2 : Unsigned_64) return Boolean;

   --  Ghost function for checking whether a timestamp is normalized.
   function Is_Normalized (S, NS : Unsigned_64) return Boolean with Ghost;

private

   MSec_Per_Sec  : constant := 1_000;
   USec_Per_MSec : constant := 1_000_000;
   USec_Per_Sec  : constant := USec_Per_MSec * MSec_Per_Sec;

   function Is_Normalized (S, NS : Unsigned_64) return Boolean is
      (NS < USec_Per_MSec);
end Lib.Time;
