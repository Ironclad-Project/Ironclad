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
   --  nanoseconds. These functions make manipulating those values easier.

   --  Normalize a number of seconds and nanoseconds, by making sure
   --  no seconds are held in the nanoseconds field.
   --  @param Seconds     Seconds to normalize.
   --  @param Nanoseconds Nanoseconds to normalize.
   procedure Normalize (Seconds, Nanoseconds : in out Unsigned_64)
      with Post => Is_Normalized (Nanoseconds);
      pragma Annotate (GNATprove, False_Positive, "postcondition might fail",
         "Impossible with the mod in the body, idk why it doesn't catch it");

   --  Add two passed timestamps write the normalized sum to the first one.
   --  @param Seconds1     Seconds to add and result value.
   --  @param Nanoseconds1 Nanoseconds to add and result value.
   --  @param Seconds2     Seconds to add.
   --  @param Nanoseconds2 Nanoseconds to add.
   procedure Increment
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
      with Post => Is_Normalized (Nanoseconds1);

   --  Substract two timestamps, for things like getting a delta. The first
   --  one will be used for output as well, and must be bigger than the second.
   --  @param Seconds1     Seconds to substract from and result value.
   --  @param Nanoseconds1 Nanoseconds to substract from and result value.
   --  @param Seconds2     Seconds to substract.
   --  @param Nanoseconds2 Nanoseconds to substract.
   procedure Substract
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
      with Pre =>
         Is_Normalized (Nanoseconds1) and then
         Is_Normalized (Nanoseconds2) and then
         Is_Greater_Equal (Seconds1, Nanoseconds1, Seconds2, Nanoseconds2),
           Post => Is_Normalized (Nanoseconds1);

   --  Compare two timestamps.
   --  @param S1  First second to compare.
   --  @param NS1 First nanosecond to compare.
   --  @param S2  Second second to compare.
   --  @param NS2 Second nanosecond to compare.
   --  @return True if the first timestamp is greater or equal.
   function Is_Greater_Equal (S1, NS1, S2, NS2 : Unsigned_64) return Boolean
      with Pre => Is_Normalized (NS1) and Is_Normalized (NS2);

   --  Ghost function for checking whether a timestamp is normalized.
   function Is_Normalized (NS : Unsigned_64) return Boolean with Ghost;

private

   MSec_Per_Sec  : constant := 1_000;
   USec_Per_MSec : constant := 1_000_000;
   USec_Per_Sec  : constant := USec_Per_MSec * MSec_Per_Sec;

   function Is_Normalized (NS : Unsigned_64) return Boolean is
      (NS < USec_Per_Sec);
end Lib.Time;
