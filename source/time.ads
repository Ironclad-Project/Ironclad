--  time.ads: Time-related functions.
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

package Time is
   --  Inside Ironclad, time is often referred to as a tuple of seconds and
   --  nanoseconds. These functions make manipulating those values easier.

   --  Normalize a number of seconds and nanoseconds, by making sure
   --  no seconds are held in the nanoseconds field.
   --  @param Seconds     Seconds to normalize.
   --  @param Nanoseconds Nanoseconds to normalize.
   procedure Normalize (Seconds, Nanoseconds : in out Unsigned_64)
      with Post => Is_Normalized (Nanoseconds);

   --  Add two passed timestamps write the normalized sum to the first one.
   --  @param Seconds1     Seconds to add and result value.
   --  @param Nanoseconds1 Nanoseconds to add and result value.
   --  @param Seconds2     Seconds to add.
   --  @param Nanoseconds2 Nanoseconds to add.
   procedure Increment
      (Seconds1, Nanoseconds1 : in out Unsigned_64;
       Seconds2, Nanoseconds2 : Unsigned_64)
      with Post => Is_Normalized (Nanoseconds1);

   --  Subtract two timestamps, for things like getting a delta. The first
   --  one will be used for output as well, and must be bigger than the second.
   --  @param Seconds1     Seconds to subtract from and result value.
   --  @param Nanoseconds1 Nanoseconds to subtract from and result value.
   --  @param Seconds2     Seconds to subtract.
   --  @param Nanoseconds2 Nanoseconds to subtract.
   procedure Subtract
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
   ----------------------------------------------------------------------------
   --  Types to represent several date elements.
   subtype Year    is Natural;
   subtype Month   is Natural range 1 .. 12;
   subtype Day     is Natural range 1 .. 31;
   subtype Hours   is Natural range 0 .. 23;
   subtype Minutes is Natural range 0 .. 59;
   subtype Seconds is Natural range 0 .. 59;

   --  Transform time of year to epoch.
   function Time_To_Epoch
      (Y   : Year;
       M   : Month;
       D   : Day;
       H   : Hours;
       Min : Minutes;
       S   : Seconds) return Unsigned_64;

private

   MSec_Per_Sec  : constant := 1_000;
   USec_Per_MSec : constant := 1_000_000;
   USec_Per_Sec  : constant := USec_Per_MSec * MSec_Per_Sec;

   function Is_Normalized (NS : Unsigned_64) return Boolean is
      (NS < USec_Per_Sec);

   function Get_Julian_Date (Days, Months, Years : Natural) return Unsigned_64;
end Time;
