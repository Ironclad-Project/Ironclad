--  time.ads: Time-related functions.
--  Copyright (C) 2025 streaksu
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
   --  Some numeric constants.
   Milliseconds_In_Second : constant := 1_000;
   Nanoseconds_In_Microsecond : constant := 1_000_000;
   Nanoseconds_In_Second : constant := 1_000_000_000;

   --  Inside Ironclad, time is often referred to as a tuple of seconds and
   --  nanoseconds.
   type Timestamp is record
      Seconds : Unsigned_64;
      Nanoseconds : Unsigned_64;
   end record;

   --  Conversion to and from nanoseconds.
   function To_Stamp (Nanoseconds : Unsigned_64) return Timestamp;
   function To_Nanoseconds (Stamp : Timestamp) return Unsigned_64;

   --  Binary operations, they dont return exceptions but (-1, -1) on overflow
   --  and (0, 0) on underflow.
   function "+" (Left, Right : Timestamp) return Timestamp;
   function "-" (Left, Right : Timestamp) return Timestamp;
   function ">=" (Left, Right : Timestamp) return Boolean;
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

   procedure Normalize (Stamp : in out Timestamp)
      with Post => Stamp.Nanoseconds <= Nanoseconds_In_Second;

   function Get_Julian_Date (Days, Months, Years : Natural) return Unsigned_64;
end Time;
