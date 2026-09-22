--  time.adb: Time-related functions.
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

with Arch.Clocks;
with Arch.Local;
with Scheduler;
with Userland.Process;

package body Time is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   function To_Stamp (Nanoseconds : Unsigned_64) return Timestamp is
   begin
      return
         (Nanoseconds / Nanoseconds_In_Second,
          Nanoseconds mod Nanoseconds_In_Second);
   end To_Stamp;

   function To_Nanoseconds (Stamp : Timestamp) return Unsigned_64 is
   begin
      return (Stamp.Seconds * Nanoseconds_In_Second) + Stamp.Nanoseconds;
   end To_Nanoseconds;

   function "+" (Left, Right : Timestamp) return Timestamp is
      Result : Timestamp;
   begin
      if Left.Seconds > Unsigned_64'Last - Right.Seconds then
         Result.Seconds := Unsigned_64'Last;
      else
         Result.Seconds := Left.Seconds + Right.Seconds;
      end if;
      Result.Nanoseconds := Left.Nanoseconds + Right.Nanoseconds;
      Normalize (Result);
      return Result;
   end "+";

   function "-" (Left, Right : Timestamp) return Timestamp is
      L      : Timestamp := Left;
      R      : Timestamp := Right;
      Result : Timestamp;
   begin
      Normalize (L);
      Normalize (R);

      Result.Seconds := L.Seconds - R.Seconds;
      Result.Nanoseconds := L.Nanoseconds;
      if L.Nanoseconds < R.Nanoseconds then
         Result.Seconds := Result.Seconds - 1;
         Result.Nanoseconds := Nanoseconds_In_Second + L.Nanoseconds;
      end if;
      Result.Nanoseconds := Result.Nanoseconds - R.Nanoseconds;
      return Result;
   end "-";

   function ">=" (Left, Right : Timestamp) return Boolean is
      L : Timestamp := Left;
      R : Timestamp := Right;
   begin
      Normalize (L);
      Normalize (R);

      return (L.Seconds > R.Seconds) or
         ((L.Seconds = R.Seconds) and
          (L.Nanoseconds >= R.Nanoseconds));
   end ">=";

   function ">" (Left, Right : Timestamp) return Boolean is
      L : Timestamp := Left;
      R : Timestamp := Right;
   begin
      Normalize (L);
      Normalize (R);

      return (L.Seconds > R.Seconds) or
         ((L.Seconds = R.Seconds) and (L.Nanoseconds > R.Nanoseconds));
   end ">";
   ----------------------------------------------------------------------------
   procedure Get_Time (Clock : Clock_Type; Stamp : out Timestamp) is
      User : Time.Timestamp;
   begin
      case Clock is
         when Monotonic_Clock =>
            Arch.Clocks.Get_Monotonic_Time (Stamp);
         when Real_Time_Clock =>
            Arch.Clocks.Get_Real_Time (Stamp);
         when Thread_CPU_Time_Clock =>
            --  Execution time is all of it, in the kernel and out of it.
            Scheduler.Get_Runtimes
               (Arch.Local.Get_Current_Thread, Stamp, User);
            Stamp := Stamp + User;
         when Process_CPU_Time_Clock =>
            Userland.Process.Get_Runtime_Times
               (Arch.Local.Get_Current_Process, Stamp, User);
            Stamp := Stamp + User;
      end case;
   end Get_Time;

   procedure Set_Time (Clock : Clock_Type; Stamp : Timestamp) is
   begin
      case Clock is
         when Real_Time_Clock =>
            Arch.Clocks.Set_Real_Time (Stamp);
         when others =>
            null;
      end case;
   end Set_Time;

   procedure Get_Resolution (Clock : Clock_Type; Stamp : out Timestamp) is
   begin
      case Clock is
         --  The Thread and process CPU times are pegged to monotonic.
         when Monotonic_Clock | Thread_CPU_Time_Clock |
              Process_CPU_Time_Clock =>
            Arch.Clocks.Get_Monotonic_Resolution (Stamp);
         when Real_Time_Clock =>
            Arch.Clocks.Get_Real_Time_Resolution (Stamp);
      end case;
   end Get_Resolution;
   ----------------------------------------------------------------------------
   function Time_To_Epoch
      (Y   : Year;
       M   : Month;
       D   : Day;
       H   : Hours;
       Min : Minutes;
       S   : Seconds) return Unsigned_64
   is
      J_Current : constant Unsigned_64 := Get_Julian_Date (D, M, Y);
      J_1970    : constant Unsigned_64 := Get_Julian_Date (1, 1, 1970);
      J_Diff    : constant Unsigned_64 := J_Current - J_1970;
   begin
      return J_Diff * (60 * 60 * 24) +
             Unsigned_64 (H) * 3600  +
             Unsigned_64 (Min) * 60    +
             Unsigned_64 (S);
   end Time_To_Epoch;
   ----------------------------------------------------------------------------
   procedure Normalize (Stamp : in out Timestamp) is
      Nanosecond_Seconds : constant Unsigned_64 :=
         Stamp.Nanoseconds / Nanoseconds_In_Second;
   begin
      if Stamp.Seconds <= Unsigned_64'Last - Nanosecond_Seconds then
         Stamp.Seconds := Stamp.Seconds + Nanosecond_Seconds;
      else
         Stamp.Seconds := Unsigned_64'Last;
      end if;
      Stamp.Nanoseconds := Stamp.Nanoseconds mod Nanoseconds_In_Second;
   end Normalize;

   function Get_Julian_Date (Days, Months, Years : Natural) return Unsigned_64
   is
      Y : constant Unsigned_64 := Unsigned_64 (Years);
      M : constant Unsigned_64 := Unsigned_64 (Months);
      D : constant Unsigned_64 := Unsigned_64 (Days);
   begin
      return (1461 * (Y + 4800 + (M - 14) / 12)) / 4 + (367 *
             (M - 2 - 12 * ((M - 14) / 12))) / 12 -
             (3 * ((Y + 4900 + (M - 14) / 12) / 100)) / 4
             + D - 32075;
   end Get_Julian_Date;
end Time;
