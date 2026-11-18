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

   procedure Epoch_To_Time
      (Epoch : Unsigned_64;
       Y     : out Year;
       M     : out Month;
       D     : out Day;
       H     : out Hours;
       Min   : out Minutes;
       S     : out Seconds)
   is
      Last_Epoch : constant := 253_402_300_799;
      Secs : constant Unsigned_64 := Unsigned_64'Min (Epoch, Last_Epoch);
      Days : constant Unsigned_64 := Secs / (60 * 60 * 24);
      Rest : constant Unsigned_64 := Secs mod (60 * 60 * 24);

      --  Days from 0000-03-01, in eras of 400 years, so that the leap day is
      --  the last day of a year and every term stays positive.
      Z    : constant Unsigned_64 := Days + 719_468;
      Era  : constant Unsigned_64 := Z / 146_097;
      Doe  : constant Unsigned_64 := Z - Era * 146_097;
      Yoe  : constant Unsigned_64 :=
         (Doe - Doe / 1_460 + Doe / 36_524 - Doe / 146_096) / 365;
      Doy  : constant Unsigned_64 := Doe - (365 * Yoe + Yoe / 4 - Yoe / 100);
      Mp   : constant Unsigned_64 := (5 * Doy + 2) / 153;
      Mon  : constant Unsigned_64 := (if Mp < 10 then Mp + 3 else Mp - 9);
   begin
      Y   := Natural (Yoe + Era * 400 + (if Mon <= 2 then 1 else 0));
      M   := Natural (Mon);
      D   := Natural (Doy - (153 * Mp + 2) / 5 + 1);
      H   := Natural (Rest / 3600);
      Min := Natural (Rest mod 3600 / 60);
      S   := Natural (Rest mod 60);
   end Epoch_To_Time;
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
      A : constant Unsigned_64 := (14 - Unsigned_64 (Months)) / 12;
      Y : constant Unsigned_64 := Unsigned_64 (Years) + 4800 - A;
      M : constant Unsigned_64 := Unsigned_64 (Months) + 12 * A - 3;
   begin
      return Unsigned_64 (Days) + (153 * M + 2) / 5 + 365 * Y + Y / 4 -
             Y / 100 + Y / 400 - 32045;
   end Get_Julian_Date;
end Time;
