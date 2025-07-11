--  arch-clocks.adb: Architectural clock sources.
--  Copyright (C) 2024 streaksu
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

package body Arch.Clocks with
   Refined_State =>
      (RT_Clock_State => (null),
       Monotonic_Clock_State => (null))
is
   procedure Initialize_Sources is
   begin
      null;
   end Initialize_Sources;

   procedure Get_Monotonic_Resolution (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Monotonic_Resolution;

   procedure Get_Monotonic_Time (Seconds, Nanoseconds : out Unsigned_64) is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Monotonic_Time;

   procedure Busy_Monotonic_Sleep (Nanoseconds : Unsigned_64) is
      pragma Unreferenced (Nanoseconds);
   begin
      null;
   end Busy_Monotonic_Sleep;

   procedure Get_Real_Time_Resolution (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Real_Time_Resolution;

   procedure Get_Real_Time (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Real_Time;

   procedure Set_Real_Time (Seconds, Nanoseconds : Unsigned_64) is
   begin
      null;
   end Set_Real_Time;
end Arch.Clocks;
