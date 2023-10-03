--  arch-clocks.ads: Architectural clock sources.
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

package Arch.Clocks with
   Abstract_State => (Monotonic_Clock_State, RT_Clock_State)
is
   --  Initialize clock sources.
   procedure Initialize_Sources
      with Global => (Output => (Monotonic_Clock_State, RT_Clock_State));

   --  Get the resolution of the monotonic clock.
   procedure Get_Monotonic_Resolution (Seconds, Nanoseconds : out Unsigned_64)
      with Global => (In_Out => Monotonic_Clock_State);

   --  Get the time from the monotonic clock.
   procedure Get_Monotonic_Time (Seconds, Nanoseconds : out Unsigned_64)
      with Global => (In_Out => Monotonic_Clock_State);

   --  Get the resolution of the real-time clock.
   procedure Get_Real_Time_Resolution (Seconds, Nanoseconds : out Unsigned_64)
      with Global => (In_Out => RT_Clock_State);

   --  Get the time from the real time clock.
   procedure Get_Real_Time (Seconds, Nanoseconds : out Unsigned_64)
      with Global => (In_Out => RT_Clock_State);

   --  Set the time for the real time clock.
   procedure Set_Real_Time (Seconds, Nanoseconds : Unsigned_64)
      with Global => (In_Out => RT_Clock_State);
end Arch.Clocks;
