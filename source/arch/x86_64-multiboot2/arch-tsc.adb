--  arch-tsc.adb: TSC driver.
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

with Arch.Snippets;

package body Arch.TSC with SPARK_Mode => Off is
   procedure Get_Resolution (Seconds, Nanoseconds : out Unsigned_64) is
   begin
      Seconds     := 0;
      Nanoseconds := 1;
   end Get_Resolution;

   procedure Get_Time (Seconds, Nanoseconds : out Unsigned_64) is
      Cycles : constant Unsigned_64 := Snippets.Read_Cycles / 2;
   begin
      Seconds     := Cycles / 1_000_000_000;
      Nanoseconds := Cycles mod 1_000_000_000;
   end Get_Time;
end Arch.TSC;
