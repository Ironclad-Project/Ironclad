--  arch-power.adb: Architecture-specific power management.
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

with Lib.Panic;

package body Arch.Power with SPARK_Mode => Off is
   function Halt return Power_Status is
   begin
      Lib.Panic.Hard_Panic ("Halting with panic! or is it panic as halting?!");
      return Failure;
   end Halt;

   function Reboot return Power_Status is
   begin
      return Not_Supported;
   end Reboot;

   function Poweroff return Power_Status is
   begin
      return Not_Supported;
   end Poweroff;
end Arch.Power;
