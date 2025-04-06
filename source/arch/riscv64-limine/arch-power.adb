--  arch-power.adb: Architecture-specific power management.
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

package body Arch.Power is
   procedure Halt (Status : out Power_Status) is
   begin
      Status := Failure;
   end Halt;

   procedure Reboot (Status : out Power_Status) is
   begin
      Status := Failure;
   end Reboot;

   procedure Poweroff (Status : out Power_Status) is
   begin
      Status := Failure;
   end Poweroff;
   ----------------------------------------------------------------------------
   procedure Get_Preferred_Profile (P : out Power_Profile) is
      P := Unspecified;
   end Get_Preferred_Profile;
end Arch.Power;
