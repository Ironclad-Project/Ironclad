--  arch-power.ads: Architecture-specific power management.
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

package Arch.Power is
   --  Possible returns of a power-related function.
   type Power_Status is (Not_Supported, Failure);

   --  Halt the system, this function will only returns in failure.
   procedure Halt (Status : out Power_Status);

   --  Reboot the system, this function will only returns in failure.
   procedure Reboot (Status : out Power_Status);

   --  Poweroff the system, this function will only returns in failure.
   procedure Poweroff (Status : out Power_Status);
   ----------------------------------------------------------------------------
   --  Hardware (usually OEMs) can give us system-wide hits on how to run the
   --  in terms of power profiles.
   type Power_Profile is
      (Unspecified,         --  Hardware has no preference.
       Desktop,             --  Desktop computer, think, a home PC.
       Mobile,              --  Power-conscious small devices and laptops.
       Workstation,         --  High-spec power-hungry desktop usage.
       Enterprise_Server,   --  Big bulky services oriented usage.
       SOHO_Server,         --  Small office server.
       Appliance,           --  Appliances, IoT, that fun stuff.
       Performance_Server,  --  Like enterprise servers but with power in mind.
       Tablet_Convertible); --  Tablet or tablet-convertible.

   --  Get the power profile hint from the hardware.
   procedure Get_Preferred_Profile (P : out Power_Profile);
   ----------------------------------------------------------------------------
   procedure Get_Buttons (Has_Power_Button, Has_Sleep_Button : out Boolean);
end Arch.Power;
