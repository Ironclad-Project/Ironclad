--  userland-power_events.ads: Managing kernel power events.
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

package Userland.Power_Events is
   --  Called by the hardware when handling a power button event.
   procedure Power_Button_Handler;

   --  Called by the hardware when handling a sleep button event.
   procedure Sleep_Button_Handler;
end Userland.Power_Events;
