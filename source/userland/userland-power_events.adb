--  userland-power_events.adb: Managing kernel power events.
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

with Userland.Process;

package body Userland.Power_Events is
   procedure Power_Button_Handler is
   begin
      --  TODO: This method is pretty bad to notify the system of power issues.
      --  in the future this should be something like SIGPWR.
      Process.Raise_Signal (Process.Convert (1), Process.Signal_Interrupted);
   end Power_Button_Handler;

   procedure Sleep_Button_Handler is
   begin
      Power_Button_Handler;
   end Sleep_Button_Handler;
end Userland.Power_Events;
