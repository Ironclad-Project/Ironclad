--  devices-pc_speaker.ads: PC speaker driver.
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

package Devices.PC_Speaker is
   --  Initialize the device.
   function Init return Boolean;

   --  Beep!
   procedure Beep (Frequency : Unsigned_32 := 1000);

private

   procedure Delay_Execution (Cycles : Unsigned_64);

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;
end Devices.PC_Speaker;
