--  devices-psmouse.ads: PS2 mouse driver.
--  Copyright (C) 2021 streaksu
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

with System;
with Interfaces; use Interfaces;

package Devices.PSMouse is
   --  Initialize the device.
   function Init return Boolean;

private

   function Read
      (Data   : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64;

   procedure Mouse_Handler;

   procedure Mouse_Wait_Read;
   procedure Mouse_Wait_Write;
   function Mouse_Read return Unsigned_8;
   procedure Mouse_Write (Data : Unsigned_8);
end Devices.PSMouse;
