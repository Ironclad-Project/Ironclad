--  devices-ps2mouse.ads: PS2 mouse driver.
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

package Devices.PS2Mouse is
   --  Initialize the device.
   function Init return Boolean;

private

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;

   procedure Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean);

   procedure Mouse_Handler;
   procedure Mouse_Wait_Read;
   procedure Mouse_Wait_Write;
   function Mouse_Read return Unsigned_8;
   procedure Mouse_Write (Data : Unsigned_8);

   procedure Set_Sample_Rate (Rate : Unsigned_8);
   function Identify_Mouse return Unsigned_8;
end Devices.PS2Mouse;
