--  devices-ps2.adb: PS2 keyboard and mouse driver.
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

package Devices.PS2 is
   --  Initialize the device.
   function Init return Boolean;

private

   procedure Kb_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Kb_Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean);
   ----------------------------------------------------------------------------
   procedure Ms_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Ms_IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean);

   procedure Ms_Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean);
   ----------------------------------------------------------------------------
   procedure Set_Sample_Rate (Rate : Unsigned_8);
   function Identify_Mouse return Unsigned_8;
   ----------------------------------------------------------------------------
   function Read_PS2 return Unsigned_8;
   procedure Write_PS2 (Port : Unsigned_16; Value : Unsigned_8);
   function Read_PS2_Config return Unsigned_8;
   procedure Write_PS2_Config (Value : Unsigned_8);
   procedure Mouse_Write (Data : Unsigned_8);
   ----------------------------------------------------------------------------
   procedure Keyboard_Handler;
   procedure Mouse_Handler;
end Devices.PS2;
