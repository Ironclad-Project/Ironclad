--  devices-i6300esb.ads: i6300ESB watchdog driver.
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

package Devices.PCI.i6300ESB is
   --  Initialize the device.
   procedure Init (Success : out Boolean);

private

   --  Data stored for the watchdog.
   type Dog_Data is record
      PCI_Data  : Devices.PCI.PCI_Device;
      Base_Addr : System.Address;
   end record;
   type Dog_Data_Acc is access all Dog_Data;

   --  Offsets of several MMIO registers.
   TIMER1 : constant := 16#00#;
   TIMER2 : constant := 16#04#;
   GINTSR : constant := 16#08#;
   RELOAD : constant := 16#0C#;

   --  Offsets of PCI registers.
   CONFIG : constant := 16#60#;
   LOCK   : constant := 16#68#;

   --  Magic values and bitfields for register values.
   UNLOCK1      : constant := 16#80#;
   UNLOCK2      : constant := 16#86#;
   DOG_TIMEOUT  : constant := 2#1000000000#;
   DOG_RELOAD   : constant := 2#0100000000#;
   DOG_ENABLE   : constant := 2#0000000010#;
   DOG_OUTPUT   : constant := 2#0000000010#;
   DOG_INT_TYPE : constant := 2#0000000001#;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean);
   ----------------------------------------------------------------------------
   procedure Unlock_Registers (Base_Addr : System.Address);
   procedure Keep_Alive (Base_Addr : System.Address);
end Devices.PCI.i6300ESB;
