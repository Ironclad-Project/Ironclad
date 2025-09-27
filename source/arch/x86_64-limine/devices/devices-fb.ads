--  devices-fb.ads: Boot-time memory framebuffer driver.
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

with Memory;
with Memory.MMU;

package Devices.FB is
   --  Early init for console printing purposes.
   procedure Early_Init;

   --  Get data about the early framebuffer.
   procedure Get_Early_Framebuffer
      (Addr                              : out System.Address;
       Width, Height, Pitch              : out Unsigned_64;
       Red_Mask_Size, Red_Mask_Shift     : out Unsigned_8;
       Green_Mask_Size, Green_Mask_Shift : out Unsigned_8;
       Blue_Mask_Size, Blue_Mask_Shift   : out Unsigned_8);
   ----------------------------------------------------------------------------
   --  Initialize the device.
   procedure Init (Success : out Boolean);

private

   procedure IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean);

   procedure Mmap
      (Data    : System.Address;
       Map     : Memory.MMU.Page_Table_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions;
       Success : out Boolean);
end Devices.FB;
