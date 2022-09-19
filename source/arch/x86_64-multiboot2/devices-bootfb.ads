--  devices-bootfb.ads: Boot-time memory framebuffer driver.
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

with VFS;
with Interfaces; use Interfaces;
with System;
with Memory;

package Devices.BootFB with SPARK_Mode => Off is
   --  Initialize the device.
   function Init return Boolean;

private

   function Read
      (Data    : VFS.Resource_Acc;
       Offset  : Unsigned_64;
       Count   : Unsigned_64;
       To_Read : System.Address) return Unsigned_64;

   function Write
      (Data     : VFS.Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64;

   function IO_Control
      (Data     : VFS.Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;

   function Mmap
      (Data        : VFS.Resource_Acc;
       Address     : Memory.Virtual_Address;
       Length      : Unsigned_64;
       Map_Read    : Boolean;
       Map_Write   : Boolean;
       Map_Execute : Boolean) return Boolean;
end Devices.BootFB;
