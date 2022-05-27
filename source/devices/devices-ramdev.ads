--  devices-ramdev.ads: RAM device library.
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
with VFS; use VFS;
with VFS.Device;
with Arch.Stivale2;
with Interfaces; use Interfaces;

package Devices.Ramdev is
   --  Initialize a device given a stivale2 module to go off from, and whether
   --  its a USTAR FS or not.
   function Init_Module
      (Module : Arch.Stivale2.Module;
       Name   : String) return VFS.Device.Device_Data;

private

   function Read
      (Data   : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64;
end Devices.Ramdev;
