--  devices-kernout.ads: Expose a device for kernel error-reporting.
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
with FS; use FS;

package Devices.KernOut is
   --  Initialize the device.
   function Init return Boolean;

private

   function Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : System.Address;
       Count    : Positive;
       To_Write : System.Address) return Natural;
end Devices.KernOut;
