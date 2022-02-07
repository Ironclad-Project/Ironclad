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
with FS; use FS;
with Arch.Stivale2;

package Devices.Ramdev is
   --  Initialize a device given a stivale2 module to go off from, and whether
   --  its a USTAR FS or not.
   function Init_Module
      (Module : Arch.Stivale2.Module;
       Name   : Root_Name) return Root;

private
      function Ramdev_Init (Data : Root_Data) return Root_Data;
      procedure Ramdev_Unload (Data : Root_Data);

      function Ramdev_Read
         (Data   : Root_Data;
          Obj    : Object;
          Offset : System.Address;
          Count  : Positive;
          Desto  : System.Address) return Natural;
end Devices.Ramdev;
