--  devices-serial.ads: Serial driver specification.
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

with Interfaces; use Interfaces;
with System;
with VFS; use VFS;

package Devices.Serial is
   --  Initialize the serial devices.
   function Init return Boolean;

private

   function Serial_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : Unsigned_64;
       Count  : Positive;
       Desto  : System.Address) return Natural;

   function Serial_Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : Unsigned_64;
       Count    : Positive;
       To_Write : System.Address) return Natural;

   function Is_Transmitter_Empty (Port : Unsigned_16) return Boolean;
   function Is_Data_Received (Port : Unsigned_16) return Boolean;
end Devices.Serial;
