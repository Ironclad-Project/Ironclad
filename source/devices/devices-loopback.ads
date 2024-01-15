--  devices-loopback.ads: Network loopback device.
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

package Devices.Loopback is
   --  Initialize the device.
   procedure Init (Success : out Boolean)
      with Pre => Is_Initialized = True;

private

   subtype Loopback_Buffer is Operation_Data (1 .. 4096 * 4);
   type Loopback_Buffer_Acc is access Loopback_Buffer;
   type Loopback_Data is record
      Data : Loopback_Buffer_Acc;
      Len  : Natural;
   end record;
   type Loopback_Data_Acc is access Loopback_Data;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;
end Devices.Loopback;
