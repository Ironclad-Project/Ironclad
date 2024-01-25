--  devices-lpt.ads: Parallel port driver.
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

package Devices.LPT is
   --  Initialize the device.
   function Init return Boolean;

private

   LPT_Ports : constant array (1 .. 3) of Unsigned_16 :=
      (16#378#, 16#278#, 16#3BC#);

   type LP_Data is record
      Port : Unsigned_16;
   end record;
   type LP_Data_Acc is access LP_Data;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);
end Devices.LPT;
