--  devices-pl011.ads: PL011-compatible driver.
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

with System.Storage_Elements; use System.Storage_Elements;
with System;
with Memory; use Memory;

package Devices.PL011 with SPARK_Mode => Off is
   --  Early initialization of PL011 for kernel output.
   procedure Configure;

   --  Output a character.
   procedure Print (Message : Character);
   procedure Print (Message : Unsigned_8);

   --  Register the already configured device.
   function Init return Boolean;

private

   PL011_Data    : Unsigned_32 with Import, Volatile;
   PL011_Status  : Unsigned_32 with Import, Volatile;
   PL011_I_Baud  : Unsigned_32 with Import, Volatile;
   PL011_F_Baud  : Unsigned_32 with Import, Volatile;
   PL011_Control : Unsigned_32 with Import, Volatile;
   for PL011_Data'Address    use To_Address (Memory_Offset + 16#9000000#);
   for PL011_Status'Address  use To_Address (Memory_Offset + 16#9000018#);
   for PL011_I_Baud'Address  use To_Address (Memory_Offset + 16#9000024#);
   for PL011_F_Baud'Address  use To_Address (Memory_Offset + 16#9000028#);
   for PL011_Control'Address use To_Address (Memory_Offset + 16#9000030#);

   Default_Baud : constant := 115200;

   procedure Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;

   procedure Set_Baud (Baud : Unsigned_32);
end Devices.PL011;
