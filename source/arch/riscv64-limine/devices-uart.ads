--  devices-uart.ads: UART driver.
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

with System.Storage_Elements; use System.Storage_Elements;
with System;
with Memory; use Memory;

package Devices.UART with SPARK_Mode => Off is
   --  Early initialization of UART0 for early kernel output.
   procedure Init_UART0;

   --  Print a character.
   procedure Write_UART0 (Message : Character);

   --  Print a string.
   procedure Write_UART0 (Message : String);

private

   UART_Base : constant :=  16#10000000#;

   UART0_TX : Unsigned_8 with Import, Volatile,
      Address => To_Address (Memory_Offset + UART_Base);
end Devices.UART;
