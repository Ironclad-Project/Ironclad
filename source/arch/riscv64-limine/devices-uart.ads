--  devices-uart.ads:  Provides basic UART functionality for RISC-V64 systems.
--  Copyright (C) 2025 scweeks, streaksu
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

   --  Print a single character to UART0.
   procedure Write_UART0 (Message : Character);

   --  Print a string to UART0.
   procedure Write_UART0 (Message : String);

   function Read_UART0 return Unsigned_8;

private

   --  UART Memory-Mapped Registers
   Base : constant := Memory_Offset + 16#10000000#;
   THR  : Unsigned_8 with Import, Volatile, Address => To_Address (Base + 0);
   RBR  : Unsigned_8 with Import, Volatile, Address => To_Address (Base + 0);
   IER  : Unsigned_8 with Import, Volatile, Address => To_Address (Base + 1);
   FCR  : Unsigned_8 with Import, Volatile, Address => To_Address (Base + 2);
   LCR  : Unsigned_8 with Import, Volatile, Address => To_Address (Base + 3);
   MCR  : Unsigned_8 with Import, Volatile, Address => To_Address (Base + 4);
   LSR  : Unsigned_8 with Import, Volatile, Address => To_Address (Base + 5);
end Devices.UART;
