--  devices-uart.adb: UART driver.
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

package body Devices.UART with SPARK_Mode => Off is
   procedure Init_UART0 is
   begin
      null;
   end Init_UART0;

   procedure Write_UART0 (Message : Character) is
      Data : Unsigned_8 with Import, Address => Message'Address;
   begin
      UART0_TX := Data;
   end Write_UART0;

   procedure Write_UART0 (Message : String) is
   begin
      for C of Message loop
         Write_UART0 (C);
      end loop;
   end Write_UART0;
end Devices.UART;
