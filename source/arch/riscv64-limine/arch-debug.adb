--  arch-debug.adb: Architecture-specific debug channels.
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

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Devices.UART;

package body Arch.Debug with SPARK_Mode => Off is
   procedure Read (Message : out Devices.Operation_Data) is
   begin
      for C of Message loop
         C := Devices.UART.Read_UART0;
      end loop;
   end Read;

   procedure Print (Message : Character) is
   begin
      Devices.UART.Write_UART0 (Message);
   end Print;

   procedure Print (Message : String) is
   begin
      Devices.UART.Write_UART0 (Message);
   end Print;

   procedure Print (Message : Devices.Operation_Data) is
      function Conv is new Ada.Unchecked_Conversion (Unsigned_8, Character);
   begin
      for C of Message loop
         Print (Conv (C));
      end loop;
   end Print;
end Arch.Debug;
