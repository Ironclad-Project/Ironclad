--  arch-debug.adb: Architecture-specific debug channels.
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

with Devices.Serial;

package body Arch.Debug is
   procedure Read (Message : out Devices.Operation_Data) is
   begin
      Devices.Serial.Read_COM1 (Message);
   end Read;

   procedure Print (Message : Character) is
   begin
      Devices.Serial.Write_COM1 (Message);
   end Print;

   procedure Print (Message : String) is
   begin
      Devices.Serial.Write_COM1 (Message);
   end Print;

   procedure Print (Message : Devices.Operation_Data) is
      S : String (1 .. Message'Length) with Import, Address => Message'Address;
   begin
      Devices.Serial.Write_COM1 (S);
   end Print;
end Arch.Debug;
