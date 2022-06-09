--  arch-debug.adb: Arch-specific debug utilities.
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

with Arch.Wrappers;

package body Arch.Debug is
   QEMU_Debug_Port : constant := 16#E9#;

   procedure Print (Message : String) is
   begin
      for C of Message loop
         Print (C);
      end loop;
   end Print;

   procedure Print (Message : Character) is
   begin
      Wrappers.Port_Out (QEMU_Debug_Port, Character'Pos (Message));
   end Print;
end Arch.Debug;
