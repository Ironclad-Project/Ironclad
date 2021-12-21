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

with Interfaces;          use Interfaces;
with System.Machine_Code; use System.Machine_Code;

package body Arch.Debug is
   procedure Print (Message : String) is
   begin
      for C of Message loop
         Asm ("outb %0, %1",
              Inputs   => (Character'Asm_Input   ("a",       C),
                           Unsigned_16'Asm_Input ("Nd", 16#E9#)),
              Volatile => True);
      end loop;
   end Print;
end Arch.Debug;