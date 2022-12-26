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

with Arch.Snippets;
with Interfaces; use Interfaces;

package body Arch.Debug with SPARK_Mode => Off is
   COM1 : constant := 16#3F8#;

   procedure Print (Message : Character) is
   begin
      while (Snippets.Port_In (16#3F8# + 5) and 16#20#) = 0 loop
         null;
      end loop;
      Snippets.Port_Out (COM1, Character'Pos (Message));
   end Print;

   procedure Print (Message : String) is
   begin
      for C of Message loop
         Print (C);
      end loop;
   end Print;
end Arch.Debug;
