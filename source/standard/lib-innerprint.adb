--  lib-innerprint.adb: Inner printing interface.
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

with Arch.Debug;
with Arch.Stivale2;

package body Lib.InnerPrint is
   procedure Inner_Print (Message : String) is
   begin
      Arch.Debug.Print (Message);
      Arch.Stivale2.Print_Terminal (Message);
   end Inner_Print;

   procedure Inner_Print (Message : Character) is
   begin
      Arch.Debug.Print (Message);
      Arch.Stivale2.Print_Terminal (Message);
   end Inner_Print;
end Lib.InnerPrint;
