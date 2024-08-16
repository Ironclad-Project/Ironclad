--  main.adb: Stub main function.
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

--  The project uses GPRBuild for compilation, because gnatprove and others
--  themselves depend on GPRBuild, issue is GPRBuild requires a main file
--  with a main function, which forces some weird semantics on us. This little
--  stub lets GPRBuild be happy while we stay happy.

with Kernel_Main;

procedure Main is
begin
   Kernel_Main.Entrypoint ("");
end Main;
