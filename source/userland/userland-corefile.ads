--  userland-corefile.ads: Corefile generator.
--  Copyright (C) 2023 streaksu
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

with Arch.Context;
with VFS;

package Userland.Corefile is
   --  Generate a corefile for the current process and passed context.
   procedure Generate_Corefile (Ctx : Arch.Context.GP_Context)
      with Pre => VFS.Is_Initialized;
end Userland.Corefile;
