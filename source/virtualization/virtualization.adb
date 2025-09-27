--  virtualization.adb: Virtualization module of the kernel.
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

with Arch.Virtualization;

package body Virtualization is
   Has_Initialized : Boolean := False;

   function Is_Supported return Boolean is
   begin
      return Has_Initialized;
   end Is_Supported;

   procedure Initialize is
   begin
      Arch.Virtualization.Initialize (Has_Initialized);
   end Initialize;
end Virtualization;
