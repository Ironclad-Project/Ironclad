--  arch-virtualization.adb: Architecture-specific virtualization code.
--  Copyright (C) 2024 mintsuki
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

with Arch.VMX;

package body Arch.Virtualization is
   procedure Initialize (Success : out Boolean) is
   begin
      Arch.VMX.Initialize (Success);
   end Initialize;

   procedure Get_MSR_List (List : out MSR_List; Count : out Natural) is
   begin
      List  := [others => 0];
      Count := 0;
   end Get_MSR_List;
end Arch.Virtualization;
