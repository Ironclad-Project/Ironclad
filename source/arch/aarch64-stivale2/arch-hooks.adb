--  arch-hooks.adb: Architecture-specific hooks for several utilities.
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

package body Arch.Hooks with SPARK_Mode => Off is
   procedure Devices_Hook is
   begin
      return;
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      pragma Unreferenced (Code);
      pragma Unreferenced (Arg);
   begin
      return False;
   end PRCTL_Hook;

   function Panic_Prepare_Hook (Addr : System.Address) return Boolean is
      pragma Unreferenced (Addr);
   begin
      return False;
   end Panic_Prepare_Hook;

   procedure Panic_Hook is
   begin
      return;
   end Panic_Hook;
end Arch.Hooks;
