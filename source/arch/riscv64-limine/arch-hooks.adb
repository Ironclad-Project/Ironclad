--  arch-hooks.adb: Architecture-specific hooks for several utilities.
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

package body Arch.Hooks is
   function Devices_Hook return Boolean is
   begin
      return True;
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      pragma Unreferenced (Code);
      pragma Unreferenced (Arg);
   begin
      return True;
   end PRCTL_Hook;

   procedure Panic_SMP_Hook is
   begin
      null;
   end Panic_SMP_Hook;

   function Get_Active_Core_Count return Positive is
   begin
      return 1;
   end Get_Active_Core_Count;

   procedure Register_RAM_Files is
   begin
      null;
   end Register_RAM_Files;
end Arch.Hooks;
