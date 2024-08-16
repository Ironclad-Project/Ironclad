--  arch-hooks.ads: Architecture-specific hooks for several utilities.
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

package Arch.Hooks is
   --  Register architecture-specific devices.
   function Devices_Hook return Boolean;

   --  PRCTL hook for the syscall.
   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean;

   --  Hook to be called in panic procedures for bringing all cores to a stop
   --  (but not the caller).
   procedure Panic_SMP_Hook with Global => null;

   --  Get the number of cores which are active and used by the OS.
   function Get_Active_Core_Count return Positive;

   --  Hook to register architecture-specific RAM files inside the kernel.
   procedure Register_RAM_Files;
end Arch.Hooks;
