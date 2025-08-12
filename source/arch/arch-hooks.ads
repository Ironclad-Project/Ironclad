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

with Interfaces; use Interfaces;

package Arch.Hooks is
   --  Register architecture-specific devices.
   procedure Devices_Hook (Success : out Boolean);

   --  PRCTL hook for the syscall.
   procedure PRCTL_Hook
      (Code       : Natural;
       Arg        : in out Unsigned_64;
       Write_Back : out Boolean;
       Success    : out Boolean);

   --  Hook to be called in panic procedures for bringing all cores to a stop
   --  (but not the caller).
   procedure Panic_SMP_Hook with Global => null;

   --  Get the number of cores provided by the system, even the ones we dont
   --  use.
   function Get_Configured_Cores return Positive;

   --  Get the number of cores which are active and used by the OS.
   function Get_Active_Core_Count return Positive;

   --  Hook to register architecture-specific RAM files inside the kernel.
   procedure Register_RAM_Files;

   --  Get the running core's CPU model, if the model is too big, it truncates
   --  silently.
   procedure Get_CPU_Model (Model : out String);

   --  Same as above with vendor.
   procedure Get_CPU_Vendor (Vendor : out String);

   --  Get frequencies of the CPU in MHz.
   procedure Get_CPU_Frequency (Base, Max, Reference : out Unsigned_32);
end Arch.Hooks;
