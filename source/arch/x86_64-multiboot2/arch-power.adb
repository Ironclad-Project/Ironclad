--  arch-power.adb: Architecture-specific power management.
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

with Arch.Snippets;
with Lib.Panic;

package body Arch.Power is
   --  TODO: Use the correct ACPI versions, these are pretty basic, possibly
   --  non functional, inferior methods. I just do not wanna sort out ACPI
   --  right now.

   function Halt return Power_Status is
   begin
      Lib.Panic.Hard_Panic ("Halting with panic! or is it panic as halting?!");
      return Failure;
   end Halt;

   function Reboot return Power_Status is
   begin
      Snippets.Port_Out (16#64#, 16#FE#); --  PS2 8042 CPU reset line.
      return Failure;
   end Reboot;

   function Poweroff return Power_Status is
   begin
      Snippets.Port_Out16 (16#0604#, 16#2000#); --  QEMU-specific magic code.
      Snippets.Port_Out16 (16#B004#, 16#2000#); --  Bochs-specific ditto
      Snippets.Port_Out16 (16#4004#, 16#3400#); --  Virtualbox-specific ditto.
      return Failure;
   end Poweroff;
end Arch.Power;
