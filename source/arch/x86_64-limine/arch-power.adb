--  arch-power.adb: Architecture-specific power management.
--  Copyright (C) 2025 streaksu
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
with Arch.ACPI;

package body Arch.Power is
   procedure Halt (Status : out Power_Status) is
   begin
      Lib.Panic.Hard_Panic ("Halting with panic! or is it panic as halting?!");
   end Halt;

   procedure Reboot (Status : out Power_Status) is
   begin
      --  If we are still here by the end of the call we can assume it failed.
      ACPI.Do_Reboot;

      --  Now that uACPI just doesnt work, we try PS2's 8042 CPU reset line.
      Snippets.Port_Out (16#64#, 16#FE#);

      Status := Failure;
   end Reboot;

   procedure Poweroff (Status : out Power_Status) is
      Discard : Boolean;
   begin
      --  First try the actual correct way with uACPI.
      ACPI.Enter_Sleep (ACPI.S5, Discard);

      --  Now that uACPI just doesnt work, try some VM specific codes or fail.
      Snippets.Port_Out16 (16#0604#, 16#2000#); --  QEMU-specific magic code.
      Snippets.Port_Out16 (16#B004#, 16#2000#); --  Bochs-specific ditto
      Snippets.Port_Out16 (16#4004#, 16#3400#); --  Virtualbox-specific ditto.
      Status := Failure;
   end Poweroff;
end Arch.Power;
