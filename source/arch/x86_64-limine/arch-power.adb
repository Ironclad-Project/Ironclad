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

with Memory; use Memory;
with Panic;
with Arch.ACPI;

package body Arch.Power is
   procedure Halt (Status : out Power_Status) is
   begin
      Panic.Hard_Panic ("Using panic as a halting mechanism");
   end Halt;

   procedure Reboot (Status : out Power_Status) is
   begin
      ACPI.Do_Reboot;
      Status := Failure;
   end Reboot;

   procedure Poweroff (Status : out Power_Status) is
      Discard : Boolean;
   begin
      ACPI.Enter_Sleep (ACPI.S5, Discard);
      Status := Failure;
   end Poweroff;
   ----------------------------------------------------------------------------
   procedure Get_Preferred_Profile (P : out Power_Profile) is
      Addr : ACPI.Table_Record;
   begin
      P := Unspecified;
      ACPI.FindTable (ACPI.FADT_Signature, Addr);
      if Addr.Virt_Addr /= Null_Address then
         declare
            F : ACPI.FADT with Import, Address => To_Address (Addr.Virt_Addr);
         begin
            case F.Preferred_PM_Profile is
               when      1 => P := Desktop;
               when      2 => P := Mobile;
               when      3 => P := Workstation;
               when      4 => P := Enterprise_Server;
               when      5 => P := SOHO_Server;
               when      6 => P := Appliance;
               when      7 => P := Performance_Server;
               when      8 => P := Tablet_Convertible;
               when others => P := Unspecified;
            end case;
         end;
         ACPI.Unref_Table (Addr);
      end if;
   end Get_Preferred_Profile;
   ----------------------------------------------------------------------------
   procedure Get_Buttons (Has_Power_Button, Has_Sleep_Button : out Boolean) is
   begin
      Has_Power_Button := ACPI.Has_Power_Button;
      Has_Sleep_Button := ACPI.Has_Sleep_Button;
   end Get_Buttons;
end Arch.Power;
