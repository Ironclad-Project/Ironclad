--  arch-acpi.adb: ACPI driver.
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

package body Arch.ACPI with SPARK_Mode => Off is
   function Is_Supported return Boolean is
   begin
      return False;
   end Is_Supported;

   function Get_Revision return Natural is
   begin
      return 0;
   end Get_Revision;

   procedure Initialize (Success : out Boolean) is
   begin
      Success := False;
   end Initialize;
   ----------------------------------------------------------------------------
   function FindTable (Signature : SDT_Signature) return Virtual_Address is
   begin
      return 0;
   end FindTable;
   ----------------------------------------------------------------------------
   procedure Enter_Sleep (Level : Sleep_Level; Success : out Boolean) is
   begin
      Success := False;
   end Enter_Sleep;

   procedure Do_Reboot is
   begin
      null;
   end Do_Reboot;
end Arch.ACPI;
