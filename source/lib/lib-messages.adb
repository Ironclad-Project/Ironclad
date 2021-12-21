--  lib-messages.adb: Utilities for reporting messages to the user.
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

with Arch.Debug;
with Arch.Power;
with Arch.Stivale2;

package body Lib.Messages is
   use ASCII;

   New_Line    : constant String := ""  & LF;
   Color_Red   : constant String := ESC & "[1;31m";
   Color_Cyan  : constant String := ESC & "[1;36m";
   Color_Reset : constant String := ESC & "[0m";

   procedure Print (Message : String) is
   begin
      Basic_Print (Color_Cyan);
      Basic_Print (">> ");
      Basic_Print (Color_Reset);
      Basic_Print (Message);
      Basic_Print (New_Line);
   end Print;

   procedure Panic (Message : String) is
   begin
      Basic_Print (Color_Red);
      Basic_Print ("++ ");
      Basic_Print (Color_Reset);
      Basic_Print (Message);
      Basic_Print (New_Line);
      Arch.Power.HCF;
   end Panic;

   procedure Basic_Print (Message : String) is
   begin
      Arch.Debug.Print (Message);
      Arch.Stivale2.Print_Terminal (Message);
   end Basic_Print;
end Lib.Messages;
