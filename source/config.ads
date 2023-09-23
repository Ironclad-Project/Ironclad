--  config.ads: Configuration values.
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

package Config is
   --  Avoid any style checks related to variable replacement.
   pragma Style_Checks (Off);

   --  Project name, contact, and basic information.
   Name      : constant String := $IroncladName;
   Version   : constant String := $Version;
   Arch_Name : constant String := $ArchName;
   Bug_Site  : constant String := $BugSite;

   --  Configuration for system internals.
   Support_Alloc_Only : constant Boolean := $ALLOCONLY;
end Config;
