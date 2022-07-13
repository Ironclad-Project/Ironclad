--  config.ads: Specification of the config values.
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

package Config is
   --  Project name and contact.
   Name     : constant String := "Ironclad";
   Version  : constant String := "0.0.1prealpha";
   Bug_Site : constant String :=
      "https://savannah.nongnu.org/bugs/?group=ironclad";

   --  Configuration for system internals.
   Is_Small : constant Boolean := $IsSmall;
end Config;
