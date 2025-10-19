--  userland-memorylocations.ads: Notable memory ranges for userland locations.
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

package Userland.Memory_Locations is
   --  We start at 2M and not 0 or 16#1000# to make userland and kernel null
   --  dereferences properly trigger page faults, even when accessed as offsets
   --  in a (reasonably sized) struct.
   Min_Memory_Offset : constant := 16#200000#;
end Userland.Memory_Locations;
