--  userland-memorylocations.ads: Notable memory ranges for userland locations.
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

package Userland.Memory_Locations is
   --  These are locations in memory for processes that the kernel will use to
   --  load programs at.
   --  All of these addresses are bound to be randomized between the Min and
   --  Max boundaries. When randomization is disabled, the kernel defaults to
   --  the Min boundary.

   Offset_Min     : constant := 16#00000001000#; --  Userland program offset.
   Offset_Max     : constant := 16#00010000000#;
   LD_Offset_Min  : constant := 16#00060000000#; --  ELF LD payload.
   LD_Offset_Max  : constant := 16#000F0000000#;
   Mmap_Anon_Min  : constant := 16#10000000000#; --  MAP_ANON style things.
   Mmap_Anon_Max  : constant := 16#F0000000000#;
   Stack_Jump_Min : constant := 16#10000000000#; --  Jump from prev MAP_ANON.
   Stack_Jump_Max : constant := 16#20000000000#;
end Userland.Memory_Locations;
