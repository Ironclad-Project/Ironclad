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
   --  Fixed memory locations we cannot randomize because they can be hardcoded
   --  by userland (linker addresses and such).
   Program_Offset : constant := 16#00000000#;

   --  Values we can randomize are given in min and max boundaries.
   --  (size is until the next structure).
   LD_Offset_Min  : constant := 16#00060000000#;
   LD_Offset_Max  : constant := 16#000F0000000#;
   Mmap_Anon_Min  : constant := 16#10000000000#;
   Mmap_Anon_Max  : constant := 16#F0000000000#;
   Stack_Jump_Min : constant := 16#10000000000#; --  Stack is a jump from anon.
   Stack_Jump_Max : constant := 16#20000000000#;
end Userland.Memory_Locations;
