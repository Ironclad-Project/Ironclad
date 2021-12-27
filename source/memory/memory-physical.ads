--  memory-physical.ads: Specification of the physical memory allocator.
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

with System;
with Memory;
with Arch.Stivale2;

package Memory.Physical is
   --  Initialize the allocator with a memmap.
   procedure Init_Allocator (Memmap : access Arch.Stivale2.Memmap_Tag);

   --  Kernel's malloc and free, also called by ada internally.
   function Alloc (Size : Memory.Size) return System.Address;
   procedure Free (Address : System.Address);
   pragma Export (C, Alloc, "__gnat_malloc");
   pragma Export (C, Free,  "__gnat_free");

   --  Get statistics about the state of the memory allocator in bytes.
   procedure Get_Info (Total, Free, Used : out Memory.Size);
end Memory.Physical;
