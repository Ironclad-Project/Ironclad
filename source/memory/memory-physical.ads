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

with Arch;
with Interfaces.C;

package Memory.Physical is
   --  Initialize the allocator with a memmap.
   --  @param Memmap Memory map to use to initialize the allocator.
   procedure Init_Allocator (Memmap : in out Arch.Boot_Memory_Map);

   --  Called when doing 'new'. Ada adds the semantics of erroring out
   --  unconditionally when Sz is size_t'Last, and 0 allocates a small block.
   --  It returns memory that is:
   --  - Always zero'ed out for security reasons, this is a kernel after all.
   --  - Always aligned to 4K.
   --  - Never null, errors are handled internally, this includes OOM.
   --  @param Sz Size to allocate in bytes.
   --  @return Address of the allocated object in the higher half.
   function Alloc (Sz : Interfaces.C.size_t) return Memory.Virtual_Address
      with Export, Convention => C, External_Name => "__gnat_malloc";

   --  Called by Unchecked_Deallocation, it deallocates a previously allocated
   --  block, apart of that, it has no special Ada semantics.
   --  @param Address Address of the object to free, higher half or not.
   procedure Free (Address : Interfaces.C.size_t)
      with Export, Convention => C, External_Name => "__gnat_free";

   --  Memory statistics.
   --  @field Total_Memory Total physical memory of the system.
   --  @field Free_Memory Memory available for allocation in the system.
   --  @field Used_Memory Memory not available for allocation.
   type Statistics is record
      Total_Memory : Memory.Size;
      Free_Memory  : Memory.Size;
      Used_Memory  : Memory.Size;
   end record;

   --  Fetch memory statistics.
   --  @return Statistics.
   function Get_Statistics return Statistics;
end Memory.Physical;
