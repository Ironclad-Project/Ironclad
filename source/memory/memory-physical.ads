--  memory-physical.ads: Specification of the physical memory allocator.
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

with Arch; use Arch;
with Interfaces.C; use Interfaces.C;

package Memory.Physical is
   --  Initialize the allocator with a memmap.
   --  @param Memmap Memory map to use to initialize the allocator.
   procedure Init_Allocator (Memmap : Arch.Boot_Memory_Map);
   ----------------------------------------------------------------------------
   --  Generic kernel allocation functions, called when doing 'new' and
   --  Unchecked_Deallocation. The memory allocated by these functions is not
   --  to be remapped or given to userland, but to just to be used and released
   --  by the kernel.

   --  Called when doing 'new'.
   --  @param Size Size to allocate in bytes, non zero.
   --  @return Address of the allocated object in the higher half.
   --  The block pointed by the address is:
   --  - Not zero'd out, this is up to the consumer.
   --  - Alignment is always Page_Size.
   --  - Never invalid, errors are handled internally, this includes OOM.
   procedure Alloc (Size : size_t; Result : out Memory.Virtual_Address)
      with Export, Convention => C, External_Name => "internal_alloc",
           Pre => Size /= 0;

   --  Allocate function with the same semantics as the one above, but
   --  fallible.
   procedure User_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean);

   --  Allocate function with the same semantics as the one above, but
   --  guarantees that Addr will end up in the lower half, and fallible.
   procedure Lower_Half_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean);

   --  Called by Unchecked_Deallocation, it deallocates a previously allocated
   --  block, apart of that, it has no special Ada semantics.
   --  @param Address Address of the object to free, higher half or not.
   procedure Free (Address : size_t)
      with Export, Convention => C, External_Name => "internal_free";
   ----------------------------------------------------------------------------
   --  Allocator-wide memory statistics.
   --  @field Total     Total physical memory of the system.
   --  @field Available Non-reserved memory amount managed by the allocator.
   --  @field Free      Free allocator-managed memory in the system.
   type Statistics is record
      Total     : Memory.Size;
      Available : Memory.Size;
      Free      : Memory.Size;
   end record;

   --  Fetch memory statistics as defined in the Statistics record.
   --  @param Stats Where to return the stats.
   procedure Get_Statistics (Stats : out Statistics);
end Memory.Physical;
