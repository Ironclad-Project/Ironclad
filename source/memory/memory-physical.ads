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
   --
   --  Called when doing 'new'.
   --  @param Sz Size to allocate in bytes. Sz = size_t'Last will
   --  unconditionally error, and Sz as 0 for allocating a small,
   --  freeable block. These are Ada-mandated semantics for `new`.
   --  @return Address of the allocated object in the higher half.
   --  The block pointed by the address is:
   --  - Not zero'd out, since SPARK requires us to initialize it ourselves.
   --  - For Sz >= Page_Size, alignment is Page_Size. Else, it is unspecified.
   --  - Never null, errors are handled internally, this includes OOM.
   function Alloc (Sz : Interfaces.C.size_t) return Memory.Virtual_Address
      with Export, Convention => C, External_Name => "__gnat_malloc";

   --  Called by Unchecked_Deallocation, it deallocates a previously allocated
   --  block, apart of that, it has no special Ada semantics.
   --  @param Address Address of the object to free, higher half or not.
   procedure Free (Address : Interfaces.C.size_t)
      with Export, Convention => C, External_Name => "__gnat_free";
   ----------------------------------------------------------------------------
   --  The functions above can allocate kernel memory past 4 GiB, some devices
   --  require memory below this boundary for their 32 bit address registers.

   --  Allocate.
   procedure Lower_Half_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean);

   --  Free.
   procedure Lower_Half_Free (Addr : Memory.Virtual_Address);
   ----------------------------------------------------------------------------
   --  The functions above are only to be called by the kernel itself, these
   --  ones are to be used by the kernel to give memory to userland. The memory
   --  allocated can be remapped, passed to userland, and mangled in all other
   --  ways.

   --  Allocate.
   procedure User_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean);

   --  Free.
   procedure User_Free (Addr : Memory.Virtual_Address);
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

private

   function Alloc_Pgs (Sz : Interfaces.C.size_t) return Memory.Virtual_Address;
   procedure Free_Pgs (Address : Interfaces.C.size_t);

   function CLZ (Num : Unsigned_64) return Interfaces.C.int;
   pragma Import (Intrinsic, CLZ, "__builtin_clzl");
end Memory.Physical;
