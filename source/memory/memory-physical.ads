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

with Memory;
with Arch.Stivale2;

package Memory.Physical is
   --  Initialize the allocator with a memmap.
   procedure Init_Allocator (Memmap : access Arch.Stivale2.Memmap_Tag);

   --  Kernel's malloc and free, also called by ada internally.
   --  The malloc cannot return null, and the free cannot be passed null
   --  either. Those are Ada's rules.
   --  Allocated memory is always zero'ed out.
   function Alloc (Size : Memory.Size) return Memory.Virtual_Address
      with Export, Convention => C, External_Name => "__gnat_malloc";
   procedure Free (Address : Memory.Virtual_Address)
      with Export, Convention => C, External_Name => "__gnat_free";

   --  Get statistics about the state of the kernel memory allocator.
   type Allocator_Info is record
      Total_Memory : Memory.Size;
      Free_Memory  : Memory.Size;
      Used_Memory  : Memory.Size;

      --  Detailed information enabled with tracing.
      Total_Allocations     : Natural;
      Still_To_Be_Freed     : Natural;
      Still_To_Be_Reclaimed : Natural;
   end record;

   --  Set whether the allocator traces detailed information, at the expense
   --  of performance.
   --  Calling this function resets the detailed information.
   procedure Set_Tracing (Enable : Boolean);

   --  Get the gathered information.
   function Get_Info return Allocator_Info;
end Memory.Physical;
