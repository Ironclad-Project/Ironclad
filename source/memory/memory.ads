--  memory.ads: System-wide memory definitions.
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

with System.Storage_Elements; use System.Storage_Elements;

package Memory is
   type Size is mod 2 ** Standard'Address_Size; --  Any object in bytes.

   --  Some notable memory locations.
   Null_Address : constant := 0;

   --  The kernel has a memory window into the rest of physical memory mapped
   --  at the beggining of the higher half. The kernel code itself is mapped
   --  2 GiB at the end of the address space.
   Kernel_Offset : constant := Size'Last - (16#80000000# - 1);
   Memory_Offset : constant := 16#FFFF800000000000#;

   --  The maximum physical address we can have is as much as we can fit into
   --  the higher half, with a virtual address covering the whole space.
   Physical_Max : constant := Kernel_Offset - Memory_Offset;
   subtype Physical_Address is Integer_Address range 0 .. Physical_Max;
   subtype Virtual_Address  is Integer_Address;
end Memory;
