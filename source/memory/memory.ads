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

with Arch.MMU;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

package Memory is
   type Size is mod 2 ** Standard'Address_Size; --  Any object in bytes.

   --  Some notable memory locations and sizes.
   Null_Address      : constant := 0;
   Kernel_Stack_Size : constant := 16#F000#;

   subtype Physical_Address is Integer_Address;
   subtype Virtual_Address  is Integer_Address;

   --  Renames of Arch.MMU because of historically having those functions here.
   function Memory_Offset return Integer_Address
      renames Arch.MMU.Memory_Offset;
   function Kernel_Offset return Integer_Address
      renames Arch.MMU.Kernel_Offset;

   function Memory_Offset return Unsigned_64;
   function Kernel_Offset return Unsigned_64;
end Memory;
