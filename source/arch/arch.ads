--  arch.ads: Architecture-specific information.
--  Copyright (C) 2024 streaksu
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
with System.Storage_Elements; use System.Storage_Elements;

package Arch is
   --  Memory map for the kernel, it must be sorted and have no gaps.
   type Boot_Memory_Type is
      (Memory_Free,             --  Memory free for use by the allocators.
       Memory_Reserved,         --  MMIO addresses or memory tables or w/e.
       Memory_Kernel,           --  Kernel or RAM files.
       Memory_ACPI_Reclaimable, --  Area used by ACPI that can be reclaimed.
       Memory_ACPI_NVS);        --  ACPI flash we cannot use for our interests.

   type Boot_Memory_Region is record
      Start   : System.Address;
      Length  : Storage_Count;
      MemType : Boot_Memory_Type;
   end record;
   type Boot_Memory_Map is array (Natural range <>) of Boot_Memory_Region;

   --  Some targets may provide RAM files for roots or other purposes.
   type Boot_RAM_File is record
      Start  : System.Address;
      Length : Storage_Count;
   end record;
   type Boot_RAM_Files is array (Natural range <>) of Boot_RAM_File;

   --  Struct to wrap it all together, along with cmdline information.
   type Boot_Information is record
      Cmdline       : String (1 .. 256);
      Cmdline_Len   : Natural range 0 .. 256;
      Memmap        : Boot_Memory_Map (1 .. 64);
      Memmap_Len    : Natural range 0 .. 64;
      RAM_Files     : Boot_RAM_Files (1 .. 4);
      RAM_Files_Len : Natural range 0 .. 4;
   end record;
end Arch;
