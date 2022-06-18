--  arch.ads: Specification of the architecture-specific package.
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
with System.Storage_Elements; use System.Storage_Elements;

package Arch is
   --  Boot information for the freestanding kernel to use.
   --  The memory map is not necessarily sorted, or may have overlapping
   --  sections, or may have gaps.
   type Boot_Memory_Region is record
      Start   : System.Address;
      Length  : Storage_Count;
      Is_Free : Boolean;
   end record;
   type Boot_Memory_Map is array (Natural range <>) of Boot_Memory_Region;

   type Boot_RAM_File is record
      Start  : System.Address;
      Length : Storage_Count;
   end record;
   type Boot_RAM_Files is array (Natural range <>) of Boot_RAM_File;

   type Boot_Information is record
      Cmdline       : String (1 .. 256);
      Cmdline_Len   : Natural range 0 .. 256;
      Memmap        : Boot_Memory_Map (1 .. 64);
      Memmap_Len    : Natural range 0 .. 64;
      RAM_Files     : Boot_RAM_Files (1 .. 4);
      RAM_Files_Len : Natural range 0 .. 4;
   end record;

   function Get_Info return Boot_Information;
   ----------------------------------------------------------------------------
   --  Architecture-specific snippets.

   --  Drive the execution thread to an irrecoverable state.
   --  (Halt and Catch Fire).
   procedure HCF with Inline_Always;

   --  Enable and disable external interrupts.
   procedure Enable_Interrupts with Inline_Always;
   procedure Disable_Interrupts with Inline_Always;

   --  Processor hint for waiting for interrupts in an energy-efficient state.
   procedure Wait_For_Interrupt with Inline_Always;

   --  Processor hint for optimizing spinlocks and another cache-intensitive
   --  situations.
   procedure Pause with Inline_Always;
   ----------------------------------------------------------------------------
   --  Debug capabilities.

   --  Print characters and strings to architecture-dependant debug channels.
   --  They are not locked.
   procedure Debug_Print (Message : Character);
   procedure Debug_Print (Message : String);
   ----------------------------------------------------------------------------
   --  Hooks for architecture-specific facilities initialized by the
   --  freestanding side.

   --  Register architecture-specific devices.
   procedure Devices_Hook;

   --  PRCTL hook for the syscall.
   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean;
end Arch;
