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
   ----------------------------------------------------------------------------
   --  Architectural virtual memory management.

   --  Opaque object for keeping track of data, not locked.
   type Page_Table is new System.Address;

   --  Kernel map, which is used by the freestanding kernel when called.
   Kernel_Table : Page_Table;

   --  Create or destroy maps, return Null_Address or False on failure.
   function Create_Table return Page_Table;
   function Destroy_Table return Boolean;

   --  Make the passed map active, will return False on failure.
   function Make_Active (Map : Page_Table) return Boolean;
   function Is_Active (Map : Page_Table) return Boolean;

   --  Do translation for a single address, this function does not fail.
   function Translate_Address
      (Map     : Page_Table;
       Virtual : System.Address) return System.Address;

   --  Map, remap, or unmap a range, will return False on failure.
   type Page_Permissions is record
      User_Accesible : Boolean; --  The user can operate on the range.
      Read_Only      : Boolean; --  The range is read only or r/w.
      Executable     : Boolean; --  The range is executable.
      Global         : Boolean; --  The range is global (TLB optimization).
   end record;
   function Map_Range
      (Map            : Page_Table;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean;
   function Remap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean;
   function Unmap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean;

   --  Issue TLB flushes apart of the ones natural to the mapping process.
   --  Example: Several CPUs run the same pagemap, etc.
   procedure Flush_Local_TLB (Addr : System.Address);
   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count);
   procedure Flush_Global_TLBs (Addr : System.Address);
   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count);
end Arch;
