--  arch-mmu.ads: Architecture-specific MMU code.
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

package Arch.MMU is
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
end Arch.MMU;
