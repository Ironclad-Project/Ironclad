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

with Arch.InnerMMU;

package Arch.MMU is
   --  Opaque object for keeping track of data, not locked.
   subtype Page_Table       is InnerMMU.Page_Map;
   subtype Page_Table_Acc   is InnerMMU.Page_Map_Acc;
   subtype Page_Permissions is InnerMMU.Page_Permissions;

   --  Kernel map, which is used by the freestanding kernel when called.
   Kernel_Table : Page_Table_Acc;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean
      renames InnerMMU.Init;

   --  Create or destroy maps, return Null_Address or False on failure.
   function Create_Table return Page_Table_Acc
      renames InnerMMU.Create_Table;
   procedure Destroy_Table (Map : in out Page_Table_Acc)
      renames InnerMMU.Destroy_Table;

   --  Make the passed map active, will return False on failure.
   function Make_Active (Map : Page_Table_Acc) return Boolean
      renames InnerMMU.Make_Active;
   function Is_Active (Map : Page_Table_Acc) return Boolean
      renames InnerMMU.Is_Active;

   --  Do translation for a single address, this function does not fail.
   function Translate_Address
      (Map     : Page_Table_Acc;
       Virtual : System.Address) return System.Address
      renames InnerMMU.Translate_Address;

   --  Map, remap, or unmap a range, will return False on failure.
   function Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
      renames InnerMMU.Map_Range;
   function Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
      renames InnerMMU.Remap_Range;
   function Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
      renames InnerMMU.Unmap_Range;

   --  Issue TLB flushes apart of the ones natural to the mapping process.
   --  Example: Several CPUs run the same pagemap, etc.
   procedure Flush_Local_TLB (Addr : System.Address)
      renames InnerMMU.Flush_Local_TLB;
   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count)
      renames InnerMMU.Flush_Local_TLB;
   procedure Flush_Global_TLBs (Addr : System.Address)
      renames InnerMMU.Flush_Global_TLBs;
   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count)
      renames InnerMMU.Flush_Global_TLBs;
end Arch.MMU;
