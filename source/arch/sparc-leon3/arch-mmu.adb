--  arch-mmu.adb: Architecture-specific MMU code.
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

with Ada.Unchecked_Deallocation;

package body Arch.MMU with SPARK_Mode => Off is
   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      pragma Unreferenced (Memmap);
   begin
      return False;
   end Init;

   function Create_Table return Page_Table_Acc is
      Map : constant Page_Table_Acc := new Page_Table;
   begin
      return Map;
   end Create_Table;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
      procedure F is new Ada.Unchecked_Deallocation
         (Page_Table, Page_Table_Acc);
   begin
      --  TODO: Free the tables themselves.
      F (Map);
      Map := null;
   end Destroy_Table;

   function Make_Active (Map : Page_Table_Acc) return Boolean is
      pragma Unreferenced (Map);
   begin
      return True;
   end Make_Active;

   function Is_Active (Map : Page_Table_Acc) return Boolean is
      pragma Unreferenced (Map);
   begin
      return True;
   end Is_Active;

   function Translate_Address
      (Map     : Page_Table_Acc;
       Virtual : System.Address) return System.Address
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual);
   begin
      return System.Null_Address;
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Physical_Start);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
   begin
      return True;
   end Map_Range;

   function Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
   begin
      return True;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
   begin
      return True;
   end Unmap_Range;

   procedure Flush_Local_TLB (Addr : System.Address) is
      pragma Unreferenced (Addr);
   begin
      return;
   end Flush_Local_TLB;

   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count) is
      pragma Unreferenced (Addr);
      pragma Unreferenced (Len);
   begin
      return;
   end Flush_Local_TLB;

   --  TODO: Code this 2 bad boys once the VMM makes use of them.

   procedure Flush_Global_TLBs (Addr : System.Address) is
      pragma Unreferenced (Addr);
   begin
      return;
   end Flush_Global_TLBs;

   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count) is
      pragma Unreferenced (Addr);
      pragma Unreferenced (Len);
   begin
      return;
   end Flush_Global_TLBs;
end Arch.MMU;
