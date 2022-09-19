--  arch-innermmu.ads: Architecture-specific MMU code.
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

with Interfaces; use Interfaces;
with Memory; use Memory;

package Arch.InnerMMU with SPARK_Mode => Off is
   type Page_Permissions is record
      User_Accesible : Boolean; --  User accesible.
      Read_Only      : Boolean; --  Read only or RW.
      Executable     : Boolean; --  Will store executable code.
      Global         : Boolean; --  Hint for global (TLB optimization).
      Write_Through  : Boolean; --  Hint for write-combining + write-through.
   end record;

   --  Object to represent a page map.
   Page_Size_4K : constant := 16#001000#;
   Page_Size_2M : constant := 16#200000#;
   type PML4 is array (1 .. 512) of Unsigned_64
      with Alignment => Page_Size_4K, Size => 512 * 64;
   type PML4_Acc is access all PML4;

   --  Default page alignment.
   Page_Size : constant := Page_Size_4K;

   --  Page maps.
   type Page_Map is record
      PML4_Level : PML4;
   end record;
   type Page_Map_Acc is access all Page_Map;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean;

   function Create_Table return Page_Map_Acc;

   procedure Destroy_Table (Map : in out Page_Map_Acc);

   function Make_Active (Map : Page_Map_Acc) return Boolean;

   function Is_Active (Map : Page_Map_Acc) return Boolean;

   function Translate_Address
      (Map     : Page_Map_Acc;
       Virtual : System.Address) return System.Address;

   function Map_Range
      (Map            : Page_Map_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean;

   function Remap_Range
      (Map           : Page_Map_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean;

   function Unmap_Range
      (Map           : Page_Map_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean;

   procedure Flush_Local_TLB (Addr : System.Address);

   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count);
   --  TODO: Code this 2 bad boys once the VMM makes use of them.

   procedure Flush_Global_TLBs (Addr : System.Address);
   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count);

private

   type Address_Components is record
      PML4_Entry : Unsigned_64;
      PML3_Entry : Unsigned_64;
      PML2_Entry : Unsigned_64;
      PML1_Entry : Unsigned_64;
   end record;
   function Get_Address_Components
      (Virtual : Virtual_Address) return Address_Components;

   function Clean_Entry (Entry_Body : Unsigned_64) return Physical_Address;

   function Get_Next_Level
      (Current_Level       : Physical_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean) return Physical_Address;

   function Get_Page_4K
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Allocate : Boolean) return Virtual_Address;

   function Get_Page_2M
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Allocate : Boolean) return Virtual_Address;

   function Flags_To_Bitmap (Perm : Page_Permissions) return Unsigned_16;
end Arch.InnerMMU;
