--  arch-mmu.ads: Architecture-specific MMU code.
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

with Interfaces; use Interfaces;
with Memory;     use Memory;

package Arch.MMU is
   --  Permissions used for mapping.
   type Page_Permissions is record
      User_Accesible : Boolean; --  User accesible.
      Read_Only      : Boolean; --  Read only or RW.
      Executable     : Boolean; --  Will store executable code.
      Global         : Boolean; --  Hint for global (TLB optimization).
      Write_Through  : Boolean; --  Hint for write-combining + write-through.
   end record with Pack, Size => 5;

   --  FIXME: We use gnatprep for setting up architecture-specific details.
   --  There are probably more idiomatic ways to do this, but for this small
   --  little type changes, it is probably fine, else we would have to put
   --  this header on each port, and that would repeat a lot of code.
   type Page_Table;
   type Page_Table_Acc is access Page_Table;
   #if ArchName = """aarch64-stivale2"""
      Page_Size : constant := 16#1000#;
      type Page_Level is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
      type Page_Level_Acc is access Page_Level;
      type Page_Table is record
         TTBR0 : Page_Level;
         TTBR1 : Page_Level;
      end record;
      type Address_Components is record
         Level0_Entry : Unsigned_64;
         Level1_Entry : Unsigned_64;
         Level2_Entry : Unsigned_64;
         Level3_Entry : Unsigned_64;
      end record;

      procedure Set_MMU_State;
      function Get_Addr_From_Entry (Entry_B : Unsigned_64) return Unsigned_64;
      function Get_Components (Ad : Integer_Address) return Address_Components;
      function Get_Next_Level
         (Current_Level       : Integer_Address;
          Index               : Unsigned_64;
          Create_If_Not_Found : Boolean) return Integer_Address;
      function Get_Page
         (Root_Lvl : System.Address;
          Virtual  : Integer_Address;
          Allocate : Boolean) return Integer_Address;
      function Get_Bits (Permissions : Page_Permissions) return Unsigned_64;
   #elsif ArchName = """arm-raspi2b"""
      Page_Size : constant := 16#1000#;
      type Page_Level is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
      type Page_Level_Acc is access Page_Level;
      type Page_Table is record
         TTBR0 : Page_Level;
         TTBR1 : Page_Level;
      end record;

      procedure Set_MMU_State;
   #elsif ArchName = """sparc-leon3"""
      Page_Size : constant := 16#1000#;
      type PML4 is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
      type PML4_Acc is access all PML4;
      type Page_Table is record
         PML4_Level : PML4;
      end record;
   #elsif ArchName = """x86_64-multiboot2"""
      Page_Size : constant := 16#1000#;
      type PML4 is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
      type PML4_Acc is access PML4;
      type Page_Table is record
         PML4_Level : PML4;
      end record;
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
      function Get_Page
         (Map      : Page_Table_Acc;
          Virtual  : Virtual_Address;
          Allocate : Boolean) return Virtual_Address;
      function Flags_To_Bitmap (Perm : Page_Permissions) return Unsigned_16;
      procedure Destroy_Level (Entry_Body : Unsigned_64; Level : Integer);
   #end if;

   --  Kernel map, which is used by the freestanding kernel when called.
   Kernel_Table : Page_Table_Acc;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean;

   --  Create or destroy maps, return Null_Address or False on failure.
   function Create_Table return Page_Table_Acc;
   procedure Destroy_Table (Map : in out Page_Table_Acc)
      with Pre => Map /= null, Post => Map = null;

   --  Make the passed map active, will return False on failure.
   function Make_Active (Map : Page_Table_Acc) return Boolean
      with Pre => Map /= null;
   function Is_Active (Map : Page_Table_Acc) return Boolean
      with Pre => Map /= null;

   --  Do translation for a single address, this function does not fail.
   function Translate_Address
      (Map     : Page_Table_Acc;
       Virtual : System.Address) return System.Address
      with Pre => Map /= null;

   --  Map, remap, or unmap a range, will return False on failure.
   function Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
      with Pre => Map /= null;

   function Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
      with Pre => Map /= null;

   function Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
      with Pre => Map /= null;

   --  Issue TLB flushes apart of the ones natural to the mapping process.
   --  Example: Several CPUs run the same pagemap, etc.
   procedure Flush_Local_TLB (Addr : System.Address);
   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count);
   procedure Flush_Global_TLBs (Addr : System.Address);
   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count);
end Arch.MMU;
