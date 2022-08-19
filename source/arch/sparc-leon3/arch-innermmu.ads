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

with System; use System;
with Interfaces; use Interfaces;

package Arch.InnerMMU with SPARK_Mode => Off is
   type Page_Permissions is record
      User_Accesible : Boolean; --  User accesible.
      Read_Only      : Boolean; --  Read only or RW.
      Executable     : Boolean; --  Will store executable code.
      Global         : Boolean; --  Hint for global (TLB optimization).
      Write_Through  : Boolean; --  Hint for write-combining + write-through.
   end record;

   --  Page structure.
   Higher_Half : constant := 16#FFFF000000000000#;
   Page_Size   : constant := 16#1000#;

   type Page_Level is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
   type Page_Level_Acc is access Page_Level;
   type Page_Map is record
      TTBR0 : Page_Level;
      TTBR1 : Page_Level;
   end record;
   type Page_Map_Acc is access Page_Map;

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
end Arch.InnerMMU;
