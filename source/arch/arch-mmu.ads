--  arch-mmu.ads: Architecture-specific MMU code.
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
with Interfaces; use Interfaces;

package Arch.MMU is
   --  Permissions used for mapping.
   --  Ironclad forces W^X, so write and execute permissions will conflict,
   --  even though they might not necessarily conflict in hardware.
   type Page_Permissions is record
      Is_User_Accessible : Boolean;
      Can_Read           : Boolean;
      Can_Write          : Boolean;
      Can_Execute        : Boolean;
      Is_Global          : Boolean; --  Hint for global (TLB optimization).
   end record;

   --  Caching models for mapping.
   type Caching_Model is
      (Write_Back,      --  Standard general purpose caching.
       Write_Through,   --  Data is updated on cache and memory simultaneously.
       Write_Combining, --  Allows write combining on the memory area.
       Uncacheable);    --  No caching of any kind whatsoever thanks.

   Page_Size : constant := 16#1000#;

   --  Get the physical address at which the kernel was loaded as part of the
   --  boot process.
   procedure Get_Load_Addr (A : out System.Address; Success : out Boolean);

   --  Offset in virtual memory of the kernel's HDDM.
   function Memory_Offset return Integer_Address;

   --  Offset of the kernel in virtual memory.
   function Kernel_Offset return Integer_Address;

   --  Extract a physical address from a page table entry.
   function Clean_Entry (Entry_Body : Unsigned_64) return Integer_Address;

   --  Extract a physical address and permissions from a page table entry.
   type Clean_Result is record
      User_Flag : Boolean;
      Perms     : Page_Permissions;
      Caching   : Caching_Model;
   end record;
   function Clean_Entry_Perms (Entr : Unsigned_64) return Clean_Result;

   --  Construct a page table entry.
   function Construct_Entry
      (Addr      : System.Address;
       Perm      : Page_Permissions;
       Caching   : Caching_Model;
       User_Flag : Boolean) return Unsigned_64;

   --  Construct a page table intermediary level.
   function Construct_Level (Addr : System.Address) return Unsigned_64;

   --  Check whether a page entry or level is present.
   function Is_Entry_Present (Entry_Body : Unsigned_64) return Boolean;

   --  Check whether a page entry or level is present.
   function Make_Not_Present (Entry_Body : Unsigned_64) return Unsigned_64;

   --  Flush the currently loaded page map's TLB.
   procedure Flush_TLBs (Map, Addr : System.Address; Len : Storage_Count);

   --  Get current map address.
   procedure Get_Current_Table (Addr : out System.Address);

   --  Set current map address.
   procedure Set_Current_Table (Addr : System.Address; Success : out Boolean);
end Arch.MMU;
