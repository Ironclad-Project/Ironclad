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

   --  Available paging levels.
   type Levels is
      (Three_Level_Paging, --  Three level paging for small devices.
       Four_Level_Paging,  --  Standard.
       Five_Level_Paging); --  Mostly only a thing in x86.

   --  Get paging levels.
   function Paging_Levels return Levels;

   --  Offset in virtual memory of the virtual memory canonical address hole
   --  that lies in between lower and higher half.
   function Canonical_Hole_Offset return Integer_Address;

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

   --  Make changes to the entries of a map on a range take effect.
   --  @param Map     Physical address of the map's top level.
   --  @param Addr    Start of the changed range.
   --  @param Len     Length of the changed range in bytes.
   --  @param Changed True if an entry that was present was changed or
   --                 removed, which cores may have cached; False if entries
   --                 only became present.
   --  @param Remote  True for a user map, which other cores may be running.
   --                 Once the call returns no core uses a translation it
   --                 replaced, so what those pointed to may be freed. It waits
   --                 for the other cores, which a core spinning with
   --                 interrupts disabled cannot answer, so it must not be made
   --                 while holding a lock that disables them. False for the
   --                 kernel map, which is flushed on this core alone.
   procedure Flush_TLBs
      (Map, Addr : System.Address;
       Len       : Storage_Count;
       Changed   : Boolean;
       Remote    : Boolean);

   --  Get current map address.
   procedure Get_Current_Table (Addr : out System.Address);

   --  Set current map address.
   procedure Set_Current_Table (Addr : System.Address; Success : out Boolean);
end Arch.MMU;
