--  memory-virtual.ads: Specification of the virtual memory manager.
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
with Arch.Stivale2;
with Lib.Synchronization;

package Memory.Virtual is
   --  Page table entries consist of an address padded to 4K, which frees
   --  the lower 12 bits for flags, those flags are described below.

   --  Page table flags, shared by all entries except the final page ones.
   type Page_Table_Flags is record
      Present         : Boolean; --  Whether the entry is present.
      Read_Write      : Boolean; --  True for R/W, false for R.
      User_Supervisor : Boolean; --  Whether the user can access, or only root.
      Write_Through   : Boolean; --  Whether write through is enabled.
      Cache_Disable   : Boolean; --  Whether caching is disabled.
      Accessed        : Boolean; --  Whether a PDE or PTE was read.
      Dirty           : Boolean; --  Whether the page has been written to.
      Table_End       : Boolean; --  Whether its a big page or it continues on.
   end record;
   for Page_Table_Flags use record
      Present         at 0 range 0 .. 0;
      Read_Write      at 0 range 1 .. 1;
      User_Supervisor at 0 range 2 .. 2;
      Write_Through   at 0 range 3 .. 3;
      Cache_Disable   at 0 range 4 .. 4;
      Accessed        at 0 range 5 .. 5;
      Dirty           at 0 range 6 .. 6;
      Table_End       at 0 range 7 .. 7;
   end record;
   for Page_Table_Flags'Size use 8;

   --  Flags of a PML1 page.
   type Page_Flags is record
      Present         : Boolean; --  Whether the entry is present.
      Read_Write      : Boolean; --  True for R/W, false for R.
      User_Supervisor : Boolean; --  Whether the user can access, or only root.
      Write_Through   : Boolean; --  Whether write through is enabled.
      Cache_Disable   : Boolean; --  Whether caching is disabled.
      Accessed        : Boolean; --  Whether a PDE or PTE was read.
      Dirty           : Boolean; --  Whether the page has been written to.
      PAT             : Boolean; --  PAT.
      Global          : Boolean; --  Whether the page TLB should be flushed.
   end record;
   for Page_Flags use record
      Present         at 0 range 0 .. 0;
      Read_Write      at 0 range 1 .. 1;
      User_Supervisor at 0 range 2 .. 2;
      Write_Through   at 0 range 3 .. 3;
      Cache_Disable   at 0 range 4 .. 4;
      Accessed        at 0 range 5 .. 5;
      Dirty           at 0 range 6 .. 6;
      PAT             at 0 range 7 .. 7;
      Global          at 0 range 8 .. 8;
   end record;
   for Page_Flags'Size use 9;

   --  Entries of the PML4, 3, and 2.
   --  This bad boys always exist in arrays of 512.
   --  Each entry points to a 512-long entry list of the lower level, or some
   --  might stop to be a big page.
   type Page_Table_Entry is record
      Flags    : Page_Table_Flags;
      Ignored  : Boolean;
      Addr     : Physical_Address;
      NX       : Boolean;
   end record;
   for Page_Table_Entry use record
      Flags    at 0 range  0 ..  7;
      Ignored  at 0 range  8 .. 11;
      Addr     at 0 range 12 .. 62;
      NX       at 0 range 63 .. 63;
   end record;
   for Page_Table_Entry'Size use 64;

   --  Each entry represents a 4KiB memory block in the final level of the
   --  paging chain.
   type Page is record
      Flags   : Page_Flags;
      Ignored : Boolean;
      Addr    : Physical_Address;
      NX      : Boolean;
   end record;
   for Page use record
      Flags    at 0 range  0 ..  8;
      Ignored  at 0 range  9 .. 11;
      Addr     at 0 range 12 .. 62;
      NX       at 0 range 63 .. 63;
   end record;
   for Page'Size use 64;

   --  Initialize the manager with a memmap and PMRs to take into account.
   procedure Init
      (Memmap : access Arch.Stivale2.Memmap_Tag;
       PMRs   : access Arch.Stivale2.PMR_Tag);

   --  Object to represent a page map.
   Page_Size    : constant := 16#1000#;
   Table_Length : constant := 512;
   type PML4 is array (1 .. Table_Length) of Page_Table_Entry
      with Alignment => Page_Size, Size => Table_Length * 64;
   type PML3 is array (1 .. Table_Length) of Page_Table_Entry
      with Alignment => Page_Size, Size => Table_Length * 64;
   type PML2 is array (1 .. Table_Length) of Page_Table_Entry
      with Alignment => Page_Size, Size => Table_Length * 64;
   type PML1 is array (1 .. Table_Length) of Page
      with Alignment => Page_Size, Size => Table_Length * 64;

   --  Page maps.
   type Mapping_Range is record
      Is_Present     : Boolean;
      Virtual_Start  : Virtual_Address;
      Physical_Start : Physical_Address;
      Length         : Unsigned_64;
      Flags          : Page_Flags;
      Not_Execute    : Boolean;
   end record;
   type Mapping_Range_Arr is array (Natural range <>) of Mapping_Range;
   type Page_Map is record
      Mutex      : aliased Lib.Synchronization.Binary_Semaphore;
      PML4_Level : PML4;
      Map_Ranges : Mapping_Range_Arr (1 .. 100);
   end record;
   type Page_Map_Acc is access all Page_Map;

   --  Functions to manipulate pagemaps.
   procedure Make_Active (Map : Page_Map_Acc);
   procedure Map_Page
      (Map         : Page_Map_Acc;
       Virtual     : Virtual_Address;
       Physical    : Physical_Address;
       Flags       : Page_Flags;
       Not_Execute : Boolean);
   procedure Map_Range
      (Map         : Page_Map_Acc;
       Virtual     : Virtual_Address;
       Physical    : Physical_Address;
       Length      : Unsigned_64;
       Flags       : Page_Flags;
       Not_Execute : Boolean;
       Register    : Boolean);
   procedure Unmap_Page (Map : Page_Map_Acc; Virtual : Virtual_Address);
   procedure Change_Page_Flags
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Flags   : Page_Flags);
   function Fork_Map (Map : Page_Map_Acc) return Page_Map_Acc;
   function Clone_Space (Map : Page_Map_Acc) return Page_Map_Acc;
   function Is_Loaded (Map : Page_Map_Acc) return Boolean;
   function Virtual_To_Physical
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address) return Physical_Address;

   --  Map meant to be used for all cores for kernel code.
   Kernel_Map : Page_Map_Acc;

private
   type Address_Components is record
      PML4_Entry : Unsigned_64;
      PML3_Entry : Unsigned_64;
      PML2_Entry : Unsigned_64;
      PML1_Entry : Unsigned_64;
   end record;
   function Get_Address_Components
      (Virtual : Virtual_Address) return Address_Components;

   function Get_Next_Level
      (Current_Level       : Physical_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean) return Physical_Address;

   function Get_Page
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Allocate : Boolean) return Virtual_Address;

   function Chomp_Flags (Address : Physical_Address) return Physical_Address;
   function Add_Flags (Address : Physical_Address) return Physical_Address;
end Memory.Virtual;
