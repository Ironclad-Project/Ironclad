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

with Interfaces; use Interfaces;
with System;     use System;
with Memory;     use Memory;
with Lib.Synchronization;

package Arch.MMU is
   --  Permissions used for mapping.
   --  Ironclad forces W^X, so write and execute permissions will conflict,
   --  even though they might not necessarily conflict in hardware.
   type Page_Permissions is record
      Is_User_Accesible : Boolean;
      Can_Read          : Boolean;
      Can_Write         : Boolean;
      Can_Execute       : Boolean;
      Is_Global         : Boolean; --  Hint for global (TLB optimization).
   end record;

   --  Caching models for mapping.
   type Caching_Model is
      (Write_Back,      --  Standard general purpose caching.
       Write_Through,   --  Data is updated on cache and memory simultaneously.
       Write_Combining, --  Allows write combining on the memory area.
       Uncacheable);    --  No caching of any kind whatsoever thanks.

   --  Types to represent page tables.
   type Page_Table     is private;
   type Page_Table_Acc is access Page_Table;

   --  Default minimum page size supported by the MMU. Ports may use bigger
   --  pages optionally if possible for optimization, but this size is always
   --  be supported and accepted.
   #if ArchName = """riscv64-limine"""
      Page_Size : constant := 16#1000#;
   #elsif ArchName = """x86_64-limine"""
      Page_Size : constant := 16#1000#;
   #end if;

   --  Kernel map, which is used by the freestanding kernel when called.
   --  Once initialized, it must have the kernel and other essentials, but
   --  nothing else! If you want to have a minimal map, this is your chance to
   --  fork it!
   Kernel_Table : Page_Table_Acc;

   --  Initialize global MMU state, at the end, it will activate Kernel_Table.
   --  @param Memmap Physical memory map, may be used to map MMIO regions.
   --  @return True in success, False in failure or if Kernel_Table failed.
   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean
      with Post => (not Init'Result xor Kernel_Table /= null);

   --  Create a new page table, which should be ready for switching to and
   --  allowing all kernel data to be accessed.
   --  @param Map Table to fork.
   --  @return Forked map, or null on failure.
   function Fork_Table (Map : Page_Table_Acc) return Page_Table_Acc
      with Pre => Map /= null;

   --  Free a table.
   --  @param Map Table to free, will always be set to null at the end.
   procedure Destroy_Table (Map : in out Page_Table_Acc)
      with Pre => Map /= null, Post => Map = null;

   --  Make the passed map active.
   --  @param Map Page table to make active.
   --  @return True in success, False on failure.
   function Make_Active (Map : Page_Table_Acc) return Boolean
      with Pre => Map /= null;

   --  Do translation for a range, and report on some qualities.
   --  If qualities vary in between the range beggining and end, that property
   --  will fail.
   --  @param Map                Page table to walk for translation.
   --  @param Virtual            Virtual address to translate.
   --  @param Length             Length in bytes to translate.
   --  @param Physical           Address pointed to by the virtual address.
   --  @param Is_Mapped          True if mapped, False if not mapped.
   --  @param Is_User_Accessible True if userland can access the address.
   --  @param Is_Readable        True if the mapping can be read.
   --  @param Is_Writeable       True if the mapping can be written to.
   --  @param Is_Executable      True if the mapping can be executed.
   procedure Translate_Address
      (Map                : Page_Table_Acc;
       Virtual            : System.Address;
       Length             : Storage_Count;
       Physical           : out System.Address;
       Is_Mapped          : out Boolean;
       Is_User_Accessible : out Boolean;
       Is_Readable        : out Boolean;
       Is_Writeable       : out Boolean;
       Is_Executable      : out Boolean)
      with Pre =>
         (Map /= null)               and
         (Virtual mod Page_Size = 0) and
         (Length  mod Page_Size = 0);

   --  Map a memory range, allocation of the mapped addresses is not managed.
   --  @param Map            Tables to map for.
   --  @param Physical_Start Physical address to start from.
   --  @param Virtual_Start  Virtual address to start from.
   --  @param Length         Length in bytes to map.
   --  @param Permissions    Permissions to map with.
   function Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Caching        : Caching_Model := Write_Back) return Boolean
      with Pre =>
         (Map /= null)                      and
         (Physical_Start mod Page_Size = 0) and
         (Virtual_Start  mod Page_Size = 0) and
         (Length         mod Page_Size = 0) and
         not (Permissions.Can_Write and Permissions.Can_Execute);

   --  Allocate and map a memory range, the contents will be forked and freed
   --  accordingly, and managed internally.
   --  @param Map            Tables to map for.
   --  @param Physical_Start Allocated physical address.
   --  @param Virtual_Start  Virtual address to start from.
   --  @param Length         Length in bytes to map.
   --  @param Permissions    Permissions to map with.
   --  @param Success        True on success, False on failure.
   procedure Map_Allocated_Range
      (Map            : Page_Table_Acc;
       Physical_Start : out System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Success        : out Boolean;
       Caching        : Caching_Model := Write_Back)
      with Pre =>
         (Map /= null)                      and
         (Virtual_Start  mod Page_Size = 0) and
         (Length         mod Page_Size = 0) and
         not (Permissions.Can_Write and Permissions.Can_Execute),
         Post => (Physical_Start mod Page_Size = 0);

   --  Remap a memory range.
   --  @param Map            Tables to map for.
   --  @param Virtual_Start  Virtual address to start from.
   --  @param Length         Length in bytes to map.
   --  @param Permissions    Permissions to remap with.
   function Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions;
       Caching       : Caching_Model := Write_Back) return Boolean
      with Pre =>
         (Map /= null)                      and
         (Virtual_Start  mod Page_Size = 0) and
         (Length         mod Page_Size = 0) and
         not (Permissions.Can_Write and Permissions.Can_Execute);

   --  Unmap a memory range.
   --  @param Map            Tables to map for.
   --  @param Virtual_Start  Virtual address to start from.
   --  @param Length         Length in bytes to unmap.
   function Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
      with Pre =>
         (Map /= null)                      and
         (Virtual_Start  mod Page_Size = 0) and
         (Length         mod Page_Size = 0);

   --  Get the user mapped memory size, thus, not including kernel space.
   --  @param Map Map to get the size for.
   --  @return Size.
   function Get_User_Mapped_Size (Map : Page_Table_Acc) return Unsigned_64
      with Pre => Map /= null;

   --  Memory statistics of the system.
   type Virtual_Statistics is record
      Kernel_Usage : Memory.Size; --  Space mapped kernel only.
      Table_Usage  : Memory.Size; --  Amount used for tables and tracking.
      Poison_Usage : Memory.Size; --  Memory marked by the hardware as faulty.
   end record;

   --  Get memory statistics of the system.
   procedure Get_Statistics (Stats : out Virtual_Statistics);

private

   #if ArchName = """riscv64-limine"""
      type Page_Level is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
      type Page_Level_Acc is access Page_Level;
      type Page_Table is record
         TTBR0 : Page_Level;
         TTBR1 : Page_Level;
      end record;
   #elsif ArchName = """x86_64-limine"""
      type PML4 is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
      type PML4_Acc is access PML4;
      type Mapping_Range;
      type Mapping_Range_Acc is access Mapping_Range;
      type Mapping_Range is record
         Next           : Mapping_Range_Acc;
         Is_Allocated   : Boolean;
         Virtual_Start  : System.Address;
         Physical_Start : System.Address;
         Length         : Storage_Count;
         Flags          : Page_Permissions;
      end record;

      type Page_Table is record
         PML4_Level      : PML4;
         Mutex           : aliased Lib.Synchronization.Binary_Semaphore;
         Map_Ranges_Root : Mapping_Range_Acc;
      end record;

      function Clean_Entry (Entry_Body : Unsigned_64) return Physical_Address;
      function Get_Next_Level
         (Current_Level       : Physical_Address;
          Index               : Unsigned_64;
          Create_If_Not_Found : Boolean) return Physical_Address;
      function Get_Page
         (Map      : Page_Table_Acc;
          Virtual  : Virtual_Address;
          Allocate : Boolean) return Virtual_Address;
      function Inner_Map_Range
         (Map            : Page_Table_Acc;
          Physical_Start : System.Address;
          Virtual_Start  : System.Address;
          Length         : Storage_Count;
          Permissions    : Page_Permissions;
          Caching        : Caching_Model) return Boolean;
      function Flags_To_Bitmap
         (Perm    : Page_Permissions;
          Caching : Caching_Model) return Unsigned_64;
      procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count);
   #end if;
end Arch.MMU;
