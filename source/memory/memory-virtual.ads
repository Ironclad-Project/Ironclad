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
with Lib.Synchronization;
with Arch.MMU; use Arch.MMU;

package Memory.Virtual with SPARK_Mode => Off is
   --  Initialize the manager using the architectural interface.
   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean;

   Page_Size : constant := 16#1000#;

   --  Page maps.
   type Mapping_Range is record
      Is_Present     : Boolean;
      Virtual_Start  : Virtual_Address;
      Physical_Start : Physical_Address;
      Length         : Unsigned_64;
      Flags          : Arch.MMU.Page_Permissions;
   end record;
   type Mapping_Range_Arr is array (Natural range <>) of Mapping_Range;
   type Page_Map is record
      Mutex      : aliased Lib.Synchronization.Binary_Semaphore;
      Inner      : Arch.MMU.Page_Table_Acc;
      Map_Ranges : Mapping_Range_Arr (1 .. 100);
   end record;
   type Page_Map_Acc is access all Page_Map;

   --  Functions to manipulate pagemaps, False on return means failure.
   function Make_Active (Map : Page_Map_Acc) return Boolean;
   function Map_Range
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Physical : Physical_Address;
       Length   : Unsigned_64;
       Flags    : Arch.MMU.Page_Permissions) return Boolean;
   function Remap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions) return Boolean;
   function Unmap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64) return Boolean;
   function New_Map return Page_Map_Acc;
   procedure Delete_Map (Map : in out Page_Map_Acc);
   function Fork_Map (Map : Page_Map_Acc) return Page_Map_Acc;
   function Is_Loaded (Map : Page_Map_Acc) return Boolean;
   function Virtual_To_Physical
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address) return Physical_Address;

   --  Map meant to be used for all cores for kernel code.
   Kernel_Map : Page_Map_Acc;
end Memory.Virtual;
