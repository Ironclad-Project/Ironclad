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
with Arch; use Arch;

package Memory.Virtual is
   --  Initialize the manager using the architectural interface.
   procedure Init;

   Page_Size : constant := 16#1000#;

   --  Page maps.
   type Mapping_Range is record
      Is_Present     : Boolean;
      Virtual_Start  : Virtual_Address;
      Physical_Start : Physical_Address;
      Length         : Unsigned_64;
      Flags          : Arch.Page_Permissions;
   end record;
   type Mapping_Range_Arr is array (Natural range <>) of Mapping_Range;
   type Page_Map is record
      Mutex      : aliased Lib.Synchronization.Binary_Semaphore;
      Inner      : Arch.Page_Table;
      Map_Ranges : Mapping_Range_Arr (1 .. 100);
   end record;
   type Page_Map_Acc is access all Page_Map;

   --  Functions to manipulate pagemaps.
   procedure Make_Active (Map : Page_Map_Acc);
   procedure Map_Range
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Physical : Physical_Address;
       Length   : Unsigned_64;
       Flags    : Arch.Page_Permissions);
   procedure Remap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.Page_Permissions);
   function New_Map return Page_Map_Acc;
   function Fork_Map (Map : Page_Map_Acc) return Page_Map_Acc;
   function Is_Loaded (Map : Page_Map_Acc) return Boolean;
   function Virtual_To_Physical
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address) return Physical_Address;

   --  Map meant to be used for all cores for kernel code.
   Kernel_Map : Page_Map_Acc;
end Memory.Virtual;
