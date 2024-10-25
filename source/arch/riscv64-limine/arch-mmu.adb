--  arch-mmu.adb: Architecture-specific MMU code.
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

package body Arch.MMU is
   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      pragma Unreferenced (Memmap);
   begin
      return True;
   end Init;

   function Fork_Table (Map : Page_Table_Acc) return Page_Table_Acc is
      pragma Unreferenced (Map);
   begin
      return null;
   end Fork_Table;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
   begin
      Map := null;
   end Destroy_Table;

   function Make_Active (Map : Page_Table_Acc) return Boolean is
      pragma Unreferenced (Map);
   begin
      return True;
   end Make_Active;

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
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual);
      pragma Unreferenced (Length);
   begin
      Physical  := System.Null_Address;
      Is_Mapped := True;
      Is_User_Accessible := True;
      Is_Readable := True;
      Is_Writeable := True;
      Is_Executable := False;
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Caching        : Caching_Model := Write_Back) return Boolean
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Physical_Start);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
   begin
      return True;
   end Map_Range;

   procedure Map_Allocated_Range
      (Map            : Page_Table_Acc;
       Physical_Start : out System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Success        : out Boolean;
       Caching        : Caching_Model := Write_Back)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
   begin
      Physical_Start := System.Null_Address;
      Success        := True;
   end Map_Allocated_Range;

   function Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions;
       Caching       : Caching_Model := Write_Back) return Boolean
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

   function Get_User_Mapped_Size (Map : Page_Table_Acc) return Unsigned_64 is
      pragma Unreferenced (Map);
   begin
      return 0;
   end Get_User_Mapped_Size;

   procedure Get_Statistics (Stats : out Virtual_Statistics) is
   begin
      Stats := (others => 0);
   end Get_Statistics;
end Arch.MMU;
