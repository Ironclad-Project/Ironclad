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

   procedure Create_Table (New_Map : out Page_Table_Acc) is
   begin
      New_Map := null;
   end Create_Table;

   procedure Fork_Table (Map : Page_Table_Acc; Forked : out Page_Table_Acc) is
      pragma Unreferenced (Map);
   begin
      Forked := null;
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

   procedure Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Success        : out Boolean;
       Caching        : Caching_Model := Write_Back)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Physical_Start);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
      pragma Unreferenced (Caching);
   begin
      Success := False;
   end Map_Range;

   procedure Map_Allocated_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions;
       Success       : out Boolean;
       Caching       : Caching_Model := Write_Back)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
      pragma Unreferenced (Caching);
   begin
      Success := True;
   end Map_Allocated_Range;

   procedure Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions;
       Success       : out Boolean;
       Caching       : Caching_Model := Write_Back)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
      pragma Unreferenced (Caching);
   begin
      Success := False;
   end Remap_Range;

   procedure Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Success       : out Boolean)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
   begin
      Success := True;
   end Unmap_Range;

   function Get_Curr_Table_Addr return System.Address is
   begin
      return System.Null_Address;
   end Get_Curr_Table_Addr;

   function Get_Map_Table_Addr (Map : Page_Table_Acc) return System.Address is
      pragma Unreferenced (Map);
   begin
      return System.Null_Address;
   end Get_Map_Table_Addr;

   procedure Set_Table_Addr (Addr : System.Address) is
      pragma Unreferenced (Addr);
   begin
      null;
   end Set_Table_Addr;

   procedure Get_User_Mapped_Size (Map : Page_Table_Acc; Sz : out Unsigned_64)
   is
      pragma Unreferenced (Map);
   begin
      Sz := 0;
   end Get_User_Mapped_Size;

   procedure Get_Statistics (Stats : out Virtual_Statistics) is
   begin
      Stats := (others => 0);
   end Get_Statistics;
end Arch.MMU;
