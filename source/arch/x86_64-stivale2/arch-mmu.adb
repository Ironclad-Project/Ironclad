--  arch-mmu.adb: Architecture-specific MMU code.
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

with System.Address_To_Access_Conversions;
with Arch.Paging;
with Interfaces; use Interfaces;
with Arch.Wrappers;

package body Arch.MMU is
   package Conv is new System.Address_To_Access_Conversions (Paging.Page_Map);

   function Create_Table return Page_Table is
      Map : constant Paging.Page_Map_Acc := Paging.New_Map;
   begin
      return Page_Table (Conv.To_Address (Conv.Object_Pointer (Map)));
   end Create_Table;

   function Destroy_Table return Boolean is
   begin
      return True;
   end Destroy_Table;

   function Make_Active (Map : Page_Table) return Boolean is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Make_Active (Table);
      return True;
   end Make_Active;

   function Is_Active (Map : Page_Table) return Boolean is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      return Paging.Is_Loaded (Table);
   end Is_Active;

   function Translate_Address
      (Map     : Page_Table;
       Virtual : System.Address) return System.Address
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
      Addr : constant Integer_Address := To_Integer (Virtual);
   begin
      return To_Address (Paging.Virtual_To_Physical (Table, Addr));
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Map_Range (
         Table,
         To_Integer (Virtual_Start),
         To_Integer (Physical_Start),
         Unsigned_64 (Length),
         (
            Present         => True,
            Read_Write      => not Permissions.Read_Only,
            User_Supervisor => Permissions.User_Accesible,
            Write_Through   => False,
            Cache_Disable   => False,
            Accessed        => False,
            Dirty           => False,
            PAT             => False,
            Global          => Permissions.Global
         ),
         not Permissions.Executable
      );
      return True;
   end Map_Range;

   function Remap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Remap_Range (
         Table,
         To_Integer (Virtual_Start),
         Unsigned_64 (Length),
         (
            Present         => True,
            Read_Write      => not Permissions.Read_Only,
            User_Supervisor => Permissions.User_Accesible,
            Write_Through   => False,
            Cache_Disable   => False,
            Accessed        => False,
            Dirty           => False,
            PAT             => False,
            Global          => Permissions.Global
         ),
         not Permissions.Executable
      );
      return True;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Unmap_Range (
         Table,
         To_Integer (Virtual_Start),
         Unsigned_64 (Length)
      );
      return True;
   end Unmap_Range;

   procedure Flush_Local_TLB (Addr : System.Address) is
   begin
      Wrappers.Invalidate_Page (To_Integer (Addr));
   end Flush_Local_TLB;

   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count) is
      Curr : Storage_Count := 0;
   begin
      while Curr < Len loop
         Wrappers.Invalidate_Page (To_Integer (Addr + Curr));
         Curr := Curr + Paging.Page_Size;
      end loop;
   end Flush_Local_TLB;

   --  TODO: Code this 2 bad boys once the VMM makes use of them.

   procedure Flush_Global_TLBs (Addr : System.Address) is
   begin
      null;
   end Flush_Global_TLBs;

   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count) is
   begin
      null;
   end Flush_Global_TLBs;
end Arch.MMU;
