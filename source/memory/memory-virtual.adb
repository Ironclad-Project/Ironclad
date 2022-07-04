--  memory-virtual.adb: Virtual memory manager.
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

with Ada.Unchecked_Deallocation;
with Lib.Alignment;

package body Memory.Virtual is
   package Align1 is new Lib.Alignment (Integer_Address);
   package Align2 is new Lib.Alignment (Unsigned_64);

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
   begin
      if not Arch.MMU.Init (Memmap) then
         return False;
      else
         Kernel_Map := new Page_Map;
         Kernel_Map.Inner := Arch.MMU.Kernel_Table;
         Lib.Synchronization.Release (Kernel_Map.Mutex'Access);
         return Make_Active (Kernel_Map);
      end if;
   end Init;

   function Make_Active (Map : Page_Map_Acc) return Boolean is
      Success : Boolean := True;
   begin
      --  Make the pagemap active on the callee core by writing the top-level
      --  address to CR3.
      Lib.Synchronization.Seize (Map.Mutex'Access);
      if not Arch.MMU.Is_Active (Map.Inner) then
         Success := Arch.MMU.Make_Active (Map.Inner);
      end if;
      Lib.Synchronization.Release (Map.Mutex'Access);
      return Success;
   end Make_Active;

   function Map_Range
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Physical : Physical_Address;
       Length   : Unsigned_64;
       Flags    : Arch.MMU.Page_Permissions) return Boolean
   is
      Success : Boolean;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);

      Success := Arch.MMU.Map_Range (
         Map.Inner,
         To_Address     (Align1.Align_Down (Physical, Page_Size)),
         To_Address     (Align1.Align_Down (Virtual,  Page_Size)),
         Storage_Offset (Align2.Align_Up   (Length,   Page_Size)),
         Flags
      );

      if not Success then
         goto Ret;
      end if;

      for Mapping of Map.Map_Ranges loop
         if not Mapping.Is_Present then
            Mapping := (
               Is_Present     => True,
               Virtual_Start  => Virtual,
               Physical_Start => Physical,
               Length         => Length,
               Flags          => Flags
            );
            exit;
         end if;
      end loop;

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex'Access);
      return Success;
   end Map_Range;

   function Remap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions) return Boolean
   is
      Success : Boolean;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);

      Success := Arch.MMU.Remap_Range (
         Map.Inner,
         To_Address    (Align1.Align_Down (Virtual, Page_Size)),
         Storage_Count (Align2.Align_Up   (Length,  Page_Size)),
         Flags
      );

      if not Success then
         goto Ret;
      end if;

      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present and then Mapping.Virtual_Start = Virtual then
            Mapping.Flags := Flags;
            exit;
         end if;
      end loop;

      --  TODO: Invalidate global TLBs if needed.

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex'Access);
      return Success;
   end Remap_Range;

   function Unmap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64) return Boolean
   is
      Success : Boolean;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);

      Success := Arch.MMU.Unmap_Range (
         Map.Inner,
         To_Address    (Align1.Align_Down (Virtual, Page_Size)),
         Storage_Count (Align2.Align_Up   (Length,  Page_Size))
      );

      if not Success then
         goto Ret;
      end if;

      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present and then Mapping.Virtual_Start = Virtual then
            Mapping.Is_Present := False;
            exit;
         end if;
      end loop;

      --  TODO: Invalidate global TLBs if needed.

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex'Access);
      return Success;
   end Unmap_Range;

   function New_Map return Page_Map_Acc is
      Inner   : constant Arch.MMU.Page_Table := Arch.MMU.Create_Table;
      New_Map : Page_Map_Acc;
   begin
      if Inner /= Arch.MMU.Page_Table (System.Null_Address) then
         New_Map := new Page_Map;
         New_Map.Inner := Inner;
         Lib.Synchronization.Release (New_Map.Mutex'Access);
         return New_Map;
      else
         return null;
      end if;
   end New_Map;

   procedure Delete_Map (Map : in out Page_Map_Acc) is
      procedure F is new Ada.Unchecked_Deallocation (Page_Map, Page_Map_Acc);
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);
      Arch.MMU.Destroy_Table (Map.Inner);
      F (Map);
      Map := null;
   end Delete_Map;

   function Fork_Map (Map : Page_Map_Acc) return Page_Map_Acc is
      type Page_Data     is array (Unsigned_64 range <>) of Unsigned_8;
      type Page_Data_Acc is access Page_Data;

      Discard : Boolean;
      Forked  : constant Page_Map_Acc := New_Map;
   begin
      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present then
            declare
               New_Data      : Page_Data_Acc with Volatile;
               Original_Data : Page_Data (1 .. Mapping.Length) with
               --  FIXME: How is this + 0x10 a fix? How does this even work?
               Address =>
                  To_Address (Mapping.Physical_Start + Memory_Offset + 16#10#);
            begin
               New_Data := new Page_Data (1 .. Mapping.Length);
               for O in 1 .. Mapping.Length loop
                  New_Data (O) := Original_Data (O);
               end loop;

               Discard := Map_Range (
                  Forked,
                  Mapping.Virtual_Start,
                  To_Integer (New_Data.all'Address) - Memory_Offset,
                  Mapping.Length,
                  Mapping.Flags
               );
            end;
         end if;
      end loop;
      return Forked;
   end Fork_Map;

   function Is_Loaded (Map : Page_Map_Acc) return Boolean is
   begin
      return Arch.MMU.Is_Active (Map.Inner);
   end Is_Loaded;

   function Virtual_To_Physical
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address) return Physical_Address
   is
      Addr   : constant System.Address := To_Address (Virtual);
      Result : Physical_Address;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);
      Result := To_Integer (Arch.MMU.Translate_Address (Map.Inner, Addr));
      Lib.Synchronization.Release (Map.Mutex'Access);
      return Result;
   end Virtual_To_Physical;
end Memory.Virtual;
