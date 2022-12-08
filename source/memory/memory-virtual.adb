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
with Memory.Physical;
with Interfaces.C;

package body Memory.Virtual with SPARK_Mode => Off is
   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
   begin
      if not Arch.MMU.Init (Memmap) then
         return False;
      else
         Kernel_Map := new Page_Map;
         Kernel_Map.Inner := Arch.MMU.Kernel_Table;
         Lib.Synchronization.Release (Kernel_Map.Mutex);
         return Make_Active (Kernel_Map);
      end if;
   end Init;

   function Is_Loaded (Map : Page_Map_Acc) return Boolean is
   begin
      return Arch.MMU.Is_Active (Map.Inner);
   end Is_Loaded;

   function Make_Active (Map : Page_Map_Acc) return Boolean is
   begin
      if Map /= null then
         return Arch.MMU.Make_Active (Map.Inner);
      else
         return False;
      end if;
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
      if Map = null then
         return False;
      end if;

      Lib.Synchronization.Seize (Map.Mutex);

      Success := Arch.MMU.Map_Range (
         Map            => Map.Inner,
         Physical_Start => To_Address     (Physical),
         Virtual_Start  => To_Address     (Virtual),
         Length         => Storage_Offset (Length),
         Permissions    => Flags
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
      Lib.Synchronization.Release (Map.Mutex);
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
      if Map = null then
         return False;
      end if;

      Lib.Synchronization.Seize (Map.Mutex);

      Success := Arch.MMU.Remap_Range (
         Map           => Map.Inner,
         Virtual_Start => To_Address    (Virtual),
         Length        => Storage_Count (Length),
         Permissions   => Flags
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
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Remap_Range;

   function Unmap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64) return Boolean
   is
      Success : Boolean;
   begin
      if Map = null then
         return False;
      end if;

      Lib.Synchronization.Seize (Map.Mutex);

      Success := Arch.MMU.Unmap_Range (
         Map           => Map.Inner,
         Virtual_Start => To_Address    (Virtual),
         Length        => Storage_Count (Length)
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
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Unmap_Range;

   function New_Map return Page_Map_Acc is
      Inner   : constant Arch.MMU.Page_Table_Acc := Arch.MMU.Create_Table;
      New_Map : Page_Map_Acc;
   begin
      if Inner /= null then
         New_Map := new Page_Map;
         New_Map.Inner := Inner;
         Lib.Synchronization.Release (New_Map.Mutex);
         return New_Map;
      else
         return null;
      end if;
   end New_Map;

   procedure Delete_Map (Map : in out Page_Map_Acc) is
      procedure F is new Ada.Unchecked_Deallocation (Page_Map, Page_Map_Acc);
   begin
      if Map /= null then
         Lib.Synchronization.Seize (Map.Mutex);
         Arch.MMU.Destroy_Table (Map.Inner);
         F (Map);
      end if;
   end Delete_Map;

   function Fork_Map (Map : Page_Map_Acc) return Page_Map_Acc is
      type Page_Data is array (Unsigned_64 range <>) of Unsigned_8;
      Forked : Page_Map_Acc := New_Map;
   begin
      if Map = null then
         return null;
      end if;

      Lib.Synchronization.Seize (Map.Mutex);

      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present then
            declare
               --  This has to be done this ugly way because an array
               --  like "access Page_Data" makes Ada allocate 16 bytes of data
               --  in front of the block, which is more than problematic when
               --  we care about exact copies with the same alignment.
               New_Data_Addr : constant Integer_Address :=
                  Memory.Physical.Alloc (Interfaces.C.size_t (Mapping.Length));
               New_Data      : Page_Data (1 .. Mapping.Length) with Import,
               Address => To_Address (New_Data_Addr);
               Original_Data : Page_Data (1 .. Mapping.Length) with Import,
               Address => To_Address (Mapping.Physical_Start + Memory_Offset);
            begin
               New_Data := Original_Data;
               if not Map_Range (
                  Forked,
                  Mapping.Virtual_Start,
                  New_Data_Addr - Memory_Offset,
                  Mapping.Length,
                  Mapping.Flags
               )
               then
                  Delete_Map (Forked);
                  goto Cleanup;
               end if;
            end;
         end if;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Map.Mutex);
      return Forked;
   end Fork_Map;

   function Virtual_To_Physical
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address) return Physical_Address
   is
      Addr   : constant System.Address := To_Address (Virtual);
      Result : Physical_Address;
   begin
      Lib.Synchronization.Seize (Map.Mutex);
      Result := To_Integer (Arch.MMU.Translate_Address (Map.Inner, Addr));
      Lib.Synchronization.Release (Map.Mutex);
      return Result;
   end Virtual_To_Physical;

   function Check_Userland_Access (Addr : Virtual_Address) return Boolean is
   begin
      --  TODO: This should use a passed map to actually check memory regions.
      --  for now, we can just check whether its in the higher half for
      --  speed.
      return Addr /= 0 and Addr < Memory_Offset;
   end Check_Userland_Access;
end Memory.Virtual;
