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

package body Memory.Virtual is
   procedure Init is
   begin
      --  Initialize the kernel pagemap.
      Kernel_Map := new Page_Map;
      Kernel_Map.Inner := Arch.Kernel_Table;
      Lib.Synchronization.Release (Kernel_Map.Mutex'Access);
      Make_Active (Kernel_Map);
   end Init;

   procedure Make_Active (Map : Page_Map_Acc) is
      Discard : Boolean;
   begin
      --  Make the pagemap active on the callee core by writing the top-level
      --  address to CR3.
      Lib.Synchronization.Seize (Map.Mutex'Access);
      if not Arch.Is_Active (Map.Inner) then
         Discard := Arch.Make_Active (Map.Inner);
      end if;
      Lib.Synchronization.Release (Map.Mutex'Access);
   end Make_Active;

   procedure Map_Range
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Physical : Physical_Address;
       Length   : Unsigned_64;
       Flags    : Arch.Page_Permissions)
   is
      Discard : Boolean;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);

      Discard := Arch.Map_Range (
         Map.Inner,
         To_Address (Physical),
         To_Address (Virtual),
         Storage_Offset (Length),
         Flags
      );

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

      Lib.Synchronization.Release (Map.Mutex'Access);
   end Map_Range;

   procedure Remap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.Page_Permissions)
   is
      Discard : Boolean;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);
      Discard := Arch.Remap_Range (
         Map.Inner,
         To_Address (Virtual),
         Storage_Count (Length),
         Flags
      );
      Lib.Synchronization.Release (Map.Mutex'Access);
   end Remap_Range;

   function New_Map return Page_Map_Acc is
      Inner   : constant Arch.Page_Table := Arch.Create_Table;
      New_Map : Page_Map_Acc;
   begin
      if Inner /= Arch.Page_Table (System.Null_Address) then
         New_Map := new Page_Map;
         New_Map.Inner := Inner;
         Lib.Synchronization.Release (New_Map.Mutex'Access);
         return New_Map;
      else
         return null;
      end if;
   end New_Map;

   function Fork_Map (Map : Page_Map_Acc) return Page_Map_Acc is
      type Page_Data     is array (Unsigned_64 range <>) of Unsigned_8;
      type Page_Data_Acc is access Page_Data;

      Forked : constant Page_Map_Acc := New_Map;
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

               Map_Range (
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
      return Arch.Is_Active (Map.Inner);
   end Is_Loaded;

   function Virtual_To_Physical
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address) return Physical_Address
   is
      Addr   : constant System.Address := To_Address (Virtual);
      Result : Physical_Address;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);
      Result := To_Integer (Arch.Translate_Address (Map.Inner, Addr));
      Lib.Synchronization.Release (Map.Mutex'Access);
      return Result;
   end Virtual_To_Physical;
end Memory.Virtual;
