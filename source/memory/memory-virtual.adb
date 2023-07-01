--  memory-virtual.adb: Virtual memory manager.
--  Copyright (C) 2023 streaksu
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
   Kernel_Map : Page_Map_Acc;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
   begin
      if Arch.MMU.Init (Memmap) then
         Kernel_Map := new Page_Map'
            (Inner      => Arch.MMU.Kernel_Table,
             Mutex      => Lib.Synchronization.Unlocked_Semaphore,
             Map_Ranges => (others => (Is_Present => False, others => <>)));
         return Make_Active (Kernel_Map);
      else
         return False;
      end if;
   end Init;

   function Get_Kernel_Map return Page_Map_Acc is
   begin
      return Kernel_Map;
   end Get_Kernel_Map;

   function Is_Loaded (Map : Page_Map_Acc) return Boolean is
   begin
      return Arch.MMU.Is_Active (Map.Inner);
   end Is_Loaded;

   function Make_Active (Map : Page_Map_Acc) return Boolean is
   begin
      return Arch.MMU.Make_Active (Map.Inner);
   end Make_Active;

   function Map_Range
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Physical : Physical_Address;
       Length   : Unsigned_64;
       Flags    : Arch.MMU.Page_Permissions) return Boolean
   is
   begin
      return Inner_Map_Range
         (Map          => Map,
          Virtual      => Virtual,
          Physical     => Physical,
          Length       => Length,
          Flags        => Flags,
          Is_Allocated => False);
   end Map_Range;

   function Remap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions) return Boolean
   is
      Success : Boolean := False;
   begin
      Lib.Synchronization.Seize (Map.Mutex);

      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present and then Mapping.Virtual_Start = Virtual then
            Mapping.Flags := Flags;
            goto Actually_Remap;
         end if;
      end loop;
      goto Ret;

   <<Actually_Remap>>
      Success := Arch.MMU.Remap_Range
         (Map           => Map.Inner,
          Virtual_Start => To_Address    (Virtual),
          Length        => Storage_Count (Length),
          Permissions   => Flags);
      Flush_Global_TLBs (To_Address (Virtual), Storage_Count (Length));

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Remap_Range;

   function Unmap_Range
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64) return Boolean
   is
      Success : Boolean := False;
   begin
      Lib.Synchronization.Seize (Map.Mutex);

      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present and then Mapping.Virtual_Start = Virtual then
            Mapping.Is_Present := False;
            if Mapping.Is_Allocated then
               Physical.Free (Interfaces.C.size_t (Mapping.Physical_Start));
            end if;
            goto Actually_Unmap;
         end if;
      end loop;
      goto Ret;

   <<Actually_Unmap>>
      Success := Arch.MMU.Unmap_Range
         (Map           => Map.Inner,
          Virtual_Start => To_Address    (Virtual),
          Length        => Storage_Count (Length));
      Flush_Global_TLBs (To_Address (Virtual), Storage_Count (Length));

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Unmap_Range;

   function Map_Memory_Backed_Region
      (Map     : Page_Map_Acc;
       Virtual : Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions;
       Writing : out Virtual_Address) return Boolean
   is
      Success : Boolean;
      Addr : constant Virtual_Address :=
         Memory.Physical.Alloc (Interfaces.C.size_t (Length));
      Allocated : array (1 .. Length) of Unsigned_8
         with Import, Address => To_Address (Addr);
   begin
      Success := Inner_Map_Range
         (Map          => Map,
          Virtual      => Virtual,
          Physical     => Addr - Memory_Offset,
          Length       => Length,
          Flags        => Flags,
          Is_Allocated => True);
      if Success then
         Allocated := (others => 0);
         Writing   := Addr;
      else
         Memory.Physical.Free (Interfaces.C.size_t (Addr));
         Writing := 0;
      end if;

      return Success;
   end Map_Memory_Backed_Region;

   function New_Map return Page_Map_Acc is
      Inner : constant Arch.MMU.Page_Table_Acc := Arch.MMU.Create_Table;
   begin
      if Inner = null then
         return null;
      end if;
      return new Page_Map'
         (Inner      => Inner,
          Mutex      => Lib.Synchronization.Unlocked_Semaphore,
          Map_Ranges => (others => (Is_Present => False, others => <>)));
   end New_Map;

   procedure Delete_Map (Map : in out Page_Map_Acc) is
      procedure F is new Ada.Unchecked_Deallocation (Page_Map, Page_Map_Acc);
   begin
      Lib.Synchronization.Seize (Map.Mutex);
      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present and Mapping.Is_Allocated then
            Physical.Free (Interfaces.C.size_t (Mapping.Physical_Start));
         end if;
      end loop;
      Arch.MMU.Destroy_Table (Map.Inner);
      F (Map);
   end Delete_Map;

   function Fork_Map (Map : Page_Map_Acc) return Page_Map_Acc is
      type Page_Data is array (Unsigned_64 range <>) of Unsigned_8;
      Forked : Page_Map_Acc := New_Map;
      Addr   : Virtual_Address;
   begin
      Lib.Synchronization.Seize (Map.Mutex);
      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present then
            if Mapping.Is_Allocated then
               if not Map_Memory_Backed_Region
                  (Forked,
                   Mapping.Virtual_Start,
                   Mapping.Length,
                   Mapping.Flags,
                   Addr)
               then
                  Delete_Map (Forked);
                  goto Cleanup;
               end if;

               declare
                  New_Data : Page_Data (1 .. Mapping.Length) with Import,
                  Address => To_Address (Addr);
                  Original_Data : Page_Data (1 .. Mapping.Length) with Import,
                  Address => To_Address (Mapping.Physical_Start +
                                         Memory_Offset);
               begin
                  New_Data := Original_Data;
               end;
            else
               if not Map_Range
                  (Forked,
                   Mapping.Virtual_Start,
                   Mapping.Physical_Start,
                   Mapping.Length,
                   Mapping.Flags)
               then
                  Delete_Map (Forked);
                  goto Cleanup;
               end if;
            end if;
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

   function Check_Userland_Access
      (Map        : Page_Map_Acc;
       Addr       : Virtual_Address;
       Byte_Count : Unsigned_64) return Boolean
   is
      Success : Boolean := False;
   begin
      Lib.Synchronization.Seize (Map.Mutex);
      for Mapping of Map.Map_Ranges loop
         if Mapping.Is_Present            and then
            Mapping.Flags.User_Accesible  and then
            Mapping.Virtual_Start <= Addr and then
            Mapping.Virtual_Start + Virtual_Address (Mapping.Length) > Addr +
            Virtual_Address (Byte_Count)
         then
            Success := True;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Check_Userland_Access;

   function Check_Userland_Mappability
      (Addr       : Virtual_Address;
       Byte_Count : Unsigned_64) return Boolean
   is
   begin
      return Addr                                < Memory_Offset and then
             Addr + Virtual_Address (Byte_Count) < Memory_Offset;
   end Check_Userland_Mappability;
   ----------------------------------------------------------------------------
   function Inner_Map_Range
      (Map          : Page_Map_Acc;
       Virtual      : Virtual_Address;
       Physical     : Physical_Address;
       Length       : Unsigned_64;
       Flags        : Arch.MMU.Page_Permissions;
       Is_Allocated : Boolean) return Boolean
   is
      Success : Boolean := False;
   begin
      Lib.Synchronization.Seize (Map.Mutex);

      for Mapping of Map.Map_Ranges loop
         if not Mapping.Is_Present then
            Mapping :=
               (Is_Present     => True,
                Is_Allocated   => Is_Allocated,
                Virtual_Start  => Virtual,
                Physical_Start => Physical,
                Length         => Length,
                Flags          => Flags);
            goto Actually_Map;
         end if;
      end loop;
      goto Ret;

   <<Actually_Map>>
      Success := Arch.MMU.Map_Range
         (Map            => Map.Inner,
          Physical_Start => To_Address     (Physical),
          Virtual_Start  => To_Address     (Virtual),
          Length         => Storage_Offset (Length),
          Permissions    => Flags);

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Inner_Map_Range;
end Memory.Virtual;
