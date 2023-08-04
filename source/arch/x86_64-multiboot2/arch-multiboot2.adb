--  arch-multiboot2.adb: multiboot2 utilities.
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

with System; use System;
with Lib;
with Lib.Panic;
with Lib.Alignment;
with Memory;
with Memory.Virtual;

package body Arch.Multiboot2 with SPARK_Mode => Off is
   Cached_RSDP : ACPI.RSDP;
   Cached_FB   : Framebuffer_Tag;

   function Get_Framebuffer return Framebuffer_Tag is
   begin
      return Cached_FB;
   end Get_Framebuffer;

   function Get_RSDP return ACPI.RSDP is
   begin
      return Cached_RSDP;
   end Get_RSDP;

   procedure Translate_Proto (Proto : Header_Acc) is
      package Align is new Lib.Alignment (Unsigned_32);

      Current_Tag_Addr : System.Address;
      Highest_Address  : Integer_Address := 0;
   begin
      Current_Tag_Addr := Proto.Start_Of_Tags'Address;
      loop
         declare
            Current_Tag : Tag with Import, Address => Current_Tag_Addr;
         begin
            exit when Current_Tag.Type_Of_Tag = End_ID;
            case Current_Tag.Type_Of_Tag is
               when End_ID         => exit;
               when Cmdline_ID     => Process_Cmdline     (Current_Tag_Addr);
               when Module_ID      => Process_Module      (Current_Tag_Addr);
               when Memmap_ID      => Process_Memmap      (Current_Tag_Addr);
               when RSDP_Old_ID    => Process_RSDP        (Current_Tag_Addr);
               when RSDP_New_ID    => Process_RSDP        (Current_Tag_Addr);
               when Framebuffer_ID => Process_Framebuffer (Current_Tag_Addr);
               when others         => null;
            end case;
            Current_Tag_Addr := Current_Tag_Addr + Storage_Offset
               (Align.Align_Up (Current_Tag.Size, 8));
         end;
      end loop;

      --  Get the sub 1mib region address.
      --  Addresses of entries are not touched because later everything under
      --  1MiB is nuked.
      for E of Global_Info.Memmap (1 .. Global_Info.Memmap_Len) loop
         if E.MemType = Memory_Free and To_Integer (E.Start) <= 16#100000# then
            if To_Integer (E.Start) >= 16#1000# and
               E.Length >= Max_Sub_1MiB_Size
            then
               Sub_1MiB_Region := E.Start;
            elsif E.Length >= 16#1000# and then
                  E.Length - 16#1000# >= Max_Sub_1MiB_Size
            then
               Sub_1MiB_Region := To_Address (16#1000#);
            end if;
            exit;
         end if;
      end loop;
      if Sub_1MiB_Region = Null_Address then
         Lib.Panic.Hard_Panic ("No SMP trampoline address could be found!");
      end if;

      --  Get the highest module address, or 5MiB at least. Everything below
      --  will be reserved, anything above will follow memory map.
      --  TODO: This could be done less wastefully, with much more finesse,
      --  more efficiently, more smart, and in all aspects better if we just
      --  actually corrected the mmap instead of discarding most of it.
      Highest_Address := 16#500000#;
      for E of Global_Info.RAM_Files (1 .. Global_Info.RAM_Files_Len) loop
         if To_Integer (E.Start + E.Length) - Memory.Memory_Offset >
            Highest_Address
         then
            Highest_Address := To_Integer (E.Start + E.Length)
               - Memory.Memory_Offset;
         end if;
      end loop;
      for E of Global_Info.Memmap (1 .. Global_Info.Memmap_Len) loop
         if To_Integer (E.Start) < Highest_Address and
            To_Integer (E.Start + E.Length) >= Highest_Address
         then
            E.Length := E.Length - Storage_Count
               (Highest_Address - To_Integer (E.Start));
            E.Start  := To_Address (Highest_Address);
         end if;
      end loop;
      for E of Global_Info.Memmap (1 .. Global_Info.Memmap_Len) loop
         if To_Integer (E.Start) < Highest_Address then
            E.Length := 0;
         end if;
      end loop;

      --  Clean the memmap and return.
      Global_Info.Memmap_Len := Clean_Memmap
         (Global_Info.Memmap (1 .. Global_Info.Memmap_Len));
   end Translate_Proto;

   procedure Process_Cmdline (Tag_Addr : System.Address) is
      Tag          : Cmdline_Tag with Import, Address => Tag_Addr;
      Cmdline_Addr : constant System.Address := Tag.Start_Of_Cmdline'Address;
      Cmdline_Len  : constant Natural := Lib.C_String_Length (Cmdline_Addr);
      Cmdline : String (1 .. Cmdline_Len) with Address => Cmdline_Addr;
   begin
      Global_Info.Cmdline (1 .. Cmdline_Len) := Cmdline;
      Global_Info.Cmdline_Len := Cmdline_Len;
   end Process_Cmdline;

   procedure Process_Module (Tag_Addr : System.Address) is
      Tag : Module_Tag with Import, Address => Tag_Addr;
      Start : constant Unsigned_64 :=
         Unsigned_64 (Tag.Start_Address) + Memory.Memory_Offset;
      Length : constant Unsigned_64 :=
         Unsigned_64 (Tag.End_Address - Tag.Start_Address);
   begin
      Global_Info.RAM_Files_Len := Global_Info.RAM_Files_Len + 1;
      Global_Info.RAM_Files (Global_Info.RAM_Files_Len) := (
         Start  => To_Address (Integer_Address (Start)),
         Length => Storage_Count (Length)
      );
   end Process_Module;

   procedure Process_Memmap (Tag_Addr : System.Address) is
      Map : Memmap_Tag with Import, Address => Tag_Addr;
      Start_Addr : constant System.Address := Map.Start_Of_Entries'Address;
      Offset     : Storage_Count  := 0;
      Type_Entry : Boot_Memory_Type;
   begin
      loop
         exit when Offset >= Storage_Count (Map.TagInfo.Size - (128 / 8)) or
                   Global_Info.Memmap_Len = Global_Info.Memmap'Length;

         declare
            Ent : Memmap_Entry with Import, Address => Start_Addr + Offset;
         begin
            if Ent.EntryType = Memmap_Entry_Available then
               Type_Entry := Memory_Free;
            else
               Type_Entry := Memory_Reserved;
            end if;
            Global_Info.Memmap_Len := Global_Info.Memmap_Len + 1;
            Global_Info.Memmap (Global_Info.Memmap_Len) := (
               Start   => To_Address (Integer_Address (Ent.Base)),
               Length  => Storage_Count (Ent.Length),
               MemType => Type_Entry
            );
            Offset := Offset + Storage_Count (Map.Entry_Size);
         end;
      end loop;
   end Process_Memmap;

   procedure Process_RSDP (Tag_Addr : System.Address) is
      Tag : RSDP_Tag with Import, Address => Tag_Addr;
   begin
      Cached_RSDP := Tag.Inner_RSDP;
   end Process_RSDP;

   procedure Process_Framebuffer (Tag_Addr : System.Address) is
      Tag : Framebuffer_Tag with Import, Address => Tag_Addr;
   begin
      Cached_FB := Tag;
   end Process_Framebuffer;

   function Clean_Memmap (Memmap : in out Boot_Memory_Map) return Natural is
      package A is new Lib.Alignment (Integer_Address);
      Value        : Integer_Address;
      Min_Index    : Natural;
      Final_Last   : Natural := Memmap'Last;
      Intermediate : Boot_Memory_Region;
   begin
      --  First, align all entries to page size.
      for E of Memmap loop
         if E.Length /= 0 then
            Value    := To_Integer (E.Start);
            E.Start  := To_Address
               (A.Align_Up (Value, Memory.Virtual.Page_Size));
            E.Length := E.Length -
               Storage_Count (To_Integer (E.Start) - Value);
            E.Length := Storage_Count (A.Align_Down
               (Integer_Address (E.Length), Memory.Virtual.Page_Size));
         end if;
      end loop;

      --  Remove entries with a 0 length, made by the previous path.
      Min_Index := Memmap'First;
      while Min_Index < Final_Last loop
         if Memmap (Min_Index).Length = 0 then
            Memmap (Min_Index) := Memmap (Final_Last);
            Min_Index  := Min_Index  - 1;
            Final_Last := Final_Last - 1;
         end if;
         Min_Index := Min_Index + 1;
      end loop;

      --  Sort the memory map with a simple bubble sort.
      for Iteration in Memmap'First .. Final_Last - 1 loop
         Value     := To_Integer (Memmap (Iteration).Start);
         Min_Index := Iteration;
         for Index in Iteration .. Memmap'Last loop
            if To_Integer (Memmap (Index).Start) < Value then
               Value     := To_Integer (Memmap (Index).Start);
               Min_Index := Index;
            end if;
         end loop;
         Intermediate := Memmap (Min_Index);
         Memmap (Min_Index) := Memmap (Iteration);
         Memmap (Iteration) := Intermediate;
      end loop;
      return Final_Last;
   end Clean_Memmap;
end Arch.Multiboot2;
