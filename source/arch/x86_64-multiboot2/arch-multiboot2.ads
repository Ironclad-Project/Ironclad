--  arch-multiboot2.ads: multiboot2 utilities and tags.
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

with System;
with Interfaces; use Interfaces;
with Arch.ACPI;

package Arch.Multiboot2 with SPARK_Mode => Off is
   --  Types of several tags.
   End_ID         : constant :=  0;
   Cmdline_ID     : constant :=  1;
   Module_ID      : constant :=  3;
   Memmap_ID      : constant :=  6;
   Framebuffer_ID : constant :=  8;
   RSDP_Old_ID    : constant := 14;
   RSDP_New_ID    : constant := 15;

   --  Tags start with one of these.
   type Tag is record
      Type_Of_Tag : Unsigned_32;
      Size        : Unsigned_32;
   end record;
   for Tag use record
      Type_Of_Tag at 0 range 00 .. 31;
      Size        at 0 range 32 .. 63;
   end record;
   for Tag'Size use 64;

   --  Multiboot2 information passed to the kernel.
   type Header is record
      Total_Size    : Unsigned_32;
      Reserved      : Unsigned_32;
      Start_Of_Tags : Tag;
   end record;
   for Header use record
      Total_Size    at 0 range 00 ..  31;
      Reserved      at 0 range 32 ..  63;
      Start_Of_Tags at 0 range 64 .. 127;
   end record;
   type Header_Acc is access Header;

   --  Command line tag.
   type Cmdline_Tag is record
      TagInfo          : Tag;
      Start_Of_Cmdline : Character;
   end record;
   for Cmdline_Tag use record
      TagInfo          at 0 range  0 .. 63;
      Start_Of_Cmdline at 0 range 64 .. 71;
   end record;
   for Cmdline_Tag'Size use 72;
   type Cmdline_Tag_Acc is access Cmdline_Tag;

   --  Module tag, one occurence for each module.
   type Module_Tag is record
      TagInfo          : Tag;
      Start_Address    : Unsigned_32;
      End_Address      : Unsigned_32;
      Start_Of_Cmdline : Character;
   end record;
   for Module_Tag use record
      TagInfo          at 0 range   0 ..  63;
      Start_Address    at 0 range  64 ..  95;
      End_Address      at 0 range  96 .. 127;
      Start_Of_Cmdline at 0 range 128 .. 135;
   end record;
   for Module_Tag'Size use 136;

   --  Memmap tag.
   Memmap_Entry_Available        : constant := 1;
   Memmap_Entry_Reserved         : constant := 2;
   Memmap_Entry_ACPI_Reclaimable : constant := 3;
   Memmap_Entry_ACPI_NVS         : constant := 4;
   Memmap_Entry_Bad              : constant := 5;

   type Memmap_Entry is record
      Base      : Unsigned_64;
      Length    : Unsigned_64;
      EntryType : Unsigned_32;
      Unused    : Unsigned_32;
   end record;
   for Memmap_Entry use record
      Base      at 0 range   0 ..  63;
      Length    at 0 range  64 .. 127;
      EntryType at 0 range 128 .. 159;
      Unused    at 0 range 160 .. 191;
   end record;
   for Memmap_Entry'Size use 192;

   type Memmap_Entries is array (Natural range <>) of Memmap_Entry;
   type Memmap_Tag is record
      TagInfo          : Tag;
      Entry_Size       : Unsigned_32;
      Entry_Version    : Unsigned_32;
      Start_Of_Entries : Memmap_Entry;
   end record;
   for Memmap_Tag use record
      TagInfo          at 0 range   0 ..  63;
      Entry_Size       at 0 range  64 ..  95;
      Entry_Version    at 0 range  96 .. 127;
      Start_Of_Entries at 0 range 128 .. 319;
   end record;
   for Memmap_Tag'Size use 320;

   --  Framebuffer tag.
   type Framebuffer_Tag is record
      TagInfo        : Tag;
      Address        : System.Address;
      Pitch          : Unsigned_32;
      Width          : Unsigned_32;
      Height         : Unsigned_32;
      BPP            : Unsigned_8;
      Type_Of_Buffer : Unsigned_8;
      Reserved       : Unsigned_8;
   end record;
   for Framebuffer_Tag use record
      TagInfo        at 0 range   0 ..  63;
      Address        at 0 range  64 .. 127;
      Pitch          at 0 range 128 .. 159;
      Width          at 0 range 160 .. 191;
      Height         at 0 range 192 .. 223;
      BPP            at 0 range 224 .. 231;
      Type_Of_Buffer at 0 range 232 .. 239;
      Reserved       at 0 range 240 .. 247;
   end record;
   for Framebuffer_Tag'Size use 248;
   type Framebuffer_Tag_Acc is access Framebuffer_Tag;

   --  RSDP (old and new) structure.
   type RSDP_Tag is record
      TagInfo    : Tag;
      Inner_RSDP : ACPI.RSDP;
   end record;
   for RSDP_Tag use record
      TagInfo    at 0 range  0 ..  63;
      Inner_RSDP at 0 range 64 .. 383;
   end record;
   for RSDP_Tag'Size use 384;

   --  Smallest address of the memmap, filled when the proto is translated,
   --  since this garbage architecture requires sub 1MiB memory regions for
   --  SMP starting. Its fucking retarded.
   --  Its meant to be always valid.
   Max_Sub_1MiB_Size : constant := 16#1000#;
   Sub_1MiB_Region : System.Address;

   --  Get the framebuffer tag.
   function Get_Framebuffer return Framebuffer_Tag;

   --  Get a pointer to the RSDP, as passed by multiboot2, null on failure.
   function Get_RSDP return ACPI.RSDP;

   --  Translate a multiboot2 header into architecture info.
   --  @param Proto Pointer to translate, if null, return cached or panic.
   --  @return Translated protocol.
   function Translate_Proto (Proto : Header_Acc) return Boot_Information;

private

   --  Process several individual tags.
   procedure Process_Cmdline     (Tag_Addr : System.Address);
   procedure Process_Module      (Tag_Addr : System.Address);
   procedure Process_Memmap      (Tag_Addr : System.Address);
   procedure Process_RSDP        (Tag_Addr : System.Address);
   procedure Process_Framebuffer (Tag_Addr : System.Address);

   --  Clean a multiboot memory map, it can only have less entries.
   function Clean_Memmap (Memmap : in out Boot_Memory_Map) return Natural;
end Arch.Multiboot2;
