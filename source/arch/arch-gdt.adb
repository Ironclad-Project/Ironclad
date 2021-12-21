--  arch-gdt.adb: GDT creation, loading, and manipulation.
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

with Interfaces;              use Interfaces;
with System.Machine_Code;     use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

package body Arch.GDT is
   --  Records for the GDT structure and its entries.
   type GDT_Entry is record
      Limit          : Unsigned_16;
      Base_Low_16    : Unsigned_16;
      Base_Mid_8     : Unsigned_8;
      Segment_Access : Unsigned_8;
      Granularity    : Unsigned_8;
      Base_High_8    : Unsigned_8;
   end record;
   for GDT_Entry use record
      Limit          at 0 range  0 .. 15;
      Base_Low_16    at 0 range 16 .. 31;
      Base_Mid_8     at 0 range 32 .. 39;
      Segment_Access at 0 range 40 .. 47;
      Granularity    at 0 range 48 .. 55;
      Base_High_8    at 0 range 56 .. 63;
   end record;
   for GDT_Entry'Size use 64;

   type TSS_Entry is record
      Length        : Unsigned_16;
      Base_Low_16   : Unsigned_16;
      Base_Mid_8    : Unsigned_8;
      Flags_1       : Unsigned_8;
      Flags_2       : Unsigned_8;
      Base_High_8   : Unsigned_8;
      Base_Upper_32 : Unsigned_32;
      Reserved      : Unsigned_32;
   end record;
   for TSS_Entry use record
      Length        at 0 range  0 ..  15;
      Base_Low_16   at 0 range 16 ..  31;
      Base_Mid_8    at 0 range 32 ..  39;
      Flags_1       at 0 range 40 ..  47;
      Flags_2       at 0 range 48 ..  55;
      Base_High_8   at 0 range 56 ..  63;
      Base_Upper_32 at 0 range 64 ..  95;
      Reserved      at 0 range 96 .. 127;
   end record;
   for TSS_Entry'Size use 128;

   --  XXX: GDT has 16 and 32 bit entries for the stivale protocol terminal.
   --  If the kernel was to move from it, remove them
   --  (unless another reason happens).
   type GDT_Entries is array (1 .. 9) of GDT_Entry;
   type GDT is record
      Entries : GDT_Entries;
      TSS     : TSS_Entry;
   end record;
   for GDT use record
      Entries at 0 range   0 .. 575;
      TSS     at 0 range 576 .. 703;
   end record;
   for GDT'Size use 704;

   type GDT_Pointer is record
      Size    : Unsigned_16;
      Address : System.Address;
   end record;
   for GDT_Pointer use record
      Size    at 0 range  0 .. 15;
      Address at 0 range 16 .. 79;
   end record;
   for GDT_Pointer'Size use 80;

   --  Global variables for the GDT and its pointer.
   Global_GDT     : GDT;
   Global_Pointer : GDT_Pointer;

   procedure Init is
   begin
      --  Filling the GDT's descriptors, which are in order Null followed by:
      --  16-bit kernel code | 16-bit kernel data | 32-bit kernel code
      --  32-bit kernel data | 64-bit kernel code | 64-bit-kernel data
      --    64-bit user data |   64-bit user code
      Global_GDT.Entries (1) :=        (0, 0, 0,           0,           0, 0);
      Global_GDT.Entries (2) := (16#FFFF#, 0, 0, 2#10011010#,           0, 0);
      Global_GDT.Entries (3) := (16#FFFF#, 0, 0, 2#10010010#,           0, 0);
      Global_GDT.Entries (4) := (16#FFFF#, 0, 0, 2#10011010#, 2#11001111#, 0);
      Global_GDT.Entries (5) := (16#FFFF#, 0, 0, 2#10010010#, 2#11001111#, 0);
      Global_GDT.Entries (6) :=        (0, 0, 0, 2#10011010#, 2#00100000#, 0);
      Global_GDT.Entries (7) :=        (0, 0, 0, 2#10010010#,           0, 0);
      Global_GDT.Entries (8) :=        (0, 0, 0, 2#11110010#,           0, 0);
      Global_GDT.Entries (9) :=        (0, 0, 0, 2#11111010#, 2#00100000#, 0);

      --  Fill up the TSS with its length and zero out the rest.
      Global_GDT.TSS := (104, 0, 0, 0, 0, 0, 0, 0);

      --  Set GDT Pointer and load the GDT for the current core.
      Global_Pointer := (Global_GDT'Size - 1, Global_GDT'Address);
      Load_GDT;
   end Init;

   procedure Load_GDT is
      use ASCII;
   begin
      Asm ("lgdt %0"               & LF & HT &
           "push %%rax"            & LF & HT &
           "push %2"               & LF & HT &
           "lea 1f(%%rip), %%rax"  & LF & HT &
           "push %%rax"            & LF & HT &
           "lretq"                 & LF & HT &
           "1:"                    & LF & HT &
           "pop %%rax"             & LF & HT &
           "mov %1, %%ds"          & LF & HT &
           "mov %1, %%es"          & LF & HT &
           "mov %1, %%fs"          & LF & HT &
           "mov %1, %%gs"          & LF & HT &
           "mov %1, %%ss"          & LF & HT,
           Inputs   => (GDT_Pointer'Asm_Input ("m",  Global_Pointer),
                        Unsigned_16'Asm_Input ("rm", Kernel_Data64_Segment),
                        Unsigned_16'Asm_Input ("i",  Kernel_Code64_Segment)),
           Volatile => True);
   end Load_GDT;

   procedure Load_TSS (Address : System.Address) is
      CAddress : constant Unsigned_64 := Unsigned_64 (To_Integer (Address));
   begin
      Global_GDT.TSS.Base_Low_16   := Unsigned_16 (CAddress);
      Global_GDT.TSS.Base_Mid_8    := Unsigned_8  (Shift_Right (CAddress, 16));
      Global_GDT.TSS.Flags_1       := 2#10001001#;
      Global_GDT.TSS.Flags_2       := 0;
      Global_GDT.TSS.Base_High_8   := Unsigned_8  (Shift_Right (CAddress, 24));
      Global_GDT.TSS.Base_Upper_32 := Unsigned_32 (Shift_Right (CAddress, 32));
      Global_GDT.TSS.Reserved      := 0;

      Asm ("ltr %0",
           Inputs   => Unsigned_16'Asm_Input ("rm", TSS_Segment),
           Volatile => True);
   end Load_TSS;
end Arch.GDT;