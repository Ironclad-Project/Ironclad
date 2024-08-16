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

with System.Machine_Code;     use System.Machine_Code;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Lib.Synchronization;

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
      Limit         : Unsigned_16;
      Base_Low_16   : Unsigned_16;
      Base_Mid_8    : Unsigned_8;
      Flags_1       : Unsigned_8;
      Flags_2       : Unsigned_8;
      Base_High_8   : Unsigned_8;
      Base_Upper_32 : Unsigned_32;
      Reserved      : Unsigned_32;
   end record;
   for TSS_Entry use record
      Limit         at 0 range  0 ..  15;
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

   --  Global GDT and GDT pointer, the TSS is initialized when needed.
   Global_GDT : GDT := (
      Entries => (
         1 =>        (0, 0, 0,           0,           0, 0), --  Null.
         2 => (16#FFFF#, 0, 0, 2#10011010#,           0, 0), --  16-bit kcode.
         3 => (16#FFFF#, 0, 0, 2#10010010#,           0, 0), --  16-bit kdata.
         4 => (16#FFFF#, 0, 0, 2#10011010#, 2#11001111#, 0), --  32-bit kcode.
         5 => (16#FFFF#, 0, 0, 2#10010010#, 2#11001111#, 0), --  32-bit kdata.
         6 =>        (0, 0, 0, 2#10011010#, 2#00100000#, 0), --  64-bit kcode.
         7 =>        (0, 0, 0, 2#10010010#,           0, 0), --  64-bit kdata.
         8 =>        (0, 0, 0, 2#11110010#,           0, 0), --  64-bit udata.
         9 =>        (0, 0, 0, 2#11111010#, 2#00100000#, 0)  --  64-bit ucode.
      ),
      TSS => <>
   );
   Global_Pointer : constant GDT_Pointer := (
      Size    => (Global_GDT'Size / 8) - 1,
      Address => Global_GDT'Address
   );
   TSS_Mutex : aliased Lib.Synchronization.Binary_Semaphore;

   procedure Init is
   begin
      Load_GDT;
      Lib.Synchronization.Release (TSS_Mutex);
   end Init;

   procedure Load_GDT is
   begin
      --  FS and GS have to be user ones because else when going from ring 0 to
      --  3 they will be zero'd out. This is a quirk of x86, so we make them
      --  user now and we just stop worrying.
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
           "mov %3, %%fs"          & LF & HT &
           "mov %3, %%gs"          & LF & HT &
           "mov %1, %%ss"          & LF & HT,
           Inputs   => (GDT_Pointer'Asm_Input ("m",  Global_Pointer),
                        Unsigned_16'Asm_Input ("rm", Kernel_Data64_Segment),
                        Unsigned_16'Asm_Input ("i",  Kernel_Code64_Segment),
                        Unsigned_16'Asm_Input ("rm", User_Data64_Segment)),
           Clobber  => "memory",
           Volatile => True);
   end Load_GDT;

   procedure Load_TSS (Address : System.Address) is
      Addr  : constant Unsigned_64 := Unsigned_64 (To_Integer (Address));
      Low16 : constant Unsigned_64 := Addr                   and 16#FFFF#;
      Mid8  : constant Unsigned_64 := Shift_Right (Addr, 16) and 16#FF#;
      High8 : constant Unsigned_64 := Shift_Right (Addr, 24) and 16#FF#;
      Up32  : constant Unsigned_64 := Shift_Right (Addr, 32) and 16#FFFFFFFF#;
   begin
      Lib.Synchronization.Seize (TSS_Mutex);

      Global_GDT.TSS := (
         Limit         => 103,
         Base_Low_16   => Unsigned_16 (Low16),
         Base_Mid_8    => Unsigned_8  (Mid8),
         Flags_1       => 2#10001001#,
         Flags_2       => 0,
         Base_High_8   => Unsigned_8  (High8),
         Base_Upper_32 => Unsigned_32 (Up32),
         Reserved      => 0
      );

      Asm ("ltr %0",
           Inputs   => Unsigned_16'Asm_Input ("rm", TSS_Segment),
           Clobber  => "memory",
           Volatile => True);

      Lib.Synchronization.Release (TSS_Mutex);
   end Load_TSS;
end Arch.GDT;
