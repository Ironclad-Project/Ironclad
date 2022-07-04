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

with System; use System;
with System.Address_To_Access_Conversions;
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with Lib.Panic;

package body Arch.MMU is
   --  The page size the kernel will use for both its ttbrs.
   type Page_Size is (
      Pages_4K,
      Pages_16K,
      Pages_64K
   );
   for Page_Size use (
      Pages_4K  => 16#1000#,
      Pages_16K => 16#4000#,
      Pages_64K => 16#10000#
   );
   Used_Size : Page_Size;

   --  Page structure.
   type Page_Level is array (1 .. 512) of Unsigned_64 with Size => 512 * 64;
   type Page_Map is record
      TTBR0 : Page_Level;
      TTBR1 : Page_Level;
   end record;
   type Page_Map_Acc is access all Page_Map;
   Kernel_Map : Page_Map_Acc;

   package Conv is new System.Address_To_Access_Conversions (Page_Map);

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      pragma Unreferenced (Memmap);
      type Unsigned_4 is mod 2 ** 4;
      type AA64MMFR0 is record
         PA_Range    : Unsigned_4;
         ASID_Bits   : Unsigned_4;
         Big_End     : Unsigned_4;
         SNS_Mem     : Unsigned_4;
         Big_End_EL0 : Unsigned_4;
         TGran_16    : Unsigned_4;
         TGran_64    : Unsigned_4;
         TGran_4     : Unsigned_4;
         TGran16_2   : Unsigned_4;
         TGran64_2   : Unsigned_4;
         TGran4_2    : Unsigned_4;
         ExS         : Unsigned_4;
         Reserved_1  : Unsigned_4;
         Reserved_2  : Unsigned_4;
         FGT         : Unsigned_4;
         ECV         : Unsigned_4;
      end record;
      for AA64MMFR0 use record
         PA_Range    at 0 range 0 .. 3;
         ASID_Bits   at 0 range 4 .. 7;
         Big_End     at 1 range 0 .. 3;
         SNS_Mem     at 1 range 4 .. 7;
         Big_End_EL0 at 2 range 0 .. 3;
         TGran_16    at 2 range 4 .. 7;
         TGran_64    at 3 range 0 .. 3;
         TGran_4     at 3 range 4 .. 7;
         TGran16_2   at 4 range 0 .. 3;
         TGran64_2   at 4 range 4 .. 7;
         TGran4_2    at 5 range 0 .. 3;
         ExS         at 5 range 4 .. 7;
         Reserved_1  at 6 range 0 .. 3;
         Reserved_2  at 6 range 4 .. 7;
         FGT         at 7 range 0 .. 3;
         ECV         at 7 range 4 .. 7;
      end record;
      for AA64MMFR0'Size use 64;

      type MAIR_EL1 is record
         Attr0 : Unsigned_8;
         Attr1 : Unsigned_8;
         Attr2 : Unsigned_8;
         Attr3 : Unsigned_8;
         Attr4 : Unsigned_8;
         Attr5 : Unsigned_8;
         Attr6 : Unsigned_8;
         Attr7 : Unsigned_8;
      end record;
      for MAIR_EL1 use record
         Attr0 at 0 range 0 .. 7;
         Attr1 at 1 range 0 .. 7;
         Attr2 at 2 range 0 .. 7;
         Attr3 at 3 range 0 .. 7;
         Attr4 at 4 range 0 .. 7;
         Attr5 at 5 range 0 .. 7;
         Attr6 at 6 range 0 .. 7;
         Attr7 at 7 range 0 .. 7;
      end record;
      for MAIR_EL1'Size use 64;

      AA64MMFR : AA64MMFR0;
      MAIR : constant MAIR_EL1 := (
         Attr0 => 2#11111111#, --  Normal, Write-back RW-Allocate non-transient
         Attr1 => 2#00000000#, --  Device, nGnRnE
         Attr2 => 2#00000000#, --  Ditto
         Attr3 => 2#00000000#, --  Ditto
         Attr4 => 2#00000000#, --  Ditto
         Attr5 => 2#00000000#, --  Ditto
         Attr6 => 2#00000000#, --  Ditto
         Attr7 => 2#00000000#  --  Ditto
      );
   begin
      --  Check for the page size, default to biggest.
      Asm ("mrs %0, id_aa64mmfr0_el1",
           Outputs  => AA64MMFR0'Asm_Output ("=r", AA64MMFR),
           Clobber  => "memory",
           Volatile => True);

      if AA64MMFR.TGran_4 = 0 then
         Used_Size := Pages_4K;
      elsif AA64MMFR.TGran_64 = 0 then
         Used_Size := Pages_64K;
      elsif AA64MMFR.TGran16_2 /= 1 and AA64MMFR.TGran16_2 /= 0 then
         Used_Size := Pages_16K;
      else
         Lib.Panic.Soft_Panic ("MMU page size could not be found");
         return False;
      end if;

      --  Set MMU state.
      Asm ("msr mair_el1, %0",
           Inputs   => MAIR_EL1'Asm_Input ("r", MAIR),
           Clobber  => "memory",
           Volatile => True);

      --  Create the kernel map.
      Kernel_Map   := new Page_Map;
      Kernel_Table := Page_Table (Kernel_Map.all'Address);

      return False;
   end Init;

   function Create_Table return Page_Table is
      Map : constant Page_Map_Acc := new Page_Map;
   begin
      Map.TTBR1 := Kernel_Map.TTBR1;
      return Page_Table (Conv.To_Address (Conv.Object_Pointer (Map)));
   end Create_Table;

   procedure Destroy_Table (Map : in out Page_Table) is
      pragma Unreferenced (Map);
   begin
      null;
   end Destroy_Table;

   function Make_Active (Map : Page_Table) return Boolean is
      Table : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Asm ("msr ttbr0_el1, %0; msr ttbr1_el1, %1; dsb st; isb sy",
           Inputs   => (System.Address'Asm_Input ("r", Table.TTBR0'Address),
                        System.Address'Asm_Input ("r", Table.TTBR1'Address)),
           Clobber  => "memory",
           Volatile => True);
      return True;
   end Make_Active;

   function Is_Active (Map : Page_Table) return Boolean is
      Table : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));

      TTBR0, TTBR1 : System.Address;
   begin
      Asm ("mrs %0, ttbr0_el1; mrs %1, ttbr1_el1",
           Outputs  => (System.Address'Asm_Output ("=r", TTBR0),
                        System.Address'Asm_Output ("=r", TTBR1)),
           Clobber  => "memory",
           Volatile => True);
      return Table.TTBR0'Address = TTBR0 and Table.TTBR1'Address = TTBR1;
   end Is_Active;

   --  Do translation for a single address, this function does not fail.
   function Translate_Address
      (Map     : Page_Table;
       Virtual : System.Address) return System.Address
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual);
   begin
      return System.Null_Address;
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Physical_Start);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
   begin
      return False;
   end Map_Range;

   function Remap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
   begin
      return False;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
   begin
      return False;
   end Unmap_Range;

   procedure Flush_Local_TLB (Addr : System.Address) is
      pragma Unreferenced (Addr);
   begin
      return;
   end Flush_Local_TLB;

   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count) is
      pragma Unreferenced (Addr);
      pragma Unreferenced (Len);
   begin
      return;
   end Flush_Local_TLB;

   procedure Flush_Global_TLBs (Addr : System.Address) is
      pragma Unreferenced (Addr);
   begin
      return;
   end Flush_Global_TLBs;

   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count) is
      pragma Unreferenced (Addr);
      pragma Unreferenced (Len);
   begin
      return;
   end Flush_Global_TLBs;
end Arch.MMU;
