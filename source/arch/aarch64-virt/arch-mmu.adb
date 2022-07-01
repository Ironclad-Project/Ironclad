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

with System.Machine_Code; use System.Machine_Code;

package body Arch.MMU is
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
         FGT         at 6 range 0 .. 3;
         ECV         at 6 range 4 .. 7;
      end record;
      for AA64MMFR0'Size use 64;

      Value : AA64MMFR0;
   begin
      --  Check capabilities.
      Asm ("mrs %0, id_aa64mmfr0_el1",
           Outputs  => AA64MMFR0'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);

      if Value.TGran_4 /= 0 then
         return False;
      end if;

      return True;
   end Init;

   function Create_Table return Page_Table is
   begin
      return Page_Table (System.Null_Address);
   end Create_Table;

   function Destroy_Table return Boolean is
   begin
      return False;
   end Destroy_Table;

   function Make_Active (Map : Page_Table) return Boolean is
      pragma Unreferenced (Map);
   begin
      return False;
   end Make_Active;

   function Is_Active (Map : Page_Table) return Boolean is
      pragma Unreferenced (Map);
   begin
      return False;
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
