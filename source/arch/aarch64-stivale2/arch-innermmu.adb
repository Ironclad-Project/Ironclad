--  arch-innermmu.adb: Architecture-specific MMU code.
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
with Arch.MMU;
with System.Machine_Code; use System.Machine_Code;
with Lib.Panic;
with Memory; use Memory;

package body Arch.InnerMMU with SPARK_Mode => Off is
   --  Page attributes.
   Page_RO      : constant Unsigned_64 := Shift_Left (1, 7);
   Page_PXN     : constant Unsigned_64 := Shift_Left (1, 53);
   Page_UXN     : constant Unsigned_64 := Shift_Left (1, 54);
   Page_nG      : constant Unsigned_64 := Shift_Left (1, 11);
   Page_nGnRnE  : constant Unsigned_64 := Shift_Left (2, 2);
   Page_OuterSh : constant Unsigned_64 := Shift_Left (2, 8);
   Page_WB      : constant Unsigned_64 := Shift_Left (0, 2);
   Page_InnerSh : constant Unsigned_64 := Shift_Left (3, 8);
   Page_Access  : constant Unsigned_64 := Shift_Left (1, 10);
   Page_Valid   : constant Unsigned_64 := Shift_Left (1, 0);
   Page_L3      : constant Unsigned_64 := Shift_Left (1, 1);

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
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
         Attr1 => 2#00001100#, --  Device, GRE
         Attr2 => 2#00000000#, --  Device, nGnRnE
         Attr3 => 2#00000100#, --  Device, nGnRE
         Attr4 => 2#01000100#, --  Normal Non-cachable
         Attr5 => 2#00000000#, --  Device, nGnRnE
         Attr6 => 2#00000000#, --  Device, nGnRnE
         Attr7 => 2#00000000#  --  Device, nGnRnE
      );

      Flags : constant Page_Permissions := (
         User_Accesible => False,
         Read_Only      => False,
         Executable     => True,
         Global         => True,
         Write_Through  => False
      );

      PA_Range, TCR : Unsigned_64;
      Success : Boolean;
   begin
      --  Check for the page size, default to biggest.
      Asm ("mrs %0, id_aa64mmfr0_el1",
           Outputs  => AA64MMFR0'Asm_Output ("=r", AA64MMFR),
           Clobber  => "memory",
           Volatile => True);

      if AA64MMFR.TGran_4 /= 0 then
         Lib.Panic.Soft_Panic ("MMU does not support 4K pages");
         return False;
      end if;

      --  Set MMU state.
      Asm ("msr mair_el1, %0",
           Inputs   => MAIR_EL1'Asm_Input ("r", MAIR),
           Clobber  => "memory",
           Volatile => True);

      PA_Range := Unsigned_64
         ((if AA64MMFR.PA_Range < 5 then AA64MMFR.PA_Range else 5));
      TCR := Shift_Left (16, 0) or Shift_Left (16, 16) or Shift_Left (1, 8)  or
             Shift_Left (1, 10) or Shift_Left (1, 24)  or Shift_Left (1, 26) or
             Shift_Left (2, 12) or Shift_Left (2, 28)  or Shift_Left (2, 30) or
             Shift_Left (PA_Range, 32);
      Asm ("msr tcr_el1, %0",
           Inputs   => Unsigned_64'Asm_Input ("r", TCR),
           Clobber  => "memory",
           Volatile => True);

      --  Create the kernel map.
      MMU.Kernel_Table := new Page_Map;

      --  Search for kernel entries and map them.
      for E of Memmap loop
         if E.MemType = Arch.Memory_Kernel then
            Success := Map_Range (
               Map            => MMU.Kernel_Table,
               Physical_Start => E.Start,
               Virtual_Start  => To_Address (Kernel_Offset + 16#100000#),
               Length         => E.Length,
               Permissions    => Flags
            );
            if not Success then
               return False;
            end if;
            exit;
         end if;
      end loop;

      --  Map into the memory window.
      Success := Map_Range (
         Map            => MMU.Kernel_Table,
         Physical_Start => To_Address (0),
         Virtual_Start  => To_Address (Memory.Memory_Offset),
         Length         => 16#9001000#,
         Permissions    => Flags
      );
      if not Success then
         return False;
      end if;

      return True;
   end Init;

   function Create_Table return Page_Map_Acc is
      Map : constant Page_Map_Acc := new Page_Map;
   begin
      Map.TTBR1 := MMU.Kernel_Table.TTBR1;
      return Map;
   end Create_Table;

   procedure Destroy_Table (Map : in out Page_Map_Acc) is
      procedure F is new Ada.Unchecked_Deallocation (Page_Map, Page_Map_Acc);
   begin
      --  TODO: Free the tables themselves.
      F (Map);
      Map := null;
   end Destroy_Table;

   function Make_Active (Map : Page_Map_Acc) return Boolean is
      Addr0 : constant Integer_Address :=
         To_Integer (Map.TTBR0'Address) - Memory.Memory_Offset;
      Addr1 : constant Integer_Address :=
         To_Integer (Map.TTBR1'Address) - Memory.Memory_Offset;
   begin
      if not Is_Active (Map) then
         Asm ("msr ttbr0_el1, %0; msr ttbr1_el1, %1; dsb nsh",
              Inputs   => (Integer_Address'Asm_Input ("r", Addr0),
                           Integer_Address'Asm_Input ("r", Addr1)),
              Clobber  => "memory",
              Volatile => True);
      end if;
      return True;
   end Make_Active;

   function Is_Active (Map : Page_Map_Acc) return Boolean is
      TTBR0, TTBR1 : Integer_Address;
   begin
      Asm ("mrs %0, ttbr0_el1; mrs %1, ttbr1_el1",
           Outputs  => (Integer_Address'Asm_Output ("=r", TTBR0),
                        Integer_Address'Asm_Output ("=r", TTBR1)),
           Clobber  => "memory",
           Volatile => True);

      TTBR0 := TTBR0 + Memory.Memory_Offset;
      TTBR1 := TTBR1 + Memory.Memory_Offset;

      return Map.TTBR0'Address = To_Address (TTBR0) and
             Map.TTBR1'Address = To_Address (TTBR1);
   end Is_Active;

   function Get_Bits (Permissions : Page_Permissions) return Unsigned_64 is
      Bits : Unsigned_64 := 0;
   begin
      --  Handle permissions.
      if Permissions.Read_Only then
         Bits := Bits or Page_RO;
      end if;
      if not Permissions.Executable then
         Bits := Bits or Page_PXN or Page_UXN;
      end if;
      if not Permissions.Global then
         Bits := Bits or Page_nG;
      end if;

      --  Caching.
      if Permissions.Write_Through then
         Bits := Bits or Page_nGnRnE or Page_OuterSh;
      else
         Bits := Bits or Page_WB or Page_InnerSh;
      end if;
      return Bits or Page_Access or Page_Valid or Page_L3;
   end Get_Bits;

   function Get_Addr_From_Entry (Entry_B : Unsigned_64) return Unsigned_64 is
   begin
      return Entry_B and 16#FFFFFFFFF000#;
   end Get_Addr_From_Entry;

   function Get_Components (Add : Integer_Address) return Address_Components is
      Addr : constant Unsigned_64 := Unsigned_64 (Add);
      L0_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 39);
      L1_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 30);
      L2_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 21);
      L3_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 12);
   begin
      return (Level0_Entry => Shift_Right (L0_E, 39),
              Level1_Entry => Shift_Right (L1_E, 30),
              Level2_Entry => Shift_Right (L2_E, 21),
              Level3_Entry => Shift_Right (L3_E, 12));
   end Get_Components;

   function Get_Next_Level
      (Current_Level       : Integer_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean) return Integer_Address
   is
      Entry_Addr : constant Integer_Address :=
         Current_Level + Memory.Memory_Offset + Integer_Address (Index * 8);
      Entry_Body : Unsigned_64 with Address => To_Address (Entry_Addr), Import;
   begin
      --  Check whether the entry is present.
      if (Entry_Body and Page_Valid) /= 0 then
         return Integer_Address (Get_Addr_From_Entry (Entry_Body));
      elsif Create_If_Not_Found then
         --  Allocate and put some default flags.
         declare
            New_Entry : constant Page_Level_Acc := new Page_Level;
            New_Addr  : constant Integer_Address :=
               To_Integer (New_Entry.all'Address) - Memory.Memory_Offset;
         begin
            Entry_Body := Unsigned_64 (New_Addr) or Page_Valid or Page_L3;
            return New_Addr;
         end;
      else
         return 0;
      end if;
   end Get_Next_Level;

   function Get_Page
      (Root_Lvl : System.Address;
       Virtual  : Integer_Address;
       Allocate : Boolean) return Integer_Address
   is
      Addr  : constant Address_Components := Get_Components (Virtual);
      Addr4 : constant Integer_Address :=
         To_Integer (Root_Lvl) - Memory.Memory_Offset;
      Addr3, Addr2, Addr1 : Integer_Address := 0;
   begin
      --  Find the entries.
      Addr3 := Get_Next_Level (Addr4, Addr.Level0_Entry, Allocate);
      if Addr3 = 0 then
         goto Error_Return;
      end if;
      Addr2 := Get_Next_Level (Addr3, Addr.Level1_Entry, Allocate);
      if Addr2 = 0 then
         goto Error_Return;
      end if;
      Addr1 := Get_Next_Level (Addr2, Addr.Level2_Entry, Allocate);
      if Addr1 = 0 then
         goto Error_Return;
      end if;

      return Addr1 + Memory.Memory_Offset +
             (Integer_Address (Addr.Level3_Entry) * 8);

   <<Error_Return>>
      Lib.Panic.Soft_Panic ("Address could not be found");
      return 0;
   end Get_Page;

   function Translate_Address
      (Map     : Page_Map_Acc;
       Virtual : System.Address) return System.Address
   is
      Virt : Integer_Address := To_Integer (Virtual);
      TTBR : System.Address  := System.Null_Address;
   begin
      if (Virt and Higher_Half) /= 0 then
         Virt       := Virt and not Higher_Half;
         TTBR       := Map.TTBR1'Address;
      else
         TTBR := Map.TTBR0'Address;
      end if;

      declare
         Addr : constant Integer_Address := Get_Page (TTBR, Virt, True);
         Entry_B : Unsigned_64 with Address => To_Address (Addr), Import;
      begin
         return To_Address (Integer_Address (Get_Addr_From_Entry (Entry_B)));
      end;
   end Translate_Address;

   function Map_Range
      (Map            : Page_Map_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
   is
      Virt       : Integer_Address := To_Integer (Virtual_Start);
      Phys       : Integer_Address := To_Integer (Physical_Start);
      Final_Virt : Integer_Address := Virt + Integer_Address (Length);
      TTBR       : System.Address  := System.Null_Address;
      Perm_Mask : constant Unsigned_64 := Get_Bits (Permissions);
   begin
      if Virt   mod Page_Size /= 0 or Phys mod Page_Size /= 0 or
         Length mod Page_Size /= 0
      then
         return False;
      end if;

      if (Virt and Higher_Half) /= 0 then
         Virt       := Virt       and not Higher_Half;
         Final_Virt := Final_Virt and not Higher_Half;
         TTBR       := Map.TTBR1'Address;
      else
         TTBR := Map.TTBR0'Address;
      end if;

      while Virt < Final_Virt loop
         declare
            Addr : constant Integer_Address := Get_Page (TTBR, Virt, True);
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Entry_Body := Unsigned_64 (Phys) or Perm_Mask;
         end;
         Virt := Virt + Page_Size;
         Phys := Phys + Page_Size;
      end loop;

      return True;
   end Map_Range;

   function Remap_Range
      (Map           : Page_Map_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
   is
      Virt       : Integer_Address := To_Integer (Virtual_Start);
      Final_Virt : Integer_Address := Virt + Integer_Address (Length);
      TTBR       : System.Address  := System.Null_Address;
      Perm_Mask : constant Unsigned_64 := Get_Bits (Permissions);
   begin
      if Virt mod Page_Size /= 0 or Length mod Page_Size /= 0 then
         return False;
      end if;

      if (Virt and Higher_Half) /= 0 then
         Virt       := Virt       and not Higher_Half;
         Final_Virt := Final_Virt and not Higher_Half;
         TTBR       := Map.TTBR1'Address;
      else
         TTBR := Map.TTBR0'Address;
      end if;

      while Virt < Final_Virt loop
         declare
            Addr : constant Integer_Address := Get_Page (TTBR, Virt, False);
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Entry_Body := Get_Addr_From_Entry (Entry_Body) or Perm_Mask;
            Flush_Local_TLB (To_Address (Virt));
         end;
         Virt := Virt + Page_Size;
      end loop;

      return True;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Map_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      Virt       : Integer_Address := To_Integer (Virtual_Start);
      Final_Virt : Integer_Address := Virt + Integer_Address (Length);
      TTBR       : System.Address  := System.Null_Address;
   begin
      if Virt mod Page_Size /= 0 or Length mod Page_Size /= 0 then
         return False;
      end if;

      if (Virt and Higher_Half) /= 0 then
         Virt       := Virt       and not Higher_Half;
         Final_Virt := Final_Virt and not Higher_Half;
         TTBR       := Map.TTBR1'Address;
      else
         TTBR := Map.TTBR0'Address;
      end if;

      while Virt < Final_Virt loop
         declare
            Addr : constant Integer_Address := Get_Page (TTBR, Virt, False);
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Entry_Body := Entry_Body xor Page_Valid;
            Flush_Local_TLB (To_Address (Virt));
         end;
         Virt := Virt + Page_Size;
      end loop;

      return True;
   end Unmap_Range;

   procedure Flush_Local_TLB (Addr : System.Address) is
      Ad : constant Unsigned_64 := Unsigned_64 (To_Integer (Addr));
   begin
      Asm ("dsb st; tlbi vale1, %0; dsb sy; isb",
           Inputs   => Unsigned_64'Asm_Input ("r", Shift_Right (Ad, 12)),
           Clobber  => "memory",
           Volatile => True);
   end Flush_Local_TLB;

   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count) is
      Curr : Storage_Count := 0;
   begin
      while Curr < Len loop
         Flush_Local_TLB (Addr + Curr);
         Curr := Curr + Page_Size;
      end loop;
   end Flush_Local_TLB;

   --  TODO: Code this 2 bad boys once the VMM makes use of them.

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
end Arch.InnerMMU;
