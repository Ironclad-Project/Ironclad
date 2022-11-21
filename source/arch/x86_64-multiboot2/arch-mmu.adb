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

with Interfaces.C;
with Ada.Unchecked_Deallocation;
with Arch.Wrappers;
with Memory; use Memory;
with Memory.Physical;

package body Arch.MMU with SPARK_Mode => Off is
   type Address_Components is record
      PML4_Entry : Unsigned_64;
      PML3_Entry : Unsigned_64;
      PML2_Entry : Unsigned_64;
      PML1_Entry : Unsigned_64;
   end record;

   function Get_Address_Components
      (Virtual : Virtual_Address) return Address_Components
   is
      Addr   : constant Unsigned_64 := Unsigned_64 (Virtual);
      PML4_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 39);
      PML3_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 30);
      PML2_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 21);
      PML1_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 12);
   begin
      return (PML4_Entry => Shift_Right (PML4_E, 39),
              PML3_Entry => Shift_Right (PML3_E, 30),
              PML2_Entry => Shift_Right (PML2_E, 21),
              PML1_Entry => Shift_Right (PML1_E, 12));
   end Get_Address_Components;

   function Clean_Entry (Entry_Body : Unsigned_64) return Physical_Address is
   begin
      return Physical_Address (Entry_Body and 16#FFFFFFF000#);
   end Clean_Entry;

   function Get_Next_Level
      (Current_Level       : Physical_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean) return Physical_Address
   is
      Entry_Addr : constant Virtual_Address :=
         Current_Level + Memory_Offset + Physical_Address (Index * 8);
      Entry_Body : Unsigned_64 with Address => To_Address (Entry_Addr), Import;
   begin
      --  Check whether the entry is present.
      if (Entry_Body and 1) /= 0 then
         return Clean_Entry (Entry_Body);
      elsif Create_If_Not_Found then
         --  Allocate and put some default flags.
         declare
            New_Entry      : constant PML4_Acc := new PML4;
            New_Entry_Addr : constant Physical_Address :=
               To_Integer (New_Entry.all'Address) - Memory_Offset;
         begin
            Entry_Body := Unsigned_64 (New_Entry_Addr) or 2#111#;
            return New_Entry_Addr;
         end;
      end if;
      return Null_Address;
   end Get_Next_Level;

   function Get_Page
      (Map               : Page_Table_Acc;
       Virtual           : Virtual_Address;
       Allocate, Is_2MiB : Boolean) return Virtual_Address
   is
      Addr  : constant Address_Components := Get_Address_Components (Virtual);
      Addr4 : constant Physical_Address :=
         To_Integer (Map.PML4_Level'Address) - Memory_Offset;
      Addr3, Addr2, Addr1 : Physical_Address := Null_Address;
   begin
      --  Find the entries.
      Addr3 := Get_Next_Level (Addr4, Addr.PML4_Entry, Allocate);
      if Addr3 = Null_Address then
         goto Error_Return;
      end if;
      Addr2 := Get_Next_Level (Addr3, Addr.PML3_Entry, Allocate);
      if Addr2 = Null_Address then
         goto Error_Return;
      end if;

      --  Either stop here or continue for 4KiB pages.
      if Is_2MiB then
         return Addr2 + Memory_Offset +
                (Physical_Address (Addr.PML2_Entry) * 8);
      else
         Addr1 := Get_Next_Level (Addr2, Addr.PML2_Entry, Allocate);
         if Addr1 = Null_Address then
            goto Error_Return;
         end if;
         return Addr1 + Memory_Offset +
                (Physical_Address (Addr.PML1_Entry) * 8);
      end if;

   <<Error_Return>>
      return Null_Address;
   end Get_Page;

   function Flags_To_Bitmap (Perm : Page_Permissions) return Unsigned_16 is
      RW  : constant Unsigned_16 := (if not Perm.Read_Only  then 1 else 0);
      U   : constant Unsigned_16 := (if Perm.User_Accesible then 1 else 0);
      PWT : constant Unsigned_16 := (if Perm.Write_Through  then 1 else 0);
      G   : constant Unsigned_16 := (if Perm.Global         then 1 else 0);
   begin
      return Shift_Left (G,   8) or
             Shift_Left (PWT, 7) or --  PAT.
             Shift_Left (PWT, 3) or --  Cache disable.
             Shift_Left (U,   2) or
             Shift_Left (RW,  1) or
             1;                     --  Present bit.
   end Flags_To_Bitmap;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      NX_Flags : constant Page_Permissions := (
         User_Accesible => False,
         Read_Only      => False,
         Executable     => False,
         Global         => True,
         Write_Through  => False
      );
      X_Flags : constant Page_Permissions := (
         User_Accesible => NX_Flags.User_Accesible,
         Read_Only      => NX_Flags.Read_Only,
         Executable     => not NX_Flags.Executable,
         Global         => NX_Flags.Global,
         Write_Through  => NX_Flags.Write_Through
      );
      Hardcoded_Region : constant := 16#100000000#;
   begin
      --  Initialize the kernel pagemap.
      MMU.Kernel_Table := new Page_Table;

      --  Map the first 4 GiB (except 0) to the window and identity mapped.
      --  This is done instead of following the pagemap to ensure that all
      --  I/O and memory tables that may not be in the memmap are mapped.
      if not Map_Range (
         Map            => MMU.Kernel_Table,
         Physical_Start => To_Address (Page_Size_4K),
         Virtual_Start  => To_Address (Page_Size_4K),
         Length         => Hardcoded_Region - Page_Size_4K,
         Permissions    => NX_Flags
      )
      or not Map_Range (
         Map            => MMU.Kernel_Table,
         Physical_Start => To_Address (Page_Size_4K),
         Virtual_Start  => To_Address (Page_Size_4K + Memory_Offset),
         Length         => Hardcoded_Region - Page_Size_4K,
         Permissions    => NX_Flags
      )
      then
         return False;
      end if;

      --  Map the memmap memory to the memory window and identity
      for E of Memmap loop
         if not Map_Range (
            Map            => MMU.Kernel_Table,
            Physical_Start => To_Address (To_Integer (E.Start)),
            Virtual_Start => To_Address (To_Integer (E.Start) + Memory_Offset),
            Length         => Storage_Offset (E.Length),
            Permissions    => NX_Flags
         )
         then
            return False;
         end if;
      end loop;

      --  Map 128MiB of kernel.
      return Map_Range (
         Map            => MMU.Kernel_Table,
         Physical_Start => To_Address (16#200000#),
         Virtual_Start  => To_Address (Kernel_Offset),
         Length         => 16#7000000#,
         Permissions    => X_Flags
      );
   end Init;

   function Create_Table return Page_Table_Acc is
      Map : constant Page_Table_Acc := new Page_Table;
   begin
      Map.PML4_Level (257 .. 512) := MMU.Kernel_Table.PML4_Level (257 .. 512);
      return Map;
   end Create_Table;

   procedure Destroy_Level (Entry_Body : Unsigned_64; Level : Integer) is
      Addr : constant Integer_Address := Clean_Entry (Entry_Body);
      PML  : PML4 with Import, Address => To_Address (Memory_Offset + Addr);
   begin
      if (Entry_Body and 1) /= 0 then
         if Level > 1 then
            for E of PML loop
               if (E and 1) /= 0 then
                  Destroy_Level (E, Level - 1);
               end if;
            end loop;
         end if;
         Memory.Physical.Free (Interfaces.C.size_t (Addr));
      end if;
   end Destroy_Level;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
      procedure F is new Ada.Unchecked_Deallocation
         (Page_Table, Page_Table_Acc);
   begin
      if Map /= null then
         for I in 1 .. 256 loop
            Destroy_Level (Map.PML4_Level (I), 3);
         end loop;
         F (Map);
         Map := null;
      end if;
   end Destroy_Table;

   function Make_Active (Map : Page_Table_Acc) return Boolean is
      Val : Unsigned_64;
   begin
      if Map = null then
         return False;
      end if;

      Val := Unsigned_64 (To_Integer (Map.PML4_Level'Address) - Memory_Offset);
      if Arch.Wrappers.Read_CR3 /= Val then
         Arch.Wrappers.Write_CR3 (Val);
      end if;
      return True;
   end Make_Active;

   function Is_Active (Map : Page_Table_Acc) return Boolean is
      Current : constant Unsigned_64 := Arch.Wrappers.Read_CR3;
      PAddr : constant Integer_Address := To_Integer (Map.PML4_Level'Address);
   begin
      return Current = Unsigned_64 (PAddr - Memory_Offset);
   end Is_Active;

   function Translate_Address
      (Map     : Page_Table_Acc;
       Virtual : System.Address) return System.Address
   is
      Addr  : constant Integer_Address := To_Integer (Virtual);
      Addr1 : constant Virtual_Address := Get_Page (Map, Addr, False, True);
      Addr2 : constant Virtual_Address := Get_Page (Map, Addr, False, False);
      Searched1 : Unsigned_64 with Address => To_Address (Addr1), Import;
      Searched2 : Unsigned_64 with Address => To_Address (Addr2), Import;
   begin
      if Addr1 /= Null_Address and then (Shift_Right (Searched1, 7) and 1) /= 0
      then
         return To_Address (Clean_Entry (Searched1));
      elsif Addr2 /= Null_Address then
         return To_Address (Clean_Entry (Searched2));
      else
         return System.Null_Address;
      end if;
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
   is
      Flags : constant Unsigned_16 := Flags_To_Bitmap (Permissions);
      NX    : constant Unsigned_64 := Boolean'Pos (not Permissions.Executable);
      PWT   : constant Unsigned_64 := Boolean'Pos (Permissions.Write_Through);
      Mask  : Unsigned_64;

      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Phys  : Virtual_Address          := To_Integer (Physical_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr, Size : Virtual_Address;
   begin
      if Map = null                   or (Phys   mod Page_Size_4K /= 0) or
         (Virt mod Page_Size_4K /= 0) or (Length mod Page_Size_4K /= 0)
      then
         return False;
      end if;

      while Virt < Final loop
         if (Virt mod Page_Size_2M /= 0) or (Phys mod Page_Size_2M /= 0) then
            Size := Page_Size_4K;
            Addr := Get_Page (Map, Virt, True, False);
            Mask := Unsigned_64 (Flags) or Shift_Left (NX, 63);
         else
            Size := Page_Size_2M;
            Addr := Get_Page (Map, Virt, True, True);
            Mask := Unsigned_64 (Flags)  or Shift_Left (NX, 63) or
                    Shift_Left (PWT, 12) or Shift_Left (1, 7);
         end if;

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Entry_Body := Unsigned_64 (Phys) or Mask;
         end;
         Virt := Virt + Size;
         Phys := Phys + Size;
      end loop;

      return True;
   end Map_Range;

   function Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
   is
      Flags : constant Unsigned_16 := Flags_To_Bitmap (Permissions);
      NX    : constant Unsigned_64 := Boolean'Pos (not Permissions.Executable);
      PWT   : constant Unsigned_64 := Boolean'Pos (Permissions.Write_Through);
      Mask  : Unsigned_64;

      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr, Size : Virtual_Address;
   begin
      if Map = null or (Virt mod Page_Size_4K /= 0) or
         (Length mod Page_Size_4K /= 0)
      then
         return False;
      end if;

      while Virt < Final loop
         if Virt mod Page_Size_2M /= 0 then
            Size := Page_Size_4K;
            Addr := Get_Page (Map, Virt, False, False);
            Mask := Unsigned_64 (Flags) or Shift_Left (NX, 63);
         else
            Size := Page_Size_2M;
            Addr := Get_Page (Map, Virt, False, True);
            Mask := Unsigned_64 (Flags)  or Shift_Left (NX, 63) or
                    Shift_Left (PWT, 12) or Shift_Left (1, 7);
         end if;

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Entry_Body := Unsigned_64 (Clean_Entry (Entry_Body)) or Mask;
            end if;
         end;
         Virt := Virt + Size;
      end loop;

      if Is_Active (Map) then
         Flush_Local_TLB (Virtual_Start, Length);
      end if;

      return True;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr, Size : Virtual_Address;
   begin
      if Map = null or (Virt mod Page_Size_4K /= 0) or
         (Length mod Page_Size_4K /= 0)
      then
         return False;
      end if;

      while Virt < Final loop
         if Virt mod Page_Size_2M /= 0 then
            Size := Page_Size_4K;
            Addr := Get_Page (Map, Virt, False, False);
         else
            Size := Page_Size_2M;
            Addr := Get_Page (Map, Virt, False, True);
         end if;

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Entry_Body := Entry_Body and 0;
            end if;
         end;
         Virt := Virt + Size;
      end loop;

      if Is_Active (Map) then
         Flush_Local_TLB (Virtual_Start, Length);
      end if;

      return True;
   end Unmap_Range;

   procedure Flush_Local_TLB (Addr : System.Address) is
   begin
      Wrappers.Invalidate_Page (To_Integer (Addr));
   end Flush_Local_TLB;

   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count) is
      Curr : Storage_Count := 0;
   begin
      while Curr < Len loop
         Wrappers.Invalidate_Page (To_Integer (Addr + Curr));
         Curr := Curr + Page_Size_4K;
      end loop;
   end Flush_Local_TLB;

   --  TODO: Code this 2 bad boys once the VMM makes use of them.

   procedure Flush_Global_TLBs (Addr : System.Address) is
   begin
      null;
   end Flush_Global_TLBs;

   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count) is
   begin
      null;
   end Flush_Global_TLBs;
end Arch.MMU;
