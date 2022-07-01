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

with System.Address_To_Access_Conversions;
with Interfaces; use Interfaces;
with Arch.Wrappers;
with Arch.Stivale2;
with Memory; use Memory;
with Lib.Panic;

package body Arch.MMU is
   --  Page table entries consist of an address padded to 4K, which frees
   --  the lower 12 bits for flags, those flags are described below.

   --  Page table flags, shared by all entries except the final page ones.
   type Page_Table_Flags is record
      Present         : Boolean; --  Whether the entry is present.
      Read_Write      : Boolean; --  True for R/W, false for R.
      User_Supervisor : Boolean; --  Whether the user can access, or only root.
      Write_Through   : Boolean; --  Whether write through is enabled.
      Cache_Disable   : Boolean; --  Whether caching is disabled.
      Accessed        : Boolean; --  Whether a PDE or PTE was read.
      Dirty           : Boolean; --  Whether the page has been written to.
      Table_End       : Boolean; --  Whether its a big page or it continues on.
   end record;
   for Page_Table_Flags use record
      Present         at 0 range 0 .. 0;
      Read_Write      at 0 range 1 .. 1;
      User_Supervisor at 0 range 2 .. 2;
      Write_Through   at 0 range 3 .. 3;
      Cache_Disable   at 0 range 4 .. 4;
      Accessed        at 0 range 5 .. 5;
      Dirty           at 0 range 6 .. 6;
      Table_End       at 0 range 7 .. 7;
   end record;
   for Page_Table_Flags'Size use 8;

   --  Flags of a PML1 page.
   type Page_Flags is record
      Present         : Boolean; --  Whether the entry is present.
      Read_Write      : Boolean; --  True for R/W, false for R.
      User_Supervisor : Boolean; --  Whether the user can access, or only root.
      Write_Through   : Boolean; --  Whether write through is enabled.
      Cache_Disable   : Boolean; --  Whether caching is disabled.
      Accessed        : Boolean; --  Whether a PDE or PTE was read.
      Dirty           : Boolean; --  Whether the page has been written to.
      PAT             : Boolean; --  PAT.
      Global          : Boolean; --  Whether the page TLB should be flushed.
   end record;
   for Page_Flags use record
      Present         at 0 range 0 .. 0;
      Read_Write      at 0 range 1 .. 1;
      User_Supervisor at 0 range 2 .. 2;
      Write_Through   at 0 range 3 .. 3;
      Cache_Disable   at 0 range 4 .. 4;
      Accessed        at 0 range 5 .. 5;
      Dirty           at 0 range 6 .. 6;
      PAT             at 0 range 7 .. 7;
      Global          at 0 range 8 .. 8;
   end record;
   for Page_Flags'Size use 9;

   --  Entries of the PML4, 3, and 2.
   --  This bad boys always exist in arrays of 512.
   --  Each entry points to a 512-long entry list of the lower level, or some
   --  might stop to be a big page.
   type Page_Table_Entry is record
      Flags    : Page_Table_Flags;
      Ignored  : Boolean;
      Addr     : Physical_Address;
      NX       : Boolean;
   end record;
   for Page_Table_Entry use record
      Flags    at 0 range  0 ..  7;
      Ignored  at 0 range  8 .. 11;
      Addr     at 0 range 12 .. 62;
      NX       at 0 range 63 .. 63;
   end record;
   for Page_Table_Entry'Size use 64;

   --  Each entry represents a 4KiB memory block in the final level of the
   --  paging chain.
   type Page is record
      Flags   : Page_Flags;
      Ignored : Boolean;
      Addr    : Physical_Address;
      NX      : Boolean;
   end record;
   for Page use record
      Flags    at 0 range  0 ..  8;
      Ignored  at 0 range  9 .. 11;
      Addr     at 0 range 12 .. 62;
      NX       at 0 range 63 .. 63;
   end record;
   for Page'Size use 64;

   --  Object to represent a page map.
   Page_Size    : constant := 16#1000#;
   Table_Length : constant := 512;
   type PML4 is array (1 .. Table_Length) of Page_Table_Entry
      with Alignment => Page_Size, Size => Table_Length * 64;

   --  Page maps.
   type Page_Map is record
      PML4_Level : PML4;
   end record;
   type Page_Map_Acc is access all Page_Map;

   Kernel_Map : Page_Map_Acc;

   type Address_Components is record
      PML4_Entry : Unsigned_64;
      PML3_Entry : Unsigned_64;
      PML2_Entry : Unsigned_64;
      PML1_Entry : Unsigned_64;
   end record;

   function Get_Address_Components
      (Virtual : Virtual_Address) return Address_Components is
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

   function Chomp_Flags (Address : Physical_Address) return Physical_Address is
      Addr_Int : constant Unsigned_64 := Unsigned_64 (Address);
   begin
      return Physical_Address (Shift_Right (Addr_Int, 12));
   end Chomp_Flags;

   function Add_Flags (Address : Physical_Address) return Physical_Address is
      Addr_Int : constant Unsigned_64 := Unsigned_64 (Address);
   begin
      return Physical_Address (Shift_Left (Addr_Int, 12));
   end Add_Flags;

   function Get_Next_Level
      (Current_Level       : Physical_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean) return Physical_Address is
      Entry_Addr : constant Virtual_Address :=
         Current_Level + Memory_Offset + Physical_Address (Index * 8);
      Entry_Body : Page_Table_Entry with Address => To_Address (Entry_Addr);
   begin
      if Entry_Body.Flags.Present then
         return Add_Flags (Entry_Body.Addr);
      elsif Create_If_Not_Found then
         --  Allocate and put some default flags.
         declare
            type PML4_Acc is access PML4;
            New_Entry      : constant PML4_Acc := new PML4;
            New_Entry_Addr : constant Physical_Address :=
               To_Integer (New_Entry.all'Address) - Memory_Offset;
         begin
            Entry_Body.Addr := Chomp_Flags (New_Entry_Addr);
            Entry_Body.Flags.Present    := True;
            Entry_Body.Flags.Read_Write := True;
            Entry_Body.Flags.User_Supervisor := True;
            return New_Entry_Addr;
         end;
      end if;
      return Null_Address;
   end Get_Next_Level;

   function Get_Page
      (Map      : Page_Map_Acc;
       Virtual  : Virtual_Address;
       Allocate : Boolean) return Virtual_Address is
      Addr  : constant Address_Components := Get_Address_Components (Virtual);
      Addr4 : constant Physical_Address :=
         To_Integer (Map.PML4_Level'Address) - Memory_Offset;
      Addr3 : Physical_Address := Null_Address;
      Addr2 : Physical_Address := Null_Address;
      Addr1 : Physical_Address := Null_Address;
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
      Addr1 := Get_Next_Level (Addr2, Addr.PML2_Entry, Allocate);
      if Addr1 = Null_Address then
         goto Error_Return;
      end if;

      return Addr1 + Memory_Offset + (Physical_Address (Addr.PML1_Entry) * 8);

   <<Error_Return>>
      Lib.Panic.Soft_Panic ("Address could not be found");
      return Null_Address;
   end Get_Page;

   procedure Map_Page
      (Map         : Page_Map_Acc;
       Virtual     : Virtual_Address;
       Physical    : Physical_Address;
       Flags       : Page_Flags;
       Not_Execute : Boolean)
   is
      Page_Address : constant Virtual_Address := Get_Page (Map, Virtual, True);
      Entry_Body   : Page with Address => To_Address (Page_Address);
   begin
      Entry_Body.Addr  := Chomp_Flags (Physical);
      Entry_Body.Flags := Flags;
      Entry_Body.NX    := Not_Execute;
   end Map_Page;

   function Is_Loaded (Map : Page_Map_Acc) return Boolean is
      Current : constant Unsigned_64 := Arch.Wrappers.Read_CR3;
      PAddr : constant Integer_Address := To_Integer (Map.PML4_Level'Address);
   begin
      return Current = Unsigned_64 (PAddr - Memory_Offset);
   end Is_Loaded;

   procedure Unmap_Page (Map : Page_Map_Acc; Virtual : Virtual_Address) is
      Map_Loaded   : constant Boolean         := Is_Loaded (Map);
      Page_Address : constant Virtual_Address := Get_Page (Map, Virtual, True);
      Entry_Body   : Page with Address => To_Address (Page_Address);
   begin
      Entry_Body.Flags.Present := False;
      if Map_Loaded then
         Arch.Wrappers.Invalidate_Page (Virtual);
      end if;
   end Unmap_Page;

   procedure Change_Page_Flags
      (Map         : Page_Map_Acc;
       Virtual     : Virtual_Address;
       Flags       : Page_Flags;
       Not_Execute : Boolean)
   is
      Map_Loaded   : constant Boolean         := Is_Loaded (Map);
      Page_Address : constant Virtual_Address := Get_Page (Map, Virtual, True);
      Entry_Body   : Page with Address => To_Address (Page_Address);
   begin
      Entry_Body.Flags := Flags;
      Entry_Body.NX    := Not_Execute;
      if Map_Loaded then
         Arch.Wrappers.Invalidate_Page (Virtual);
      end if;
   end Change_Page_Flags;

   function Convert_Permissions (Perm : Page_Permissions) return Page_Flags is
   begin
      return (
         Present         => True,
         Read_Write      => not Perm.Read_Only,
         User_Supervisor => Perm.User_Accesible,
         Write_Through   => Perm.Write_Through,
         Cache_Disable   => False,
         Accessed        => False,
         Dirty           => False,
         PAT             => Perm.Write_Through,
         Global          => Perm.Global
      );
   end Convert_Permissions;

   package Conv is new System.Address_To_Access_Conversions (Page_Map);

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      pragma Unreferenced (Memmap);
      package ST renames Stivale2;
      package C1 is new System.Address_To_Access_Conversions (ST.PMR_Tag);
      package C2 is new System.Address_To_Access_Conversions (ST.Memmap_Tag);

      PMRs : constant access ST.PMR_Tag :=
      C1.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.PMR_ID)));
      Memma : constant access ST.Memmap_Tag :=
      C2.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Memmap_ID)));
      Flags : constant Page_Permissions := (
         User_Accesible => False,
         Read_Only      => False,
         Executable     => True,
         Global         => True,
         Write_Through  => False
      );
      Page_Size        : constant := 16#1000#;
      Hardcoded_Region : constant := 16#100000000#;
      Discard          : Boolean;
   begin
      --  Initialize the kernel pagemap.
      Kernel_Map   := new Page_Map;
      Kernel_Table := Page_Table (Kernel_Map.all'Address);

      --  Map PMRs of the kernel.
      --  This will always be mapped, so we can mark them global.
      for E of PMRs.Entries loop
         Discard := Map_Range (
            Map            => Kernel_Table,
            Physical_Start => To_Address (E.Base - Kernel_Offset),
            Virtual_Start  => To_Address (E.Base),
            Length         => Storage_Offset (E.Length),
            Permissions    => (
               False,
               (E.Permissions and Arch.Stivale2.PMR_Writable_Mask)    = 0,
               (E.Permissions and Arch.Stivale2.PMR_Executable_Mask) /= 0,
               True,
               False
            )
         );
      end loop;

      --  Map the first 2 GiB (except 0) to the window and identity mapped.
      --  This is done instead of following the pagemap to ensure that all
      --  I/O and memory tables that may not be in the memmap are mapped.
      Discard := Map_Range (
         Map            => Kernel_Table,
         Physical_Start => To_Address (Page_Size),
         Virtual_Start  => To_Address (Page_Size),
         Length         => Hardcoded_Region - Page_Size,
         Permissions    => Flags
      );
      Discard := Map_Range (
         Map            => Kernel_Table,
         Physical_Start => To_Address (Page_Size),
         Virtual_Start  => To_Address (Page_Size + Memory_Offset),
         Length         => Hardcoded_Region - Page_Size,
         Permissions    => Flags
      );

      --  Map the memmap memory identity mapped and to the memory window.
      for E of Memma.Entries loop
         --  We already mapped the kernel.
         if E.EntryType /= Arch.Stivale2.Memmap_Entry_Kernel_And_Modules then
            Discard := Map_Range (
               Map            => Kernel_Table,
               Physical_Start => To_Address (E.Base),
               Virtual_Start  => To_Address (E.Base),
               Length         => Storage_Offset (E.Length),
               Permissions    => Flags
            );
            Discard := Map_Range (
               Map            => Kernel_Table,
               Physical_Start => To_Address (E.Base),
               Virtual_Start  => To_Address (E.Base + Memory_Offset),
               Length         => Storage_Offset (E.Length),
               Permissions    => Flags
            );
         end if;
      end loop;

      return True;
   end Init;

   function Create_Table return Page_Table is
      Map : constant Page_Map_Acc := new Page_Map;
   begin
      for I in 256 .. 512 loop
         Map.PML4_Level (I) := Kernel_Map.PML4_Level (I);
      end loop;
      return Page_Table (Conv.To_Address (Conv.Object_Pointer (Map)));
   end Create_Table;

   function Destroy_Table return Boolean is
   begin
      return True;
   end Destroy_Table;

   function Make_Active (Map : Page_Table) return Boolean is
      Table : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
      Addr : constant Unsigned_64 := Unsigned_64 (Physical_Address
         (To_Integer (Table.PML4_Level'Address) - Memory_Offset));
   begin
      --  Make the pagemap active on the callee core by writing the top-level
      --  address to CR3.
      if Arch.Wrappers.Read_CR3 /= Addr then
         Arch.Wrappers.Write_CR3 (Addr);
      end if;
      return True;
   end Make_Active;

   function Is_Active (Map : Page_Table) return Boolean is
      Table : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      return Is_Loaded (Table);
   end Is_Active;

   function Translate_Address
      (Map     : Page_Table;
       Virtual : System.Address) return System.Address
   is
      Result : Physical_Address;
      Table  : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
      Addr  : constant Integer_Address := To_Integer (Virtual);
   begin
      declare
         Addr2 : constant Virtual_Address := Get_Page (Table, Addr, False);
         Searched : Page with Address => To_Address (Addr2);
      begin
         Result := Searched.Addr;
      end;
      return To_Address (Result);
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
   is
      Table : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
      Physical : constant Integer_Address := To_Integer (Physical_Start);
      Virtual  : constant Integer_Address := To_Integer (Virtual_Start);
      Len      : constant Unsigned_64     := Unsigned_64 (Length);
      PStart : constant Physical_Address := (Physical / Page_Size) * Page_Size;
      VStart : constant Virtual_Address  := (Virtual  / Page_Size) * Page_Size;
      I : Physical_Address := 0;
      Flags   : constant Page_Flags := Convert_Permissions (Permissions);
      Not_Execute : constant Boolean := not Permissions.Executable;
   begin
      while Unsigned_64 (I) < Len loop
         Map_Page (Table, VStart + I, PStart + I, Flags, Not_Execute);
         I := I + Page_Size;
      end loop;
      return True;
   end Map_Range;

   function Remap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
   is
      Table : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
      Virtual : constant Integer_Address := To_Integer (Virtual_Start);
      Len     : constant Unsigned_64     := Unsigned_64 (Length);
      VStart  : constant Virtual_Address := (Virtual / Page_Size) * Page_Size;
      I       : Physical_Address := 0;
      Flags   : constant Page_Flags := Convert_Permissions (Permissions);
      Not_Execute : constant Boolean := not Permissions.Executable;
   begin
      while Unsigned_64 (I) < Len loop
         Change_Page_Flags (Table, VStart + I, Flags, Not_Execute);
         I := I + Page_Size;
      end loop;
      return True;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      Table : constant Page_Map_Acc :=
         Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
      Virtual : constant Integer_Address := To_Integer (Virtual_Start);
      Len     : constant Unsigned_64     := Unsigned_64 (Length);
      VStart : constant Virtual_Address  := (Virtual / Page_Size) * Page_Size;
      I      : Physical_Address := 0;
   begin
      while Unsigned_64 (I) < Len loop
         Unmap_Page (Table, VStart + I);
         I := I + Page_Size;
      end loop;
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
         Curr := Curr + Page_Size;
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
