--  arch-mmu.adb: Architecture-specific MMU code.
--  Copyright (C) 2024 streaksu
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
with Lib.Atomic;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
with Arch.APIC;
with Arch.Interrupts;
with Arch.Local;
with Userland.Process; use Userland.Process;
with Memory.Physical;
with Arch.Limine;

package body Arch.MMU is
   --  Bits in the 4K page entries.
   Page_P     : constant Unsigned_64 := Shift_Left (1,  0);
   Page_RW    : constant Unsigned_64 := Shift_Left (1,  1);
   Page_U     : constant Unsigned_64 := Shift_Left (1,  2);
   Page_PWT   : constant Unsigned_64 := Shift_Left (1,  3);
   Page_PCD   : constant Unsigned_64 := Shift_Left (1,  4);
   Page_PAT   : constant Unsigned_64 := Shift_Left (1,  7);
   Page_G     : constant Unsigned_64 := Shift_Left (1,  8);
   Page_NX    : constant Unsigned_64 := Shift_Left (1, 63);

   --  Global statistics.
   function Atomic_Load_Size is new Lib.Atomic.Atomic_Load      (Memory.Size);
   function Atomic_Add_Size  is new Lib.Atomic.Atomic_Fetch_Add (Memory.Size);
   function Atomic_Sub_Size  is new Lib.Atomic.Atomic_Fetch_Sub (Memory.Size);
   Global_Kernel_Usage : Memory.Size := 0;
   Global_Table_Usage  : Memory.Size := 0;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      NX_Flags : constant Page_Permissions :=
         (Is_User_Accesible => False,
          Can_Read          => True,
          Can_Write         => True,
          Can_Execute       => False,
          Is_Global         => True);
      X_Flags : constant Page_Permissions :=
         (Is_User_Accesible => False,
          Can_Read          => True,
          Can_Write         => True,
          Can_Execute       => True,
          Is_Global         => True);
      RX_Flags : constant Page_Permissions :=
         (Is_User_Accesible => False,
          Can_Read          => True,
          Can_Write         => False,
          Can_Execute       => True,
          Is_Global         => True);
      R_Flags : constant Page_Permissions :=
         (Is_User_Accesible => False,
          Can_Read          => True,
          Can_Write         => False,
          Can_Execute       => False,
          Is_Global         => True);

      First_MiB : constant := 16#000100000#;

      --  Start of sections for correct permission loading.
      text_start   : Character with Import, Convention => C;
      text_end     : Character with Import, Convention => C;
      rodata_start : Character with Import, Convention => C;
      rodata_end   : Character with Import, Convention => C;
      data_start   : Character with Import, Convention => C;
      data_end     : Character with Import, Convention => C;
      TSAddr : constant Integer_Address := To_Integer (text_start'Address);
      OSAddr : constant Integer_Address := To_Integer (rodata_start'Address);
      DSAddr : constant Integer_Address := To_Integer (data_start'Address);
      Phys   : constant Integer_Address :=
         To_Integer (Limine.Get_Physical_Address);
   begin
      --  Initialize the kernel pagemap.
      MMU.Kernel_Table := new Page_Table'
         (PML4_Level      => (others => 0),
          Mutex           => Lib.Synchronization.Unlocked_Semaphore,
          Map_Ranges_Root => null);

      --  Map the first 4KiB - 1 MiB not NX, because we have the smp bootstrap
      --  there and else hell will break loose.
      if not Inner_Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (Page_Size),
          Virtual_Start  => To_Address (Page_Size),
          Length         => First_MiB - Page_Size,
          Permissions    => X_Flags,
          Caching        => Write_Back)
      then
         return False;
      end if;

      --  Map the memmap memory to the memory window and identity
      for E of Memmap loop
         if not Inner_Map_Range
            (Map            => Kernel_Table,
             Physical_Start => To_Address (To_Integer (E.Start)),
             Virtual_Start  => To_Address (To_Integer (E.Start) +
                                           Memory_Offset),
             Length         => Storage_Offset (E.Length),
             Permissions    => NX_Flags,
             Caching        => Write_Back)
         then
            return False;
         end if;
      end loop;

      --  Map the kernel sections.
      if not Inner_Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (TSAddr - Kernel_Offset + Phys),
          Virtual_Start  => text_start'Address,
          Length         => text_end'Address - text_start'Address,
          Permissions    => RX_Flags,
          Caching        => Write_Back) or
         not Inner_Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (OSAddr - Kernel_Offset + Phys),
          Virtual_Start  => rodata_start'Address,
          Length         => rodata_end'Address - rodata_start'Address,
          Permissions    => R_Flags,
          Caching        => Write_Back) or
         not Inner_Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (DSAddr - Kernel_Offset + Phys),
          Virtual_Start  => data_start'Address,
          Length         => data_end'Address - data_start'Address,
          Permissions    => NX_Flags,
          Caching        => Write_Back)
      then
         return False;
      end if;

      --  Update the stats we can update now and unlock.
      Global_Kernel_Usage :=
         Memory.Size (text_end'Address - text_start'Address)     +
         Memory.Size (rodata_end'Address - rodata_start'Address) +
         Memory.Size (data_end'Address - data_start'Address);

      --  Load the kernel table at last.
      return Make_Active (Kernel_Table);
   end Init;

   function Fork_Table (Map : Page_Table_Acc) return Page_Table_Acc is
      type Page_Data is array (Storage_Count range <>) of Unsigned_8;

      Addr       : System.Address;
      Curr_Range : Mapping_Range_Acc := Map.Map_Ranges_Root;
      Success    : Boolean;
      Result     : Page_Table_Acc := new Page_Table'
         (PML4_Level      => (others => 0),
          Mutex           => Lib.Synchronization.Unlocked_Semaphore,
          Map_Ranges_Root => null);
   begin
      Lib.Synchronization.Seize (Map.Mutex);

      --  Clone the higher half, which is the same in all maps.
      Result.PML4_Level (257 .. 512) := Map.PML4_Level (257 .. 512);

      --  Duplicate the rest of maps, which are mostly going to be lower half.
      while Curr_Range /= null loop
         if Curr_Range.Is_Allocated then
            Map_Allocated_Range
               (Map            => Result,
                Physical_Start => Addr,
                Virtual_Start  => Curr_Range.Virtual_Start,
                Length         => Curr_Range.Length,
                Permissions    => Curr_Range.Flags,
                Success        => Success);
            if not Success then
               Destroy_Table (Result);
               goto Cleanup;
            end if;

            declare
               New_Data : Page_Data (1 .. Curr_Range.Length)
                  with Import, Address => Addr;
               Original_Data : Page_Data (1 .. Curr_Range.Length)
                  with Import, Address => To_Address
                     (To_Integer (Curr_Range.Physical_Start) + Memory_Offset);
            begin
               New_Data := Original_Data;
            end;
         else
            if not Map_Range
               (Map            => Result,
                Physical_Start => Curr_Range.Physical_Start,
                Virtual_Start  => Curr_Range.Virtual_Start,
                Length         => Curr_Range.Length,
                Permissions    => Curr_Range.Flags)
            then
               Destroy_Table (Result);
               goto Cleanup;
            end if;
         end if;

         Curr_Range := Curr_Range.Next;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Map.Mutex);
      return Result;
   end Fork_Table;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
      procedure F is new Ada.Unchecked_Deallocation
         (Page_Table, Page_Table_Acc);
      procedure F is new Ada.Unchecked_Deallocation
         (Mapping_Range, Mapping_Range_Acc);
      Last_Range : Mapping_Range_Acc;
      Curr_Range : Mapping_Range_Acc := Map.Map_Ranges_Root;
      Discard    : Memory.Size;
   begin
      Lib.Synchronization.Seize (Map.Mutex);
      while Curr_Range /= null loop
         if Curr_Range.Is_Allocated then
            Physical.Free
               (Interfaces.C.size_t (To_Integer (Curr_Range.Physical_Start)));
         end if;
         Last_Range := Curr_Range;
         Curr_Range := Curr_Range.Next;
         F (Last_Range);
      end loop;

      for L3 of Map.PML4_Level (1 .. 256) loop
         declare
            A3   : constant Integer_Address := Clean_Entry (L3);
            PML3 : PML4
               with Import, Address => To_Address (Memory_Offset + A3);
         begin
            if (L3 and Page_P) /= 0 then
               for L2 of PML3 loop
                  declare
                     A2   : constant Integer_Address := Clean_Entry (L2);
                     PML2 : PML4 with Import,
                        Address => To_Address (Memory_Offset + A2);
                  begin
                     if (L2 and Page_P) /= 0 then
                        for L1 of PML2 loop
                           Memory.Physical.Free
                              (Interfaces.C.size_t (Clean_Entry (L1)));
                        end loop;
                     end if;
                     Memory.Physical.Free (Interfaces.C.size_t (A2));
                  end;
               end loop;
            end if;
            Discard := Atomic_Sub_Size
               (Global_Table_Usage'Address,
                PML4'Size / 8);
            Memory.Physical.Free (Interfaces.C.size_t (A3));
         end;
      end loop;
      F (Map);
   end Destroy_Table;

   function Make_Active (Map : Page_Table_Acc) return Boolean is
      Val : constant Unsigned_64 :=
         Unsigned_64 (To_Integer (Map.PML4_Level'Address) - Memory_Offset);
   begin
      if Arch.Snippets.Read_CR3 /= Val then
         Arch.Snippets.Write_CR3 (Val);
      end if;
      return True;
   end Make_Active;

   procedure Translate_Address
      (Map                : Page_Table_Acc;
       Virtual            : System.Address;
       Length             : Storage_Count;
       Physical           : out System.Address;
       Is_Mapped          : out Boolean;
       Is_User_Accessible : out Boolean;
       Is_Readable        : out Boolean;
       Is_Writeable       : out Boolean;
       Is_Executable      : out Boolean)
   is
      Virt       : Virtual_Address          := To_Integer (Virtual);
      Final      : constant Virtual_Address := Virt + Virtual_Address (Length);
      Page_Addr  : Virtual_Address;
      First_Iter : Boolean := True;
   begin
      Lib.Synchronization.Seize (Map.Mutex);
      Physical           := System.Null_Address;
      Is_Mapped          := False;
      Is_User_Accessible := False;
      Is_Readable        := False;
      Is_Writeable       := False;
      Is_Executable      := False;

      while Virt < Final loop
         Page_Addr := Get_Page (Map, Virt, False);
         declare
            Page : Unsigned_64 with Address => To_Address (Page_Addr), Import;
         begin
            if First_Iter then
               if Page_Addr /= 0 then
                  Physical           := To_Address (Clean_Entry (Page));
                  Is_Mapped          := (Page and Page_P) /= 0;
                  Is_User_Accessible := (Page and Page_U) /= 0;
                  Is_Readable        := True;
                  Is_Writeable       := (Page and Page_RW) /= 0;
                  Is_Executable      := (Page and Page_NX) = 0;
               end if;
               First_Iter := False;
            elsif Page_Addr = 0                                      or else
                  (Is_Mapped          and ((Page and Page_P)   = 0)) or else
                  (Is_User_Accessible and ((Page and Page_U)   = 0)) or else
                  (Is_Writeable       and ((Page and Page_RW)  = 0)) or else
                  (Is_Executable      and ((Page and Page_NX) /= 0))
            then
               Physical           := System.Null_Address;
               Is_Mapped          := False;
               Is_User_Accessible := False;
               Is_Readable        := False;
               Is_Writeable       := False;
               Is_Executable      := False;
               exit;
            end if;
         end;
         Virt := Virt + Page_Size;
      end loop;

      Lib.Synchronization.Release (Map.Mutex);
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Caching        : Caching_Model := Write_Back) return Boolean
   is
      Last_Range : Mapping_Range_Acc;
      Curr_Range : Mapping_Range_Acc := Map.Map_Ranges_Root;
      Success    :           Boolean := False;
   begin
      Lib.Synchronization.Seize (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start <= Virtual_Start and
            Curr_Range.Virtual_Start + Length >= Virtual_Start + Length
         then
            goto Ret;
         end if;

         Last_Range := Curr_Range;
         Curr_Range := Curr_Range.Next;
      end loop;

      Curr_Range := new Mapping_Range'
         (Next           => null,
          Is_Allocated   => False,
          Virtual_Start  => Virtual_Start,
          Physical_Start => Physical_Start,
          Length         => Length,
          Flags          => Permissions);
      if Map.Map_Ranges_Root = null then
         Map.Map_Ranges_Root := Curr_Range;
      else
         Last_Range.Next := Curr_Range;
      end if;

      Success := Inner_Map_Range
         (Map            => Map,
          Physical_Start => Physical_Start,
          Virtual_Start  => Virtual_Start,
          Length         => Length,
          Permissions    => Permissions,
          Caching        => Caching);

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Map_Range;

   procedure Map_Allocated_Range
      (Map            : Page_Table_Acc;
       Physical_Start : out System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Success        : out Boolean;
       Caching        : Caching_Model := Write_Back)
   is
      Addr : constant Virtual_Address :=
         Memory.Physical.Alloc (Interfaces.C.size_t (Length));
      Allocated : array (1 .. Length) of Unsigned_8
         with Import, Address => To_Address (Addr);
      Last_Range : Mapping_Range_Acc;
      Curr_Range : Mapping_Range_Acc := Map.Map_Ranges_Root;
   begin
      Success := False;
      Lib.Synchronization.Seize (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start <= Virtual_Start and
            Curr_Range.Virtual_Start + Length >= Virtual_Start + Length
         then
            goto Ret;
         end if;

         Last_Range := Curr_Range;
         Curr_Range := Curr_Range.Next;
      end loop;

      Curr_Range := new Mapping_Range'
         (Next           => null,
          Is_Allocated   => True,
          Virtual_Start  => Virtual_Start,
          Physical_Start => To_Address (Addr - Memory.Memory_Offset),
          Length         => Length,
          Flags          => Permissions);
      if Map.Map_Ranges_Root = null then
         Map.Map_Ranges_Root := Curr_Range;
      else
         Last_Range.Next := Curr_Range;
      end if;

      Success := Inner_Map_Range
         (Map            => Map,
          Physical_Start => To_Address (Addr - Memory.Memory_Offset),
          Virtual_Start  => Virtual_Start,
          Length         => Length,
          Permissions    => Permissions,
          Caching        => Caching);

   <<Ret>>
      if Success then
         Allocated      := (others => 0);
         Physical_Start := To_Address (Addr);
      else
         Memory.Physical.Free (Interfaces.C.size_t (Addr));
         Physical_Start := System.Null_Address;
      end if;
      Lib.Synchronization.Release (Map.Mutex);
   end Map_Allocated_Range;

   function Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions;
       Caching       : Caching_Model := Write_Back) return Boolean
   is
      Flags : constant Unsigned_64 := Flags_To_Bitmap (Permissions, Caching);
      Virt       : Virtual_Address          := To_Integer (Virtual_Start);
      Final      : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr       : Virtual_Address;
      Curr_Range : Mapping_Range_Acc := Map.Map_Ranges_Root;
      Success    : Boolean := False;
   begin
      Lib.Synchronization.Seize (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start = Virtual_Start and
            Curr_Range.Length        = Length
         then
            Curr_Range.Flags := Permissions;
            goto Actually_Remap;
         end if;

         Curr_Range := Curr_Range.Next;
      end loop;
      goto Ret;

   <<Actually_Remap>>
      while Virt < Final loop
         Addr := Get_Page (Map, Virt, False);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Entry_Body := Unsigned_64 (Clean_Entry (Entry_Body)) or Flags;
            end if;
         end;

         Virt := Virt + Page_Size;
      end loop;
      Flush_Global_TLBs (Virtual_Start, Length);
      Success := True;

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      procedure F is new Ada.Unchecked_Deallocation
         (Mapping_Range, Mapping_Range_Acc);

      Virt       : Virtual_Address          := To_Integer (Virtual_Start);
      Final      : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr       : Virtual_Address;
      Success    : Boolean := False;
      Last_Range : Mapping_Range_Acc := null;
      Curr_Range : Mapping_Range_Acc := Map.Map_Ranges_Root;
   begin
      Lib.Synchronization.Seize (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start = Virtual_Start and
            Curr_Range.Length        = Length
         then
            if Curr_Range.Is_Allocated then
               Physical.Free (Interfaces.C.size_t
                  (To_Integer (Curr_Range.Physical_Start)));
            end if;
            goto Actually_Unmap;
         end if;

         Last_Range := Curr_Range;
         Curr_Range := Curr_Range.Next;
      end loop;
      goto Ret;

   <<Actually_Unmap>>
      if Last_Range /= null then
         Last_Range.Next := Curr_Range.Next;
      else
         Map.Map_Ranges_Root := null;
      end if;
      F (Curr_Range);

      while Virt < Final loop
         Addr := Get_Page (Map, Virt, False);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Entry_Body := Entry_Body and not Page_P;
            end if;
         end;
         Virt := Virt + Page_Size;
      end loop;
      Flush_Global_TLBs (Virtual_Start, Length);
      Success := True;

   <<Ret>>
      Lib.Synchronization.Release (Map.Mutex);
      return Success;
   end Unmap_Range;

   function Get_User_Mapped_Size (Map : Page_Table_Acc) return Unsigned_64 is
      Value      : Unsigned_64 := 0;
      Curr_Range : Mapping_Range_Acc := Map.Map_Ranges_Root;
   begin
      Lib.Synchronization.Seize (Map.Mutex);
      while Curr_Range /= null loop
         Value      := Value + Unsigned_64 (Curr_Range.Length);
         Curr_Range := Curr_Range.Next;
      end loop;
      Lib.Synchronization.Release (Map.Mutex);
      return Value;
   end Get_User_Mapped_Size;

   procedure Get_Statistics (Stats : out Virtual_Statistics) is
      Val1, Val2 : Memory.Size;
   begin
      Val1 := Atomic_Load_Size (Global_Kernel_Usage'Address);
      Val2 := Atomic_Load_Size (Global_Table_Usage'Address);
      Stats := (Val1, Val2, 0);
   end Get_Statistics;
   ----------------------------------------------------------------------------
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
      Discard : Memory.Size;
   begin
      --  Check whether the entry is present.
      if (Entry_Body and Page_P) /= 0 then
         return Clean_Entry (Entry_Body);
      elsif Create_If_Not_Found then
         --  Allocate and put some default flags.
         declare
            New_Entry      : constant PML4_Acc := new PML4'(others => 0);
            New_Entry_Addr : constant Physical_Address :=
               To_Integer (New_Entry.all'Address) - Memory_Offset;
         begin
            Discard := Atomic_Add_Size
               (Global_Table_Usage'Address,
                PML4'Size / 8);
            Entry_Body := Unsigned_64 (New_Entry_Addr) or Page_P or Page_U or
                          Page_RW;
            return New_Entry_Addr;
         end;
      end if;
      return Memory.Null_Address;
   end Get_Next_Level;

   function Get_Page
      (Map      : Page_Table_Acc;
       Virtual  : Virtual_Address;
       Allocate : Boolean) return Virtual_Address
   is
      Addr : constant Unsigned_64 := Unsigned_64 (Virtual);
      PML4_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 39), 39);
      PML3_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 30), 30);
      PML2_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 21), 21);
      PML1_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 12), 12);
      Addr4 : constant Physical_Address :=
         To_Integer (Map.PML4_Level'Address) - Memory_Offset;
      Addr3, Addr2, Addr1 : Physical_Address := Memory.Null_Address;
   begin
      --  Find the entries.
      Addr3 := Get_Next_Level (Addr4, PML4_Entry, Allocate);
      if Addr3 = Memory.Null_Address then
         goto Error_Return;
      end if;
      Addr2 := Get_Next_Level (Addr3, PML3_Entry, Allocate);
      if Addr2 = Memory.Null_Address then
         goto Error_Return;
      end if;
      Addr1 := Get_Next_Level (Addr2, PML2_Entry, Allocate);
      if Addr1 = Memory.Null_Address then
         goto Error_Return;
      end if;
      return Addr1 + Memory_Offset + (Physical_Address (PML1_Entry) * 8);

   <<Error_Return>>
      return Memory.Null_Address;
   end Get_Page;

   function Inner_Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Caching        : Caching_Model) return Boolean
   is
      Flags : constant Unsigned_64 := Flags_To_Bitmap (Permissions, Caching);
      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Phys  : Virtual_Address          := To_Integer (Physical_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr  : Virtual_Address;
   begin
      while Virt < Final loop
         Addr := Get_Page (Map, Virt, True);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Entry_Body := Unsigned_64 (Phys) or Flags;
         end;

         Virt := Virt + Page_Size;
         Phys := Phys + Page_Size;
      end loop;
      return True;
   end Inner_Map_Range;

   function Flags_To_Bitmap
      (Perm    : Page_Permissions;
       Caching : Caching_Model) return Unsigned_64
   is
      Result : Unsigned_64;
   begin
      Result :=
         (if Perm.Can_Execute       then 0                  else Page_NX) or
         (if Perm.Can_Write         then Page_RW            else       0) or
         (if Perm.Is_Global         then Page_G             else       0) or
         (if Perm.Is_User_Accesible then Page_U             else       0) or
         Page_P;

      case Caching is
         when Write_Back      => null;
         when Write_Through   => Result := Result or Page_PWT;
         when Write_Combining => Result := Result or Page_PAT or Page_PWT;
         when Uncacheable     => Result := Result or Page_PWT or Page_PCD;
      end case;

      return Result;
   end Flags_To_Bitmap;

   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count) is
      Final : constant System.Address := Addr + Len;
      Curr  :          System.Address := Addr;
      Current_Proc : Userland.Process.PID;
   begin
      --  First, invalidate for ourselves.
      while To_Integer (Curr) < To_Integer (Final) loop
         Snippets.Invalidate_Page (To_Integer (Curr));
         Curr := Curr + Page_Size;
      end loop;

      --  If we are running on a process, and said process is running with more
      --  than one thread, we need to invalidate using funky IPIs.
      if CPU.Core_Locals /= null then
         Current_Proc := Local.Get_Current_Process;
         if Current_Proc = Error_PID or else
            Userland.Process.Get_Thread_Count (Current_Proc) < 2
         then
            return;
         end if;

         for I in CPU.Core_Locals.all'Range loop
            if I /= CPU.Get_Local.Number then
               CPU.Core_Locals (I).Invalidate_Start := Addr;
               CPU.Core_Locals (I).Invalidate_End   := Final;
               APIC.LAPIC_Send_IPI
                  (CPU.Core_Locals (I).LAPIC_ID,
                   Interrupts.Invalidate_Interrupt);
            end if;
         end loop;
      end if;
   end Flush_Global_TLBs;
end Arch.MMU;
