--  arch-mmu.adb: Architecture-specific MMU code.
--  Copyright (C) 2025 streaksu
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
with Memory.Physical;
with Panic;
with Messages;
with Arch.MMU; use Arch.MMU;
with Arch; use Arch;

package body Memory.MMU with SPARK_Mode => Off is
   --  Global statistics.
   Global_Kernel_Usage : Memory.Size := 0;
   Global_Table_Usage  : Memory.Size := 0;

   procedure F is new Ada.Unchecked_Deallocation (Page_Table, Page_Table_Acc);

   procedure Init (Memmap : Arch.Boot_Memory_Map; Success : out Boolean) is
      NX_Flags : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accessible => False,
          Can_Read           => True,
          Can_Write          => True,
          Can_Execute        => False,
          Is_Global          => True);
      RX_Flags : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accessible => False,
          Can_Read           => True,
          Can_Write          => False,
          Can_Execute        => True,
          Is_Global          => True);
      R_Flags : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accessible => False,
          Can_Read           => True,
          Can_Write          => False,
          Can_Execute        => False,
          Is_Global          => True);

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

      --  Physical address.
      Tmp  : System.Address;
      Phys : Integer_Address;
   begin
      Messages.Put_Line ("Paging type used: " & Arch.MMU.Paging_Levels'Image);
      Arch.MMU.Get_Load_Addr (Tmp, Success);
      if not Success then
         return;
      end if;
      Phys := To_Integer (Tmp);

      --  Initialize the kernel pagemap.
      MMU.Kernel_Table := new Page_Table'
         (Top_Level => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          User_Size  => 0);

      --  Preallocate the higher half PML, so when we clone the kernel memmap,
      --  we can just blindly copy the entries and share mapping between all
      --  kernel instances.
      for E of Kernel_Table.Top_Level (257 .. 512) loop
         declare
            New_Entry      : constant PML_Acc := new PML'(others => 0);
            New_Entry_Addr : constant Physical_Address :=
               To_Integer (New_Entry.all'Address) - Memory_Offset;
         begin
            Global_Table_Usage := Global_Table_Usage + (PML'Size / 8);
            E := Arch.MMU.Construct_Level (To_Address (New_Entry_Addr));
         end;
      end loop;

      --  Map the usable and bootloader memmap memory to the memory window.
      for E of Memmap loop
         if E.MemType = Memory_Bootloader or else E.MemType = Memory_Free then
            Map_Range
               (Map            => Kernel_Table,
                Physical_Start => E.Start,
                Virtual_Start  => To_Address (To_Integer (E.Start) +
                                              Memory_Offset),
                Length         => Storage_Offset (E.Length),
                Permissions    => NX_Flags,
                Caching        => Arch.MMU.Write_Back,
                Success        => Success);
            if not Success then
               return;
            end if;
         end if;
      end loop;

      --  Map the kernel sections.
      Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (TSAddr - Kernel_Offset + Phys),
          Virtual_Start  => text_start'Address,
          Length         => text_end'Address - text_start'Address,
          Permissions    => RX_Flags,
          Caching        => Arch.MMU.Write_Back,
          Success        => Success);
      if not Success then return; end if;
      Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (OSAddr - Kernel_Offset + Phys),
          Virtual_Start  => rodata_start'Address,
          Length         => rodata_end'Address - rodata_start'Address,
          Permissions    => R_Flags,
          Caching        => Arch.MMU.Write_Back,
          Success        => Success);
      if not Success then return; end if;
      Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (DSAddr - Kernel_Offset + Phys),
          Virtual_Start  => data_start'Address,
          Length         => data_end'Address - data_start'Address,
          Permissions    => NX_Flags,
          Caching        => Arch.MMU.Write_Back,
          Success        => Success);
      if not Success then return; end if;

      --  Update the stats we can update now and unlock.
      Global_Kernel_Usage :=
         Memory.Size (text_end'Address - text_start'Address)     +
         Memory.Size (rodata_end'Address - rodata_start'Address) +
         Memory.Size (data_end'Address - data_start'Address);

      --  Load the kernel table at last.
      Success := Make_Active (Kernel_Table);
   exception
      when Constraint_Error =>
         Success := False;
   end Init;

   procedure Create_Table (New_Map : out Page_Table_Acc) is
   begin
      Synchronization.Seize (Kernel_Table.Mutex);
      New_Map := new Page_Table'
         (Top_Level => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          User_Size  => 0);
      New_Map.Top_Level (257 .. 512) := Kernel_Table.Top_Level (257 .. 512);
      Synchronization.Release (Kernel_Table.Mutex);
   exception
      when Constraint_Error =>
         New_Map := null;
   end Create_Table;

   procedure Fork_Table (Map : Page_Table_Acc; Forked : out Page_Table_Acc) is
      Success : Boolean;
      Starting_Level : Positive;
   begin
      Forked := new Page_Table'
         (Top_Level => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          User_Size  => 0);

      Synchronization.Seize (Map.Mutex);

      --  Clone the higher half, which is the same in all maps, and user size.
      Forked.Top_Level (257 .. 512) := Map.Top_Level (257 .. 512);
      Forked.User_Size := Map.User_Size;

      --  Go thru the lower half entries and copy.
      case Arch.MMU.Paging_Levels is
         when Arch.MMU.Five_Level_Paging => Starting_Level := 5;
         when Arch.MMU.Four_Level_Paging => Starting_Level := 4;
         when Arch.MMU.Three_Level_Paging => Starting_Level := 3;
      end case;

      Clone_Level
         (Idx_5 => 1,
          Idx_4 => 1,
          Idx_3 => 1,
          Idx_2 => 1,
          Current_Level => Variable_PML (Map.Top_Level (1 .. 256)),
          Current_Depth => Starting_Level,
          Target => Forked,
          Success => Success);
      if not Success then
         F (Forked);
      end if;

      Synchronization.Release (Map.Mutex);
   exception
      when Constraint_Error =>
         Forked := null;
   end Fork_Table;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
      Starting_Level : Positive;
      Success : Boolean;
   begin
      Synchronization.Seize (Map.Mutex);

      --  Go thru the lower half entries and copy.
      case Arch.MMU.Paging_Levels is
         when Arch.MMU.Five_Level_Paging => Starting_Level := 5;
         when Arch.MMU.Four_Level_Paging => Starting_Level := 4;
         when Arch.MMU.Three_Level_Paging => Starting_Level := 3;
      end case;

      Destroy_Level
         (Current_Level => Variable_PML (Map.Top_Level (1 .. 256)),
          Current_Depth => Starting_Level,
          Map => Map,
          Success => Success);
      if not Success then
         Messages.Put_Line ("Failed to free map");
      end if;

      --  FIXME: Unlocking here should not be necessary, it should be left
      --  locked as a security measure for use after free. Leaving it blocked
      --  though causes a deadlock somewhere outside the MMU code, as printing
      --  makes it clear that the pagemap is not used after destruction.
      Synchronization.Release (Map.Mutex);

      F (Map);
   exception
      when Constraint_Error =>
         return;
   end Destroy_Table;

   function Make_Active (Map : Page_Table_Acc) return Boolean is
      Success : Boolean;
   begin
      Arch.MMU.Set_Current_Table
         (To_Address (To_Integer (Map.Top_Level'Address) - Memory_Offset),
          Success);
      return Success;
   exception
      when Constraint_Error =>
         return False;
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
      Perms      : Arch.MMU.Page_Permissions;
   begin
      Physical           := System.Null_Address;
      Is_Mapped          := False;
      Is_User_Accessible := False;
      Is_Readable        := False;
      Is_Writeable       := False;
      Is_Executable      := False;

      Synchronization.Seize (Map.Mutex);
      while Virt < Final loop
         Get_Page (Map, Virt, False, Page_Addr);
         declare
            Page : Unsigned_64 with Address => To_Address (Page_Addr), Import;
         begin
            if Page_Addr /= 0 then
               Physical := To_Address (Arch.MMU.Clean_Entry (Page));
               Perms    := Arch.MMU.Clean_Entry_Perms (Page).Perms;
            end if;
            if First_Iter then
               if Page_Addr /= 0 then
                  Is_Mapped          := Arch.MMU.Is_Entry_Present (Page);
                  Is_User_Accessible := Perms.Is_User_Accessible;
                  Is_Readable        := Perms.Can_Read;
                  Is_Writeable       := Perms.Can_Write;
                  Is_Executable      := Perms.Can_Execute;
               end if;
               First_Iter := False;
            elsif Page_Addr = 0 or else
                  (Is_Mapped and not Arch.MMU.Is_Entry_Present (Page)) or else
                  (Is_User_Accessible and not Perms.Is_User_Accessible) or else
                  (Is_Readable and not Perms.Can_Read) or else
                  (Is_Writeable and not Perms.Can_Write) or else
                  (Is_Executable and not Perms.Can_Execute)
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
      Synchronization.Release (Map.Mutex);
   exception
      when Constraint_Error =>
         Physical           := System.Null_Address;
         Is_Mapped          := False;
         Is_User_Accessible := False;
         Is_Readable        := False;
         Is_Writeable       := False;
         Is_Executable      := False;
   end Translate_Address;

   procedure Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Arch.MMU.Page_Permissions;
       Success        : out Boolean;
       Caching        : Arch.MMU.Caching_Model := Arch.MMU.Write_Back)
   is
      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Phys  : Virtual_Address          := To_Integer (Physical_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr  : Virtual_Address;
      Orig  : Integer_Address;
      Perms : Arch.MMU.Clean_Result;
   begin
      Synchronization.Seize (Map.Mutex);
      while Virt < Final loop
         Get_Page (Map, Virt, True, Addr);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Orig  := Arch.MMU.Clean_Entry (Entry_Body);
            Perms := Arch.MMU.Clean_Entry_Perms (Entry_Body);
            if Perms.User_Flag and then Orig /= Phys then
               Physical.User_Free (Memory.Memory_Offset + Orig);
            end if;
            Entry_Body := Arch.MMU.Construct_Entry
               (To_Address (Phys), Permissions, Caching, False);
            if Perms.Perms.Is_User_Accessible then
               Map.User_Size := Map.User_Size - Page_Size;
            end if;
            if Permissions.Is_User_Accessible then
               Map.User_Size := Map.User_Size + Page_Size;
            end if;
         end;

         Virt := Virt + Page_Size;
         Phys := Phys + Page_Size;
      end loop;
      Synchronization.Release (Map.Mutex);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Map_Range;

   procedure Map_Allocated_Range
      (Map            : Page_Table_Acc;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Arch.MMU.Page_Permissions;
       Success        : out Boolean;
       Caching        : Arch.MMU.Caching_Model := Arch.MMU.Write_Back)
   is
      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr  : Virtual_Address;
      Addr1 : Virtual_Address;
      Phys  : Virtual_Address;
      Orig  : Virtual_Address;
      Perms : Arch.MMU.Clean_Result;
   begin
      Synchronization.Seize (Map.Mutex);
      while Virt < Final loop
         Get_Page (Map, Virt, True, Addr);

         Memory.Physical.User_Alloc
            (Addr    => Addr1,
             Size    => Page_Size,
             Success => Success);
         if not Success then
            Synchronization.Release (Map.Mutex);
            return;
         end if;
         Phys := Addr1 - Memory.Memory_Offset;
         declare
            Allocated : array (1 .. Page_Size) of Unsigned_8
               with Import, Address => To_Address (Addr1);
         begin
            Allocated := [others => 0];
         end;

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Orig  := Arch.MMU.Clean_Entry (Entry_Body);
            Perms := Arch.MMU.Clean_Entry_Perms (Entry_Body);
            if Perms.User_Flag and then Orig /= Phys then
               Physical.User_Free (Memory.Memory_Offset + Orig);
            end if;
            Entry_Body := Arch.MMU.Construct_Entry
               (To_Address (Phys), Permissions, Caching, True);
            if Perms.Perms.Is_User_Accessible then
               Map.User_Size := Map.User_Size - Page_Size;
            end if;
            if Permissions.Is_User_Accessible then
               Map.User_Size := Map.User_Size + Page_Size;
            end if;
         end;

         Virt := Virt + Page_Size;
         Phys := Phys + Page_Size;
      end loop;
      Synchronization.Release (Map.Mutex);

      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Map_Allocated_Range;

   procedure Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Arch.MMU.Page_Permissions;
       Success       : out Boolean;
       Caching       : Arch.MMU.Caching_Model := Arch.MMU.Write_Back)
   is
      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr  : Virtual_Address;
      Perms : Arch.MMU.Clean_Result;
   begin
      Synchronization.Seize (Map.Mutex);
      while Virt < Final loop
         Get_Page (Map, Virt, False, Addr);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Perms      := Arch.MMU.Clean_Entry_Perms (Entry_Body);
               Entry_Body := Arch.MMU.Construct_Entry
                  (To_Address (Arch.MMU.Clean_Entry (Entry_Body)),
                      Permissions, Caching, Perms.User_Flag);
               if Perms.Perms.Is_User_Accessible then
                  Map.User_Size := Map.User_Size - Page_Size;
               end if;
               if Permissions.Is_User_Accessible then
                  Map.User_Size := Map.User_Size + Page_Size;
               end if;
            end if;
         end;

         Virt := Virt + Page_Size;
      end loop;
      Arch.MMU.Flush_TLBs (Get_Map_Table_Addr (Map), Virtual_Start, Length);
      Synchronization.Release (Map.Mutex);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Remap_Range;

   procedure Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Success       : out Boolean)
   is
      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr  : Virtual_Address;
      Orig  : Virtual_Address;
      Perms : Arch.MMU.Clean_Result;
   begin
      Synchronization.Seize (Map.Mutex);
      while Virt < Final loop
         Get_Page (Map, Virt, False, Addr);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Orig  := Arch.MMU.Clean_Entry (Entry_Body);
               Perms := Arch.MMU.Clean_Entry_Perms (Entry_Body);
               if Perms.User_Flag then
                  Physical.User_Free (Memory.Memory_Offset + Orig);
               end if;
               Entry_Body := Arch.MMU.Make_Not_Present (Entry_Body);
               if Perms.Perms.Is_User_Accessible then
                  Map.User_Size := Map.User_Size - Page_Size;
               end if;
            end if;
         end;
         Virt := Virt + Page_Size;
      end loop;
      Arch.MMU.Flush_TLBs (Get_Map_Table_Addr (Map), Virtual_Start, Length);
      Synchronization.Release (Map.Mutex);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Unmap_Range;

   function Get_Curr_Table_Addr return System.Address is
      Ret : System.Address;
   begin
      Arch.MMU.Get_Current_Table (Ret);
      return Ret;
   end Get_Curr_Table_Addr;

   function Get_Map_Table_Addr (Map : Page_Table_Acc) return System.Address is
   begin
      return To_Address (To_Integer (Map.Top_Level'Address) - Memory_Offset);
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Get_Map_Table_Addr;

   procedure Set_Table_Addr (Addr : System.Address) is
      Success : Boolean;
   begin
      Arch.MMU.Set_Current_Table (Addr, Success);
      if not Success then
         Panic.Hard_Panic ("Failed to set kernel table");
      end if;
   end Set_Table_Addr;

   procedure Get_User_Mapped_Size (Map : Page_Table_Acc; Sz : out Unsigned_64)
   is
   begin
      Synchronization.Seize (Map.Mutex);
      Sz := Map.User_Size;
      Synchronization.Release (Map.Mutex);
   exception
      when Constraint_Error =>
         Sz := 0;
   end Get_User_Mapped_Size;

   procedure Get_Statistics (Stats : out Virtual_Statistics) is
      Val1, Val2 : Memory.Size;
   begin
      Val1 := Global_Kernel_Usage;
      Val2 := Global_Table_Usage;
      Stats := (Val1, Val2, 0);
   end Get_Statistics;
   ----------------------------------------------------------------------------
   procedure Get_Next_Level
      (Current_Level       : Physical_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean;
       Addr                : out Physical_Address)
   is
      Discard : Memory.Size;
   begin
      declare
         Entry_Addr : constant Virtual_Address :=
            Current_Level + Memory_Offset + Physical_Address (Index * 8);
         Entry_Body : Unsigned_64
            with Address => To_Address (Entry_Addr), Import;
      begin
         --  Check whether the entry is present.
         if Arch.MMU.Is_Entry_Present (Entry_Body) then
            Addr := Arch.MMU.Clean_Entry (Entry_Body);
            return;
         elsif Create_If_Not_Found then
            --  Allocate and put some default flags.
            declare
               New_Entry      : constant PML_Acc := new PML'(others => 0);
               New_Entry_Addr : constant Physical_Address :=
                  To_Integer (New_Entry.all'Address) - Memory_Offset;
            begin
               Global_Table_Usage := Global_Table_Usage + (PML'Size / 8);
               Entry_Body := Arch.MMU.Construct_Level
                  (To_Address (New_Entry_Addr));
               Addr := New_Entry_Addr;
               return;
            end;
         else
            Addr := Memory.Null_Address;
         end if;
      end;
   end Get_Next_Level;

   procedure Get_Page
      (Map      : Page_Table_Acc;
       Virtual  : Virtual_Address;
       Allocate : Boolean;
       Result   : out Virtual_Address)
   is
      Addr : constant Unsigned_64 := Unsigned_64 (Virtual);
      PML5_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 48), 48);
      PML4_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 39), 39);
      PML3_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 30), 30);
      PML2_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 21), 21);
      PML1_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 12), 12);
      Addr5, Addr4, Addr3, Addr2, Addr1 : Physical_Address :=
         Memory.Null_Address;
   begin
      --  Find the entries.
      case Arch.MMU.Paging_Levels is
         when Arch.MMU.Five_Level_Paging =>
            Addr5 := To_Integer (Map.Top_Level'Address) - Memory_Offset;
            Get_Next_Level (Addr5, PML5_Entry, Allocate, Addr4);
            if Addr4 = Memory.Null_Address then
               goto Error_Return;
            end if;
            Get_Next_Level (Addr4, PML4_Entry, Allocate, Addr3);
            if Addr3 = Memory.Null_Address then
               goto Error_Return;
            end if;
         when Arch.MMU.Four_Level_Paging =>
            Addr4 := To_Integer (Map.Top_Level'Address) - Memory_Offset;
            Get_Next_Level (Addr4, PML4_Entry, Allocate, Addr3);
            if Addr3 = Memory.Null_Address then
               goto Error_Return;
            end if;
         when Arch.MMU.Three_Level_Paging =>
            Addr3 := To_Integer (Map.Top_Level'Address) - Memory_Offset;
      end case;

      Get_Next_Level (Addr3, PML3_Entry, Allocate, Addr2);
      if Addr2 = Memory.Null_Address then
         goto Error_Return;
      end if;
      Get_Next_Level (Addr2, PML2_Entry, Allocate, Addr1);
      if Addr1 = Memory.Null_Address then
         goto Error_Return;
      end if;
      Result := Addr1 + Memory_Offset + (Physical_Address (PML1_Entry) * 8);
      return;

   <<Error_Return>>
      Result := Memory.Null_Address;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception when fetching/allocating page");
   end Get_Page;

   function Idx_To_Addr (Idx_5, Idx_4, Idx_3, Idx_2, Idx_1 : Positive)
      return Integer_Address
   is
   begin
      return
         (Integer_Address (Idx_5) - 1) * 16#1000000000000# +
         (Integer_Address (Idx_4) - 1) * 16#0008000000000# +
         (Integer_Address (Idx_3) - 1) * 16#0000040000000# +
         (Integer_Address (Idx_2) - 1) * 16#0000000200000# +
         (Integer_Address (Idx_1) - 1) * 16#0000000001000#;
   end Idx_To_Addr;

   procedure Clone_Level
      (Idx_5, Idx_4, Idx_3, Idx_2 : Positive;
       Current_Level : Variable_PML;
       Current_Depth : Positive;
       Target : Page_Table_Acc;
       Success : out Boolean)
   is
      type Arr is array (1 .. Page_Size) of Unsigned_8;
      Addr, Addr2, Addr3 : Virtual_Address;
      Perms : Arch.MMU.Clean_Result;
   begin
      for I in Current_Level'Range loop
         declare
            L : constant Unsigned_64 := Current_Level (I);
            A : constant Integer_Address :=
               Arch.MMU.Clean_Entry (Current_Level (I));
            Next : PML with Import, Address => To_Address (Memory_Offset + A);
         begin
            if Arch.MMU.Is_Entry_Present (L) then
               case Current_Depth is
                  when 5 =>
                     Clone_Level
                        (I, Idx_4, Idx_3, Idx_2, Variable_PML (Next),
                         4, Target, Success);
                  when 4 =>
                     Clone_Level
                        (Idx_5, I, Idx_3, Idx_2, Variable_PML (Next),
                         3, Target, Success);
                  when 3 =>
                     Clone_Level
                        (Idx_5, Idx_4, I, Idx_2, Variable_PML (Next),
                         2, Target, Success);
                  when 2 =>
                     Clone_Level
                        (Idx_5, Idx_4, Idx_3, I, Variable_PML (Next),
                         1, Target, Success);
                  when others =>
                     Addr := Idx_To_Addr (Idx_5, Idx_4, Idx_3, Idx_2, I);
                     Perms := Arch.MMU.Clean_Entry_Perms (L);
                     Get_Page (Target, Addr, True, Addr2);
                     declare
                        Res : Unsigned_64 with
                           Import, Address => To_Address (Addr2);
                     begin
                        if Perms.User_Flag then
                           Memory.Physical.User_Alloc
                              (Addr    => Addr3,
                               Size    => Page_Size,
                               Success => Success);
                           if not Success then
                              return;
                           end if;
                           declare
                              Allocated : Arr
                                 with Import, Address => To_Address (Addr3);
                              Orig : Arr with Import,
                                 Address => To_Address (Memory_Offset + A);
                           begin
                              Allocated := Orig;
                           end;
                           Res := Arch.MMU.Construct_Entry
                              (To_Address (Addr3 - Memory.Memory_Offset),
                               Perms.Perms, Perms.Caching, True);
                        else
                           Res := L;
                        end if;
                     end;
                     Success := True;
               end case;
            end if;
         end;
      end loop;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while cloning page tables");
         Success := False;
   end Clone_Level;

   procedure Destroy_Level
      (Current_Level : Variable_PML;
       Current_Depth : Positive;
       Map : Page_Table_Acc;
       Success : out Boolean)
   is
      Perms : Arch.MMU.Clean_Result;
      PML_Sz : constant Memory.Size := PML'Size / 8;
   begin
      for I in Current_Level'Range loop
         declare
            L : constant Unsigned_64 := Current_Level (I);
            A : constant Integer_Address :=
               Arch.MMU.Clean_Entry (Current_Level (I));
            Next : PML with Import, Address => To_Address (Memory_Offset + A);
         begin
            if Arch.MMU.Is_Entry_Present (L) then
               case Current_Depth is
                  when 5 =>
                     Destroy_Level (Variable_PML (Next), 4, Map, Success);
                  when 4 =>
                     Destroy_Level (Variable_PML (Next), 3, Map, Success);
                  when 3 =>
                     Destroy_Level (Variable_PML (Next), 2, Map, Success);
                  when 2 =>
                     Destroy_Level (Variable_PML (Next), 1, Map, Success);
                  when others =>
                     Perms := Arch.MMU.Clean_Entry_Perms (L);
                     if Perms.User_Flag then
                        Physical.User_Free (Memory_Offset + A);
                     end if;
                     goto Iter_End;
               end case;

               Global_Table_Usage := Global_Table_Usage - PML_Sz;
               Memory.Physical.Free (Interfaces.C.size_t (A));
            <<Iter_End>>
            end if;
         end;
      end loop;

      Success := True;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while deleting page tables");
         Success := False;
   end Destroy_Level;
end Memory.MMU;
