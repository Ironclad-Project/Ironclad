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

with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Deallocation;
with Alignment;
with Memory.Physical;
with Panic;
with Messages;
with Arch.MMU; use Arch.MMU;
with Arch; use Arch;
with Scheduler;

package body Memory.MMU with SPARK_Mode => Off is
   --  Global statistics.
   Global_Kernel_Usage : Memory.Size := 0;
   Global_Table_Usage  : Memory.Size := 0;

   procedure F is new Ada.Unchecked_Deallocation (Page_Table, Page_Table_Acc);

   procedure Init
      (Memmap   : Arch.Boot_Memory_Map;
       Segments : Arch.Boot_Kernel_Segments;
       Success  : out Boolean)
   is
      package Align is new Alignment (Integer_Address);

      NX_Flags : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accessible => False,
          Can_Read           => True,
          Can_Write          => True,
          Can_Execute        => False,
          Is_Global          => True);
   begin
      Messages.Put_Line ("Paging type used: " & Arch.MMU.Paging_Levels'Image);

      --  Initialize the kernel pagemap.
      MMU.Kernel_Table := new Page_Table'
         (Top_Level  => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          Space_Lock => Synchronization.Unlocked_Mutex,
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

      --  Map the kernel as its own program headers ask for. Ironclad forces
      --  W^X, so a segment asking for both is refused rather than weakened.
      for Seg of Segments loop
         declare
            Perms : constant Arch.MMU.Page_Permissions :=
               (Is_User_Accessible => False,
                Can_Read           => True,
                Can_Write          => Seg.Can_Write,
                Can_Execute        => Seg.Can_Execute,
                Is_Global          => True);
            Start : constant Integer_Address :=
               To_Integer (Seg.Virtual_Start);
            Virt : Integer_Address := Start;
            Len  : Integer_Address := Integer_Address (Seg.Length);
         begin
            if Seg.Can_Write and Seg.Can_Execute then
               Success := False;
               return;
            end if;

            Align.Align_Memory_Range (Virt, Len, Page_Size);
            Map_Range
               (Map            => Kernel_Table,
                Physical_Start => To_Address
                   (To_Integer (Seg.Physical_Start) - (Start - Virt)),
                Virtual_Start  => To_Address (Virt),
                Length         => Storage_Count (Len),
                Permissions    => Perms,
                Caching        => Arch.MMU.Write_Back,
                Success        => Success);
            if not Success then return; end if;

            --  Update the stats we can update now.
            Global_Kernel_Usage := Global_Kernel_Usage + Memory.Size (Len);
         end;
      end loop;

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
         (Top_Level  => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          Space_Lock => Synchronization.Unlocked_Mutex,
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
         (Top_Level  => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          Space_Lock => Synchronization.Unlocked_Mutex,
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
         Destroy_Table (Forked);
      end if;

      Synchronization.Release (Map.Mutex);
   exception
      when Constraint_Error =>
         declare
            pragma Suppress (All_Checks);
         begin
            Synchronization.Release (Map.Mutex);
         end;
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

      --  Binary semaphores in ironclad keep track of interrupt state, so we
      --  must unlock to avoid interrupt deadlock even when deleting object.
      Synchronization.Release (Map.Mutex);

      F (Map);
   exception
      when Constraint_Error =>
         declare
            pragma Suppress (All_Checks);
         begin
            Synchronization.Release (Map.Mutex);
         end;
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
         declare
            pragma Suppress (All_Checks);
         begin
            Synchronization.Release (Map.Mutex);
         end;
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
      Virt    : Virtual_Address          := To_Integer (Virtual_Start);
      Phys    : Virtual_Address          := To_Integer (Physical_Start);
      Final   : constant Virtual_Address := Virt + Virtual_Address (Length);
      First   : Virtual_Address;
      Addr    : Virtual_Address;
      Orig    : Integer_Address;
      Perms   : Arch.MMU.Clean_Result;
      Frames  : Frame_Batch;
      Count   : Natural;
      Changed : Boolean;
      Locked  : Boolean := False;
      Fill    : constant Boolean := Permissions.Can_Read or
                                    Permissions.Can_Write or
                                    Permissions.Can_Execute;
   begin
      --  XXX: A mapping that grants no access reserves address space, it does
      --  not ask for memory. Software like glibc tends to reserve this way
      --  large swathes of memory and actually use it when Remap_Range'ing it
      --  later on. We will do a little hack and not do anything, since
      --  Remap_Range can allocate.
      if not Fill then
         Success := True;
         return;
      end if;

      Seize_Space (Map);
      while Virt < Final loop
         First   := Virt;
         Count   := 0;
         Changed := False;
         Synchronization.Seize (Map.Mutex);
         Locked := True;
         while Virt < Final and Count < Batch_Size loop
            Get_Page (Map, Virt, True, Addr);

            declare
               Entry_Body : Unsigned_64
                  with Address => To_Address (Addr), Import;
            begin
               Orig  := Arch.MMU.Clean_Entry (Entry_Body);
               Perms := Arch.MMU.Clean_Entry_Perms (Entry_Body);
               if Arch.MMU.Is_Entry_Present (Entry_Body) then
                  Changed := True;
                  if Perms.User_Flag and then Orig /= Phys then
                     Count := Count + 1;
                     Frames (Count) := Orig;
                  end if;
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
         Locked := False;

         Finish_Batch (Map, First, Virt - First, Changed, Frames, Count);
      end loop;
      Release_Space (Map);

      Success := True;
   exception
      when Constraint_Error =>
         declare
            pragma Suppress (All_Checks);
         begin
            if Locked then
               Synchronization.Release (Map.Mutex);
            end if;
            Release_Space (Map);
         end;
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
      Virt    : Virtual_Address          := To_Integer (Virtual_Start);
      Final   : constant Virtual_Address := Virt + Virtual_Address (Length);
      First   : Virtual_Address;
      Addr    : Virtual_Address;
      Addr1   : Virtual_Address;
      Phys    : Virtual_Address;
      Orig    : Virtual_Address;
      Perms   : Arch.MMU.Clean_Result;
      Frames  : Frame_Batch;
      Count   : Natural;
      Changed : Boolean;
      Locked  : Boolean := False;
      Fill    : constant Boolean := Permissions.Can_Read or
                                    Permissions.Can_Write or
                                    Permissions.Can_Execute;
   begin
      --  XXX: See above for Map_Range.
      if not Fill then
         Success := True;
         return;
      end if;

      --  A page this replaces may still be cached by any core running the
      --  map, so it is only freed once they have all dropped it.
      Seize_Space (Map);
      Success := True;
      while Success and Virt < Final loop
         First   := Virt;
         Count   := 0;
         Changed := False;
         Synchronization.Seize (Map.Mutex);
         Locked := True;
         while Virt < Final and Count < Batch_Size loop
            Get_Page (Map, Virt, True, Addr);

            Memory.Physical.User_Alloc
               (Addr    => Addr1,
                Size    => Page_Size,
                Success => Success);
            exit when not Success;
            Phys := Addr1 - Memory.Memory_Offset;
            declare
               Allocated : array (1 .. Page_Size) of Unsigned_8
                  with Import, Address => To_Address (Addr1);
            begin
               Allocated := [others => 0];
            end;

            declare
               Entry_Body : Unsigned_64
                  with Address => To_Address (Addr), Import;
            begin
               Orig  := Arch.MMU.Clean_Entry (Entry_Body);
               Perms := Arch.MMU.Clean_Entry_Perms (Entry_Body);
               if Arch.MMU.Is_Entry_Present (Entry_Body) then
                  Changed := True;
                  if Perms.User_Flag and then Orig /= Phys then
                     Count := Count + 1;
                     Frames (Count) := Orig;
                  end if;
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
         end loop;
         Synchronization.Release (Map.Mutex);
         Locked := False;

         Finish_Batch (Map, First, Virt - First, Changed, Frames, Count);
      end loop;
      Release_Space (Map);
   exception
      when Constraint_Error =>
         declare
            pragma Suppress (All_Checks);
         begin
            if Locked then
               Synchronization.Release (Map.Mutex);
            end if;
            Release_Space (Map);
         end;
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
      Virt    : Virtual_Address          := To_Integer (Virtual_Start);
      Final   : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr    : Virtual_Address;
      Addr1   : Virtual_Address;
      Frame   : Integer_Address;
      User    : Boolean;
      Perms   : Arch.MMU.Clean_Result;
      Count   : Natural := 0;
      Changed : Boolean := False;
      Locked  : Boolean := False;
      Fill    : constant Boolean := Permissions.Can_Read or
                                    Permissions.Can_Write or
                                    Permissions.Can_Execute;
   begin
      Seize_Space (Map);
      Synchronization.Seize (Map.Mutex);
      Locked := True;
      Success := True;
      while Virt < Final loop
         --  Interrupts are let in every batch of pages, as the space lock
         --  keeps the map as it is meanwhile. A thread deleted meanwhile
         --  gives up the rest, to leave at the edge of its syscall before
         --  its map goes, see Scheduler.Wait_For_Removed.
         if Count = Batch_Size then
            Synchronization.Release (Map.Mutex);
            Synchronization.Seize (Map.Mutex);
            Count := 0;
            if Scheduler.Is_Doomed then
               Success := False;
               exit;
            end if;
         end if;
         Count := Count + 1;

         Get_Page (Map, Virt, Fill, Addr);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Perms := Arch.MMU.Clean_Entry_Perms (Entry_Body);
               Frame := Arch.MMU.Clean_Entry (Entry_Body);
               User  := Perms.User_Flag;
               if Arch.MMU.Is_Entry_Present (Entry_Body) then
                  Changed := True;
               end if;

               --  Nothing is behind this address yet. If this call is what
               --  grants access to it, that is the point at which it has to
               --  be given memory, and the point at which the process is
               --  charged for it.
               if Fill and then Frame = 0 then
                  Memory.Physical.User_Alloc (Addr1, Page_Size, Success);
                  exit when not Success;
                  declare
                     Allocated : array (1 .. Page_Size) of Unsigned_8
                        with Import, Address => To_Address (Addr1);
                  begin
                     Allocated := [others => 0];
                  end;
                  Frame := Addr1 - Memory.Memory_Offset;
                  User  := Permissions.Is_User_Accessible;
               end if;

               --  An address with nothing behind it stays that way. Writing
               --  an entry for it would hand out physical address zero as if
               --  it were the caller's page.
               if Frame /= 0 then
                  Entry_Body := Arch.MMU.Construct_Entry
                     (To_Address (Frame), Permissions, Caching, User);
                  if Perms.Perms.Is_User_Accessible then
                     Map.User_Size := Map.User_Size - Page_Size;
                  end if;
                  if Permissions.Is_User_Accessible then
                     Map.User_Size := Map.User_Size + Page_Size;
                  end if;
               end if;
            end if;
         end;

         Virt := Virt + Page_Size;
      end loop;
      Synchronization.Release (Map.Mutex);
      Locked := False;

      --  Nothing is freed, but access is only taken away once no core can
      --  still use what it had.
      Arch.MMU.Flush_TLBs
         (Map     => Get_Map_Table_Addr (Map),
          Addr    => Virtual_Start,
          Len     => Storage_Count (Virt - To_Integer (Virtual_Start)),
          Changed => Changed,
          Remote  => Map /= Kernel_Table);
      Release_Space (Map);
   exception
      when Constraint_Error =>
         declare
            pragma Suppress (All_Checks);
         begin
            if Locked then
               Synchronization.Release (Map.Mutex);
            end if;
            Release_Space (Map);
         end;
         Success := False;
   end Remap_Range;

   procedure Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Success       : out Boolean)
   is
      Virt    : Virtual_Address          := To_Integer (Virtual_Start);
      Final   : constant Virtual_Address := Virt + Virtual_Address (Length);
      First   : Virtual_Address;
      Addr    : Virtual_Address;
      Orig    : Virtual_Address;
      Perms   : Arch.MMU.Clean_Result;
      Frames  : Frame_Batch;
      Count   : Natural;
      Changed : Boolean;
      Locked  : Boolean := False;
   begin
      Seize_Space (Map);
      while Virt < Final loop
         First   := Virt;
         Count   := 0;
         Changed := False;
         Synchronization.Seize (Map.Mutex);
         Locked := True;
         while Virt < Final and Count < Batch_Size loop
            Get_Page (Map, Virt, False, Addr);

            declare
               Entry_Body : Unsigned_64
                  with Address => To_Address (Addr), Import;
            begin
               if Addr /= 0 and then Arch.MMU.Is_Entry_Present (Entry_Body)
               then
                  Changed := True;
                  Orig    := Arch.MMU.Clean_Entry (Entry_Body);
                  Perms   := Arch.MMU.Clean_Entry_Perms (Entry_Body);
                  if Perms.User_Flag then
                     Count := Count + 1;
                     Frames (Count) := Orig;
                  end if;
                  Entry_Body := Arch.MMU.Make_Not_Present (Entry_Body);
                  if Perms.Perms.Is_User_Accessible then
                     Map.User_Size := Map.User_Size - Page_Size;
                  end if;
               end if;
            end;
            Virt := Virt + Page_Size;
         end loop;
         Synchronization.Release (Map.Mutex);
         Locked := False;

         Finish_Batch (Map, First, Virt - First, Changed, Frames, Count);
      end loop;
      Release_Space (Map);
      Success := True;
   exception
      when Constraint_Error =>
         declare
            pragma Suppress (All_Checks);
         begin
            if Locked then
               Synchronization.Release (Map.Mutex);
            end if;
            Release_Space (Map);
         end;
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
         declare
            pragma Suppress (All_Checks);
         begin
            Synchronization.Release (Map.Mutex);
         end;
         Sz := 0;
   end Get_User_Mapped_Size;

   procedure Get_Statistics (Stats : out Virtual_Statistics) is
      Val1, Val2 : Memory.Size;
   begin
      Val1 := Global_Kernel_Usage;
      Val2 := Global_Table_Usage;
      Stats := (Val1, Val2, 0);
   end Get_Statistics;

   procedure Seize_Space (Map : Page_Table_Acc) is
   begin
      if Map /= Kernel_Table then
         Synchronization.Seize (Map.Space_Lock);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Seize_Space;

   procedure Release_Space (Map : Page_Table_Acc) is
   begin
      if Map /= Kernel_Table then
         Synchronization.Release (Map.Space_Lock);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Release_Space;

   procedure Finish_Batch
      (Map     : Page_Table_Acc;
       First   : Virtual_Address;
       Length  : Virtual_Address;
       Changed : Boolean;
       Frames  : Frame_Batch;
       Count   : Natural)
   is
   begin
      Arch.MMU.Flush_TLBs
         (Map     => Get_Map_Table_Addr (Map),
          Addr    => To_Address (First),
          Len     => Storage_Count (Length),
          Changed => Changed,
          Remote  => Map /= Kernel_Table);
      for I in 1 .. Count loop
         Physical.Free (size_t (Memory.Memory_Offset + Frames (I)));
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Finish_Batch;
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
      Target_Table : Virtual_Address := Memory.Null_Address;
   begin
      --  A level with nothing present has nothing to copy.
      Success := True;
      for I in Current_Level'Range loop
         if Arch.MMU.Is_Entry_Present (Current_Level (I)) then
            declare
               L : constant Unsigned_64 := Current_Level (I);
               A : constant Integer_Address := Arch.MMU.Clean_Entry (L);
               Next : PML
                  with Import, Address => To_Address (Memory_Offset + A);
            begin
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
                     --  The entries of a table all go to one table of the
                     --  target, which is only looked up for the first.
                     if Target_Table = Memory.Null_Address then
                        Addr := Idx_To_Addr (Idx_5, Idx_4, Idx_3, Idx_2, I);
                        Get_Page (Target, Addr, True, Addr2);
                        if Addr2 = Memory.Null_Address then
                           Success := False;
                           return;
                        end if;
                        Target_Table := Addr2 - Virtual_Address (I - 1) * 8;
                     else
                        Addr2 := Target_Table + Virtual_Address (I - 1) * 8;
                     end if;
                     Perms := Arch.MMU.Clean_Entry_Perms (L);
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
            end;
            if not Success then
               return;
            end if;
         end if;
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
                        Physical.Free (size_t (Memory_Offset + A));
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
