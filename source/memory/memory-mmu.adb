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
      Arch.MMU.Get_Load_Addr (Tmp, Success);
      if not Success then
         return;
      end if;
      Phys := To_Integer (Tmp);

      --  Initialize the kernel pagemap.
      MMU.Kernel_Table := new Page_Table'
         (PML4_Level => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          User_Size  => 0);

      --  Preallocate the higher half PML4, so when we clone the kernel memmap,
      --  we can just blindly copy the entries and share mapping between all
      --  kernel instances.
      for E of Kernel_Table.PML4_Level (257 .. 512) loop
         declare
            New_Entry      : constant PML4_Acc := new PML4'(others => 0);
            New_Entry_Addr : constant Physical_Address :=
               To_Integer (New_Entry.all'Address) - Memory_Offset;
         begin
            Global_Table_Usage := Global_Table_Usage + (PML4'Size / 8);
            E := Arch.MMU.Construct_Level (To_Address (New_Entry_Addr));
         end;
      end loop;

      --  Map the memmap memory to the memory window.
      for E of Memmap loop
         Map_Range
            (Map            => Kernel_Table,
             Physical_Start => To_Address (To_Integer (E.Start)),
             Virtual_Start  => To_Address (To_Integer (E.Start) +
                                           Memory_Offset),
             Length         => Storage_Offset (E.Length),
             Permissions    => NX_Flags,
             Caching        => Arch.MMU.Write_Back,
             Success        => Success);
         if not Success then
            return;
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
         (PML4_Level => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          User_Size  => 0);
      New_Map.PML4_Level (257 .. 512) := Kernel_Table.PML4_Level (257 .. 512);
      Synchronization.Release (Kernel_Table.Mutex);
   exception
      when Constraint_Error =>
         New_Map := null;
   end Create_Table;

   procedure Fork_Table (Map : Page_Table_Acc; Forked : out Page_Table_Acc) is
      type Arr is array (1 .. Page_Size) of Unsigned_8;
      Addr, Addr2, Addr3 : Virtual_Address;
      Perms : Arch.MMU.Clean_Result;
      Success : Boolean;
   begin
      Forked := new Page_Table'
         (PML4_Level => [others => 0],
          Mutex      => Synchronization.Unlocked_Semaphore,
          User_Size  => 0);

      Synchronization.Seize (Map.Mutex);

      --  Clone the higher half, which is the same in all maps, and user size.
      Forked.PML4_Level (257 .. 512) := Map.PML4_Level (257 .. 512);
      Forked.User_Size := Map.User_Size;

      --  Go thru the lower half entries and copy.
      for I3 in Map.PML4_Level (1 .. 256)'Range loop
         declare
            L3   : constant Unsigned_64 := Map.PML4_Level (I3);
            A3   : constant Integer_Address :=
               Arch.MMU.Clean_Entry (Map.PML4_Level (I3));
            PML3 : PML4
               with Import, Address => To_Address (Memory_Offset + A3);
         begin
            if Arch.MMU.Is_Entry_Present (L3) then
               for I2 in PML3'Range loop
                  declare
                     L2   : constant Unsigned_64 := PML3 (I2);
                     A2   : constant Integer_Address :=
                        Arch.MMU.Clean_Entry (PML3 (I2));
                     PML2 : PML4 with Import,
                        Address => To_Address (Memory_Offset + A2);
                  begin
                     if Arch.MMU.Is_Entry_Present (L2) then
                        for I1 in PML2'Range loop
                           declare
                              L1  : constant Unsigned_64 := PML2 (I1);
                              A1  : constant Integer_Address :=
                                 Arch.MMU.Clean_Entry (L1);
                              PML1 : PML4 with Import,
                                 Address => To_Address (Memory_Offset + A1);
                           begin
                              if Arch.MMU.Is_Entry_Present (L1) then
                                 for I0 in PML1'Range loop
                                    declare
                                       L0  : constant Unsigned_64 := PML1 (I0);
                                       A0  : constant Integer_Address :=
                                          Arch.MMU.Clean_Entry (L0);
                                    begin
                                       Addr := Idx_To_Addr (I3, I2, I1, I0);
                                       Perms :=
                                          Arch.MMU.Clean_Entry_Perms (L0);
                                       Get_Page (Forked, Addr, True, Addr2);
                                       declare
                                          Res : Unsigned_64 with Import,
                                             Address => To_Address (Addr2);
                                       begin
                                          if Perms.User_Flag then
                                             Memory.Physical.User_Alloc
                                                (Addr    => Addr3,
                                                 Size    => Page_Size,
                                                 Success => Success);
                                             if not Success then
                                                goto Error_Cleanup;
                                             end if;
                                             declare
                                                Allocated : Arr with Import,
                                                   Address =>
                                                      To_Address (Addr3);
                                                Orig : Arr with Import,
                                                   Address => To_Address
                                                   (Memory.Memory_Offset + A0);
                                             begin
                                                Allocated := Orig;
                                             end;
                                             Res := Arch.MMU.Construct_Entry
                                                (To_Address (Addr3 -
                                                 Memory.Memory_Offset),
                                                 Perms.Perms, Perms.Caching,
                                                 True);
                                          else
                                             Res := L0;
                                          end if;
                                       end;
                                    end;
                                 end loop;
                              end if;
                           end;
                        end loop;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

      Synchronization.Release (Map.Mutex);
      return;

   <<Error_Cleanup>>
      F (Forked);
   exception
      when Constraint_Error =>
         Forked := null;
   end Fork_Table;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
      Discard    : Memory.Size;
      Perms      : Arch.MMU.Clean_Result;
      PML4_Sz    : constant Memory.Size := PML4'Size / 8;
   begin
      Synchronization.Seize (Map.Mutex);
      for L3 of Map.PML4_Level (1 .. 256) loop
         declare
            A3   : constant Integer_Address := Arch.MMU.Clean_Entry (L3);
            PML3 : PML4
               with Import, Address => To_Address (Memory_Offset + A3);
         begin
            if Arch.MMU.Is_Entry_Present (L3) then
               for L2 of PML3 loop
                  declare
                     A2   : constant Integer_Address :=
                        Arch.MMU.Clean_Entry (L2);
                     PML2 : PML4 with Import,
                        Address => To_Address (Memory_Offset + A2);
                  begin
                     if Arch.MMU.Is_Entry_Present (L2) then
                        for L1 of PML2 loop
                           declare
                              A1   : constant Integer_Address :=
                                 Arch.MMU.Clean_Entry (L1);
                              PML1 : PML4 with Import,
                                 Address => To_Address (Memory_Offset + A1);
                           begin
                              if Arch.MMU.Is_Entry_Present (L1) then
                                 for L0 of PML1 loop
                                    if Arch.MMU.Is_Entry_Present (L0) then
                                       Perms :=
                                          Arch.MMU.Clean_Entry_Perms (L0);
                                       if Perms.User_Flag then
                                          Physical.User_Free
                                             (Memory.Memory_Offset +
                                              Arch.MMU.Clean_Entry (L0));
                                       end if;
                                    end if;
                                 end loop;
                                 Global_Table_Usage :=
                                    Global_Table_Usage - PML4_Sz;
                                 Memory.Physical.Free
                                    (Interfaces.C.size_t (A1));
                              end if;
                           end;
                        end loop;
                        Global_Table_Usage := Global_Table_Usage - PML4_Sz;
                        Memory.Physical.Free (Interfaces.C.size_t (A2));
                     end if;
                  end;
               end loop;
               Global_Table_Usage := Global_Table_Usage - PML4_Sz;
               Memory.Physical.Free (Interfaces.C.size_t (A3));
            end if;
         end;
      end loop;
      F (Map);
   exception
      when Constraint_Error =>
         return;
   end Destroy_Table;

   function Make_Active (Map : Page_Table_Acc) return Boolean is
      Success : Boolean;
   begin
      Arch.MMU.Set_Current_Table
         (To_Address (To_Integer (Map.PML4_Level'Address) - Memory_Offset),
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

      while Virt < Final loop
         Synchronization.Seize (Map.Mutex);
         Get_Page (Map, Virt, False, Page_Addr);
         Synchronization.Release (Map.Mutex);
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
            elsif Page_Addr = 0                                      or else
                  (Is_Mapped          and not Arch.MMU.Is_Entry_Present (Page))
                     or else
                  (Is_User_Accessible and not Perms.Is_User_Accessible) or else
                  (Is_Readable       and not Perms.Can_Read) or else
                  (Is_Writeable       and not Perms.Can_Write) or else
                  (Is_Executable      and not Perms.Can_Execute)
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
      return To_Address (To_Integer (Map.PML4_Level'Address) - Memory_Offset);
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
               New_Entry      : constant PML4_Acc := new PML4'(others => 0);
               New_Entry_Addr : constant Physical_Address :=
                  To_Integer (New_Entry.all'Address) - Memory_Offset;
            begin
               Global_Table_Usage := Global_Table_Usage + (PML4'Size / 8);
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
      PML4_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 39), 39);
      PML3_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 30), 30);
      PML2_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 21), 21);
      PML1_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 12), 12);
      Addr4, Addr3, Addr2, Addr1 : Physical_Address := Memory.Null_Address;
   begin
      --  Find the entries.
      Addr4 := To_Integer (Map.PML4_Level'Address) - Memory_Offset;
      Get_Next_Level (Addr4, PML4_Entry, Allocate, Addr3);
      if Addr3 = Memory.Null_Address then
         goto Error_Return;
      end if;
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

   function Idx_To_Addr (Idx_4, Idx_3, Idx_2, Idx_1 : Positive)
      return Integer_Address
   is
   begin
      return
         (Integer_Address (Idx_4) - 1) * 16#8000000000# +
         (Integer_Address (Idx_3) - 1) * 16#0040000000# +
         (Integer_Address (Idx_2) - 1) * 16#0000200000# +
         (Integer_Address (Idx_1) - 1) * 16#0000001000#;
   end Idx_To_Addr;
end Memory.MMU;
