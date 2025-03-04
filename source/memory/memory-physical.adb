--  memory-physical.adb: Physical memory allocator and other utils.
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

with Lib.Panic;
with Lib.Synchronization; use Lib.Synchronization;
with Lib.Alignment;
with Arch.MMU;
with System; use System;
with Lib.Messages;
with Userland.OOM_Failure;

package body Memory.Physical is
   Block_Size :         constant := Arch.MMU.Page_Size;
   Block_Free : constant Boolean := True;
   Block_Used : constant Boolean := False;
   type Bitmap is array (Unsigned_64 range <>) of Boolean with Pack;

   --  Information that the allocator keeps track of.
   Total_Memory, Available_Memory, Free_Memory : Memory.Size;
   Block_Count      :              Unsigned_64 := 0;
   Bitmap_Length    :              Memory.Size := 0;
   Bitmap_Address   :          Virtual_Address := Null_Address;
   Bitmap_Last_Used :              Unsigned_64 := 0;
   Alloc_Mutex      : aliased Binary_Semaphore := Unlocked_Semaphore;

   --  Header of each memory allocation.
   type Allocation_Header is record
      Block_Count : Size;
   end record;

   --  We have slabs built on top of the default allocator. These slabs
   --  are all one page in size and individually locked.
   type Page_Data is array (0 .. (4096 * 100) - 1) of Unsigned_8;
   type Slab (Entity_Size : Natural) is record
      Lock          : aliased Binary_Semaphore;
      Element_Count : Natural;
      Element_Bump  : Natural;
      Pool          : Page_Data;
   end record;
   type Slab_Acc is access Slab;

   type Slab_Arr     is array (1 .. 9) of Slab_Acc;
   type Slab_Arr_Acc is access Slab_Arr;

   Slab_Init : Boolean := False;
   Slabs     : Slab_Arr_Acc;

   procedure Init_Allocator (Memmap : Arch.Boot_Memory_Map) is
      Adjusted_Length : Storage_Count  := 0;
      Adjusted_Start  : System.Address := System.Null_Address;
   begin
      for E of Memmap loop
         if E.MemType = Arch.Memory_Free then
            Free_Memory := Free_Memory + Size (E.Length);
         end if;
      end loop;

      Available_Memory := Free_Memory;
      Total_Memory     := Size (To_Integer (Memmap (Memmap'Last).Start +
                                Memmap (Memmap'Last).Length));

      --  Calculate what we will need for the bitmap, and find a hole for it.
      Block_Count   := Unsigned_64 (Total_Memory) / Block_Size;
      Bitmap_Length := Size (Block_Count) / 8;
      for E of Memmap loop
         if E.MemType = Arch.Memory_Free and Size (E.Length) > Bitmap_Length
         then
            Bitmap_Address  := To_Integer (E.Start) + Memory_Offset;
            Adjusted_Length := E.Length - Storage_Count (Bitmap_Length);
            Adjusted_Start  := E.Start + Storage_Count (Bitmap_Length);
            Free_Memory     := Free_Memory - Bitmap_Length;
            exit;
         end if;
      end loop;
      if Bitmap_Address = Null_Address then
         Lib.Panic.Hard_Panic ("Could not allocate the bitmap");
      end if;

      --  Initialize and fill the bitmap.
      declare
         Bitmap_Body : Bitmap (0 .. Block_Count - 1) with Import;
         for Bitmap_Body'Address use To_Address (Bitmap_Address);
         Index : Unsigned_64 := 0;
         Block_Value : Boolean;
         Block_Start, Block_Length : Unsigned_64;
      begin
         for Item of Bitmap_Body loop
            Item := Block_Used;
         end loop;

         for E of Memmap loop
            if E.MemType = Arch.Memory_Free then
               Block_Value := Block_Free;
            else
               Block_Value := Block_Used;
            end if;

            if E.Start = To_Address (Bitmap_Address - Memory_Offset) then
               Block_Start  := Unsigned_64 (To_Integer (Adjusted_Start));
               Block_Length := Unsigned_64 (Adjusted_Length);
            else
               Block_Start  := Unsigned_64 (To_Integer (E.Start));
               Block_Length := Unsigned_64 (E.Length);
            end if;

            while Index < Block_Length loop
               Bitmap_Body ((Block_Start + Index) / Block_Size) := Block_Value;
               Index := Index + Block_Size;
            end loop;
         end loop;
      end;

      --  Allocate the slabs.
      Slabs := new Slab_Arr'
         [1 => new Slab'(0008, Unlocked_Semaphore, 0, 0, Pool => <>),
          2 => new Slab'(0016, Unlocked_Semaphore, 0, 0, Pool => <>),
          3 => new Slab'(0032, Unlocked_Semaphore, 0, 0, Pool => <>),
          4 => new Slab'(0064, Unlocked_Semaphore, 0, 0, Pool => <>),
          5 => new Slab'(0128, Unlocked_Semaphore, 0, 0, Pool => <>),
          6 => new Slab'(0256, Unlocked_Semaphore, 0, 0, Pool => <>),
          7 => new Slab'(0512, Unlocked_Semaphore, 0, 0, Pool => <>),
          8 => new Slab'(1024, Unlocked_Semaphore, 0, 0, Pool => <>),
          9 => new Slab'(2048, Unlocked_Semaphore, 0, 0, Pool => <>)];
      Slab_Init := True;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception initializing the allocator");
   end Init_Allocator;
   ----------------------------------------------------------------------------
   function Alloc (Sz : Interfaces.C.size_t) return Virtual_Address is
      Size   : Interfaces.C.size_t := Sz;
      Result : Virtual_Address;
      I      : Natural;
   begin
      --  Check the specific GNAT semantics.
      if Size = Interfaces.C.size_t'Last then
         Lib.Panic.Hard_Panic ("size_t'Last passed to 'new'");
      elsif Size = 0 then
         Size := 1;
      end if;

      if Slab_Init then
         case Unsigned_32 (CLZ (Unsigned_64 (Size))) xor 63 is
            when 0 .. 2 => I := 1;
            when 3  => I := 2;
            when 4  => I := 3;
            when 5  => I := 4;
            when 6  => I := 5;
            when 7  => I := 6;
            when 8  => I := 7;
            when 9  => I := 8;
            when 10 => I := 9;
            when others => goto Default_Alloc;
         end case;

      <<Next_Slab_Try>>
         Lib.Synchronization.Seize (Slabs (I).Lock);
         if Slabs (I).Element_Bump >=
            Slabs (I).Pool'Length / Slabs (I).Entity_Size
         then
            Lib.Synchronization.Release (Slabs (I).Lock);
            if I = Slabs'Last then
               goto Default_Alloc;
            else
               I := I + 1;
               goto Next_Slab_Try;
            end if;
         end if;

         Result :=
            To_Integer (Slabs (I).Pool (Slabs (I).Element_Bump *
                        Slabs (I).Entity_Size)'Address);
         Slabs (I).Element_Bump  := Slabs (I).Element_Bump  + 1;
         Slabs (I).Element_Count := Slabs (I).Element_Count + 1;
         Lib.Synchronization.Release (Slabs (I).Lock);
         return Result;
      end if;

   <<Default_Alloc>>
      Result := Alloc_Pgs (Size);
      if Result = 0 then
         Lib.Messages.Put_Line ("Kernel OOM imminent, running handlers");
         Userland.OOM_Failure.Handle_Failure;
         Result := Alloc_Pgs (Size);
         if Result = 0 then
            Lib.Panic.Hard_Panic ("Exhausted memory (OOM)");
         end if;
      end if;
      return Result;
   exception
      when Constraint_Error =>
         return 0;
   end Alloc;

   procedure Free (Address : Interfaces.C.size_t) is
      Real_Address : Virtual_Address := Virtual_Address (Address);
   begin
      --  Ensure the address is in the higher half and not null.
      if Real_Address = 0 then
         return;
      elsif Real_Address < Memory_Offset then
         Real_Address := Real_Address + Memory_Offset;
      end if;

      if Slab_Init then
         for S of Slabs.all loop
            if Real_Address >= To_Integer (S.Pool (S.Pool'First)'Address) and
               Real_Address <= To_Integer (S.Pool (S.Pool'Last)'Address)
            then
               Lib.Synchronization.Seize (S.Lock);
               if S.Element_Count = 1 then
                  S.Element_Count := 0;
                  S.Element_Bump  := 0;
               else
                  S.Element_Count := S.Element_Count - 1;
               end if;
               Lib.Synchronization.Release (S.Lock);
               return;
            end if;
         end loop;
      end if;

      Free_Pgs (size_t (Real_Address));
   exception
      when Constraint_Error =>
         null;
   end Free;
   ----------------------------------------------------------------------------
   procedure Lower_Half_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean)
   is
   begin
      --  Alloc_Pgs allocates from the bottom of memory, so if we can just wrap
      --  the function with a simple sanity check.
      Addr    := Alloc_Pgs (size_t (Size));
      Success := Addr /= 0 and Addr + Virtual_Address (Size) <= (16#100000000#
       + Memory.Memory_Offset);
   end Lower_Half_Alloc;

   procedure Lower_Half_Free (Addr : Memory.Virtual_Address) is
      Real_Address : Virtual_Address := Addr;
   begin
      --  Ensure the address is in the higher half and not null.
      if Real_Address = 0 then
         return;
      elsif Real_Address < Memory_Offset then
         Real_Address := Real_Address + Memory_Offset;
      end if;

      Free_Pgs (size_t (Real_Address));
   end Lower_Half_Free;
   ----------------------------------------------------------------------------
   procedure User_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean)
   is
   begin
      Addr    := Alloc_Pgs (size_t (Size));
      Success := Addr /= 0;
   end User_Alloc;

   procedure User_Free (Addr : Memory.Virtual_Address) is
      Real_Address : Virtual_Address := Addr;
   begin
      --  Ensure the address is in the higher half and not null.
      if Real_Address = 0 then
         return;
      elsif Real_Address < Memory_Offset then
         Real_Address := Real_Address + Memory_Offset;
      end if;

      Free_Pgs (size_t (Real_Address));
   end User_Free;
   ----------------------------------------------------------------------------
   procedure Get_Statistics (Stats : out Statistics) is
   begin
      Lib.Synchronization.Seize (Alloc_Mutex);
      Stats :=
         (Total     => Total_Memory,
          Available => Available_Memory,
          Free      => Free_Memory);
      Lib.Synchronization.Release (Alloc_Mutex);
   end Get_Statistics;
   ----------------------------------------------------------------------------
   function Alloc_Pgs (Sz : Interfaces.C.size_t) return Memory.Virtual_Address
   is
      pragma SPARK_Mode (Off);

      package Align is new Lib.Alignment (Memory.Size);

      Bitmap_Body : Bitmap (0 .. Block_Count - 1) with Import;
      for Bitmap_Body'Address use To_Address (Bitmap_Address);

      First_Found_Index  : Unsigned_64 := 0;
      Found_Count        : Unsigned_64 := 0;
      Size               : Memory.Size := Memory.Size (Sz);
      Blocks_To_Allocate : Memory.Size;
   begin
      --  Calculate how many blocks to allocate, if we are doing alloconly, we
      --  do not need to use blocks for headers and checksums.
      Size               := Align.Align_Up (Size, Block_Size);
      Blocks_To_Allocate := (Size / Block_Size) + 1;

      --  Search for contiguous blocks, as many as needed.
      Lib.Synchronization.Seize (Alloc_Mutex);
   <<Search_Blocks>>
      for I in Bitmap_Last_Used .. Block_Count - 1 loop
         if Bitmap_Body (I) = Block_Free then
            if I /= First_Found_Index + Found_Count then
               First_Found_Index := I;
               Found_Count       := 1;
            else
               Found_Count := Found_Count + 1;
            end if;

            if Blocks_To_Allocate = Memory.Size (Found_Count) then
               goto Fill_Bitmap;
            end if;
         end if;
      end loop;

      --  Rewind to the beggining if memory was not found and we did not do
      --  it already.
      if Bitmap_Last_Used /= Bitmap_Body'First then
         Bitmap_Last_Used := Bitmap_Body'First;
         goto Search_Blocks;
      end if;

      --  Handle OOM.
      Lib.Synchronization.Release (Alloc_Mutex);
      return 0;

   <<Fill_Bitmap>>
      for I in 1 .. Blocks_To_Allocate loop
         Bitmap_Body (First_Found_Index + Unsigned_64 (I - 1)) := Block_Used;
      end loop;

      --  Set statistic, global variables, the allocation header and return.
      Bitmap_Last_Used := First_Found_Index;
      Free_Memory      := Free_Memory - (Blocks_To_Allocate * Block_Size);
      Lib.Synchronization.Release (Alloc_Mutex);

      --  If we are doing alloc only, we only have to return the allocated
      --  address, else, we have to actually fill the header and checksums.
      declare
         Ret : constant Virtual_Address :=
            Virtual_Address (First_Found_Index * Block_Size) + Memory_Offset;
         Header : Allocation_Header with Import, Address => To_Address (Ret);
      begin
         Header := (Block_Count => Blocks_To_Allocate);
         return Ret + Block_Size;
      end;
   exception
      when Constraint_Error =>
         return 0;
   end Alloc_Pgs;

   procedure Free_Pgs (Address : Interfaces.C.size_t) is
      pragma SPARK_Mode (Off);

      Real_Address : constant Virtual_Address := Virtual_Address (Address);
      Real_Block   : Unsigned_64;
      Bitmap_Body  : Bitmap (0 .. Block_Count - 1)
         with Address => To_Address (Bitmap_Address), Import;
   begin
      --  Free the blocks in the header.
      declare
         IAddr  : constant Integer_Address := Real_Address - Block_Size;
         SAddr  : constant  System.Address := To_Address (IAddr);
         Header : Allocation_Header with Import, Address => SAddr;
      begin
         Lib.Synchronization.Seize (Alloc_Mutex);

         Real_Block  := Unsigned_64 (IAddr - Memory_Offset) / Block_Size;
         Free_Memory := Free_Memory + (Header.Block_Count * Block_Size);
         for I in 1 .. Header.Block_Count loop
            Bitmap_Body (Real_Block + Unsigned_64 (I - 1)) := Block_Free;
         end loop;

         Lib.Synchronization.Release (Alloc_Mutex);
      end;
   exception
      when Constraint_Error =>
         null;
   end Free_Pgs;
end Memory.Physical;
