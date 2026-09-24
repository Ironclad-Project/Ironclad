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

with Panic;
with Synchronization; use Synchronization;
with Alignment;
with Memory.MMU;
with System; use System;

package body Memory.Physical with SPARK_Mode => Off is
   --  The physical memory allocator of Ironclad consists of two components.
   --  - A bitmap allocator that manages memory blocks directly of arbitrary
   --    size. It is slow and bulky but it takes care of every allocation size
   --    always.
   --  - An allocator pool/slab of one page objects for fast handing and
   --    freeing, to increase responsiveness and reduce load from the main
   --    bitmap.

   --  Information that the bitmap allocator keeps track of.
   Block_Size :         constant := Memory.MMU.Page_Size;
   Block_Free : constant Boolean := True;
   Block_Used : constant Boolean := False;
   type Bitmap is array (Unsigned_64 range <>) of Boolean with Pack;
   Total_Memory, Available_Memory, Free_Memory : Memory.Size;
   Block_Count      :              Unsigned_64 := 0;
   Bitmap_Length    :              Memory.Size := 0;
   Bitmap_Address   :          Virtual_Address := Null_Address;
   Bitmap_Last_Used :              Unsigned_64 := 0;
   Alloc_Mutex      : aliased Binary_Semaphore := Unlocked_Semaphore;

   --  Header of each memory allocation in the bitmap.
   type Allocation_Header is record
      Block_Count : Size;
   end record;

   --  Slab information for the 1 page object slab.
   Slab_Item_Count : constant := 10_000;
   type Page_Data      is array (1 .. Block_Size) of Unsigned_8;
   type Slab_Data      is array (1 .. Slab_Item_Count) of Page_Data;
   type Slab_Data_Acc  is access Slab_Data;
   type Slab_Stack     is array (1 .. Slab_Item_Count) of Unsigned_16;
   type Slab_Stack_Acc is access Slab_Stack;
   Slab_Init  : Boolean := False;
   Slab_Mutex : aliased Binary_Semaphore := Unlocked_Semaphore;
   Slab       : Slab_Data_Acc;
   Slab_Track : Slab_Stack_Acc;
   Slab_Idx   : Natural;

   procedure Init_Allocator (Memmap : Arch.Boot_Memory_Map) is
      package Align is new Alignment (Memory.Size);
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
      Total_Memory := Align.Align_Down (Total_Memory, Block_Size);

      --  Calculate what we will need for the bitmap, and find a hole for it.
      Block_Count   := Unsigned_64 (Total_Memory) / Block_Size;
      Bitmap_Length := Align.Align_Up (Size (Block_Count) / 8, Block_Size);
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
         Panic.Hard_Panic ("Could not allocate the bitmap");
      end if;

      --  Initialize and fill the bitmap.
      declare
         Bitmap_Body : Bitmap (0 .. Block_Count - 1) with Import;
         for Bitmap_Body'Address use To_Address (Bitmap_Address);
         Block_Start, Block_Length : Unsigned_64;
      begin
         for Item of Bitmap_Body loop
            Item := Block_Used;
         end loop;

         for E of Memmap loop
            if E.MemType = Arch.Memory_Free then
               if E.Start = To_Address (Bitmap_Address - Memory_Offset) then
                  Block_Start  := Unsigned_64 (To_Integer (Adjusted_Start));
                  Block_Length := Unsigned_64 (Adjusted_Length);
               else
                  Block_Start  := Unsigned_64 (To_Integer (E.Start));
                  Block_Length := Unsigned_64 (E.Length);
               end if;

               Block_Start := Block_Start / Block_Size;
               Block_Length := Block_Length / Block_Size;
               for I in 1 .. Block_Length loop
                  Bitmap_Body (Block_Start + I - 1) := Block_Free;
               end loop;
            end if;
         end loop;
      end;

      --  Initialize the slab.
      Slab := new Slab_Data;
      Slab_Track := new Slab_Stack;
      for I in Slab_Track'Range loop
         Slab_Track (I) := Unsigned_16 (I);
      end loop;
      Slab_Idx := Slab'Last;
      Slab_Init := True;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception initializing the allocator");
   end Init_Allocator;
   ----------------------------------------------------------------------------
   procedure Alloc (Size : size_t; Result : out Memory.Virtual_Address) is
      Success : Boolean;
   begin
      User_Alloc (Result, Unsigned_64 (Size), Success);
      if not Success then
         Panic.Hard_Panic ("Exhausted memory (OOM)");
      end if;
   end Alloc;

   procedure User_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean)
   is
      pragma SPARK_Mode (Off);
      package Align is new Alignment (Memory.Size);

      Bitmap_Body : Bitmap (0 .. Block_Count - 1)
         with Import, Address => To_Address (Bitmap_Address);

      First_Found, Found_Count : Unsigned_64 := 0;
      Sz, Blocks_To_Allocate   : Memory.Size;
   begin
      --  Calculate how many blocks to allocate.
      Sz := Align.Align_Up (Memory.Size (Size), Block_Size);

      --  If one block or below, lets use the faster stack.
      if Slab_Init and then Sz = Block_Size then
         Synchronization.Seize (Slab_Mutex);
         if Slab_Idx /= 0 then
            Addr :=
               To_Integer (Slab (Natural (Slab_Track (Slab_Idx)))'Address);
            Slab_Idx := Slab_Idx - 1;
            Success := True;
            Synchronization.Release (Slab_Mutex);
            return;
         else
            Synchronization.Release (Slab_Mutex);
         end if;
      end if;

      --  Search for contiguous blocks, as many as needed.
      Blocks_To_Allocate := (Sz / Block_Size) + 1;
      Synchronization.Seize (Alloc_Mutex);
   <<Search_Blocks>>
      for I in Bitmap_Last_Used .. Bitmap_Body'Last loop
         if Bitmap_Body (I) = Block_Free then
            if I /= First_Found + Found_Count then
               First_Found := I;
               Found_Count := 1;
            else
               Found_Count := Found_Count + 1;
            end if;

            if Blocks_To_Allocate = Memory.Size (Found_Count) then
               goto Fill_Bitmap;
            end if;
         end if;
      end loop;

      --  Rewind to the beginning if memory was not found and we did not do
      --  it already.
      if Bitmap_Last_Used /= Bitmap_Body'First then
         Bitmap_Last_Used := Bitmap_Body'First;
         goto Search_Blocks;
      end if;

      --  Handle OOM.
      Synchronization.Release (Alloc_Mutex);
      Addr := 0;
      Success := False;
      return;

   <<Fill_Bitmap>>
      for I in 1 .. Blocks_To_Allocate loop
         Bitmap_Body (First_Found + Unsigned_64 (I - 1)) := Block_Used;
      end loop;

      --  Set statistic, global variables, the allocation header and return.
      Bitmap_Last_Used := First_Found + Unsigned_64 (Blocks_To_Allocate) - 1;
      Free_Memory      := Free_Memory - (Blocks_To_Allocate * Block_Size);
      Synchronization.Release (Alloc_Mutex);

      declare
         Ret : constant Virtual_Address :=
            Virtual_Address (First_Found * Block_Size) + Memory_Offset;
         Header : Allocation_Header with Import, Address => To_Address (Ret);
      begin
         Header := (Block_Count => Blocks_To_Allocate);
         Addr := Ret + Block_Size;
         Success := True;
      end;
   exception
      when Constraint_Error =>
         Addr    := 0;
         Success := False;
   end User_Alloc;

   procedure Lower_Half_Alloc
      (Addr    : out Memory.Virtual_Address;
       Size    : Unsigned_64;
       Success : out Boolean)
   is
   begin
      --  Alloc_Pgs allocates from the bottom of memory, so we can just wrap.
      User_Alloc (Addr, Size, Success);
      Success :=
         Addr /= 0 and then
         Addr + Virtual_Address (Size) <= 16#100000000# + Memory.Memory_Offset;
   end Lower_Half_Alloc;

   procedure Free (Address : size_t) is
      pragma SPARK_Mode (Off);

      Real_Address : Virtual_Address := Virtual_Address (Address);
      Real_Block   : Unsigned_64;
      Bitmap_Body  : Bitmap (0 .. Block_Count - 1)
         with Address => To_Address (Bitmap_Address), Import;
   begin
      --  Ensure the address is in the higher half and not null.
      if Real_Address = 0 then
         return;
      elsif Real_Address < Memory_Offset then
         Real_Address := Real_Address + Memory_Offset;
      end if;

      if Slab_Init then
         if Real_Address >= To_Integer (Slab (Slab'First)'Address) and
            Real_Address <= To_Integer (Slab (Slab'Last)'Address)
         then
            Real_Block :=
               (Unsigned_64 (Real_Address -
                To_Integer (Slab (Slab'First)'Address)) / Block_Size) + 1;

            Synchronization.Seize (Slab_Mutex);
            Slab_Idx := Slab_Idx + 1;
            Slab_Track (Slab_Idx) := Unsigned_16 (Real_Block);
            Synchronization.Release (Slab_Mutex);
            return;
         end if;
      end if;

      --  Free the blocks in the header.
      declare
         IAddr  : constant Integer_Address := Real_Address - Block_Size;
         SAddr  : constant  System.Address := To_Address (IAddr);
         Header : Allocation_Header with Import, Address => SAddr;
      begin
         Synchronization.Seize (Alloc_Mutex);

         Real_Block  := Unsigned_64 (IAddr - Memory_Offset) / Block_Size;
         Free_Memory := Free_Memory + (Header.Block_Count * Block_Size);
         for I in 1 .. Header.Block_Count loop
            Bitmap_Body (Real_Block + Unsigned_64 (I - 1)) := Block_Free;
         end loop;

         --  Search from the freed blocks next, so that memory is reused before
         --  memory that was never touched.
         if Real_Block < Bitmap_Last_Used then
            Bitmap_Last_Used := Real_Block;
         end if;

         Synchronization.Release (Alloc_Mutex);
      end;
   exception
      when Constraint_Error =>
         null;
   end Free;
   ----------------------------------------------------------------------------
   procedure Get_Statistics (Stats : out Statistics) is
   begin
      Synchronization.Seize (Alloc_Mutex);
      Stats :=
         (Total     => Total_Memory,
          Available => Available_Memory,
          Free      => Free_Memory);
      Synchronization.Release (Alloc_Mutex);
   end Get_Statistics;
end Memory.Physical;
