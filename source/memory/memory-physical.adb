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
   Block_Size :         constant := Memory.MMU.Page_Size;
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
         Panic.Hard_Panic ("Could not allocate the bitmap");
      end if;

      --  Initialize and fill the bitmap.
      declare
         Bitmap_Body : Bitmap (0 .. Block_Count - 1) with Import;
         for Bitmap_Body'Address use To_Address (Bitmap_Address);
         Index : Unsigned_64 := 0;
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

               while Index < Block_Length loop
                  Bitmap_Body ((Block_Start + Index) / Block_Size) :=
                     Block_Free;
                  Index := Index + Block_Size;
               end loop;
            end if;
         end loop;
      end;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception initializing the allocator");
   end Init_Allocator;
   ----------------------------------------------------------------------------
   procedure Alloc
      (Sz : Interfaces.C.size_t; Result : out Memory.Virtual_Address)
   is
      Size : Interfaces.C.size_t := Sz;
   begin
      --  Check the specific GNAT semantics.
      if Size = Interfaces.C.size_t'Last then
         Panic.Hard_Panic ("size_t'Last passed to 'new'");
      elsif Size = 0 then
         Size := 1;
      end if;

      Alloc_Pgs (Size, Result);
      if Result = 0 then
         Panic.Hard_Panic ("Exhausted memory (OOM)");
      end if;
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

      Free_Pgs (size_t (Real_Address));
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
      Alloc_Pgs (size_t (Size), Addr);
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
      Alloc_Pgs (size_t (Size), Addr);
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
      Synchronization.Seize (Alloc_Mutex);
      Stats :=
         (Total     => Total_Memory,
          Available => Available_Memory,
          Free      => Free_Memory);
      Synchronization.Release (Alloc_Mutex);
   end Get_Statistics;
   ----------------------------------------------------------------------------
   procedure Alloc_Pgs
      (Sz     : Interfaces.C.size_t;
       Result : out Memory.Virtual_Address)
   is
      pragma SPARK_Mode (Off);

      package Align is new Alignment (Memory.Size);

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
      Synchronization.Seize (Alloc_Mutex);
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

      --  Rewind to the beginning if memory was not found and we did not do
      --  it already.
      if Bitmap_Last_Used /= Bitmap_Body'First then
         Bitmap_Last_Used := Bitmap_Body'First;
         goto Search_Blocks;
      end if;

      --  Handle OOM.
      Synchronization.Release (Alloc_Mutex);
      Result := 0;
      return;

   <<Fill_Bitmap>>
      for I in 1 .. Blocks_To_Allocate loop
         Bitmap_Body (First_Found_Index + Unsigned_64 (I - 1)) := Block_Used;
      end loop;

      --  Set statistic, global variables, the allocation header and return.
      Bitmap_Last_Used := First_Found_Index;
      Free_Memory      := Free_Memory - (Blocks_To_Allocate * Block_Size);
      Synchronization.Release (Alloc_Mutex);

      --  If we are doing alloc only, we only have to return the allocated
      --  address, else, we have to actually fill the header and checksums.
      declare
         Ret : constant Virtual_Address :=
            Virtual_Address (First_Found_Index * Block_Size) + Memory_Offset;
         Header : Allocation_Header with Import, Address => To_Address (Ret);
      begin
         Header := (Block_Count => Blocks_To_Allocate);
         Result := Ret + Block_Size;
      end;
   exception
      when Constraint_Error =>
         Result := 0;
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
         Synchronization.Seize (Alloc_Mutex);

         Real_Block  := Unsigned_64 (IAddr - Memory_Offset) / Block_Size;
         Free_Memory := Free_Memory + (Header.Block_Count * Block_Size);
         for I in 1 .. Header.Block_Count loop
            Bitmap_Body (Real_Block + Unsigned_64 (I - 1)) := Block_Free;
         end loop;

         Synchronization.Release (Alloc_Mutex);
      end;
   exception
      when Constraint_Error =>
         null;
   end Free_Pgs;
end Memory.Physical;
