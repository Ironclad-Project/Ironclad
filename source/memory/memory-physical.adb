--  memory-physical.adb: Physical memory allocator and other utils.
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

with Interfaces; use Interfaces;
with Lib.Panic;
with Lib.Synchronization; use Lib.Synchronization;
with Lib.Alignment;
with Arch.MMU;
with System; use System;
with Lib.Messages;
with Config;
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
      Signature   : Size;
   end record;

   function Calculate_Signature (Count : Size) return Size is
   begin
      return Count xor 2#10001011101010#;
   end Calculate_Signature;

   procedure Init_Allocator (Memmap : Arch.Boot_Memory_Map) is
      Adjusted_Length : Storage_Count  := 0;
      Adjusted_Start  : System.Address := System.Null_Address;
   begin
      for E of Memmap loop
         if E.MemType = Arch.Memory_Free then
            Free_Memory := Free_Memory + Size (E.Length);
         end if;
      end loop;
      declare
         Sta : constant Size := Size (To_Integer (Memmap (Memmap'Last).Start));
         Len : constant Size := Size (Memmap (Memmap'Last).Length);
      begin
         Available_Memory := Free_Memory;
         Total_Memory     := Sta + Len;
      end;

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
   end Init_Allocator;

   function Alloc (Sz : Interfaces.C.size_t) return Virtual_Address is
      package Align is new Lib.Alignment (Memory.Size);

      Bitmap_Body : Bitmap (0 .. Block_Count - 1) with Import;
      for Bitmap_Body'Address use To_Address (Bitmap_Address);

      Did_OOM_Once       :     Boolean := False;
      First_Found_Index  : Unsigned_64 := 0;
      Found_Count        : Unsigned_64 := 0;
      Size               : Memory.Size := Memory.Size (Sz);
      Blocks_To_Allocate : Memory.Size;
   begin
      --  Check the specific GNAT semantics.
      if Size = Memory.Size (Interfaces.C.size_t'Last) then
         Lib.Panic.Hard_Panic ("Storage error (size_t'Last passed)");
      elsif Size = 0 then
         Size := 1;
      end if;

      --  Calculate how many blocks to allocate, if we are doing alloconly, we
      --  do not need to use blocks for headers and checksums.
      Size               := Align.Align_Up (Size, Block_Size);
      Blocks_To_Allocate := Size / Block_Size;
      if not Config.Support_Alloc_Only then
         Blocks_To_Allocate := Blocks_To_Allocate + 1;
      end if;

      --  Search for contiguous blocks, as many as needed.
      Lib.Synchronization.Seize (Alloc_Mutex);
   <<Search_Blocks>>
      for I in Bitmap_Last_Used + 1 .. Block_Count - 1 loop
         if Bitmap_Body (I) = Block_Free then
            if First_Found_Index = 0 or I /= First_Found_Index + Found_Count
            then
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
      if Did_OOM_Once then
         Lib.Panic.Hard_Panic ("Exhausted memory (OOM)");
      else
         Lib.Messages.Put_Line ("OOM imminent, running handlers");
         Did_OOM_Once := True;
         Lib.Synchronization.Release (Alloc_Mutex);
         Userland.OOM_Failure.Handle_Failure;
         Lib.Synchronization.Seize (Alloc_Mutex);
         First_Found_Index := 0;
         Found_Count       := 0;
         goto Search_Blocks;
      end if;

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
         Data : array (1 .. Blocks_To_Allocate * Block_Size) of Unsigned_8
            with Import, Address => To_Address (Ret);
      begin
         for V of Data loop
            V := 0;
         end loop;

         if Config.Support_Alloc_Only then
            return Ret;
         else
            Header :=
               (Block_Count => Blocks_To_Allocate,
                Signature   => Calculate_Signature (Blocks_To_Allocate));
            return Ret + Block_Size;
         end if;
      end;
   end Alloc;

   procedure Free (Address : Interfaces.C.size_t) is
      Real_Address : Virtual_Address := Virtual_Address (Address);
      Real_Block   : Unsigned_64;
      Bitmap_Body  : Bitmap (0 .. Block_Count - 1)
         with Address => To_Address (Bitmap_Address), Import;
   begin
      --  Ensure the address is in the higher half and not null.
      if Real_Address = 0 or Config.Support_Alloc_Only then
         return;
      elsif Real_Address < Memory_Offset then
         Real_Address := Real_Address + Memory_Offset;
      end if;

      --  Free the blocks in the header.
      declare
         IAddr  : constant Integer_Address := Real_Address - Block_Size;
         SAddr  : constant  System.Address := To_Address (IAddr);
         Header : Allocation_Header with Import, Address => SAddr;
      begin
         Lib.Synchronization.Seize (Alloc_Mutex);

         if Calculate_Signature (Header.Block_Count) = Header.Signature then
            Real_Block  := Unsigned_64 (IAddr - Memory_Offset) / Block_Size;
            Free_Memory := Free_Memory + (Header.Block_Count * Block_Size);
            for I in 1 .. Header.Block_Count loop
               Bitmap_Body (Real_Block + Unsigned_64 (I - 1)) := Block_Free;
            end loop;
         else
            Lib.Messages.Put_Line ("Tried to deallocate a corrupted block!");
         end if;

         Lib.Synchronization.Release (Alloc_Mutex);
      end;
   end Free;

   procedure Get_Statistics (Stats : out Statistics) is
   begin
      Lib.Synchronization.Seize (Alloc_Mutex);
      Stats :=
         (Total     => Total_Memory,
          Available => Available_Memory,
          Free      => Free_Memory);
      Lib.Synchronization.Release (Alloc_Mutex);
   end Get_Statistics;
end Memory.Physical;
