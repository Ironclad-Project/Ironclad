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

--  FIXME: This allocator will not work for aarch64-stivale2, and alloconly
--  has to be used instead, this must be debugged.

package body Memory.Physical with SPARK_Mode => Off is
   Block_Free : constant Boolean := True;
   Block_Used : constant Boolean := False;
   type Bitmap is array (Unsigned_64 range <>) of Boolean;
   pragma Pack (Bitmap);

   --  Information that the allocator keeps track of.
   Total_Memory, Free_Memory, Used_Memory : Memory.Size;

   Block_Size       : constant        := 16#1000#;
   Block_Count      : Unsigned_64     := 0;
   Bitmap_Length    : Memory.Size     := 0;
   Bitmap_Address   : Virtual_Address := Null_Address;
   Bitmap_Last_Used : Unsigned_64     := 1;
   Alloc_Mutex      : aliased Binary_Semaphore;

   procedure Init_Allocator (Memmap : in out Arch.Boot_Memory_Map) is
   begin
      --  XXX: Take into account unordered memory maps, or overlapping entries.
      --  Count memory and get the total memory size.
      for E of Memmap loop
         if E.MemType = Arch.Memory_Free then
            Free_Memory := Free_Memory + Size (E.Length);
         else
            Used_Memory := Used_Memory + Size (E.Length);
         end if;
      end loop;
      declare
         Sta : constant Size := Size (To_Integer (Memmap (Memmap'Last).Start));
         Len : constant Size := Size (Memmap (Memmap'Last).Length);
      begin
         Total_Memory := Sta + Len;
      end;

      --  Calculate what we will need for the bitmap, and find a hole for it.
      Block_Count   := Unsigned_64 (Total_Memory) / Block_Size;
      Bitmap_Length := Size (Block_Count) / 8;
      for E of Memmap loop
         if E.MemType = Arch.Memory_Free and
            Size (E.Length) > Bitmap_Length
         then
            Bitmap_Address :=
               Virtual_Address (To_Integer (E.Start) + Memory_Offset);
            E.Length := E.Length - Storage_Count (Bitmap_Length);
            E.Start  := E.Start + Storage_Count (Bitmap_Length);
            Free_Memory := Free_Memory - Bitmap_Length;
            Used_Memory := Used_Memory + Bitmap_Length;
            exit;
         end if;
      end loop;
      if Bitmap_Address = Null_Address then
         Lib.Panic.Hard_Panic ("Could not allocate the bitmap");
      end if;

      --  Initialize and fill the bitmap.
      declare
         Bitmap_Body : Bitmap (1 .. Block_Count) with Import;
         for Bitmap_Body'Address use To_Address (Bitmap_Address);
         Index : Unsigned_64 := 0;
         Block_Value : Boolean;
         Block_Start : Unsigned_64;
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
            Block_Start := Unsigned_64 (To_Integer (E.Start));
            while Index < Unsigned_64 (E.Length) loop
               Bitmap_Body ((Block_Start + Index) / Block_Size) := Block_Value;
               Index := Index + Block_Size;
            end loop;
         end loop;
      end;

      --  Prepare the mutex.
      Lib.Synchronization.Release (Alloc_Mutex'Access);
   end Init_Allocator;

   function Alloc (Sz : Interfaces.C.size_t) return Virtual_Address is
      package Align is new Lib.Alignment (Memory.Size);

      Bitmap_Body : Bitmap (1 .. Block_Count) with Import;
      for Bitmap_Body'Address use To_Address (Bitmap_Address);

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

      Blocks_To_Allocate := Align.Align_Up (Size, Block_Size) / Block_Size;

      --  Search for contiguous blocks, as many as needed.
      Lib.Synchronization.Seize (Alloc_Mutex'Access);
   <<Search_Blocks>>
      for I in Bitmap_Last_Used .. Block_Count loop
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

      if Bitmap_Last_Used /= 1 then
         Bitmap_Last_Used := 1;
         goto Search_Blocks;
      else
         Lib.Panic.Hard_Panic ("Exhausted memory (OOM)");
      end if;

   <<Fill_Bitmap>>
      for I in 1 .. Blocks_To_Allocate loop
         Bitmap_Body (First_Found_Index + Unsigned_64 (I - 1)) := Block_Used;
      end loop;

      --  Set statistic and global variables.
      Bitmap_Last_Used := First_Found_Index;
      Free_Memory      := Free_Memory - Size;
      Used_Memory      := Used_Memory + Size;
      Lib.Synchronization.Release (Alloc_Mutex'Access);

      --  Zero out memory and return value.
      declare
         Addr : constant Virtual_Address :=
            Virtual_Address (First_Found_Index * Block_Size) + Memory_Offset;
         Pool : array (1 .. Unsigned_64 (Size)) of Unsigned_8 with Import;
         for Pool'Address use To_Address (Addr);
      begin
         Pool := (others => 0);
         return Addr;
      end;
   end Alloc;

   procedure Free (Address : Interfaces.C.size_t) is
      Real_Address : Virtual_Address := Virtual_Address (Address);
      Bitmap_Body  : Bitmap (1 .. Block_Count)
         with Address => To_Address (Bitmap_Address), Import;
   begin
      --  Ensure the address is in the lower half.
      if Real_Address > Memory_Offset then
         Real_Address := Real_Address - Memory_Offset;
      end if;

      Lib.Synchronization.Seize (Alloc_Mutex'Access);

      --  TODO: This is basically a placeholder free that will free only the
      --  first block. Blocks per address should be tracked and freed.
      Bitmap_Body (Unsigned_64 (Real_Address / Block_Size)) := Block_Free;
      Free_Memory := Free_Memory + Block_Size;
      Used_Memory := Used_Memory - Block_Size;

      Lib.Synchronization.Release (Alloc_Mutex'Access);
   end Free;

   function Get_Statistics return Statistics is
      Ret : Statistics;
   begin
      Lib.Synchronization.Seize (Alloc_Mutex'Access);
      Ret := (
         Total_Memory => Total_Memory,
         Free_Memory  => Free_Memory,
         Used_Memory  => Used_Memory
      );
      Lib.Synchronization.Release (Alloc_Mutex'Access);
      return Ret;
   end Get_Statistics;
end Memory.Physical;
