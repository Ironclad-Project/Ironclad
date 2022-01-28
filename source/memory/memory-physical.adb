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
with Memory;     use Memory;
with Lib.Panic;
with Lib.Synchronization; use Lib.Synchronization;

package body Memory.Physical is
   Block_Free : constant Boolean := True;
   Block_Used : constant Boolean := False;
   type Bitmap is array (Unsigned_64 range <>) of Boolean;
   pragma Pack (Bitmap);

   Block_Size       : constant        := 16#1000#;
   Block_Count      : Unsigned_64     := 0;
   Bitmap_Length    : Memory.Size     := 0;
   Bitmap_Address   : Virtual_Address := Null_Address;
   Bitmap_Last_Used : Unsigned_64     := 1;

   Alloc_Mutex  : aliased Binary_Semaphore;
   Free_Memory  : Memory.Size := 0;
   Used_Memory  : Memory.Size := 0;
   Total_Memory : Memory.Size := 0;

   procedure Init_Allocator (Memmap : access Arch.Stivale2.Memmap_Tag) is
   begin
      --  Count memory by just taking into account usable entries.
      --  XXX: If we ever move from the stivale2 term, one can also use
      --  reclaimable.
      for E of Memmap.Entries loop
         Total_Memory := Total_Memory + E.Length;

         if E.EntryType = Arch.Stivale2.Memmap_Entry_Usable then
            Free_Memory := Free_Memory + E.Length;
         else
            Used_Memory := Used_Memory + E.Length;
         end if;
      end loop;

      --  Calculate what we will need for the bitmap, and find a hole for it.
      Block_Count   := Unsigned_64 (Total_Memory) / Block_Size;
      Bitmap_Length := Size (Block_Count) / 8;
      for I in Memmap.Entries'Range loop
         if Memmap.Entries (I).EntryType = Arch.Stivale2.Memmap_Entry_Usable
            and Memmap.Entries (I).Length > Bitmap_Length
         then
            Bitmap_Address := Memmap.Entries (I).Base + Memory_Offset;
            Memmap.Entries (I).Length :=
               Memmap.Entries (I).Length - Bitmap_Length;
            Memmap.Entries (I).Base   :=
               Memmap.Entries (I).Base   + Physical_Address (Bitmap_Length);
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
         Bitmap_Body : Bitmap (1 .. Block_Count);
         for Bitmap_Body'Address use To_Address (Bitmap_Address);
         I : Unsigned_64 := 0;
      begin
         for E of Memmap.Entries loop
            if E.EntryType = Arch.Stivale2.Memmap_Entry_Usable then
               while I < Unsigned_64 (E.Length) loop
                  Bitmap_Body
                     ((Unsigned_64 (E.Base) + I) / Block_Size) := Block_Free;
                  I := I + Block_Size;
               end loop;
            else
               while I < Unsigned_64 (E.Length) loop
                  Bitmap_Body
                     ((Unsigned_64 (E.Base) + I) / Block_Size) := Block_Used;
                  I := I + Block_Size;
               end loop;
            end if;
         end loop;
      end;

      --  Prepare the mutex.
      Lib.Synchronization.Release (Alloc_Mutex'Access);
   end Init_Allocator;

   function Alloc (Size : Memory.Size) return Virtual_Address is
      Bitmap_Body : Bitmap (1 .. Block_Count);
      for Bitmap_Body'Address use To_Address (Bitmap_Address);
      First_Found_Index  : Unsigned_64 := 0;
      Found_Count        : Unsigned_64 := 0;
      Blocks_To_Allocate : Memory.Size := Size / Block_Size;
   begin
      --  Check we can allocate at all.
      if Size = 0 or Size > Free_Memory then
         goto Error_Return;
      end if;

      --  Adjust the block count.
      if Blocks_To_Allocate = 0 then
         Blocks_To_Allocate := 1;
      end if;

      --  Search for contiguous blocks, as many as needed.
      Lib.Synchronization.Seize (Alloc_Mutex'Access);
   <<Search_Blocks>>
      for I in Bitmap_Last_Used .. Block_Count loop
         if Bitmap_Body (I) = Block_Free and First_Found_Index = 0 then
            First_Found_Index := I;
            Found_Count       := 1;
         elsif Bitmap_Body (I) = Block_Free then
            if I /= First_Found_Index + Found_Count then
               First_Found_Index := I;
               Found_Count       := 1;
            else
               Found_Count := Found_Count + 1;
            end if;
         end if;
         exit when Blocks_To_Allocate = Memory.Size (Found_Count);
      end loop;
      if Blocks_To_Allocate /= Memory.Size (Found_Count) and
         Bitmap_Last_Used   /= 1
      then
         Bitmap_Last_Used := 1;
         goto Search_Blocks;
      end if;

      if Blocks_To_Allocate /= Memory.Size (Found_Count) and
         Bitmap_Last_Used = 1
      then
         goto Error_Return;
      end if;

      --  Mark the block of memory as used.
      for I in 0 .. Blocks_To_Allocate loop
         Bitmap_Body (First_Found_Index + Unsigned_64 (I)) := Block_Used;
      end loop;
      Lib.Synchronization.Release (Alloc_Mutex'Access);

      --  Set statistic and global variables.
      Bitmap_Last_Used := First_Found_Index;
      Free_Memory      := Free_Memory - (Blocks_To_Allocate * Block_Size);
      Used_Memory      := Used_Memory + (Blocks_To_Allocate * Block_Size);

      --  Zero out memory and return value.
      declare
         Addr : constant Virtual_Address :=
            Virtual_Address (First_Found_Index * Block_Size) + Memory_Offset;
         Pool : array (1 .. Unsigned_64 (Size)) of Unsigned_8;
         for Pool'Address use To_Address (Addr);
      begin
         for I in Pool'First .. Pool'Last loop
            Pool (I) := 0;
         end loop;
         return Addr;
      end;

   <<Error_Return>>
      Lib.Panic.Hard_Panic ("Could not allocate block");
   end Alloc;

   procedure Free (Address : Virtual_Address) is
      Real_Address : constant Virtual_Address := (Address - Memory_Offset);
      Index        : constant Virtual_Address := Real_Address / Block_Size;
      Bitmap_Body : Bitmap (1 .. Block_Count);
      for Bitmap_Body'Address use To_Address (Bitmap_Address);
   begin
      --  TODO: This is basically a placeholder free that will free only the
      --  first block. Blocks per address should be tracked and freed.
      Lib.Synchronization.Seize (Alloc_Mutex'Access);
      Bitmap_Body (Unsigned_64 (Index)) := Block_Free;
      Lib.Synchronization.Release (Alloc_Mutex'Access);
   end Free;

   procedure Get_Info (Total, Free, Used : out Memory.Size) is
   begin
      Lib.Synchronization.Seize (Alloc_Mutex'Access);
      Total := Total_Memory;
      Free  := Free_Memory;
      Used  := Used_Memory;
      Lib.Synchronization.Release (Alloc_Mutex'Access);
   end Get_Info;
end Memory.Physical;
