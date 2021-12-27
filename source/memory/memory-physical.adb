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

package body Memory.Physical is
   procedure Init_Allocator (Memmap : access Arch.Stivale2.Memmap_Tag) is
      type Mmap is array (Unsigned_64 range <>) of Arch.Stivale2.Memmap_Entry;

      Entries : Mmap (1 .. Memmap.EntryCount);
      for Entries'Address use Memmap.Entries'Address;
   begin
      null;
   end Init_Allocator;

   function Memory_Alloc (Size : Unsigned_64) return System.Address is
   begin
      return System'To_Address (0);
   end Memory_Alloc;

   procedure Memory_Free (Address : System.Address) is
   begin
      null;
   end Memory_Free;

   procedure Get_Info (Total_Memory, Free, Used : out Natural) is
   begin
      Total_Memory := 0;
      Free         := 0;
      Used         := 0;
   end Get_Info;
end Memory.Physical;
