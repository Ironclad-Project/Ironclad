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
   Total_Memory : Memory.Size; --  Total size of memory.

   procedure Init_Allocator (Memmap : access Arch.Stivale2.Memmap_Tag) is
   begin
      Total_Memory := 0;
   end Init_Allocator;

   function Alloc (Size : Memory.Size) return System.Address is
   begin
      return System'To_Address (0);
   end Alloc;

   procedure Free (Address : System.Address) is
   begin
      null;
   end Free;

   procedure Get_Info (Total, Free, Used : out Memory.Size) is
   begin
      Total := Total_Memory;
      Free  := 0;
      Used  := 0;
   end Get_Info;
end Memory.Physical;
