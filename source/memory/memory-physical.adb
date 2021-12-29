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

package body Memory.Physical is
   Free_Memory  : Memory.Size := 0;
   Used_Memory  : Memory.Size := 0;
   Total_Memory : Memory.Size := 0;

   procedure Init_Allocator (Memmap : access Arch.Stivale2.Memmap_Tag) is
   begin
      --  Count memory by just taking into account usable entries.
      --  If we ever move from the stivale2 term, one can also use reclaimable.
      Count_Memory :
         for E of Memmap.Entries loop
            Total_Memory := Total_Memory + Memory.Size (E.Length);

            if E.EntryType = Arch.Stivale2.Memmap_Entry_Usable then
               Free_Memory := Free_Memory + Memory.Size (E.Length);
            else
               Used_Memory := Used_Memory + Memory.Size (E.Length);
            end if;
         end loop Count_Memory;
   end Init_Allocator;

   function Alloc (Size : Memory.Size) return System.Address is
      Error_Return : constant System.Address := System'To_Address (0);
   begin
      --  Check we can allocate at all.
      if Size = 0 or Size > Free_Memory then
         return Error_Return;
      end if;

      --  Adjust statistic variables.
      Free_Memory := Free_Memory - Size;
      Used_Memory := Used_Memory + Size;
      return Error_Return;
   end Alloc;

   procedure Free (Address : System.Address) is
   begin
      null;
   end Free;

   procedure Get_Info (Total, Free, Used : out Memory.Size) is
   begin
      Total := Total_Memory;
      Free  := Free_Memory;
      Used  := Used_Memory;
   end Get_Info;
end Memory.Physical;
