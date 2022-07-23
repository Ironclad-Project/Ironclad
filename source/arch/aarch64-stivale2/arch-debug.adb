--  arch-debug.adb: Architecture-specific debug channels.
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
with Arch.Snippets;
with Memory; use Memory;

package body Arch.Debug with SPARK_Mode => Off is
   --  PL011-compatible serial registers.
   PL011_Data   : Unsigned_32 with Import, Volatile;
   PL011_Status : Unsigned_32 with Import, Volatile;

   for PL011_Data'Address   use To_Address (Memory_Offset + 16#9000000#);
   for PL011_Status'Address use To_Address (Memory_Offset + 16#9000018#);

   procedure Print (Message : Character) is
   begin
      while (PL011_Status and 16#100000#) /= 0 loop
         Snippets.Pause;
      end loop;
      PL011_Data := Character'Pos (Message);
   end Print;

   procedure Print (Message : String) is
   begin
      for C of Message loop
         Print (C);
      end loop;
   end Print;
end Arch.Debug;
