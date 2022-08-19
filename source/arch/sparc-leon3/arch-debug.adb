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

package body Arch.Debug with SPARK_Mode => Off is
   --  Serial registers.
   TRX_Data   : Unsigned_32 with Import, Volatile;
   TRX_Status : Unsigned_32 with Import, Volatile;
   for TRX_Data'Address   use To_Address (16#80000100#);
   for TRX_Status'Address use To_Address (16#80000104#);

   procedure Print (Message : Character) is
   begin
      while (TRX_Status and 2#100#) = 0 loop
         Snippets.Pause;
      end loop;
      TRX_Data := Character'Pos (Message);
   end Print;

   procedure Print (Message : String) is
   begin
      for C of Message loop
         Print (C);
      end loop;
   end Print;
end Arch.Debug;
