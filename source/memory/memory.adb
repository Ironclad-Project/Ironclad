--  memory.adb: System-wide memory definitions.
--  Copyright (C) 2023 streaksu
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

package body Memory is
   function Memory_Offset return Unsigned_64 is
      Ret : constant Integer_Address := Memory_Offset;
   begin
      return Unsigned_64 (Ret);
   end Memory_Offset;

   function Kernel_Offset return Unsigned_64 is
      Ret : constant Integer_Address := Kernel_Offset;
   begin
      return Unsigned_64 (Ret);
   end Kernel_Offset;
end Memory;
