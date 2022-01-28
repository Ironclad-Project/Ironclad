--  arch-cr.adb: Control Register handling.
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

with System.Machine_Code; use System.Machine_Code;

package body Arch.CR is
   function Read_CR3 return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("mov %%cr3, %0",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_CR3;

   procedure Write_CR3 (Value : Unsigned_64) is
   begin
      Asm ("mov %0, %%cr3",
           Inputs   => Unsigned_64'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Write_CR3;
end Arch.CR;
