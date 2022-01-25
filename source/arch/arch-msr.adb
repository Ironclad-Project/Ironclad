--  arch-msr.adb: MSR reading and writting library.
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

package body Arch.MSR is
   function Read (MSRNumber : Unsigned_32) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      Asm ("rdmsr",
           Outputs  => Unsigned_64'Asm_Output ("=A", Result),
           Inputs   => Unsigned_32'Asm_Input  ("c", MSRNumber),
           Clobber  => "memory",
           Volatile => True);
      return Result;
   end Read;

   procedure Write (MSRNumber : Unsigned_32; Value : Unsigned_64) is
   begin
      Asm ("wrmsr",
           Inputs   => (Unsigned_64'Asm_Input ("A", Value),
                        Unsigned_32'Asm_Input ("c", MSRNumber)),
           Clobber  => "memory",
           Volatile => True);
   end Write;
end Arch.MSR;
