--  arch-ioports.adb: IO port functions.
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

package body Arch.IOPorts is
   procedure Port_Out (Port : Unsigned_16; Value : Unsigned_8) is
   begin
      Asm ("outb %0, %1",
           Inputs   => (Unsigned_8'Asm_Input  ("a",  Value),
                        Unsigned_16'Asm_Input ("Nd", Port)),
           Clobber  => "memory",
           Volatile => True);
   end Port_Out;

   function Port_In (Port : Unsigned_16) return Unsigned_8 is
      Value : Unsigned_8;
   begin
      Asm ("inb %1, %0",
           Outputs  => Unsigned_8'Asm_Output ("=a", Value),
           Inputs   => Unsigned_16'Asm_Input ("Nd", Port),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Port_In;
end Arch.IOPorts;
