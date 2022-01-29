--  arch-wrappers.adb: Wrappers for several ASM functions.
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

package body Arch.Wrappers is
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
   ----------------------------------------------------------------------------
   procedure Invalidate_Page (Value : Virtual_Address) is
   begin
      Asm ("invlpg (%0)",
           Inputs   => Virtual_Address'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Invalidate_Page;
   ----------------------------------------------------------------------------
   function Read_MSR (MSRNumber : Unsigned_32) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      Asm ("rdmsr",
           Outputs  => Unsigned_64'Asm_Output ("=A", Result),
           Inputs   => Unsigned_32'Asm_Input  ("c", MSRNumber),
           Clobber  => "memory",
           Volatile => True);
      return Result;
   end Read_MSR;

   procedure Write_MSR (MSRNumber : Unsigned_32; Value : Unsigned_64) is
   begin
      Asm ("wrmsr",
           Inputs   => (Unsigned_64'Asm_Input ("A", Value),
                        Unsigned_32'Asm_Input ("c", MSRNumber)),
           Clobber  => "memory",
           Volatile => True);
   end Write_MSR;
   ----------------------------------------------------------------------------
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
end Arch.Wrappers;
