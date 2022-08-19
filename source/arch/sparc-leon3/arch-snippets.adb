--  arch-snippets.adb: Architecture-specific bits.
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

package body Arch.Snippets with SPARK_Mode => Off is
   procedure HCF is
   begin
      Disable_Interrupts;
      loop
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
      Value : Unsigned_32;
   begin
      Asm ("mov %%psr, %0",
           Outputs  => Unsigned_32'Asm_Output ("=r", Value),
           Volatile => True);
      Asm ("mov %0, %%psr",
           Inputs   => Unsigned_32'Asm_Input ("r", Value or 32),
           Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
      Value : Unsigned_32;
   begin
      Asm ("mov %%psr, %0",
           Outputs  => Unsigned_32'Asm_Output ("=r", Value),
           Volatile => True);
      Asm ("mov %0, %%psr",
           Inputs   => Unsigned_32'Asm_Input ("r", Value and not 32),
           Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      --  On platforms that dont support the feature, this will be a NOP.
      Asm ("wr %%g0, %%asr19", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      --  There is not such an equivalent for SPARCv8-LEON3.
      null;
   end Pause;

   function Read_Cycles return Unsigned_64 is
   begin
      --  There is not such an equivalent for SPARCv8-LEON3.
      return 45634;
   end Read_Cycles;
end Arch.Snippets;
