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

with System.Machine_Code;

package body Arch.Snippets with SPARK_Mode => Off is
   procedure HCF is
   begin
      --  Interrupts ought to be disabled every iteration and not only once
      --  because of spurious interrupts.
      loop
         Disable_Interrupts;
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("sti", Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("cli", Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm ("hlt", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      System.Machine_Code.Asm ("pause", Volatile => True);
   end Pause;

   function Read_Cycles return Unsigned_64 is
      High : Unsigned_32;
      Low  : Unsigned_32;
   begin
      --  Use the TSC since that doesnt need checking unlike rdrand.
      System.Machine_Code.Asm
         ("rdtsc",
          Outputs => (Unsigned_32'Asm_Output ("=d", High),
                      Unsigned_32'Asm_Output ("=a", Low)),
          Volatile => True);
      return Unsigned_64 (Shift_Left (High, 32) or Low);
   end Read_Cycles;
end Arch.Snippets;
