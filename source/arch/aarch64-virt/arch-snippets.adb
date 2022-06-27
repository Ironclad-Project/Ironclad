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

package body Arch.Snippets is
   procedure HCF is
   begin
      Disable_Interrupts;
      loop
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("msr daifclr, #0xf", Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("msr daifset, #0xf", Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm ("wfi", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      System.Machine_Code.Asm ("yield", Volatile => True);
   end Pause;
end Arch.Snippets;
