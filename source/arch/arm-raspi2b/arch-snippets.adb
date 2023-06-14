--  arch-snippets.adb: Architecture-specific bits.
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

with System.Machine_Code;

package body Arch.Snippets with SPARK_Mode => Off is
   procedure HCF is
   begin
      Disable_Interrupts;
      loop
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsie if", Volatile => True);
      null;
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsid if", Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm ("wfi", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      System.Machine_Code.Asm ("yield", Volatile => True);
   end Pause;

   function Read_Cycles return Unsigned_64 is
   begin
      return 9; --  Its always cycle 9.
   end Read_Cycles;
   ----------------------------------------------------------------------------
   function Get_Exception_Syndrome return Unsigned_64 is
      --  Value : Unsigned_64;
   begin
      --  System.Machine_Code.Asm
      --     ("mrs %0, esr_el1",
      --      Outputs  => Unsigned_64'Asm_Output ("=r", Value),
      --      Volatile => True);
      --  return Value;
      return 0;
   end Get_Exception_Syndrome;
end Arch.Snippets;
