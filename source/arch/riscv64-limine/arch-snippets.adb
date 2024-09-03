--  arch-snippets.ads: Architecture-specific bits.
--  Copyright (C) 2024 streaksu
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

package body Arch.Snippets is
   procedure HCF is
   begin
      loop
         Disable_Interrupts;
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      Write_SStatus (Read_SStatus or 2#10#);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      Write_SStatus (Read_SStatus and not 2#10#);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      Asm ("wfi", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      null; --  No pause equivalent sadly, inneficient busy loops for you!
   end Pause;

   function Read_SStatus return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("csrr %0, sstatus",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_SStatus;

   procedure Write_SStatus (Value : Unsigned_64) is
   begin
      Asm ("csrw sstatus, %0",
           Inputs   => Unsigned_64'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Write_SStatus;
end Arch.Snippets;
