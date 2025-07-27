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

with Interfaces; use Interfaces;
with System.Machine_Code;

package body Arch.Snippets is
   procedure HCF is
   begin
      loop
         Disable_Interrupts;
         Wait_For_Interrupt;
      end loop;
   end HCF;

   INT_BIT : constant := 2#10#;

   procedure Enable_Interrupts is
      Value : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("csrr %0, sstatus",
          Outputs  => Unsigned_64'Asm_Output ("=r", Value),
          Clobber  => "memory",
          Volatile => True);
      System.Machine_Code.Asm
         ("csrw sstatus, %0",
          Inputs   => Unsigned_64'Asm_Input ("r", Value or INT_BIT),
          Clobber  => "memory",
          Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
      Value : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("csrr %0, sstatus",
          Outputs  => Unsigned_64'Asm_Output ("=r", Value),
          Clobber  => "memory",
          Volatile => True);
      System.Machine_Code.Asm
         ("csrw sstatus, %0",
          Inputs   => Unsigned_64'Asm_Input ("r", Value and not INT_BIT),
          Clobber  => "memory",
          Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm ("wfi", Volatile => True);
   end Wait_For_Interrupt;

   function Interrupts_Enabled return Boolean is
      Value : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("csrr %0, sstatus",
          Outputs  => Unsigned_64'Asm_Output ("=r", Value),
          Clobber  => "memory",
          Volatile => True);
      return (Value and INT_BIT) /= 0;
   end Interrupts_Enabled;

   procedure Pause is
   begin
      null; --  We dont target Zihintpause.
   end Pause;

   MSTATUS_SUM : constant Unsigned_64 := Shift_Right (1, 18);

   procedure Enable_Userland_Memory_Access is
      Orig : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("csrr %0, mstatus",
          Outputs  => Unsigned_64'Asm_Output ("=r", Orig),
          Clobber  => "memory",
          Volatile => True);
      System.Machine_Code.Asm
         ("csrw mstatus, %0",
          Inputs   => Unsigned_64'Asm_Input ("r", Orig or MSTATUS_SUM),
          Clobber  => "memory",
          Volatile => True);
   end Enable_Userland_Memory_Access;

   procedure Disable_Userland_Memory_Access is
      Orig : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("csrr %0, mstatus",
          Outputs  => Unsigned_64'Asm_Output ("=r", Orig),
          Clobber  => "memory",
          Volatile => True);
      System.Machine_Code.Asm
         ("csrw mstatus, %0",
          Inputs   => Unsigned_64'Asm_Input ("r", Orig and not MSTATUS_SUM),
          Clobber  => "memory",
          Volatile => True);
   end Disable_Userland_Memory_Access;
end Arch.Snippets;
