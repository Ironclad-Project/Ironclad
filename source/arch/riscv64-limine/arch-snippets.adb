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

package body Arch.Snippets is
   procedure HCF is
   begin
      loop
         null;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      null;
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      null;
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      null;
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      null;
   end Pause;
end Arch.Snippets;
