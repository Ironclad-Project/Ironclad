--  arch-snippets.ads: Architecture-specific bits.
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

package Arch.Snippets with SPARK_Mode => Off is
   --  Drive the execution thread to an irrecoverable state.
   --  (Halt and Catch Fire).
   procedure HCF with Inline_Always;

   --  Enable and disable external interrupts.
   procedure Enable_Interrupts with Inline_Always;
   procedure Disable_Interrupts with Inline_Always;

   --  Processor hint for waiting for interrupts in an energy-efficient state.
   procedure Wait_For_Interrupt with Inline_Always;

   --  Processor hint for optimizing spinlocks and another cache-intensitive
   --  situations.
   procedure Pause with Inline_Always;
end Arch.Snippets;
