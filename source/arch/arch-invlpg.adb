--  arch-invlpg.adb: Invalidate page wrapper.
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

package body Arch.INVLPG is
   procedure Invalidate (Value : Memory.Virtual_Address) is
   begin
      Asm ("invlpg (%0)",
           Inputs   => Memory.Virtual_Address'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Invalidate;
end Arch.INVLPG;
