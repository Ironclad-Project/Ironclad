--  lib-panic.ads: For when recovering is not an option!
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

with Lib.Messages;
with Arch.Clocks;
with Ada.Characters.Latin_1;

package Lib.Panic with
   Abstract_State => Panic_State,
   Initializes    => Panic_State
is
   --  String header and ending to be added to passed message strings.
   HP           : constant String := Ada.Characters.Latin_1.ESC & "[31m";
   RC           : constant String := Ada.Characters.Latin_1.ESC & "[0m";
   Panic_Header : constant String := HP & "Panic: ";

   --  For situations that are too risky for recovery!
   --  @param Message Message to print before halting the system.
   procedure Hard_Panic (Message : String)
      with Pre =>
         Message'Length <= Messages.Max_Line - Panic_Header'Length - RC'Length,
      Global =>
         (In_Out => (Panic_State, Arch.Clocks.Monotonic_Clock_State,
                     Messages.Message_State)),
      No_Return;
end Lib.Panic;
