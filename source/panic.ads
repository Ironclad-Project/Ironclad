--  panic.ads: For when recovering is not an option!
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
with Messages;
with Arch.Clocks;
with Arch.Context;

package Panic with
   Abstract_State => Panic_State,
   Initializes    => Panic_State
is
   --  For situations that are too risky for recovery!
   --  @param Message Message to print before halting the system.
   procedure Hard_Panic (Message : String)
      with No_Return, Global =>
         (In_Out => (Panic_State, Arch.Clocks.Monotonic_Clock_State,
                     Messages.Message_State));

   --  The same as above, but this one takes a context for string printing.
   procedure Hard_Panic (Message : String; Ctx : Arch.Context.GP_Context)
      with No_Return, Global =>
         (In_Out => (Panic_State, Arch.Clocks.Monotonic_Clock_State,
                     Messages.Message_State));

private

   procedure Panic_Common (Message : String);
   procedure Print_Triple (N1, N2, N3 : String; V1, V2, V3 : Unsigned_64)
      with Pre => N1'Length <= 3 and N2'Length <= 3 and N3'Length <= 3;
end Panic;
