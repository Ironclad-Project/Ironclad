--  lib-panic.ads: Specification of the panic function package.
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

with Lib.Messages;
with Arch.Debug;
with Arch.Clocks;

package Lib.Panic with
   Abstract_State => Panic_State,
   Initializes    => Panic_State
is
   --  For situations that are too risky for recovery!
   --  @param Message Message to print before halting the system.
   procedure Hard_Panic (Message : String)
      with Global =>
         (In_Out => (Panic_State, Arch.Debug.Debug_State,
                     Arch.Clocks.Monotonic_Clock_State,
                     Messages.Message_State)),
           Pre => Message'Length <= 100, No_Return;
end Lib.Panic;
