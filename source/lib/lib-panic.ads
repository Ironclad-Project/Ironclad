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

package Lib.Panic with
   Abstract_State => Panic_State,
   Initializes    => Panic_State
is
   --  Will report the issue and then lock up the system, for situations that
   --  are too risky or unrecoverable.
   procedure Hard_Panic (Message : String)
      with No_Return, Global => (In_Out => (Panic_State,
         Messages.Message_State));
end Lib.Panic;
