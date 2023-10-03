--  arch-debug.ads: Architecture-specific debug channels.
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

package Arch.Debug with
   Abstract_State => Debug_State,
   Initializes => Debug_State
is
   --  These functions print values to a target-specific debug channel, which
   --  could take the shape of anything, from a serial port to a network
   --  packet.
   --  It comes with its own abstracted state for ease of modeling.

   --  Print a character message.
   --  @param Message Character to print.
   procedure Print (Message : Character)
      with Global => (In_Out => Debug_State);

   --  Print a string message (!!!!!!!NOT ATOMICALLY!!!!!!!).
   --  @param Message String to print.
   procedure Print (Message : String)
      with Global => (In_Out => Debug_State);
end Arch.Debug;
