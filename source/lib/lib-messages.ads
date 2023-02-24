--  lib-messages.ads: Convenient and safe interface for printing debug output.
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

with System;

package Lib.Messages with
   Abstract_State => Message_State,
   Initializes    => Message_State
is
   --  Print a warning message to debug outputs and add a newline.
   --  @param Message String to print appending a newline.
   procedure Warn (Message : String) with Global => (In_Out => Message_State);

   --  Prints a string to debug outputs and adds a newline.
   --  @param Message String to print to append with a new line.
   procedure Put_Line (Message : String)
      with Global => (In_Out => Message_State);

   --  Prints a string to debug outputs.
   --  @param Message String to print.
   procedure Put (Message : String)
      with Global => (In_Out => Message_State);

   --  Prints a character.
   --  @param Message Character to print.
   procedure Put (Message : Character)
      with Global => (In_Out => Message_State);

   --  Prints an integer to debug outputs.
   --  @param Message Integer to print.
   --  @param Pad Whether to pad the message with zeros or not.
   --  @param Use_Hex Whether to use hexadecimal or decimal for printing.
   procedure Put (Message : Integer; Pad, Use_Hex : Boolean := False)
      with Global => (In_Out => Message_State);
   procedure Put (Message : Integer_64; Pad, Use_Hex : Boolean := False)
      with Global => (In_Out => Message_State);
   procedure Put (Message : Unsigned_64; Pad, Use_Hex : Boolean := False)
      with Global => (In_Out => Message_State);

   --  Prints an address to debug outputs, always hexadecimal.
   --  @param Message Address to print.
   --  @param Pad Whether to pad the message with zeros or not.
   procedure Put (Message : System.Address; Pad : Boolean := False)
      with Global => (In_Out => Message_State);
end Lib.Messages;
