--  lib-messages.ads: Specification of the messages package.
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

package Lib.Messages is
   --  Prints and adds a newline.
   procedure Put_Line (Message : String);

   --  Prints a message of different types.
   procedure Put (Message : String);
   procedure Put (Message : Character);
   procedure Put (Message : Integer;     Pad, Use_Hex : Boolean := False);
   procedure Put (Message : Integer_64;  Pad, Use_Hex : Boolean := False);
   procedure Put (Message : Unsigned_64; Pad, Use_Hex : Boolean := False);
   procedure Put (Message : System.Address; Pad : Boolean := False);

private
   procedure Inner_Print (Message : String);
   procedure Inner_Print (Message : Character);
end Lib.Messages;
