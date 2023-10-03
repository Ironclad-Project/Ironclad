--  lib-messages.ads: Utilities for reporting messages to the user.
--  Copyright (C) 2023 streaksu
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
with Arch.Clocks;
with Arch.Debug;

package Lib.Messages with
   Abstract_State => Message_State,
   Initializes    => Message_State
is
   --  Print a warning message to debug outputs and add a newline.
   --  @param Message String to print appending a newline.
   procedure Warn (Message : String)
      with Global => (In_Out => (
         Arch.Clocks.Monotonic_Clock_State,
         Arch.Debug.Debug_State,
         Message_State));

   --  Prints a string to debug outputs and adds a newline.
   --  @param Message String to print to append with a new line.
   procedure Put_Line (Message : String)
      with Global => (In_Out => (
         Arch.Clocks.Monotonic_Clock_State,
         Arch.Debug.Debug_State,
         Message_State));
   ----------------------------------------------------------------------------
   --  Types for translation strings.
   subtype Translated_String is String (1 .. 20);
   subtype Translated_Length is Natural range 0 .. 20;

   --  Put the value of an integer into a buffer, starting from the end.
   --  @param Value   Value to translate.
   --  @param Buffer  Buffer to build.
   --  @param Length  Written length.
   --  @param Use_Hex True for hex, false for decimal.
   procedure Image
      (Value   : Unsigned_32;
       Buffer  : out Translated_String;
       Length  : out Translated_Length;
       Use_Hex : Boolean := False);

   --  Put the value of an integer into a buffer, starting from the end.
   --  @param Value   Value to translate.
   --  @param Buffer  Buffer to build.
   --  @param Length  Written length.
   --  @param Use_Hex True for hex, false for decimal.
   procedure Image
      (Value   : Unsigned_64;
       Buffer  : out Translated_String;
       Length  : out Translated_Length;
       Use_Hex : Boolean := False);

private

   procedure Print_Timestamp_And_Lock
      with Global => (In_Out => (
         Arch.Clocks.Monotonic_Clock_State,
         Arch.Debug.Debug_State,
         Message_State));
end Lib.Messages;
