--  lib-messages.adb: Utilities for reporting messages to the user.
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

with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.Debug;
with Lib.Synchronization;

package body Lib.Messages with
   Refined_State => (Message_State => Messages_Mutex)
is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   Messages_Mutex : aliased Lib.Synchronization.Binary_Semaphore :=
      Lib.Synchronization.Unlocked_Semaphore;

   procedure Warn (Message : String) is
      Header_Str : constant String := Ada.Characters.Latin_1.ESC & "[35m";
      Reset_Str  : constant String := Ada.Characters.Latin_1.ESC & "[0m";
   begin
      Lib.Synchronization.Seize (Messages_Mutex);
      Arch.Debug.Print (Header_Str & "Warning" & Reset_Str & ": ");
      Arch.Debug.Print (Message);
      Arch.Debug.Print (Ada.Characters.Latin_1.CR);
      Arch.Debug.Print (Ada.Characters.Latin_1.LF);
      Lib.Synchronization.Release (Messages_Mutex);
   end Warn;

   procedure Put_Line (Message : String) is
   begin
      Lib.Synchronization.Seize (Messages_Mutex);
      Arch.Debug.Print (Message);
      Arch.Debug.Print (Ada.Characters.Latin_1.CR);
      Arch.Debug.Print (Ada.Characters.Latin_1.LF);
      Lib.Synchronization.Release (Messages_Mutex);
   end Put_Line;

   procedure Put (Message : String) is
   begin
      Lib.Synchronization.Seize (Messages_Mutex);
      Arch.Debug.Print (Message);
      Lib.Synchronization.Release (Messages_Mutex);
   end Put;

   procedure Put (Message : Character) is
   begin
      Lib.Synchronization.Seize (Messages_Mutex);
      Arch.Debug.Print (Message);
      Lib.Synchronization.Release (Messages_Mutex);
   end Put;

   procedure Put (Message : Integer; Pad, Use_Hex : Boolean := False) is
   begin
      Put (Integer_64 (Message), Pad, Use_Hex);
   end Put;

   procedure Put (Message : Integer_64; Pad, Use_Hex : Boolean := False) is
   begin
      if Message < 0 then
         Put ('-');
      end if;

      --  Check for possible overflow.
      if Message = Integer_64'First then
         Put (Unsigned_64 (Integer_64'Last + 1), Pad, Use_Hex);
      else
         Put (Unsigned_64 (abs Message), Pad, Use_Hex);
      end if;
   end Put;

   procedure Put (Message : Unsigned_64; Pad, Use_Hex : Boolean := False) is
      Conversion : constant String  := "0123456789ABCDEF";
      To_Convert : Unsigned_64      := Message;
      Base       : Unsigned_64      := 10;
      Char_Limit : Natural          := 20;
      Written    : Integer          := 0;
      Result     : String (1 .. 20) := (others => Ada.Characters.Latin_1.NUL);
   begin
      Lib.Synchronization.Seize (Messages_Mutex);
      if Use_Hex then
         Arch.Debug.Print ("0x");
         Base       := 16;
         Char_Limit := 16;
      end if;

      if To_Convert = 0 then
         Result (1) := '0';
         Written    := 1;
      else
         while To_Convert /= 0 and Written < Char_Limit loop
            pragma Loop_Invariant (Written >= 0 and Written <= Result'Length);
            Written          := Written + 1;
            Result (Written) := Conversion (Integer (To_Convert rem Base) + 1);
            To_Convert       := To_Convert / Base;
         end loop;
      end if;

      if Pad then
         for I in Written + 1 .. Char_Limit loop
            Arch.Debug.Print ('0');
         end loop;
      end if;

      for I in reverse 1 .. Written loop
         Arch.Debug.Print (Result (I));
      end loop;
      Lib.Synchronization.Release (Messages_Mutex);
   end Put;

   procedure Put (Message : System.Address; Pad : Boolean := False) is
   begin
      Put (Unsigned_64 (To_Integer (Message)), Pad, True);
   end Put;
end Lib.Messages;
