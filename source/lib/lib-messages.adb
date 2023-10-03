--  lib-messages.adb: Utilities for reporting messages to the user.
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

with Ada.Characters.Latin_1;
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
      Print_Timestamp_And_Lock;
      Arch.Debug.Print (Header_Str & "Warning" & Reset_Str & ": ");
      Arch.Debug.Print (Message);
      Arch.Debug.Print (Ada.Characters.Latin_1.CR);
      Arch.Debug.Print (Ada.Characters.Latin_1.LF);
      Lib.Synchronization.Release (Messages_Mutex);
   end Warn;

   procedure Put_Line (Message : String) is
   begin
      Print_Timestamp_And_Lock;
      Arch.Debug.Print (Message);
      Arch.Debug.Print (Ada.Characters.Latin_1.CR);
      Arch.Debug.Print (Ada.Characters.Latin_1.LF);
      Lib.Synchronization.Release (Messages_Mutex);
   end Put_Line;
   ----------------------------------------------------------------------------
   procedure Image
      (Value   : Unsigned_32;
       Buffer  : out Translated_String;
       Length  : out Translated_Length;
       Use_Hex : Boolean := False)
   is
   begin
      Image (Unsigned_64 (Value), Buffer, Length, Use_Hex);
   end Image;

   procedure Image
      (Value   : Unsigned_64;
       Buffer  : out Translated_String;
       Length  : out Translated_Length;
       Use_Hex : Boolean := False)
   is
      Conversion : constant      String := "0123456789ABCDEF";
      To_Convert :          Unsigned_64 := Value;
      Base       : constant Unsigned_64 := (if Use_Hex then 16 else 10);
      Current    :              Natural := Buffer'Last;
   begin
      Buffer := (others => '0');
      while To_Convert /= 0 and Current >= Buffer'First loop
         pragma Loop_Invariant (Current <= Buffer'Last);
         Buffer (Current) := Conversion (Integer (To_Convert rem Base) + 1);
         To_Convert       := To_Convert / Base;
         Current          := Current - 1;
      end loop;
      Length := Buffer'Length - Current;
   end Image;
   ----------------------------------------------------------------------------
   procedure Print_Timestamp_And_Lock is
      Stp     : Translated_String;
      Stp_Len : Natural;
      Sec, NSec : Unsigned_64;
   begin
      Arch.Clocks.Get_Monotonic_Time (Sec, NSec);
      Image ((Sec * 1000) + (NSec / 1_000_000), Stp, Stp_Len);

      Lib.Synchronization.Seize (Messages_Mutex);
      Arch.Debug.Print ("(" & Stp (Stp'Last - Stp_Len + 1 .. Stp'Last) & ") ");
   end Print_Timestamp_And_Lock;
end Lib.Messages;
