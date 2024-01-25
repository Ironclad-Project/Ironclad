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
with Arch.Debug;

package body Lib.Messages with
   Refined_State => (Message_State =>
      (Messages_Mutex, Curr_Entry, Small_Log_Buffer, Log_Ring_Buffer))
is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Enable_Logging is
   begin
      Lib.Synchronization.Seize (Messages_Mutex);
      Log_Ring_Buffer := new Message_Buffer'(1 .. 100 => (others => ' '));
      Log_Ring_Buffer (1 .. Small_Log_Buffer'Length) := Small_Log_Buffer;
      Lib.Synchronization.Release (Messages_Mutex);
   end Enable_Logging;

   procedure Put_Line (Message : String) is
      Timestamp : String (1 .. 10);
      Final     : String (1 .. Max_Line) := (others => ' ');
      Last_Idx  : Natural;
   begin
      Get_Timestamp (Timestamp);
      Final (1) := '(';
      Final (2 .. 11) := Timestamp;
      Final (12 .. 13) := ") ";
      Final (14 .. Message'Length + 13) := Message;

      Lib.Synchronization.Seize (Messages_Mutex);

      if Log_Ring_Buffer /= null then
         Last_Idx := Log_Ring_Buffer'Last;
         Log_Ring_Buffer (Curr_Entry) := Final;
      else
         Last_Idx := Small_Log_Buffer'Last;
         Small_Log_Buffer (Curr_Entry) := Final;
      end if;

      Arch.Debug.Print (Final);
      Arch.Debug.Print (Ada.Characters.Latin_1.CR);
      Arch.Debug.Print (Ada.Characters.Latin_1.LF);

      Curr_Entry := Curr_Entry + 1;
      if Curr_Entry > Last_Idx then
         Curr_Entry := 1;
      end if;

      Lib.Synchronization.Release (Messages_Mutex);
   end Put_Line;

   procedure Dump_Logs (Buffer : out String; Length : out Natural) is
      Idx : Natural := 0;
   begin
      Lib.Synchronization.Seize (Messages_Mutex);

      for C of Buffer loop
         C := ' ';
      end loop;
      Length := Log_Ring_Buffer'Length * Max_Line;

      if Buffer'Length < Length or Buffer'Length mod Max_Line /= 0 then
         return;
      end if;

      for Line of Log_Ring_Buffer.all loop
         Buffer (Buffer'First + Idx .. Buffer'First + Idx + Max_Line - 1) :=
            Line;
         Idx := Idx + Max_Line;
      end loop;

      Lib.Synchronization.Release (Messages_Mutex);
   end Dump_Logs;
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
   procedure Get_Timestamp (Timestamp : out Timestamp_Str) is
      Stp     : Translated_String;
      Stp_Len : Natural;
      Sec, NSec : Unsigned_64;
   begin
      Arch.Clocks.Get_Monotonic_Time (Sec, NSec);
      Image (Sec, Stp, Stp_Len, True);
      Timestamp := Stp (11 .. Stp'Last);
   end Get_Timestamp;
end Lib.Messages;
