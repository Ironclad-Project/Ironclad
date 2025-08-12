--  messages.adb: Utilities for reporting messages to the user.
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
with Interfaces; use Interfaces;

package body Messages with
   SPARK_Mode => Off,
   Refined_State => [Message_State =>
      [Messages_Mutex, Curr_Entry, Small_Log_Buffer, Log_Ring_Buffer]]
is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Enable_Logging is
   begin
      Synchronization.Seize (Messages_Mutex);
      Log_Ring_Buffer := new Message_Buffer'[1 .. 100 => [others => ' ']];
      Log_Ring_Buffer (1 .. Small_Log_Buffer'Length) := Small_Log_Buffer;
      Synchronization.Release (Messages_Mutex);
   end Enable_Logging;

   procedure Put_Line (Message : String) is
      Max_Len : constant Natural := Max_Line - Timestamp_Str'Length - 3;
      Msg_Idx, Msg_LIdx : Natural;
   begin
      if Message'Length = 0 then
         Add_To_Buffers (Message);
      else
         Msg_Idx := Message'First;
         loop
            if Message (Msg_Idx .. Message'Last)'Length > Max_Len then
               Msg_LIdx := Msg_Idx + Max_Len - 1;
            else
               Msg_LIdx := Message'Last;
            end if;

            Add_To_Buffers (Message (Msg_Idx .. Msg_LIdx));

            exit when Msg_Idx + Max_Len >= Message'Last;
            Msg_Idx := Msg_Idx + Max_Len;
         end loop;
      end if;
   end Put_Line;

   procedure Dump_Logs (Buffer : out String; Length : out Natural) is
      Idx : Natural := 0;
   begin
      Synchronization.Seize (Messages_Mutex);

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

      Synchronization.Release (Messages_Mutex);
   end Dump_Logs;
   ----------------------------------------------------------------------------
   procedure Get_Timestamp (Timestamp : out Timestamp_Str) is
      Sec, NSec : Unsigned_64;
   begin
      Arch.Clocks.Get_Monotonic_Time (Sec, NSec);
      Timestamp := [others => '0'];
      Timestamp (Timestamp'Last - Sec'Image'Length + 1 .. Timestamp'Last) :=
         Sec'Image;
   end Get_Timestamp;

   procedure Add_To_Buffers (Message : String) is
      Timestamp : String (1 .. 10);
      Final     : String (1 .. Max_Line) := (others => ' ');
      Last_Idx  : Natural;
   begin
      Get_Timestamp (Timestamp);
      Final (1) := '(';
      Final (2 .. 11) := Timestamp;
      Final (12 .. 13) := ") ";
      Final (14 .. Message'Length + 13) := Message;

      Synchronization.Seize (Messages_Mutex);

      if Log_Ring_Buffer /= null then
         Last_Idx := Log_Ring_Buffer'Last;
         Log_Ring_Buffer (Curr_Entry) := Final;
      else
         Last_Idx := Small_Log_Buffer'Last;
         Small_Log_Buffer (Curr_Entry) := Final;
      end if;

      Arch.Debug.Print (Final (1 .. Message'Length + 13));
      Arch.Debug.Print (Ada.Characters.Latin_1.CR);
      Arch.Debug.Print (Ada.Characters.Latin_1.LF);

      Curr_Entry := Curr_Entry + 1;
      if Curr_Entry > Last_Idx then
         Curr_Entry := 1;
      end if;

      Synchronization.Release (Messages_Mutex);
   end Add_To_Buffers;
end Messages;
