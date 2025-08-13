--  messages.ads: Utilities for reporting messages to the user.
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

with Arch.Clocks;
with Synchronization; use Synchronization;

package Messages with
   Abstract_State => Message_State,
   Initializes    => Message_State
is
   --  Enable in-memory message logging.
   --  Until this function is called, the kernel uses a really small built-in
   --  buffer.
   procedure Enable_Logging
      with Pre    => not Is_Initialized,
           Post   => Is_Initialized,
           Global => (In_Out => Message_State);

   --  Header added to warning messages.
   Warning_String : constant String := "Warning: ";

   --  Prints a string to debug outputs and adds a newline.
   --  @param Message String to print to append with a new line.
   procedure Put_Line (Message : String)
      with Global =>
         (In_Out => (Arch.Clocks.Monotonic_Clock_State, Message_State));

   --  Dump the logs from the ring buffer, they may be out of order!
   --  @param Buffer Buffer to store the messages.
   --  @param Length Total length in bytes of the logs, even if it doesnt fit.
   procedure Dump_Logs (Buffer : out String; Length : out Natural)
      with Pre    => Is_Initialized and Buffer'First = 1,
           Global => (In_Out => (Message_State));
   ----------------------------------------------------------------------------
   --  Ghost function for checking whether login was initialized.
   function Is_Initialized return Boolean with Ghost;

private

   --  Maximum length of a single log line.
   Max_Line : constant := 80;

   --  Buffers for holding log lines.
   type Message_Buffer is array (Natural range <>) of String (1 .. Max_Line);
   type Message_Buffer_Acc is access Message_Buffer (1 .. 100);

   Messages_Mutex   : aliased Binary_Semaphore := Unlocked_Semaphore
      with Part_Of => Message_State;
   Curr_Entry       : Natural := 1
      with Part_Of => Message_State;
   Small_Log_Buffer : Message_Buffer (1 .. 15) := [others => [1 .. 80 => ' ']]
      with Part_Of => Message_State;
   Log_Ring_Buffer  : Message_Buffer_Acc := null
      with Part_Of => Message_State;

   subtype Timestamp_Str is String (1 .. 10);
   procedure Get_Timestamp (Timestamp : out Timestamp_Str)
      with Global => (In_Out => Arch.Clocks.Monotonic_Clock_State);

   procedure Add_To_Buffers (Message : String)
      with Pre    => Message'Length <= Max_Line - Timestamp_Str'Length - 3,
           Global => (In_Out =>
                       (Arch.Clocks.Monotonic_Clock_State, Message_State));

   function Is_Initialized return Boolean is (Log_Ring_Buffer /= null);
end Messages;
