--  networking-tcp.ads: TCP protocol support.
--  Copyright (C) 2025 streaksu
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
with Devices;
with Synchronization;
with Time;

package Networking.TCP is
   --  TCP Header structure (20 bytes minimum, without options).
   type Unsigned_4 is mod 2 ** 4;
   type Unsigned_6 is mod 2 ** 6;

   pragma Warnings (Off, "scalar storage order specified");
   type TCP_Header is record
      Source_Port      : Unsigned_16;
      Destination_Port : Unsigned_16;
      Sequence_Number  : Unsigned_32;
      Ack_Number       : Unsigned_32;
      Data_Offset      : Unsigned_4;    --  Header length in 32-bit words.
      Reserved         : Unsigned_6;
      Flag_URG         : Boolean;
      Flag_ACK         : Boolean;
      Flag_PSH         : Boolean;
      Flag_RST         : Boolean;
      Flag_SYN         : Boolean;
      Flag_FIN         : Boolean;
      Window           : Unsigned_16;
      Checksum         : Unsigned_16;
      Urgent_Pointer   : Unsigned_16;
   end record with Size => 20 * 8, Bit_Order => System.High_Order_First,
      Scalar_Storage_Order => System.High_Order_First;
   for TCP_Header use record
      Source_Port      at  0 range 0 .. 15;
      Destination_Port at  2 range 0 .. 15;
      Sequence_Number  at  4 range 0 .. 31;
      Ack_Number       at  8 range 0 .. 31;
      Data_Offset      at 12 range 0 .. 3;
      Reserved         at 12 range 4 .. 9;
      Flag_URG         at 13 range 2 .. 2;
      Flag_ACK         at 13 range 3 .. 3;
      Flag_PSH         at 13 range 4 .. 4;
      Flag_RST         at 13 range 5 .. 5;
      Flag_SYN         at 13 range 6 .. 6;
      Flag_FIN         at 13 range 7 .. 7;
      Window           at 14 range 0 .. 15;
      Checksum         at 16 range 0 .. 15;
      Urgent_Pointer   at 18 range 0 .. 15;
   end record;
   pragma Warnings (On, "scalar storage order specified");

   Header_Size : constant Natural := 20;

   --  TCP Connection States.
   type TCP_State is
      (State_Closed,
       State_Listen,
       State_Syn_Sent,
       State_Syn_Received,
       State_Established,
       State_Fin_Wait_1,
       State_Fin_Wait_2,
       State_Close_Wait,
       State_Closing,
       State_Last_Ack,
       State_Time_Wait);

   --  TCP Connection Control Block.
   --  Represents a single TCP connection with all state.
   Default_Window_Size : constant Unsigned_16 := 16#4000#;  --  16KB.
   Max_Segment_Size    : constant Natural := 1460;  --  MTU - IP - TCP headers.
   Recv_Buffer_Size    : constant Natural := 16#8000#;  --  32KB.
   Send_Buffer_Size    : constant Natural := 16#8000#;  --  32KB.

   type TCP_Connection is record
      State         : TCP_State;
      Mutex         : aliased Synchronization.Mutex;

      --  Local and remote endpoints.
      Local_IP      : IPv4_Address;
      Local_Port    : Unsigned_16;
      Remote_IP     : IPv4_Address;
      Remote_Port   : Unsigned_16;

      --  Sequence numbers.
      Send_Unack    : Unsigned_32;  --  Oldest unacknowledged.
      Send_Next     : Unsigned_32;  --  Next to send.
      Send_Window   : Unsigned_16;  --  Peer's receive window.
      Send_ISS      : Unsigned_32;  --  Initial send sequence number.

      Recv_Next     : Unsigned_32;  --  Next expected sequence.
      Recv_Window   : Unsigned_16;  --  Our receive window.
      Recv_IRS      : Unsigned_32;  --  Initial receive sequence number.

      --  Receive buffer.
      Recv_Buffer   : Devices.Operation_Data (1 .. Recv_Buffer_Size);
      Recv_Len      : Natural range 0 .. Recv_Buffer_Size;

      --  Send buffer.
      Send_Buffer   : Devices.Operation_Data (1 .. Send_Buffer_Size);
      Send_Len      : Natural range 0 .. Send_Buffer_Size;

      --  Receive timeout (0 = infinite/blocking).
      Recv_Timeout : Time.Timestamp;
   end record;
   type TCP_Connection_Acc is access TCP_Connection;

   --  Parse a TCP header from raw data.
   --  @param Data    Raw packet data.
   --  @param Header  Parsed header output.
   --  @param Success True if successfully parsed.
   procedure Parse_Header
      (Data    : Devices.Operation_Data;
       Header  : out TCP_Header;
       Success : out Boolean)
      with Pre => Data'Length >= Header_Size;

   function Generate_Header_Base
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Window    : Unsigned_16;
       Flags_ACK : Boolean;
       Flags_SYN : Boolean;
       Flags_FIN : Boolean;
       Flags_RST : Boolean;
       Flags_PSH : Boolean) return TCP_Header;

   --  Calculate TCP checksum (with IP pseudo-header).
   --  @param Hdr     TCP header.
   --  @param Src_IP  Source IP address.
   --  @param Dest_IP Destination IP address.
   --  @param Payload TCP payload data.
   --  @return 16-bit checksum.
   function Calculate_Checksum
      (Hdr     : TCP_Header;
       Src_IP  : IPv4_Address;
       Dest_IP : IPv4_Address;
       Payload : Devices.Operation_Data) return Unsigned_16;

   --  Generate a SYN packet header for connection initiation.
   --  @param Src_Port  Source port.
   --  @param Dest_Port Destination port.
   --  @param Seq_Num   Initial sequence number.
   --  @param Src_IP    Source IP (for checksum).
   --  @param Dest_IP   Destination IP (for checksum).
   --  @return TCP header with SYN flag set.
   function Generate_SYN
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header;

   --  Generate a SYN-ACK packet header.
   --  @param Src_Port  Source port.
   --  @param Dest_Port Destination port.
   --  @param Seq_Num   Our sequence number.
   --  @param Ack_Num   Acknowledgment number (their seq + 1).
   --  @param Src_IP    Source IP (for checksum).
   --  @param Dest_IP   Destination IP (for checksum).
   --  @return TCP header with SYN+ACK flags set.
   function Generate_SYN_ACK
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header;

   --  Generate an ACK packet header.
   --  @param Src_Port  Source port.
   --  @param Dest_Port Destination port.
   --  @param Seq_Num   Our sequence number.
   --  @param Ack_Num   Acknowledgment number.
   --  @param Window    Our receive window size.
   --  @param Src_IP    Source IP (for checksum).
   --  @param Dest_IP   Destination IP (for checksum).
   --  @return TCP header with ACK flag set.
   function Generate_ACK
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Window    : Unsigned_16;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header;

   --  Generate a data packet header (ACK + PSH + data).
   --  @param Src_Port  Source port.
   --  @param Dest_Port Destination port.
   --  @param Seq_Num   Sequence number.
   --  @param Ack_Num   Acknowledgment number.
   --  @param Window    Our receive window size.
   --  @param Src_IP    Source IP (for checksum).
   --  @param Dest_IP   Destination IP (for checksum).
   --  @param Payload   Data to send (for checksum).
   --  @return TCP header with ACK+PSH flags set.
   function Generate_Data
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Window    : Unsigned_16;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address;
       Payload   : Devices.Operation_Data) return TCP_Header;

   --  Generate a FIN packet header for connection termination.
   --  @param Src_Port  Source port.
   --  @param Dest_Port Destination port.
   --  @param Seq_Num   Sequence number.
   --  @param Ack_Num   Acknowledgment number.
   --  @param Src_IP    Source IP (for checksum).
   --  @param Dest_IP   Destination IP (for checksum).
   --  @return TCP header with FIN+ACK flags set.
   function Generate_FIN
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header;

   --  Generate a RST packet header.
   --  @param Src_Port  Source port.
   --  @param Dest_Port Destination port.
   --  @param Seq_Num   Sequence number.
   --  @param Src_IP    Source IP (for checksum).
   --  @param Dest_IP   Destination IP (for checksum).
   --  @return TCP header with RST flag set.
   function Generate_RST
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header;

   --  Convert TCP header to bytes.
   --  @param Hdr  TCP header.
   --  @param Data Output buffer.
   procedure To_Bytes
      (Hdr  : TCP_Header;
       Data : out Devices.Operation_Data)
      with Pre => Data'Length >= Header_Size;

   --  Generate a pseudo-random initial sequence number.
   --  @return A sequence number based on current time/counter.
   procedure Generate_ISN (ISN : out Unsigned_32);
end Networking.TCP;
