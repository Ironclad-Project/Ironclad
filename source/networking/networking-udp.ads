--  networking-udp.ads: UDP protocol support.
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

package Networking.UDP is
   --  UDP Header structure (8 bytes).
   type UDP_Header is record
      Source_Port      : Unsigned_16;
      Destination_Port : Unsigned_16;
      Length           : Unsigned_16;
      Checksum         : Unsigned_16;
   end record with Size => 8 * 8, Bit_Order => System.High_Order_First,
      Scalar_Storage_Order => System.High_Order_First;
   for UDP_Header use record
      Source_Port      at 0 range 0 .. 15;
      Destination_Port at 2 range 0 .. 15;
      Length           at 4 range 0 .. 15;
      Checksum         at 6 range 0 .. 15;
   end record;

   Header_Size : constant Natural := UDP_Header'Size / 8;

   --  Generate a UDP header with checksum.
   --  @param Src_Port   Source port number.
   --  @param Dest_Port  Destination port number.
   --  @param Data_Len   Length of payload data.
   --  @param Src_IP     Source IP (for pseudo-header checksum).
   --  @param Dest_IP    Destination IP (for pseudo-header checksum).
   --  @param Payload    Payload data (for checksum).
   --  @return UDP header with valid checksum.
   function Generate_Header
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Data_Len  : Natural;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address;
       Payload   : Devices.Operation_Data) return UDP_Header;

   --  Parse a UDP header from raw data.
   --  @param Data    Raw packet data.
   --  @param Header  Parsed header output.
   --  @param Success True if successfully parsed.
   procedure Parse_Header
      (Data    : Devices.Operation_Data;
       Header  : out UDP_Header;
       Success : out Boolean)
      with Pre => Data'Length >= Header_Size;

   --  Calculate UDP checksum (with IP pseudo-header).
   --  @param Hdr     UDP header.
   --  @param Src_IP  Source IP address.
   --  @param Dest_IP Destination IP address.
   --  @param Payload UDP payload data.
   --  @return 16-bit checksum (0 if checksum disabled).
   function Calculate_Checksum
      (Hdr     : UDP_Header;
       Src_IP  : IPv4_Address;
       Dest_IP : IPv4_Address;
       Payload : Devices.Operation_Data) return Unsigned_16;

   --  Verify UDP checksum.
   --  @param Hdr     UDP header (with checksum).
   --  @param Src_IP  Source IP address.
   --  @param Dest_IP Destination IP address.
   --  @param Payload UDP payload data.
   --  @return True if checksum is valid (or zero, meaning disabled).
   function Verify_Checksum
      (Hdr     : UDP_Header;
       Src_IP  : IPv4_Address;
       Dest_IP : IPv4_Address;
       Payload : Devices.Operation_Data) return Boolean;
end Networking.UDP;
