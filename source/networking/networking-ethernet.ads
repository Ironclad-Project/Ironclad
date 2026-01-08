--  networking-ethernet.ads: Ethernet frame handling.
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

package Networking.Ethernet is
   --  Ethernet frame header (14 bytes).
   --  Does not include preamble/SFD (handled by hardware) or FCS (often
   --  handled by hardware or stripped by virtio).

   --  EtherType values.
   EtherType_IPv4 : constant Unsigned_16 := 16#0800#;
   EtherType_ARP  : constant Unsigned_16 := 16#0806#;
   EtherType_IPv6 : constant Unsigned_16 := 16#86DD#;

   --  Broadcast MAC address.
   Broadcast_MAC : constant MAC_Address := [16#FF#, 16#FF#, 16#FF#, 16#FF#,
                                            16#FF#, 16#FF#];

   pragma Warnings (Off, "scalar storage order specified");
   type Ethernet_Header is record
      Destination : MAC_Address;
      Source      : MAC_Address;
      EtherType   : Unsigned_16;
   end record with Size => 14 * 8, Bit_Order => System.High_Order_First,
      Scalar_Storage_Order => System.High_Order_First;
   for Ethernet_Header use record
      Destination at  0 range 0 .. 47;
      Source      at  6 range 0 .. 47;
      EtherType   at 12 range 0 .. 15;
   end record;
   pragma Warnings (On, "scalar storage order specified");

   Header_Size : constant Natural := Ethernet_Header'Size / 8;

   --  Minimum Ethernet payload (padding may be needed).
   Min_Payload_Size : constant Natural := 46;
   --  Maximum Ethernet payload (MTU).
   Max_Payload_Size : constant Natural := 1500;

   --  Create an Ethernet frame header.
   --  @param Dest      Destination MAC address.
   --  @param Src       Source MAC address.
   --  @param Ether_Typ EtherType (protocol identifier).
   --  @return The constructed header.
   function Create_Header
      (Dest      : MAC_Address;
       Src       : MAC_Address;
       Ether_Typ : Unsigned_16) return Ethernet_Header;

   --  Wrap a payload in an Ethernet frame.
   --  @param Dest      Destination MAC address.
   --  @param Src       Source MAC address.
   --  @param Ether_Typ EtherType value.
   --  @param Payload   The payload data.
   --  @param Frame     Output buffer for the complete frame.
   --  @param Frame_Len Actual length of the frame written.
   procedure Create_Frame
      (Dest      : MAC_Address;
       Src       : MAC_Address;
       Ether_Typ : Unsigned_16;
       Payload   : Devices.Operation_Data;
       Frame     : out Devices.Operation_Data;
       Frame_Len : out Natural)
      with Pre => Frame'Length >= Header_Size + Payload'Length;

   --  Parse an Ethernet frame header from raw data.
   --  @param Data    Raw frame data.
   --  @param Header  Parsed header output.
   --  @param Success True if header was successfully parsed.
   procedure Parse_Header
      (Data    : Devices.Operation_Data;
       Header  : out Ethernet_Header;
       Success : out Boolean)
      with Pre => Data'Length >= Header_Size;

   --  Get the payload portion of an Ethernet frame.
   --  @param Frame       Complete Ethernet frame.
   --  @param Payload     Output buffer for payload.
   --  @param Payload_Len Length of payload extracted.
   procedure Get_Payload
      (Frame       : Devices.Operation_Data;
       Payload     : out Devices.Operation_Data;
       Payload_Len : out Natural)
      with Pre => Frame'Length > Header_Size and then
                  Payload'Length >= Frame'Length - Header_Size;
end Networking.Ethernet;
