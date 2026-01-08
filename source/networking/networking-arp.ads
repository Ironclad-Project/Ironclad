--  networking-arp.ads: Address resolution, from MAC to IP (be it 4 or 6).
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

with System;
with Devices;

package Networking.ARP is
   --  Addresses will be cached if discovery is needed, which can lead to
   --  inconsistent lookup times if requests are needed after an eviction.

   --  Initialize the module.
   procedure Initialize
      with Pre  => not Is_Initialized,
           Post => Is_Initialized;

   --  Add a static address for an interface's MAC address.
   --  These addresses will never be evicted from cache, so use of these
   --  addresses can be assumed to always be consistent.
   procedure Add_Static
      (MAC        : MAC_Address;
       IP4        : IPv4_Address;
       IP4_Subnet : IPv4_Address)
      with Pre => Is_Initialized;

   procedure Modify_Static
      (MAC        : MAC_Address;
       IP4        : IPv4_Address;
       IP4_Subnet : IPv4_Address)
      with Pre => Is_Initialized;

   --  Lookup the associated IPv4 addresses for a MAC address.
   procedure Lookup (MAC : MAC_Address; IP, Subnet : out IPv4_Address)
      with Pre => Is_Initialized;

   --  Lookup the associated MAC address for an IPv4 one.
   procedure Lookup (IP : IPv4_Address; MAC : out MAC_Address)
      with Pre => Is_Initialized;

   --  Ghost function for checking whether the device handling is initialized.
   function Is_Initialized return Boolean with Ghost;
   ----------------------------------------------------------------------------
   --  ARP Packet Handling

   --  ARP operation codes.
   ARP_OP_Request : constant Unsigned_16 := 1;
   ARP_OP_Reply   : constant Unsigned_16 := 2;

   --  ARP hardware types.
   ARP_HW_Ethernet : constant Unsigned_16 := 1;

   --  ARP protocol types (same as EtherType).
   ARP_Proto_IPv4 : constant Unsigned_16 := 16#0800#;

   --  ARP packet structure for Ethernet/IPv4.
   pragma Warnings (Off, "scalar storage order specified");
   type ARP_Packet is record
      Hardware_Type    : Unsigned_16;
      Protocol_Type    : Unsigned_16;
      Hardware_Size    : Unsigned_8;
      Protocol_Size    : Unsigned_8;
      Operation        : Unsigned_16;
      Sender_MAC       : MAC_Address;
      Sender_IP        : IPv4_Address;
      Target_MAC       : MAC_Address;
      Target_IP        : IPv4_Address;
   end record with Size => 28 * 8, Bit_Order => System.High_Order_First,
      Scalar_Storage_Order => System.High_Order_First;
   for ARP_Packet use record
      Hardware_Type    at  0 range 0 .. 15;
      Protocol_Type    at  2 range 0 .. 15;
      Hardware_Size    at  4 range 0 .. 7;
      Protocol_Size    at  5 range 0 .. 7;
      Operation        at  6 range 0 .. 15;
      Sender_MAC       at  8 range 0 .. 47;
      Sender_IP        at 14 range 0 .. 31;
      Target_MAC       at 18 range 0 .. 47;
      Target_IP        at 24 range 0 .. 31;
   end record;
   pragma Warnings (On, "scalar storage order specified");

   ARP_Packet_Size : constant Natural := ARP_Packet'Size / 8;

   --  Create an ARP request packet.
   --  @param Sender_MAC Our MAC address.
   --  @param Sender_IP  Our IP address.
   --  @param Target_IP  IP address we're looking for.
   --  @return ARP request packet.
   function Create_Request
      (Sender_MAC : MAC_Address;
       Sender_IP  : IPv4_Address;
       Target_IP  : IPv4_Address) return ARP_Packet;

   --  Create an ARP reply packet.
   --  @param Sender_MAC Our MAC address.
   --  @param Sender_IP  Our IP address.
   --  @param Target_MAC Destination MAC address.
   --  @param Target_IP  Destination IP address.
   --  @return ARP reply packet.
   function Create_Reply
      (Sender_MAC : MAC_Address;
       Sender_IP  : IPv4_Address;
       Target_MAC : MAC_Address;
       Target_IP  : IPv4_Address) return ARP_Packet;

   --  Parse an ARP packet from raw data.
   --  @param Data    Raw packet data.
   --  @param Packet  Parsed ARP packet.
   --  @param Success True if successfully parsed.
   procedure Parse_Packet
      (Data    : Devices.Operation_Data;
       Packet  : out ARP_Packet;
       Success : out Boolean)
      with Pre => Data'Length >= ARP_Packet_Size;

   --  Convert an ARP packet to raw bytes.
   --  @param Packet ARP packet to convert.
   --  @param Data   Output buffer.
   procedure To_Bytes
      (Packet : ARP_Packet;
       Data   : out Devices.Operation_Data)
      with Pre => Data'Length >= ARP_Packet_Size;

private

   type ARP_Entry is record
      MAC        : MAC_Address;
      IP4        : IPv4_Address;
      IP4_Subnet : IPv4_Address;
   end record;
   type ARP_Entries is array (1 .. 50) of ARP_Entry;
   type ARP_Entries_Acc is access ARP_Entries;

   Interface_Entries : ARP_Entries_Acc := null;

   function Is_Initialized return Boolean is (Interface_Entries /= null);
end Networking.ARP;
