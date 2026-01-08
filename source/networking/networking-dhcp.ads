--  networking-dhcp.ads: DHCP client protocol support.
--  Copyright (C) 2024 streaksu
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

with Devices;
with Time;

package Networking.DHCP is
   --  DHCP ports.
   DHCP_Server_Port : constant Unsigned_16 := 67;
   DHCP_Client_Port : constant Unsigned_16 := 68;

   --  DHCP message types (option 53).
   DHCP_DISCOVER : constant Unsigned_8 := 1;
   DHCP_OFFER    : constant Unsigned_8 := 2;
   DHCP_REQUEST  : constant Unsigned_8 := 3;
   DHCP_DECLINE  : constant Unsigned_8 := 4;
   DHCP_ACK      : constant Unsigned_8 := 5;
   DHCP_NAK      : constant Unsigned_8 := 6;
   DHCP_RELEASE  : constant Unsigned_8 := 7;

   --  DHCP message opcodes.
   BOOTREQUEST : constant Unsigned_8 := 1;
   BOOTREPLY   : constant Unsigned_8 := 2;

   --  Hardware types.
   HTYPE_ETHERNET : constant Unsigned_8 := 1;

   --  DHCP magic cookie.
   DHCP_Magic_Cookie : constant Unsigned_32 := 16#63825363#;

   --  DHCP option codes.
   OPT_PAD            : constant Unsigned_8 := 0;
   OPT_SUBNET_MASK    : constant Unsigned_8 := 1;
   OPT_ROUTER         : constant Unsigned_8 := 3;
   OPT_DNS_SERVER     : constant Unsigned_8 := 6;
   OPT_HOSTNAME       : constant Unsigned_8 := 12;
   OPT_REQUESTED_IP   : constant Unsigned_8 := 50;
   OPT_LEASE_TIME     : constant Unsigned_8 := 51;
   OPT_MESSAGE_TYPE   : constant Unsigned_8 := 53;
   OPT_SERVER_ID      : constant Unsigned_8 := 54;
   OPT_PARAMETER_LIST : constant Unsigned_8 := 55;
   OPT_END            : constant Unsigned_8 := 255;

   --  DHCP lease information obtained from the server.
   type DHCP_Lease is record
      Assigned_IP   : IPv4_Address;
      Subnet_Mask   : IPv4_Address;
      Gateway_IP    : IPv4_Address;
      DNS_Server_IP : IPv4_Address;
      Server_IP     : IPv4_Address;
      Lease_Time    : Unsigned_32;  --  Seconds.
      Is_Valid      : Boolean;
   end record;

   --  Perform DHCP discovery on a network interface.
   --  Sends DISCOVER, waits for OFFER, sends REQUEST, waits for ACK.
   --  @param Dev     Network device handle.
   --  @param MAC     MAC address of the interface.
   --  @param Lease   Output lease information.
   --  @param Success True if DHCP succeeded.
   procedure Discover
      (Dev     : Devices.Device_Handle;
       MAC     : MAC_Address;
       Lease   : out DHCP_Lease;
       Success : out Boolean);

private

   procedure Wait_DHCP_Response
      (Dev          : Devices.Device_Handle;
       Expected_Xid : Unsigned_32;
       Timeout      : Time.Timestamp;
       Pkt          : out Devices.Operation_Data;
       Pkt_Len      : out Natural;
       Success      : out Boolean);

   procedure Build_Discover
      (MAC   : MAC_Address;
       Xid   : Unsigned_32;
       Pkt   : out Devices.Operation_Data;
       Len   : out Natural);

   procedure Build_Request
      (MAC         : MAC_Address;
       Xid         : Unsigned_32;
       Offered_IP  : IPv4_Address;
       Server_IP   : IPv4_Address;
       Pkt         : out Devices.Operation_Data;
       Len         : out Natural);

   procedure Parse_Options
      (Pkt         : Devices.Operation_Data;
       Msg_Type    : out Unsigned_8;
       Subnet      : out IPv4_Address;
       Router      : out IPv4_Address;
       DNS         : out IPv4_Address;
       Server_ID   : out IPv4_Address;
       Lease_Time  : out Unsigned_32);

   procedure Send_DHCP_Packet
      (Dev     : Devices.Device_Handle;
       Src_MAC : MAC_Address;
       Payload : Devices.Operation_Data;
       Len     : Natural;
       Success : out Boolean);
end Networking.DHCP;
