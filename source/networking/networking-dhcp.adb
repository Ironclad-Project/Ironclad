--  networking-dhcp.adb: DHCP client protocol implementation.
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

with Arch.Clocks;
with Scheduler;
with Networking.Ethernet;
with Networking.IPv4;
with Networking.UDP;

package body Networking.DHCP is
   use type Devices.Operation_Data;
   use type Devices.Dev_Status;

   --  DHCP packet size (fixed header + options).
   DHCP_Header_Size : constant := 236;
   DHCP_Options_Max : constant := 312;
   DHCP_Packet_Size : constant := DHCP_Header_Size + DHCP_Options_Max;

   --  Broadcast addresses.
   Broadcast_IP  : constant IPv4_Address := [others => 255];
   Zero_IP       : constant IPv4_Address := [others => 0];
   Broadcast_MAC : constant  MAC_Address := [others => 16#FF#];

   --  DHCP packet layout offsets (1-indexed for Ada).
   OFF_OP     : constant := 1;
   OFF_HTYPE  : constant := 2;
   OFF_HLEN   : constant := 3;
   OFF_HOPS   : constant := 4;
   OFF_XID    : constant := 5;   --  4 bytes.
   OFF_SECS   : constant := 9;   --  2 bytes.
   OFF_FLAGS  : constant := 11;  --  2 bytes.
   OFF_YIADDR : constant := 17;  --  4 bytes.
   OFF_CHADDR : constant := 29;  --  16 bytes.
   OFF_COOKIE : constant := 237; --  4 bytes.
   OFF_OPTIONS : constant := 241;

   procedure Discover
      (Dev     : Devices.Device_Handle;
       MAC     : MAC_Address;
       Lease   : out DHCP_Lease;
       Success : out Boolean)
   is
      Xid        : Unsigned_32;
      Stamp      : Time.Timestamp;
      Pkt        : Devices.Operation_Data (1 .. DHCP_Packet_Size);
      Pkt_Len    : Natural;
      Send_Ok    : Boolean;
      Recv_Ok    : Boolean;
      Resp_Pkt   : Devices.Operation_Data (1 .. DHCP_Packet_Size);
      Resp_Len   : Natural;
      Msg_Type   : Unsigned_8;
      Subnet     : IPv4_Address;
      Router     : IPv4_Address;
      DNS        : IPv4_Address;
      Server_ID  : IPv4_Address;
      Lease_Time : Unsigned_32;
      Offered_IP : IPv4_Address;
      Retries    : Natural := 3;
   begin
      --  Initialize output.
      Lease := (Assigned_IP   => [0, 0, 0, 0],
                Subnet_Mask   => [0, 0, 0, 0],
                Gateway_IP    => [0, 0, 0, 0],
                DNS_Server_IP => [0, 0, 0, 0],
                Server_IP     => [0, 0, 0, 0],
                Lease_Time    => 0,
                Is_Valid      => False);
      Success := False;

      --  Generate transaction ID from monotonic time.
      Arch.Clocks.Get_Monotonic_Time (Stamp);
      Xid := Unsigned_32 (Stamp.Seconds and 16#FFFFFFFF#);

      --  Retry loop.
      while Retries > 0 loop
         --  Build and send DISCOVER.
         Build_Discover (MAC, Xid, Pkt, Pkt_Len);
         Send_DHCP_Packet (Dev, MAC, Pkt, Pkt_Len, Send_Ok);
         if not Send_Ok then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Wait for OFFER (5 second timeout).
         Wait_DHCP_Response (Dev, Xid, (5, 0), Resp_Pkt, Resp_Len, Recv_Ok);
         if not Recv_Ok then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Parse OFFER.
         Parse_Options (Resp_Pkt, Msg_Type, Subnet, Router, DNS,
                        Server_ID, Lease_Time);
         if Msg_Type /= DHCP_OFFER then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Get offered IP from yiaddr field.
         Offered_IP := [Resp_Pkt (OFF_YIADDR),
                        Resp_Pkt (OFF_YIADDR + 1),
                        Resp_Pkt (OFF_YIADDR + 2),
                        Resp_Pkt (OFF_YIADDR + 3)];

         --  Build and send REQUEST.
         Build_Request (MAC, Xid, Offered_IP, Server_ID, Pkt, Pkt_Len);
         Send_DHCP_Packet (Dev, MAC, Pkt, Pkt_Len, Send_Ok);
         if not Send_Ok then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Wait for ACK (5 second timeout).
         Wait_DHCP_Response (Dev, Xid, (5, 0), Resp_Pkt, Resp_Len, Recv_Ok);
         if not Recv_Ok then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Parse ACK.
         Parse_Options (Resp_Pkt, Msg_Type, Subnet, Router, DNS,
                        Server_ID, Lease_Time);
         if Msg_Type = DHCP_NAK then
            Retries := Retries - 1;
            goto Continue;
         elsif Msg_Type /= DHCP_ACK then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Success! Fill in lease.
         Lease.Assigned_IP := Offered_IP;
         Lease.Subnet_Mask := Subnet;
         Lease.Gateway_IP := Router;
         Lease.DNS_Server_IP := DNS;
         Lease.Server_IP := Server_ID;
         Lease.Lease_Time := Lease_Time;
         Lease.Is_Valid := True;
         Success := True;
         return;

      <<Continue>>
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Discover;
   ----------------------------------------------------------------------------
   procedure Build_Discover
      (MAC   : MAC_Address;
       Xid   : Unsigned_32;
       Pkt   : out Devices.Operation_Data;
       Len   : out Natural)
   is
      Idx : Natural := OFF_OPTIONS;
   begin
      --  Initialize to zeros.
      Pkt := [others => 0];

      --  Fixed header.
      Pkt (OFF_OP) := BOOTREQUEST;
      Pkt (OFF_HTYPE) := HTYPE_ETHERNET;
      Pkt (OFF_HLEN) := 6;
      Pkt (OFF_HOPS) := 0;

      --  Transaction ID (big-endian).
      Pkt (OFF_XID) := Unsigned_8 (Shift_Right (Xid, 24) and 16#FF#);
      Pkt (OFF_XID + 1) := Unsigned_8 (Shift_Right (Xid, 16) and 16#FF#);
      Pkt (OFF_XID + 2) := Unsigned_8 (Shift_Right (Xid, 8) and 16#FF#);
      Pkt (OFF_XID + 3) := Unsigned_8 (Xid and 16#FF#);

      --  Seconds = 0.
      Pkt (OFF_SECS) := 0;
      Pkt (OFF_SECS + 1) := 0;

      --  Flags: broadcast bit set.
      Pkt (OFF_FLAGS) := 16#80#;
      Pkt (OFF_FLAGS + 1) := 16#00#;

      --  Client hardware address (MAC).
      for I in MAC'Range loop
         Pkt (OFF_CHADDR + I - 1) := MAC (I);
      end loop;

      --  Magic cookie (big-endian).
      Pkt (OFF_COOKIE) := 16#63#;
      Pkt (OFF_COOKIE + 1) := 16#82#;
      Pkt (OFF_COOKIE + 2) := 16#53#;
      Pkt (OFF_COOKIE + 3) := 16#63#;

      --  Option 53: DHCP Message Type = DISCOVER.
      Pkt (Idx) := OPT_MESSAGE_TYPE;
      Pkt (Idx + 1) := 1;
      Pkt (Idx + 2) := DHCP_DISCOVER;
      Idx := Idx + 3;

      --  Option 55: Parameter Request List.
      Pkt (Idx) := OPT_PARAMETER_LIST;
      Pkt (Idx + 1) := 4;
      Pkt (Idx + 2) := OPT_SUBNET_MASK;
      Pkt (Idx + 3) := OPT_ROUTER;
      Pkt (Idx + 4) := OPT_DNS_SERVER;
      Pkt (Idx + 5) := OPT_LEASE_TIME;
      Idx := Idx + 6;

      --  End option.
      Pkt (Idx) := OPT_END;
      Idx := Idx + 1;

      Len := Idx - 1;
      --  Minimum DHCP packet is 300 bytes (including padding).
      if Len < 300 then
         Len := 300;
      end if;
   exception
      when Constraint_Error =>
         Pkt := [others => 0];
         Len := 0;
   end Build_Discover;

   procedure Build_Request
      (MAC         : MAC_Address;
       Xid         : Unsigned_32;
       Offered_IP  : IPv4_Address;
       Server_IP   : IPv4_Address;
       Pkt         : out Devices.Operation_Data;
       Len         : out Natural)
   is
      Idx : Natural := OFF_OPTIONS;
   begin
      --  Initialize to zeros.
      Pkt := [others => 0];

      --  Fixed header (same as DISCOVER).
      Pkt (OFF_OP) := BOOTREQUEST;
      Pkt (OFF_HTYPE) := HTYPE_ETHERNET;
      Pkt (OFF_HLEN) := 6;
      Pkt (OFF_HOPS) := 0;

      --  Transaction ID.
      Pkt (OFF_XID) := Unsigned_8 (Shift_Right (Xid, 24) and 16#FF#);
      Pkt (OFF_XID + 1) := Unsigned_8 (Shift_Right (Xid, 16) and 16#FF#);
      Pkt (OFF_XID + 2) := Unsigned_8 (Shift_Right (Xid, 8) and 16#FF#);
      Pkt (OFF_XID + 3) := Unsigned_8 (Xid and 16#FF#);

      --  Flags: broadcast bit set.
      Pkt (OFF_FLAGS) := 16#80#;
      Pkt (OFF_FLAGS + 1) := 16#00#;

      --  Client hardware address (MAC).
      for I in MAC'Range loop
         Pkt (OFF_CHADDR + I - 1) := MAC (I);
      end loop;

      --  Magic cookie.
      Pkt (OFF_COOKIE) := 16#63#;
      Pkt (OFF_COOKIE + 1) := 16#82#;
      Pkt (OFF_COOKIE + 2) := 16#53#;
      Pkt (OFF_COOKIE + 3) := 16#63#;

      --  Option 53: DHCP Message Type = REQUEST.
      Pkt (Idx) := OPT_MESSAGE_TYPE;
      Pkt (Idx + 1) := 1;
      Pkt (Idx + 2) := DHCP_REQUEST;
      Idx := Idx + 3;

      --  Option 50: Requested IP Address.
      Pkt (Idx) := OPT_REQUESTED_IP;
      Pkt (Idx + 1) := 4;
      Pkt (Idx + 2) := Offered_IP (1);
      Pkt (Idx + 3) := Offered_IP (2);
      Pkt (Idx + 4) := Offered_IP (3);
      Pkt (Idx + 5) := Offered_IP (4);
      Idx := Idx + 6;

      --  Option 54: Server Identifier.
      Pkt (Idx) := OPT_SERVER_ID;
      Pkt (Idx + 1) := 4;
      Pkt (Idx + 2) := Server_IP (1);
      Pkt (Idx + 3) := Server_IP (2);
      Pkt (Idx + 4) := Server_IP (3);
      Pkt (Idx + 5) := Server_IP (4);
      Idx := Idx + 6;

      --  Option 55: Parameter Request List.
      Pkt (Idx) := OPT_PARAMETER_LIST;
      Pkt (Idx + 1) := 4;
      Pkt (Idx + 2) := OPT_SUBNET_MASK;
      Pkt (Idx + 3) := OPT_ROUTER;
      Pkt (Idx + 4) := OPT_DNS_SERVER;
      Pkt (Idx + 5) := OPT_LEASE_TIME;
      Idx := Idx + 6;

      --  End option.
      Pkt (Idx) := OPT_END;
      Idx := Idx + 1;

      Len := Idx - 1;
      if Len < 300 then
         Len := 300;
      end if;
   exception
      when Constraint_Error =>
         Pkt := [others => 0];
         Len := 0;
   end Build_Request;

   procedure Parse_Options
      (Pkt         : Devices.Operation_Data;
       Msg_Type    : out Unsigned_8;
       Subnet      : out IPv4_Address;
       Router      : out IPv4_Address;
       DNS         : out IPv4_Address;
       Server_ID   : out IPv4_Address;
       Lease_Time  : out Unsigned_32)
   is
      Idx : Natural := OFF_OPTIONS;
      Opt_Code : Unsigned_8;
      Opt_Len  : Unsigned_8;
   begin
      --  Initialize outputs.
      Msg_Type := 0;
      Subnet := [0, 0, 0, 0];
      Router := [0, 0, 0, 0];
      DNS := [0, 0, 0, 0];
      Server_ID := [0, 0, 0, 0];
      Lease_Time := 0;

      --  Verify magic cookie.
      if Pkt (OFF_COOKIE) /= 16#63# or else
         Pkt (OFF_COOKIE + 1) /= 16#82# or else
         Pkt (OFF_COOKIE + 2) /= 16#53# or else
         Pkt (OFF_COOKIE + 3) /= 16#63#
      then
         return;
      end if;

      --  Parse options.
      while Idx <= Pkt'Last loop
         Opt_Code := Pkt (Idx);

         if Opt_Code = OPT_END then
            exit;
         elsif Opt_Code = OPT_PAD then
            Idx := Idx + 1;
         else
            if Idx + 1 > Pkt'Last then
               exit;
            end if;
            Opt_Len := Pkt (Idx + 1);
            if Idx + 1 + Natural (Opt_Len) > Pkt'Last then
               exit;
            end if;

            case Opt_Code is
               when OPT_MESSAGE_TYPE =>
                  if Opt_Len >= 1 then
                     Msg_Type := Pkt (Idx + 2);
                  end if;

               when OPT_SUBNET_MASK =>
                  if Opt_Len >= 4 then
                     Subnet := [Pkt (Idx + 2), Pkt (Idx + 3),
                                Pkt (Idx + 4), Pkt (Idx + 5)];
                  end if;

               when OPT_ROUTER =>
                  if Opt_Len >= 4 then
                     Router := [Pkt (Idx + 2), Pkt (Idx + 3),
                                Pkt (Idx + 4), Pkt (Idx + 5)];
                  end if;

               when OPT_DNS_SERVER =>
                  if Opt_Len >= 4 then
                     DNS := [Pkt (Idx + 2), Pkt (Idx + 3),
                             Pkt (Idx + 4), Pkt (Idx + 5)];
                  end if;

               when OPT_SERVER_ID =>
                  if Opt_Len >= 4 then
                     Server_ID := [Pkt (Idx + 2), Pkt (Idx + 3),
                                   Pkt (Idx + 4), Pkt (Idx + 5)];
                  end if;

               when OPT_LEASE_TIME =>
                  if Opt_Len >= 4 then
                     Lease_Time := Shift_Left (Unsigned_32 (Pkt (Idx + 2)), 24)
                        or Shift_Left (Unsigned_32 (Pkt (Idx + 3)), 16)
                        or Shift_Left (Unsigned_32 (Pkt (Idx + 4)), 8)
                        or Unsigned_32 (Pkt (Idx + 5));
                  end if;

               when others =>
                  null;
            end case;

            Idx := Idx + 2 + Natural (Opt_Len);
         end if;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Parse_Options;

   procedure Send_DHCP_Packet
      (Dev     : Devices.Device_Handle;
       Src_MAC : MAC_Address;
       Payload : Devices.Operation_Data;
       Len     : Natural;
       Success : out Boolean)
   is
      --  Build UDP header.
      UDP_Hdr      : UDP.UDP_Header;
      UDP_Hdr_Size : constant := 8;

      pragma Warnings (Off, "storage order");
      UDP_Hdr_Bytes : Devices.Operation_Data (1 .. UDP_Hdr_Size)
         with Import, Address => UDP_Hdr'Address;
      pragma Warnings (On, "storage order");

      --  Build IP header.
      IP_Hdr      : IPv4.IPv4_Packet_Header;
      IP_Hdr_Size : constant := 20;

      pragma Warnings (Off, "storage order");
      IP_Hdr_Bytes : Devices.Operation_Data (1 .. IP_Hdr_Size)
         with Import, Address => IP_Hdr'Address;
      pragma Warnings (On, "storage order");

      --  Full packet.
      Eth_Len   : Natural;
      Ret_Count : Natural;
      Status    : Devices.Dev_Status;
   begin
      declare
         Frame_Len : constant Natural :=
            Ethernet.Header_Size + IP_Hdr_Size + UDP_Hdr_Size + Len;
         Frame     : Devices.Operation_Data (1 .. Frame_Len);
      begin
         --  UDP header.
         UDP_Hdr.Source_Port := DHCP_Client_Port;
         UDP_Hdr.Destination_Port := DHCP_Server_Port;
         UDP_Hdr.Length := Unsigned_16 (UDP_Hdr_Size + Len);
         UDP_Hdr.Checksum := 0;  --  Optional for IPv4.

         --  IP header: 0.0.0.0 -> 255.255.255.255.
         IP_Hdr := IPv4.Generate_Header
            (Zero_IP, Broadcast_IP, UDP_Hdr_Size + Len, IPv4.Protocol_UDP);

         --  Build Ethernet frame.
         Ethernet.Create_Frame
            (Broadcast_MAC, Src_MAC, Ethernet.EtherType_IPv4,
             IP_Hdr_Bytes & UDP_Hdr_Bytes & Payload (1 .. Len),
             Frame, Eth_Len);

         --  Send.
         Devices.Write (Dev, 0, Frame (1 .. Eth_Len), Ret_Count, Status);
         Success := Status = Devices.Dev_Success;
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Send_DHCP_Packet;

   procedure Wait_DHCP_Response
      (Dev          : Devices.Device_Handle;
       Expected_Xid : Unsigned_32;
       Timeout      : Time.Timestamp;
       Pkt          : out Devices.Operation_Data;
       Pkt_Len      : out Natural;
       Success      : out Boolean)
   is
      use Time;

      Recv_Buf   : Devices.Operation_Data (1 .. 1518);
      Recv_Count : Natural;
      Recv_Stat  : Devices.Dev_Status;
      Eth_Hdr    : Ethernet.Ethernet_Header;
      Parse_Ok   : Boolean;
      IP_Hdr     : IPv4.IPv4_Packet_Header;
      IP_Len     : Natural;
      Start_Time : Time.Timestamp;
      Now_Time   : Time.Timestamp;
      Rx_Xid     : Unsigned_32;
   begin
      Success := False;
      Pkt_Len := 0;

      Arch.Clocks.Get_Monotonic_Time (Start_Time);

      loop
         --  Check timeout.
         Arch.Clocks.Get_Monotonic_Time (Now_Time);
         if (Now_Time - Start_Time) >= Timeout then
            return;
         end if;

         --  Try to receive a packet.
         Devices.Read (Dev, 0, Recv_Buf, Recv_Count, Recv_Stat);
         if Recv_Stat = Devices.Dev_Success and Recv_Count > 0 then
            --  Parse Ethernet header.
            Ethernet.Parse_Header (Recv_Buf (1 .. Recv_Count), Eth_Hdr,
                                   Parse_Ok);
            if Parse_Ok and then
               Eth_Hdr.EtherType = Ethernet.EtherType_IPv4
            then
               --  Parse IP header.
               IPv4.Parse_Header
                  (Recv_Buf (Ethernet.Header_Size + 1 .. Recv_Count),
                   IP_Hdr, Parse_Ok);
               if Parse_Ok and then
                  IP_Hdr.Protocol = IPv4.Protocol_UDP
               then
                  IP_Len := Natural (IP_Hdr.IHL) * 4;
                  declare
                     UDP_Start : constant Natural :=
                        Ethernet.Header_Size + IP_Len + 1;
                     UDP_Len   : constant Natural :=
                        Recv_Count - UDP_Start + 1;
                     Src_Port  : Unsigned_16;
                     Dst_Port  : Unsigned_16;
                  begin
                     if UDP_Len >= 8 then
                        --  Access UDP header directly from Recv_Buf.
                        Src_Port := Shift_Left (Unsigned_16 (
                           Recv_Buf (UDP_Start)), 8) or
                           Unsigned_16 (Recv_Buf (UDP_Start + 1));
                        Dst_Port := Shift_Left (Unsigned_16 (
                           Recv_Buf (UDP_Start + 2)), 8) or
                           Unsigned_16 (Recv_Buf (UDP_Start + 3));

                        if Src_Port = DHCP_Server_Port and
                           Dst_Port = DHCP_Client_Port
                        then
                           --  DHCP packet. Check XID.
                           --  Skip 8-byte UDP header to get to DHCP.
                           declare
                              DHCP_Start : constant Natural := UDP_Start + 8;
                              DHCP_Len   : constant Natural :=
                                 Recv_Count - DHCP_Start + 1;
                              Xid_Off    : constant Natural := DHCP_Start + 4;
                           begin
                              if DHCP_Len >= DHCP_Header_Size then
                                 Rx_Xid :=
                                    Shift_Left (Unsigned_32 (
                                       Recv_Buf (Xid_Off)), 24) or
                                    Shift_Left (Unsigned_32 (
                                       Recv_Buf (Xid_Off + 1)), 16) or
                                    Shift_Left (Unsigned_32 (
                                       Recv_Buf (Xid_Off + 2)), 8) or
                                    Unsigned_32 (Recv_Buf (Xid_Off + 3));

                                 if Rx_Xid = Expected_Xid then
                                    --  Match! Copy DHCP packet.
                                    Pkt_Len := DHCP_Len;
                                    if Pkt_Len > Pkt'Length then
                                       Pkt_Len := Pkt'Length;
                                    end if;
                                    for I in 1 .. Pkt_Len loop
                                       Pkt (I) := Recv_Buf
                                          (DHCP_Start + I - 1);
                                    end loop;
                                    Success := True;
                                    return;
                                 end if;
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end if;
            end if;
         end if;

         Scheduler.Yield_If_Able;
      end loop;
   exception
      when Constraint_Error =>
         Success := True;
   end Wait_DHCP_Response;
end Networking.DHCP;
