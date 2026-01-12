--  networking-stack.ads: Network stack manager.
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

with Devices;
with Networking.TCP;
with Synchronization;
with Time;

package Networking.Stack is
   --  This module manages the network stack, including:
   --  - Packet transmission with Ethernet framing
   --  - ARP resolution
   --  - TCP connection management
   --  - UDP socket management

   --  Set the gateway IP address.
   procedure Set_Gateway (IP : IPv4_Address);

   --  Get the gateway IP address.
   function Get_Gateway return IPv4_Address;
   ----------------------------------------------------------------------------
   --  Packet Transmission

   --  Send an IPv4 packet to a destination.
   --  Handles ARP resolution and Ethernet framing.
   --  @param Dev      Network device to send on.
   --  @param Dest_IP  Destination IP address.
   --  @param Protocol IP protocol number.
   --  @param Payload  Packet payload (excluding IP header).
   --  @param Success  True if packet was sent.
   procedure Send_IPv4_Packet
      (Dev      : Devices.Device_Handle;
       Src_IP   : IPv4_Address;
       Dest_IP  : IPv4_Address;
       Protocol : Unsigned_8;
       Payload  : Devices.Operation_Data;
       Success  : out Boolean);

   --  Send a raw Ethernet frame.
   --  @param Dev       Network device to send on.
   --  @param Dest_MAC  Destination MAC address.
   --  @param Src_MAC   Source MAC address.
   --  @param EtherType Ethernet type field.
   --  @param Payload   Frame payload.
   --  @param Success   True if frame was sent.
   procedure Send_Ethernet_Frame
      (Dev       : Devices.Device_Handle;
       Dest_MAC  : MAC_Address;
       Src_MAC   : MAC_Address;
       EtherType : Unsigned_16;
       Payload   : Devices.Operation_Data;
       Success   : out Boolean);
   ----------------------------------------------------------------------------
   --  ARP Resolution

   --  Resolve an IP address to a MAC address.
   --  Sends ARP request if not in cache and waits for reply.
   --  @param Dev     Network device.
   --  @param Our_IP  Our IP address.
   --  @param Our_MAC Our MAC address.
   --  @param Target  IP address to resolve.
   --  @param Result  Resolved MAC address (all zeros if failed).
   --  @param Success True if resolution succeeded.
   procedure Resolve_ARP
      (Dev     : Devices.Device_Handle;
       Our_IP  : IPv4_Address;
       Our_MAC : MAC_Address;
       Target  : IPv4_Address;
       Result  : out MAC_Address;
       Success : out Boolean);
   ----------------------------------------------------------------------------
   --  TCP Connection Management

   --  Maximum number of concurrent TCP connections.
   Max_TCP_Connections : constant := 64;

   --  TCP Connection handle.
   subtype TCP_Conn_Handle is Natural range 0 .. Max_TCP_Connections;
   Invalid_TCP_Handle : constant TCP_Conn_Handle := 0;

   --  Create a new TCP connection (client mode).
   --  Performs the 3-way handshake.
   --  @param Local_IP    Our IP address.
   --  @param Local_Port  Our port number.
   --  @param Remote_IP   Server IP address.
   --  @param Remote_Port Server port number.
   --  @param Handle      Connection handle on success.
   --  @param Success     True if connection established.
   procedure TCP_Connect
      (Local_IP    : IPv4_Address;
       Local_Port  : Unsigned_16;
       Remote_IP   : IPv4_Address;
       Remote_Port : Unsigned_16;
       Handle      : out TCP_Conn_Handle;
       Success     : out Boolean);

   --  Create a listening TCP socket.
   --  @param Local_IP   IP address to listen on.
   --  @param Local_Port Port to listen on.
   --  @param Handle     Listener handle on success.
   --  @param Success    True if listening started.
   procedure TCP_Listen
      (Local_IP   : IPv4_Address;
       Local_Port : Unsigned_16;
       Handle     : out TCP_Conn_Handle;
       Success    : out Boolean);

   --  Accept an incoming TCP connection.
   --  Blocks until a connection arrives.
   --  @param Listener_Handle Listening socket handle.
   --  @param Client_Handle   New connection handle.
   --  @param Remote_IP       Client's IP address.
   --  @param Remote_Port     Client's port.
   --  @param Success         True if connection accepted.
   procedure TCP_Accept
      (Listener_Handle : TCP_Conn_Handle;
       Client_Handle   : out TCP_Conn_Handle;
       Remote_IP       : out IPv4_Address;
       Remote_Port     : out Unsigned_16;
       Success         : out Boolean);

   --  Send data on a TCP connection.
   --  @param Handle    Connection handle.
   --  @param Data      Data to send.
   --  @param Sent      Number of bytes sent.
   --  @param Success   True if send succeeded.
   procedure TCP_Send
      (Handle  : TCP_Conn_Handle;
       Data    : Devices.Operation_Data;
       Sent    : out Natural;
       Success : out Boolean);

   --  Receive data from a TCP connection.
   --  @param Handle      Connection handle.
   --  @param Data        Buffer to receive into.
   --  @param Received    Number of bytes received.
   --  @param Is_Blocking If True, wait for data.
   --  @param Success     True if receive succeeded.
   procedure TCP_Recv
      (Handle      : TCP_Conn_Handle;
       Data        : out Devices.Operation_Data;
       Received    : out Natural;
       Is_Blocking : Boolean;
       Success     : out Boolean);

   --  Close a TCP connection.
   --  Performs graceful shutdown with FIN handshake.
   --  @param Handle Connection handle to close.
   procedure TCP_Close (Handle : in out TCP_Conn_Handle);

   --  Get the state of a TCP connection.
   --  @param Handle Connection handle.
   --  @return Current TCP state.
   function TCP_Get_State (Handle : TCP_Conn_Handle) return TCP.TCP_State;

   --  Check if a TCP connection has data available to read.
   --  @param Handle Connection handle.
   --  @return True if there is data in the receive buffer.
   function TCP_Has_Data (Handle : TCP_Conn_Handle) return Boolean;

   --  Set receive timeout for a TCP connection.
   --  @param Handle  Connection handle.
   --  @param Timeout Timeout value.
   --  @param Success True if timeout was set successfully.
   procedure TCP_Set_Recv_Timeout
      (Handle : TCP_Conn_Handle;
       Timeout : Time.Timestamp;
       Success : out Boolean);
   ----------------------------------------------------------------------------
   --  UDP Socket Management

   --  Maximum number of concurrent UDP sockets.
   Max_UDP_Sockets : constant := 32;

   --  UDP Socket handle.
   subtype UDP_Socket_Handle is Natural range 0 .. Max_UDP_Sockets;
   Invalid_UDP_Handle : constant UDP_Socket_Handle := 0;

   --  Bind a UDP socket to a local port.
   --  @param Local_IP   IP address to bind to (0.0.0.0 for any).
   --  @param Local_Port Port to bind to.
   --  @param Handle     Socket handle on success.
   --  @param Success    True if bound successfully.
   procedure UDP_Bind
      (Local_IP   : IPv4_Address;
       Local_Port : Unsigned_16;
       Handle     : out UDP_Socket_Handle;
       Success    : out Boolean);

   --  Send a UDP datagram (using handle).
   --  @param Handle    Socket handle.
   --  @param Dest_IP   Destination IP address.
   --  @param Dest_Port Destination port.
   --  @param Data      Datagram payload.
   --  @param Sent      Number of bytes sent.
   --  @param Success   True if sent successfully.
   procedure UDP_Sendto
      (Handle    : UDP_Socket_Handle;
       Dest_IP   : IPv4_Address;
       Dest_Port : Unsigned_16;
       Data      : Devices.Operation_Data;
       Sent      : out Natural;
       Success   : out Boolean);

   --  Receive a UDP datagram.
   --  @param Handle      Socket handle.
   --  @param Data        Buffer to receive into.
   --  @param Received    Number of bytes received.
   --  @param Src_IP      Source IP of datagram.
   --  @param Src_Port    Source port of datagram.
   --  @param Is_Blocking If True, wait for data.
   --  @param Success     True if receive succeeded.
   procedure UDP_Recvfrom
      (Handle      : UDP_Socket_Handle;
       Data        : out Devices.Operation_Data;
       Received    : out Natural;
       Src_IP      : out IPv4_Address;
       Src_Port    : out Unsigned_16;
       Is_Blocking : Boolean;
       Success     : out Boolean);

   --  Close a UDP socket.
   --  @param Handle Socket handle to close.
   procedure UDP_Close (Handle : in out UDP_Socket_Handle);

   --  Check if a UDP socket has data available.
   --  @param Handle Socket handle.
   --  @return True if there is data in the receive buffer.
   function UDP_Has_Data (Handle : UDP_Socket_Handle) return Boolean;

   --  Send a UDP datagram (legacy, without handle).
   --  @param Src_IP    Source IP address.
   --  @param Src_Port  Source port.
   --  @param Dest_IP   Destination IP address.
   --  @param Dest_Port Destination port.
   --  @param Data      Datagram payload.
   --  @param Success   True if sent successfully.
   procedure UDP_Send
      (Src_IP    : IPv4_Address;
       Src_Port  : Unsigned_16;
       Dest_IP   : IPv4_Address;
       Dest_Port : Unsigned_16;
       Data      : Devices.Operation_Data;
       Success   : out Boolean);
   ----------------------------------------------------------------------------
   --  Packet Processing (called by NIC driver or polling loop)

   --  Process a received Ethernet frame.
   --  Handles ARP, IPv4, and dispatches to TCP/UDP.
   --  @param Dev   Device that received the frame.
   --  @param Frame Raw Ethernet frame data.
   procedure Process_Received_Frame
      (Dev   : Devices.Device_Handle;
       Frame : Devices.Operation_Data);

   procedure Send_TCP_Packet
      (Conn    : TCP.TCP_Connection_Acc;
       Dev     : Devices.Device_Handle;
       Tcp_Hdr : TCP.TCP_Header;
       Payload : Devices.Operation_Data;
       Success : out Boolean);

   function Allocate_TCP_Slot return TCP_Conn_Handle;

   procedure Get_Ephemeral_Port (Port : out Unsigned_16);

private

   Stack_Mutex : aliased Synchronization.Mutex :=
      Synchronization.Unlocked_Mutex;

   --  Gateway configuration (set by DHCP or statically).
   Gateway_IP  : IPv4_Address := [0, 0, 0, 0];
   Gateway_MAC : MAC_Address  := [others => 0];

   --  TCP connection table.
   type TCP_Conn_Entry is record
      In_Use : Boolean;
      Conn   : TCP.TCP_Connection_Acc;
      Dev    : Devices.Device_Handle;
   end record;

   TCP_Connections : array (1 .. Max_TCP_Connections) of TCP_Conn_Entry :=
      [others => (In_Use => False, Conn => null, Dev => Devices.Error_Handle)];
   TCP_Mutex : aliased Synchronization.Mutex := Synchronization.Unlocked_Mutex;

   --  Ephemeral port counter for client connections.
   Next_Ephemeral_Port : Unsigned_16 := 49152;

   --  UDP receive buffer size.
   UDP_Recv_Buffer_Size : constant := 8192;

   --  UDP socket entry.
   type UDP_Socket_Entry is record
      In_Use     : Boolean;
      Local_IP   : IPv4_Address;
      Local_Port : Unsigned_16;
      Dev        : Devices.Device_Handle;
      --  Receive buffer for one datagram.
      Recv_Buffer : Devices.Operation_Data (1 .. UDP_Recv_Buffer_Size);
      Recv_Len    : Natural;
      Recv_Src_IP : IPv4_Address;
      Recv_Src_Port : Unsigned_16;
      Has_Data    : Boolean;
   end record;

   UDP_Sockets : array (1 .. Max_UDP_Sockets) of UDP_Socket_Entry :=
      [others => (In_Use => False, Local_IP => [others => 0], Local_Port => 0,
                  Dev => Devices.Error_Handle,
                  Recv_Buffer => [others => 0], Recv_Len => 0,
                  Recv_Src_IP => [others => 0], Recv_Src_Port => 0,
                  Has_Data => False)];
   UDP_Mutex : aliased Synchronization.Mutex := Synchronization.Unlocked_Mutex;

   procedure Handle_IPv4_Packet
      (Dev  : Devices.Device_Handle;
       Data : Devices.Operation_Data);

   procedure Deliver_UDP_Packet
      (Dev         : Devices.Device_Handle;
       Src_IP      : IPv4_Address;
       Src_Port    : Unsigned_16;
       Dest_Port   : Unsigned_16;
       Payload     : Devices.Operation_Data;
       Payload_Len : Natural);

   procedure Deliver_TCP_Packet
      (Dev      : Devices.Device_Handle;
       Src_IP   : IPv4_Address;
       TCP_Hdr  : TCP.TCP_Header;
       Payload  : Devices.Operation_Data);
end Networking.Stack;
