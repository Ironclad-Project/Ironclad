--  networking-stack.adb: Network stack manager.
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

with Ada.Unchecked_Deallocation;
with Networking.Ethernet;
with Networking.ARP;
with Networking.IPv4;
with Networking.UDP;
with Networking.Interfaces;
with Scheduler;
with Arch.Clocks;

package body Networking.Stack with SPARK_Mode => Off is
   use type Devices.Dev_Status;
   use type Devices.Device_Handle;
   use type TCP.TCP_State;

   procedure Free is new Ada.Unchecked_Deallocation
      (TCP.TCP_Connection, TCP.TCP_Connection_Acc);

   procedure Set_Gateway (IP : IPv4_Address) is
   begin
      Gateway_IP := IP;
      Gateway_MAC := [others => 0];  --  Clear cached MAC, will be resolved.
   end Set_Gateway;

   function Get_Gateway return IPv4_Address is
   begin
      return Gateway_IP;
   end Get_Gateway;
   ----------------------------------------------------------------------------
   procedure Send_Ethernet_Frame
      (Dev       : Devices.Device_Handle;
       Dest_MAC  : MAC_Address;
       Src_MAC   : MAC_Address;
       EtherType : Unsigned_16;
       Payload   : Devices.Operation_Data;
       Success   : out Boolean)
   is
      Frame_Len : Natural;
      Ret_Count : Natural;
      Status    : Devices.Dev_Status;
   begin
      declare
         Frame     : Devices.Operation_Data
            (1 .. Ethernet.Header_Size + Payload'Length);
      begin
         Ethernet.Create_Frame (Dest_MAC, Src_MAC, EtherType, Payload,
                                Frame, Frame_Len);
         Devices.Write (Dev, 0, Frame (1 .. Frame_Len), Ret_Count, Status);
         Success := Status = Devices.Dev_Success;
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Send_Ethernet_Frame;

   procedure Send_IPv4_Packet
      (Dev      : Devices.Device_Handle;
       Src_IP   : IPv4_Address;
       Dest_IP  : IPv4_Address;
       Protocol : Unsigned_8;
       Payload  : Devices.Operation_Data;
       Success  : out Boolean)
   is
      Src_MAC   : MAC_Address;
      Dest_MAC  : MAC_Address;
      Target_IP : IPv4_Address;
      Ip_Hdr    : IPv4.IPv4_Packet_Header;

      pragma Warnings (Off, "storage order");
      Ip_Hdr_Bytes : Devices.Operation_Data (1 .. IPv4.Header_Size)
         with Import, Address => Ip_Hdr'Address;
      pragma Warnings (On, "storage order");
   begin
      Success := False;

      --  Get our MAC address.
      ARP.Lookup (Src_IP, Src_MAC);
      if Src_MAC = [0, 0, 0, 0, 0, 0] then
         --  Try looking up by interface.
         declare
            Dummy_IP : IPv4_Address;
            Dummy_Subnet : IPv4_Address;
         begin
            Interfaces.Get_Interface_Address (Dev, Dummy_IP);
            ARP.Lookup (Dummy_IP, Src_MAC);
         end;
      end if;

      --  Determine if destination is on local network or needs gateway.
      --  Simple check: if different /8 subnet, use gateway.
      if Dest_IP (1) /= Src_IP (1) then
         Target_IP := Gateway_IP;
      else
         Target_IP := Dest_IP;
      end if;

      --  Resolve destination MAC via ARP.
      Resolve_ARP (Dev, Src_IP, Src_MAC, Target_IP, Dest_MAC, Success);
      if not Success then
         return;
      end if;

      --  Build IP packet.
      declare
         Pkt_Len   : constant Natural := IPv4.Header_Size + Payload'Length;
         Ip_Pkt    : Devices.Operation_Data (1 .. Pkt_Len);
      begin
         Ip_Hdr := IPv4.Generate_Header
            (Src_IP, Dest_IP, Payload'Length, Protocol);
         Ip_Pkt (1 .. IPv4.Header_Size) := Ip_Hdr_Bytes;
         Ip_Pkt (IPv4.Header_Size + 1 .. Ip_Pkt'Last) := Payload;

         --  Send in Ethernet frame.
         Send_Ethernet_Frame (Dev, Dest_MAC, Src_MAC, Ethernet.EtherType_IPv4,
                              Ip_Pkt, Success);
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Send_IPv4_Packet;
   ----------------------------------------------------------------------------
   procedure Resolve_ARP
      (Dev     : Devices.Device_Handle;
       Our_IP  : IPv4_Address;
       Our_MAC : MAC_Address;
       Target  : IPv4_Address;
       Result  : out MAC_Address;
       Success : out Boolean)
   is
      Arp_Req    : ARP.ARP_Packet;
      Arp_Bytes  : Devices.Operation_Data (1 .. ARP.ARP_Packet_Size);
      Retries    : Natural := 3;
      Recv_Buf   : Devices.Operation_Data (1 .. 1518);
      Recv_Count : Natural;
      Recv_Stat  : Devices.Dev_Status;
      Eth_Hdr    : Ethernet.Ethernet_Header;
      Parse_Ok   : Boolean;
      Arp_Reply  : ARP.ARP_Packet;
   begin
      --  First check cache.
      ARP.Lookup (Target, Result);
      if Result /= [0, 0, 0, 0, 0, 0] then
         Success := True;
         return;
      end if;

      --  Send ARP request.
      Arp_Req := ARP.Create_Request (Our_MAC, Our_IP, Target);
      ARP.To_Bytes (Arp_Req, Arp_Bytes);

      while Retries > 0 loop
         --  Send ARP request as broadcast.
         Send_Ethernet_Frame (Dev, Ethernet.Broadcast_MAC, Our_MAC,
                              Ethernet.EtherType_ARP, Arp_Bytes, Success);
         if not Success then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Wait for reply (simple polling, should have timeout).
         for Wait_Iter in 1 .. 1000 loop
            --  Check cache first in case interrupt handler already updated it.
            ARP.Lookup (Target, Result);
            if Result /= [0, 0, 0, 0, 0, 0] then
               Success := True;
               return;
            end if;

            Devices.Read (Dev, 0, Recv_Buf, Recv_Count, Recv_Stat);
            if Recv_Stat = Devices.Dev_Success and then
               Recv_Count >= Ethernet.Header_Size + ARP.ARP_Packet_Size
            then
               Ethernet.Parse_Header (Recv_Buf, Eth_Hdr, Parse_Ok);
               if Parse_Ok and then Eth_Hdr.EtherType = Ethernet.EtherType_ARP
               then
                  ARP.Parse_Packet
                     (Recv_Buf (Ethernet.Header_Size + 1 .. Recv_Count),
                      Arp_Reply, Parse_Ok);

                  --  Handle incoming ARP requests for our IP.
                  if Parse_Ok and then
                     Arp_Reply.Operation = ARP.ARP_OP_Request and then
                     Arp_Reply.Target_IP = Our_IP
                  then
                     declare
                        Reply_Pkt : ARP.ARP_Packet;
                        Reply_Bytes : Devices.Operation_Data
                           (1 .. ARP.ARP_Packet_Size);
                        Send_Ok : Boolean;
                     begin
                        Reply_Pkt := ARP.Create_Reply
                           (Our_MAC, Our_IP,
                            Arp_Reply.Sender_MAC, Arp_Reply.Sender_IP);
                        ARP.To_Bytes (Reply_Pkt, Reply_Bytes);
                        Send_Ethernet_Frame
                           (Dev, Arp_Reply.Sender_MAC, Our_MAC,
                            Ethernet.EtherType_ARP, Reply_Bytes, Send_Ok);
                     end;
                  end if;

                  if Parse_Ok and then
                     Arp_Reply.Operation = ARP.ARP_OP_Reply and then
                     Arp_Reply.Sender_IP = Target
                  then
                     --  Got reply! Add to cache.
                     ARP.Add_Static
                        (Arp_Reply.Sender_MAC, Arp_Reply.Sender_IP,
                         [others => 255]);
                     Result := Arp_Reply.Sender_MAC;
                     Success := True;
                     return;
                  end if;
               end if;
            end if;
            Scheduler.Yield_If_Able;
         end loop;

      <<Continue>>
         Retries := Retries - 1;
      end loop;

      Result := [0, 0, 0, 0, 0, 0];
      Success := False;
   exception
      when Constraint_Error =>
         Success := False;
   end Resolve_ARP;
   ----------------------------------------------------------------------------
   function Allocate_TCP_Slot return TCP_Conn_Handle is
   begin
      for I in TCP_Connections'Range loop
         if not TCP_Connections (I).In_Use then
            return I;
         end if;
      end loop;
      return Invalid_TCP_Handle;
   end Allocate_TCP_Slot;

   procedure Get_Ephemeral_Port (Port : out Unsigned_16) is
   begin
      Port := Next_Ephemeral_Port;
      if Next_Ephemeral_Port = 65535 then
         Next_Ephemeral_Port := 49152;
      else
         Next_Ephemeral_Port := Next_Ephemeral_Port + 1;
      end if;
   end Get_Ephemeral_Port;

   procedure Send_TCP_Packet
      (Conn    : TCP.TCP_Connection_Acc;
       Dev     : Devices.Device_Handle;
       Tcp_Hdr : TCP.TCP_Header;
       Payload : Devices.Operation_Data;
       Success : out Boolean)
   is
      pragma Warnings (Off, "storage order");
      Tcp_Hdr_Bytes : constant Devices.Operation_Data (1 .. TCP.Header_Size)
         with Import, Address => Tcp_Hdr'Address;
      pragma Warnings (On, "storage order");
   begin
      declare
         Tcp_Pkt : Devices.Operation_Data
            (1 .. TCP.Header_Size + Payload'Length);
      begin
         Tcp_Pkt (1 .. TCP.Header_Size) := Tcp_Hdr_Bytes;
         if Payload'Length > 0 then
            Tcp_Pkt (TCP.Header_Size + 1 .. Tcp_Pkt'Last) := Payload;
         end if;

         Send_IPv4_Packet (Dev, Conn.Local_IP, Conn.Remote_IP,
                           IPv4.Protocol_TCP, Tcp_Pkt, Success);
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Send_TCP_Packet;

   procedure TCP_Connect
      (Local_IP    : IPv4_Address;
       Local_Port  : Unsigned_16;
       Remote_IP   : IPv4_Address;
       Remote_Port : Unsigned_16;
       Handle      : out TCP_Conn_Handle;
       Success     : out Boolean)
   is
      Slot     : TCP_Conn_Handle;
      Conn     : TCP.TCP_Connection_Acc;
      Dev      : Devices.Device_Handle;
      ISN      : Unsigned_32;
      Syn_Hdr  : TCP.TCP_Header;
      Send_Ok  : Boolean;
      Empty_Payload : constant Devices.Operation_Data (1 .. 0) :=
         [others => 0];
      Act_Local_Port : Unsigned_16 := Local_Port;
   begin
      Handle := Invalid_TCP_Handle;
      Success := False;

      --  Get network device.
      Interfaces.Get_Suitable_Interface (Remote_IP, Dev);
      if Dev = Devices.Error_Handle then
         Interfaces.Get_Suitable_Interface (Local_IP, Dev);
         if Dev = Devices.Error_Handle then
            return;
         end if;
      end if;

      Synchronization.Seize (TCP_Mutex);

      --  Allocate connection slot.
      Slot := Allocate_TCP_Slot;
      if Slot = Invalid_TCP_Handle then
         Synchronization.Release (TCP_Mutex);
         return;
      end if;

      --  Allocate ephemeral port if needed.
      if Act_Local_Port = 0 then
         Get_Ephemeral_Port (Act_Local_Port);
      end if;

      --  Create connection.
      TCP.Generate_ISN (ISN);
      Conn := new TCP.TCP_Connection'
         (State       => TCP.State_Syn_Sent,
          Mutex       => Synchronization.Unlocked_Mutex,
          Local_IP    => Local_IP,
          Local_Port  => Act_Local_Port,
          Remote_IP   => Remote_IP,
          Remote_Port => Remote_Port,
          Send_Unack  => ISN,
          Send_Next   => ISN + 1,
          Send_Window => TCP.Default_Window_Size,
          Send_ISS    => ISN,
          Recv_Next   => 0,
          Recv_Window => TCP.Default_Window_Size,
          Recv_IRS    => 0,
          Recv_Buffer => [others => 0],
          Recv_Len    => 0,
          Send_Buffer => [others => 0],
          Send_Len    => 0,
          Recv_Timeout => (Seconds => 5, Nanoseconds => 0)); --  5 secs default

      TCP_Connections (Slot) := (In_Use => True, Conn => Conn, Dev => Dev);
      Synchronization.Release (TCP_Mutex);

      --  Send SYN.
      Syn_Hdr := TCP.Generate_SYN (Act_Local_Port, Remote_Port, ISN,
                                   Local_IP, Remote_IP);
      Send_TCP_Packet (Conn, Dev, Syn_Hdr, Empty_Payload, Send_Ok);
      if not Send_Ok then
         TCP_Close (Slot);
         return;
      end if;

      --  Wait for state to become Established (interrupt handler processes
      --  SYN-ACK via Deliver_TCP_Packet).
      for Wait_Iter in 1 .. 5000 loop
         if Conn.State = TCP.State_Established then
            Handle := Slot;
            Success := True;
            return;
         end if;
         Scheduler.Yield_If_Able;
      end loop;

      --  Timeout - clean up.
      TCP_Close (Slot);
   exception
      when Constraint_Error =>
         Success := False;
   end TCP_Connect;

   procedure TCP_Listen
      (Local_IP   : IPv4_Address;
       Local_Port : Unsigned_16;
       Handle     : out TCP_Conn_Handle;
       Success    : out Boolean)
   is
      Slot : TCP_Conn_Handle;
      Conn : TCP.TCP_Connection_Acc;
      Dev  : Devices.Device_Handle;
   begin
      Handle := Invalid_TCP_Handle;
      Success := False;

      Interfaces.Get_Suitable_Interface (Local_IP, Dev);
      if Dev = Devices.Error_Handle then
         return;
      end if;

      Synchronization.Seize (TCP_Mutex);
      Slot := Allocate_TCP_Slot;
      if Slot = Invalid_TCP_Handle then
         Synchronization.Release (TCP_Mutex);
         return;
      end if;

      Conn := new TCP.TCP_Connection'
         (State       => TCP.State_Listen,
          Mutex       => Synchronization.Unlocked_Mutex,
          Local_IP    => Local_IP,
          Local_Port  => Local_Port,
          Remote_IP   => [0, 0, 0, 0],
          Remote_Port => 0,
          Send_Unack  => 0,
          Send_Next   => 0,
          Send_Window => TCP.Default_Window_Size,
          Send_ISS    => 0,
          Recv_Next   => 0,
          Recv_Window => TCP.Default_Window_Size,
          Recv_IRS    => 0,
          Recv_Buffer => [others => 0],
          Recv_Len    => 0,
          Send_Buffer => [others => 0],
          Send_Len    => 0,
          Recv_Timeout => (Seconds => 0, Nanoseconds => 0)); --  5 secs default

      TCP_Connections (Slot) := (In_Use => True, Conn => Conn, Dev => Dev);
      Synchronization.Release (TCP_Mutex);

      Handle := Slot;
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end TCP_Listen;

   procedure TCP_Accept
      (Listener_Handle : TCP_Conn_Handle;
       Client_Handle   : out TCP_Conn_Handle;
       Remote_IP       : out IPv4_Address;
       Remote_Port     : out Unsigned_16;
       Success         : out Boolean)
   is
      pragma Unreferenced (Listener_Handle);
   begin
      --  TODO: Implement accept with incoming connection queue.
      Client_Handle := Invalid_TCP_Handle;
      Remote_IP := [0, 0, 0, 0];
      Remote_Port := 0;
      Success := False;
   end TCP_Accept;

   procedure TCP_Send
      (Handle  : TCP_Conn_Handle;
       Data    : Devices.Operation_Data;
       Sent    : out Natural;
       Success : out Boolean)
   is
      Conn    : TCP.TCP_Connection_Acc;
      Dev     : Devices.Device_Handle;
      Tcp_Hdr : TCP.TCP_Header;
      Send_Ok : Boolean;
      To_Send : Natural;
      --  Local copies to avoid holding mutex during send.
      Local_Port  : Unsigned_16;
      Remote_Port : Unsigned_16;
      Send_Next   : Unsigned_32;
      Recv_Next   : Unsigned_32;
      Recv_Window : Unsigned_16;
      Local_IP    : IPv4_Address;
      Remote_IP   : IPv4_Address;
   begin
      Sent := 0;
      Success := False;

      if Handle = Invalid_TCP_Handle or else
         not TCP_Connections (Handle).In_Use
      then
         return;
      end if;

      Conn := TCP_Connections (Handle).Conn;
      Dev := TCP_Connections (Handle).Dev;

      Synchronization.Seize (Conn.Mutex);

      if Conn.State /= TCP.State_Established then
         Synchronization.Release (Conn.Mutex);
         return;
      end if;

      --  Send data in segments.
      To_Send := Data'Length;
      if To_Send > TCP.Max_Segment_Size then
         To_Send := TCP.Max_Segment_Size;
      end if;
      if To_Send > Natural (Conn.Send_Window) then
         To_Send := Natural (Conn.Send_Window);
      end if;

      if To_Send > 0 then
         --  Copy connection parameters before releasing mutex.
         Local_Port  := Conn.Local_Port;
         Remote_Port := Conn.Remote_Port;
         Send_Next   := Conn.Send_Next;
         Recv_Next   := Conn.Recv_Next;
         Recv_Window := Conn.Recv_Window;
         Local_IP    := Conn.Local_IP;
         Remote_IP   := Conn.Remote_IP;

         --  Update Send_Next before releasing mutex.
         Conn.Send_Next := Conn.Send_Next + Unsigned_32 (To_Send);
         Synchronization.Release (Conn.Mutex);

         Tcp_Hdr := TCP.Generate_Data
            (Local_Port, Remote_Port,
             Send_Next, Recv_Next,
             Recv_Window,
             Local_IP, Remote_IP,
             Data (Data'First .. Data'First + To_Send - 1));

         Send_TCP_Packet (Conn, Dev, Tcp_Hdr,
                          Data (Data'First .. Data'First + To_Send - 1),
                          Send_Ok);
         if Send_Ok then
            Sent := To_Send;
            Success := True;
         else
            --  Roll back Send_Next on failure.
            Synchronization.Seize (Conn.Mutex);
            Conn.Send_Next := Conn.Send_Next - Unsigned_32 (To_Send);
            Synchronization.Release (Conn.Mutex);
         end if;
      else
         Synchronization.Release (Conn.Mutex);
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end TCP_Send;

   procedure TCP_Recv
      (Handle      : TCP_Conn_Handle;
       Data        : out Devices.Operation_Data;
       Received    : out Natural;
       Is_Blocking : Boolean;
       Success     : out Boolean)
   is
      use Time;

      Conn          : TCP.TCP_Connection_Acc;
      Timeout       : Time.Timestamp;
      Start_Time    : Time.Timestamp;
      Current_Time  : Time.Timestamp;
      Elapsed       : Time.Timestamp;
      Timed_Out     : Boolean := False;
   begin
      Data := [others => 0];
      Received := 0;
      Success := False;

      if Handle = Invalid_TCP_Handle or else
         not TCP_Connections (Handle).In_Use
      then
         return;
      end if;

      Conn := TCP_Connections (Handle).Conn;

      --  Get timeout and start time.
      Synchronization.Seize (Conn.Mutex);
      Timeout := Conn.Recv_Timeout;
      Synchronization.Release (Conn.Mutex);

      Arch.Clocks.Get_Monotonic_Time (Start_Time);

      --  Wait for data in receive buffer (filled by interrupt handler).
      loop
         Synchronization.Seize (Conn.Mutex);

         if Conn.Recv_Len > 0 then
            --  Data available, copy it out.
            Received := Conn.Recv_Len;
            if Received > Data'Length then
               Received := Data'Length;
            end if;
            Data (Data'First .. Data'First + Received - 1) :=
               Conn.Recv_Buffer (1 .. Received);
            --  Shift remaining data.
            for I in 1 .. Conn.Recv_Len - Received loop
               Conn.Recv_Buffer (I) := Conn.Recv_Buffer (I + Received);
            end loop;
            Conn.Recv_Len := Conn.Recv_Len - Received;
            Synchronization.Release (Conn.Mutex);
            Success := True;
            return;
         end if;

         --  No data available. Check connection state.
         if Conn.State = TCP.State_Close_Wait or else
            Conn.State = TCP.State_Closed
         then
            --  Connection closed, return EOF.
            Synchronization.Release (Conn.Mutex);
            Success := True;  --  EOF is success with Received=0.
            return;
         end if;

         if Conn.State /= TCP.State_Established then
            --  Connection in bad state.
            Synchronization.Release (Conn.Mutex);
            return;
         end if;

         Synchronization.Release (Conn.Mutex);

         --  If non-blocking, return immediately.
         exit when not Is_Blocking;

         --  Check for timeout.
         if Timeout > (0, 0) then
            Arch.Clocks.Get_Monotonic_Time (Current_Time);
            Elapsed := Current_Time - Start_Time;
            if Elapsed >= Timeout then
               Timed_Out := True;
               exit;
            end if;
         end if;

         --  Yield to allow interrupt handler to process incoming packets.
         Scheduler.Yield_If_Able;
      end loop;

      --  If we exited due to timeout, indicate EAGAIN-like behavior.
      if Timed_Out then
         Success := False;  --  Timeout is treated as would-block error.
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end TCP_Recv;

   procedure TCP_Close (Handle : in out TCP_Conn_Handle) is
      Conn    : TCP.TCP_Connection_Acc;
      Dev     : Devices.Device_Handle;
      Fin_Hdr : TCP.TCP_Header;
      Send_Ok : Boolean;
      Empty_Payload : constant Devices.Operation_Data (1 .. 0) :=
         [others => 0];
   begin
      if Handle = Invalid_TCP_Handle or else
         not TCP_Connections (Handle).In_Use
      then
         Handle := Invalid_TCP_Handle;
         return;
      end if;

      Conn := TCP_Connections (Handle).Conn;
      Dev := TCP_Connections (Handle).Dev;

      Synchronization.Seize (Conn.Mutex);

      --  Send FIN if connected.
      if Conn.State = TCP.State_Established then
         --  Copy parameters and release mutex before sending to avoid deadlock
         --  with interrupt handler which also acquires Conn.Mutex.
         declare
            Local_Port  : constant Unsigned_16 := Conn.Local_Port;
            Remote_Port : constant Unsigned_16 := Conn.Remote_Port;
            Send_Next   : constant Unsigned_32 := Conn.Send_Next;
            Recv_Next   : constant Unsigned_32 := Conn.Recv_Next;
            Local_IP    : constant IPv4_Address := Conn.Local_IP;
            Remote_IP   : constant IPv4_Address := Conn.Remote_IP;
         begin
            Conn.State := TCP.State_Fin_Wait_1;
            Synchronization.Release (Conn.Mutex);

            Fin_Hdr := TCP.Generate_FIN
               (Local_Port, Remote_Port, Send_Next, Recv_Next,
                Local_IP, Remote_IP);
            Send_TCP_Packet (Conn, Dev, Fin_Hdr, Empty_Payload, Send_Ok);
         end;
      else
         Synchronization.Release (Conn.Mutex);
      end if;

      --  Clean up.
      Synchronization.Seize (TCP_Mutex);
      TCP_Connections (Handle).In_Use := False;
      Free (TCP_Connections (Handle).Conn);
      Synchronization.Release (TCP_Mutex);

      Handle := Invalid_TCP_Handle;
   exception
      when Constraint_Error =>
         Handle := Invalid_TCP_Handle;
   end TCP_Close;

   function TCP_Get_State (Handle : TCP_Conn_Handle) return TCP.TCP_State is
   begin
      if Handle = Invalid_TCP_Handle or else
         not TCP_Connections (Handle).In_Use
      then
         return TCP.State_Closed;
      end if;
      return TCP_Connections (Handle).Conn.State;
   exception
      when Constraint_Error =>
         return TCP.State_Closed;
   end TCP_Get_State;

   function TCP_Has_Data (Handle : TCP_Conn_Handle) return Boolean is
   begin
      if Handle = Invalid_TCP_Handle or else
         not TCP_Connections (Handle).In_Use
      then
         return False;
      end if;
      return TCP_Connections (Handle).Conn.Recv_Len > 0;
   exception
      when Constraint_Error =>
         return False;
   end TCP_Has_Data;

   procedure TCP_Set_Recv_Timeout
      (Handle : TCP_Conn_Handle;
       Timeout : Time.Timestamp;
       Success : out Boolean)
   is
      Conn : TCP.TCP_Connection_Acc;
   begin
      Success := False;

      if Handle = Invalid_TCP_Handle or else
         not TCP_Connections (Handle).In_Use
      then
         return;
      end if;

      Conn := TCP_Connections (Handle).Conn;
      Synchronization.Seize (Conn.Mutex);
      Conn.Recv_Timeout := Timeout;
      Synchronization.Release (Conn.Mutex);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end TCP_Set_Recv_Timeout;
   ----------------------------------------------------------------------------
   procedure UDP_Send
      (Src_IP    : IPv4_Address;
       Src_Port  : Unsigned_16;
       Dest_IP   : IPv4_Address;
       Dest_Port : Unsigned_16;
       Data      : Devices.Operation_Data;
       Success   : out Boolean)
   is
      Dev     : Devices.Device_Handle;
      Udp_Hdr : UDP.UDP_Header;

      pragma Warnings (Off, "storage order");
      Udp_Hdr_Bytes : Devices.Operation_Data (1 .. UDP.Header_Size)
         with Import, Address => Udp_Hdr'Address;
      pragma Warnings (On, "storage order");
   begin
      Success := False;

      Interfaces.Get_Suitable_Interface (Dest_IP, Dev);
      if Dev = Devices.Error_Handle then
         Interfaces.Get_Suitable_Interface (Src_IP, Dev);
         if Dev = Devices.Error_Handle then
            return;
         end if;
      end if;

      declare
         Udp_Pkt : Devices.Operation_Data (1 .. UDP.Header_Size + Data'Length);
      begin
         Udp_Hdr := UDP.Generate_Header
            (Src_Port, Dest_Port, Data'Length, Src_IP, Dest_IP, Data);
         Udp_Pkt (1 .. UDP.Header_Size) := Udp_Hdr_Bytes;
         Udp_Pkt (UDP.Header_Size + 1 .. Udp_Pkt'Last) := Data;

         Send_IPv4_Packet (Dev, Src_IP, Dest_IP, IPv4.Protocol_UDP, Udp_Pkt,
                           Success);
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end UDP_Send;

   procedure UDP_Bind
      (Local_IP   : IPv4_Address;
       Local_Port : Unsigned_16;
       Handle     : out UDP_Socket_Handle;
       Success    : out Boolean)
   is
      Dev         : Devices.Device_Handle;
      Actual_Port : Unsigned_16;
      Port_In_Use : Boolean;
   begin
      Handle := Invalid_UDP_Handle;
      Success := False;

      --  Get network device.
      Interfaces.Get_Suitable_Interface (Local_IP, Dev);
      if Dev = Devices.Error_Handle then
         --  Use any available device.
         Interfaces.Get_Suitable_Interface ([10, 0, 2, 2], Dev);
         if Dev = Devices.Error_Handle then
            return;
         end if;
      end if;
      Synchronization.Seize (UDP_Mutex);

      --  Determine actual port (ephemeral if 0).
      if Local_Port = 0 then
         --  Find an unused ephemeral port.
         Actual_Port := Next_Ephemeral_Port;
         for Attempt in 1 .. 1000 loop
            Port_In_Use := False;
            for I in 1 .. Max_UDP_Sockets loop
               if UDP_Sockets (I).In_Use and then
                  UDP_Sockets (I).Local_Port = Actual_Port
               then
                  Port_In_Use := True;
                  exit;
               end if;
            end loop;
            exit when not Port_In_Use;
            Actual_Port := Actual_Port + 1;
            if Actual_Port < 49152 then
               Actual_Port := 49152;
            end if;
         end loop;
         if Port_In_Use then
            Synchronization.Release (UDP_Mutex);
            return;
         end if;
         Next_Ephemeral_Port := Actual_Port + 1;
         if Next_Ephemeral_Port < 49152 then
            Next_Ephemeral_Port := 49152;
         end if;
      else
         Actual_Port := Local_Port;
         --  Check if port already bound.
         for I in 1 .. Max_UDP_Sockets loop
            if UDP_Sockets (I).In_Use and then
               UDP_Sockets (I).Local_Port = Actual_Port
            then
               Synchronization.Release (UDP_Mutex);
               return;
            end if;
         end loop;
      end if;

      --  Find free slot.
      for I in 1 .. Max_UDP_Sockets loop
         if not UDP_Sockets (I).In_Use then
            UDP_Sockets (I).In_Use := True;
            UDP_Sockets (I).Local_IP := Local_IP;
            UDP_Sockets (I).Local_Port := Actual_Port;
            UDP_Sockets (I).Dev := Dev;
            UDP_Sockets (I).Recv_Len := 0;
            UDP_Sockets (I).Has_Data := False;
            Handle := I;
            Success := True;
            exit;
         end if;
      end loop;

      Synchronization.Release (UDP_Mutex);
   end UDP_Bind;

   procedure UDP_Sendto
      (Handle    : UDP_Socket_Handle;
       Dest_IP   : IPv4_Address;
       Dest_Port : Unsigned_16;
       Data      : Devices.Operation_Data;
       Sent      : out Natural;
       Success   : out Boolean)
   is
      Src_IP  : IPv4_Address;
      Src_Port : Unsigned_16;
   begin
      Sent := 0;
      Success := False;

      if Handle = Invalid_UDP_Handle or else not UDP_Sockets (Handle).In_Use
      then
         return;
      end if;

      Src_Port := UDP_Sockets (Handle).Local_Port;
      Interfaces.Get_Interface_Address
         (UDP_Sockets (Handle).Dev, Src_IP);

      UDP_Send (Src_IP, Src_Port, Dest_IP, Dest_Port, Data, Success);
      if Success then
         Sent := Data'Length;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end UDP_Sendto;

   procedure UDP_Recvfrom
      (Handle      : UDP_Socket_Handle;
       Data        : out Devices.Operation_Data;
       Received    : out Natural;
       Src_IP      : out IPv4_Address;
       Src_Port    : out Unsigned_16;
       Is_Blocking : Boolean;
       Success     : out Boolean)
   is
      Dev       : Devices.Device_Handle;
      Recv_Buf  : Devices.Operation_Data (1 .. 1518);
      Recv_Cnt  : Natural;
      Recv_Stat : Devices.Dev_Status;
   begin
      Data := [others => 0];
      Received := 0;
      Src_IP := [others => 0];
      Src_Port := 0;
      Success := False;

      if Handle = Invalid_UDP_Handle or else not UDP_Sockets (Handle).In_Use
      then
         return;
      end if;

      Dev := UDP_Sockets (Handle).Dev;

      loop
         --  Check if there's buffered data.
         Synchronization.Seize (UDP_Mutex);
         if UDP_Sockets (Handle).Has_Data then
            Received := UDP_Sockets (Handle).Recv_Len;
            if Received > Data'Length then
               Received := Data'Length;
            end if;
            Data (Data'First .. Data'First + Received - 1) :=
               UDP_Sockets (Handle).Recv_Buffer (1 .. Received);
            Src_IP := UDP_Sockets (Handle).Recv_Src_IP;
            Src_Port := UDP_Sockets (Handle).Recv_Src_Port;
            UDP_Sockets (Handle).Has_Data := False;
            UDP_Sockets (Handle).Recv_Len := 0;
            Synchronization.Release (UDP_Mutex);
            Success := True;
            return;
         end if;
         Synchronization.Release (UDP_Mutex);

         --  Try to receive and process a packet.
         Devices.Read (Dev, 0, Recv_Buf, Recv_Cnt, Recv_Stat, Is_Blocking);
         if Recv_Stat = Devices.Dev_Success and Recv_Cnt > 0 then
            --  Dispatch to network stack (will buffer for matching socket).
            Process_Received_Frame (Dev, Recv_Buf (1 .. Recv_Cnt));
         end if;

         exit when not Is_Blocking;
         Scheduler.Yield_If_Able;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end UDP_Recvfrom;

   procedure UDP_Close (Handle : in out UDP_Socket_Handle) is
   begin
      if Handle = Invalid_UDP_Handle or else not UDP_Sockets (Handle).In_Use
      then
         Handle := Invalid_UDP_Handle;
         return;
      end if;

      Synchronization.Seize (UDP_Mutex);
      UDP_Sockets (Handle).In_Use := False;
      UDP_Sockets (Handle).Has_Data := False;
      UDP_Sockets (Handle).Recv_Len := 0;
      Synchronization.Release (UDP_Mutex);

      Handle := Invalid_UDP_Handle;
   exception
      when Constraint_Error =>
         Handle := Invalid_UDP_Handle;
   end UDP_Close;

   function UDP_Has_Data (Handle : UDP_Socket_Handle) return Boolean is
   begin
      if Handle = Invalid_UDP_Handle or else not UDP_Sockets (Handle).In_Use
      then
         return False;
      end if;
      return UDP_Sockets (Handle).Has_Data;
   exception
      when Constraint_Error =>
         return False;
   end UDP_Has_Data;
   ----------------------------------------------------------------------------
   procedure Process_Received_Frame
      (Dev   : Devices.Device_Handle;
       Frame : Devices.Operation_Data)
   is
      Eth_Hdr   : Ethernet.Ethernet_Header;
      Parse_Ok  : Boolean;
      Arp_Pkt   : ARP.ARP_Packet;
      Our_IP    : IPv4_Address;
      Our_MAC   : MAC_Address;
      Arp_Reply : ARP.ARP_Packet;
      Arp_Bytes : Devices.Operation_Data (1 .. ARP.ARP_Packet_Size);
      Send_Ok   : Boolean;
   begin
      if Frame'Length < Ethernet.Header_Size then
         return;
      end if;

      Ethernet.Parse_Header (Frame, Eth_Hdr, Parse_Ok);
      if not Parse_Ok then
         return;
      end if;

      case Eth_Hdr.EtherType is
         when Ethernet.EtherType_ARP =>
            --  Handle ARP.
            if Frame'Length >= Ethernet.Header_Size + ARP.ARP_Packet_Size then
               ARP.Parse_Packet
                  (Frame (Ethernet.Header_Size + 1 .. Frame'Last),
                   Arp_Pkt, Parse_Ok);
               if Parse_Ok then
                  --  Learn from any ARP packet.
                  ARP.Add_Static
                     (Arp_Pkt.Sender_MAC, Arp_Pkt.Sender_IP,
                      [others => 255]);

                  --  If it's a request for us, reply.
                  if Arp_Pkt.Operation = ARP.ARP_OP_Request then
                     Interfaces.Get_Interface_Address (Dev, Our_IP);
                     if Our_IP = Arp_Pkt.Target_IP then
                        ARP.Lookup (Our_IP, Our_MAC);
                        Arp_Reply := ARP.Create_Reply
                           (Our_MAC, Our_IP,
                            Arp_Pkt.Sender_MAC, Arp_Pkt.Sender_IP);
                        ARP.To_Bytes (Arp_Reply, Arp_Bytes);
                        Send_Ethernet_Frame
                           (Dev, Arp_Pkt.Sender_MAC, Our_MAC,
                            Ethernet.EtherType_ARP, Arp_Bytes, Send_Ok);
                     end if;
                  end if;
               end if;
            end if;

         when Ethernet.EtherType_IPv4 =>
            Handle_IPv4_Packet
               (Dev, Frame (Ethernet.Header_Size + 1 .. Frame'Last));

         when others =>
            null;
      end case;
   exception
      when Constraint_Error =>
         return;
   end Process_Received_Frame;
   ----------------------------------------------------------------------------
   procedure Handle_IPv4_Packet
      (Dev  : Devices.Device_Handle;
       Data : Devices.Operation_Data)
   is
      IP_Hdr      : IPv4.IPv4_Packet_Header;
      UDP_Hdr     : UDP.UDP_Header;
      Parse_Ok    : Boolean;
      IP_Hdr_Len  : Natural;
      Payload_Off : Natural;
      Payload_Len : Natural;
   begin
      if Data'Length < IPv4.Header_Size then
         return;
      end if;

      IPv4.Parse_Header (Data, IP_Hdr, Parse_Ok);
      if not Parse_Ok then
         return;
      end if;

      --  Verify checksum.
      if not IPv4.Verify_Checksum (IP_Hdr) then
         return;
      end if;

      --  Calculate actual IP header length (IHL is in 32-bit words).
      IP_Hdr_Len := Natural (IP_Hdr.IHL) * 4;

      case IP_Hdr.Protocol is
         when IPv4.Protocol_UDP =>
            if Data'Length < IP_Hdr_Len + UDP.Header_Size then
               return;
            end if;

            UDP.Parse_Header
               (Data (Data'First + IP_Hdr_Len .. Data'Last),
                UDP_Hdr, Parse_Ok);
            if not Parse_Ok then
               return;
            end if;

            Payload_Off := Data'First + IP_Hdr_Len + UDP.Header_Size;
            Payload_Len := Natural (UDP_Hdr.Length) - UDP.Header_Size;
            if Payload_Off <= Data'Last then
               Deliver_UDP_Packet
                  (Dev              => Dev,
                   Src_IP           => IP_Hdr.Source_IP,
                   Src_Port         => UDP_Hdr.Source_Port,
                   Dest_Port        => UDP_Hdr.Destination_Port,
                   Payload          => Data (Payload_Off .. Data'Last),
                   Payload_Len      => Payload_Len);
            end if;

         when IPv4.Protocol_TCP =>
            if Data'Length >= IP_Hdr_Len + TCP.Header_Size then
               declare
                  TCP_Hdr   : TCP.TCP_Header;
                  Hdr_Len   : Natural;
                  Pld_Start : Natural;
                  Pld_End   : Natural;
                  IP_Pkt_End : Natural;
               begin
                  TCP.Parse_Header
                     (Data (Data'First + IP_Hdr_Len .. Data'Last),
                      TCP_Hdr, Parse_Ok);
                  if Parse_Ok then
                     Hdr_Len := Natural (TCP_Hdr.Data_Offset) * 4;
                     Pld_Start := Data'First + IP_Hdr_Len + Hdr_Len;
                     IP_Pkt_End := Data'First +
                        Natural (IP_Hdr.Total_Length) - 1;
                     Pld_End := Natural'Min (IP_Pkt_End, Data'Last);
                     if Pld_Start <= Pld_End then
                        Deliver_TCP_Packet
                           (Dev, IP_Hdr.Source_IP, TCP_Hdr,
                            Data (Pld_Start .. Pld_End));
                     else
                        Deliver_TCP_Packet
                           (Dev, IP_Hdr.Source_IP, TCP_Hdr,
                            Devices.Operation_Data'(1 .. 0 => 0));
                     end if;
                  end if;
               end;
            end if;

         when others =>
            null;
      end case;
   exception
      when Constraint_Error =>
         return;
   end Handle_IPv4_Packet;

   procedure Deliver_UDP_Packet
      (Dev         : Devices.Device_Handle;
       Src_IP      : IPv4_Address;
       Src_Port    : Unsigned_16;
       Dest_Port   : Unsigned_16;
       Payload     : Devices.Operation_Data;
       Payload_Len : Natural)
   is
      pragma Unreferenced (Dev);
      Copy_Len : Natural;
   begin
      Synchronization.Seize (UDP_Mutex);
      for I in UDP_Sockets'Range loop
         if UDP_Sockets (I).In_Use and then
            UDP_Sockets (I).Local_Port = Dest_Port
         then
            if not UDP_Sockets (I).Has_Data then
               Copy_Len := Natural'Min
                  (Payload_Len, UDP_Sockets (I).Recv_Buffer'Length);
               Copy_Len := Natural'Min (Copy_Len, Payload'Length);
               UDP_Sockets (I).Recv_Buffer (1 .. Copy_Len) :=
                  Payload (Payload'First .. Payload'First + Copy_Len - 1);
               UDP_Sockets (I).Recv_Len := Copy_Len;
               UDP_Sockets (I).Recv_Src_IP := Src_IP;
               UDP_Sockets (I).Recv_Src_Port := Src_Port;
               UDP_Sockets (I).Has_Data := True;
            end if;
            Synchronization.Release (UDP_Mutex);
            return;
         end if;
      end loop;
      Synchronization.Release (UDP_Mutex);
   exception
      when Constraint_Error =>
         return;
   end Deliver_UDP_Packet;

   procedure Deliver_TCP_Packet
      (Dev      : Devices.Device_Handle;
       Src_IP   : IPv4_Address;
       TCP_Hdr  : TCP.TCP_Header;
       Payload  : Devices.Operation_Data)
   is
      Conn      : TCP.TCP_Connection_Acc;
      Ack_Hdr   : TCP.TCP_Header;
      Send_Ok   : Boolean;
      Copy_Len  : Natural;
      Data_Len  : Natural;
      Empty     : constant Devices.Operation_Data (1 .. 0) := [others => 0];
   begin
      Synchronization.Seize (TCP_Mutex);
      for I in TCP_Connections'Range loop
         if TCP_Connections (I).In_Use and then
            TCP_Connections (I).Conn.Remote_IP = Src_IP and then
            TCP_Connections (I).Conn.Remote_Port = TCP_Hdr.Source_Port and then
            TCP_Connections (I).Conn.Local_Port = TCP_Hdr.Destination_Port
         then
            Conn := TCP_Connections (I).Conn;
            Synchronization.Release (TCP_Mutex);
            Synchronization.Seize (Conn.Mutex);

            case Conn.State is
               when TCP.State_Syn_Sent =>
                  if TCP_Hdr.Flag_SYN and TCP_Hdr.Flag_ACK and
                     TCP_Hdr.Ack_Number = Conn.Send_ISS + 1
                  then
                     Conn.Recv_IRS := TCP_Hdr.Sequence_Number;
                     Conn.Recv_Next := TCP_Hdr.Sequence_Number + 1;
                     Conn.Send_Unack := TCP_Hdr.Ack_Number;
                     Conn.Send_Window := TCP_Hdr.Window;

                     Ack_Hdr := TCP.Generate_ACK
                        (Conn.Local_Port, Conn.Remote_Port,
                         Conn.Send_Next, Conn.Recv_Next,
                         Conn.Recv_Window,
                         Conn.Local_IP, Conn.Remote_IP);
                     Send_TCP_Packet (Conn, Dev, Ack_Hdr, Empty, Send_Ok);

                     Conn.State := TCP.State_Established;
                  end if;

               when TCP.State_Established =>
                  Data_Len := Payload'Length;

                  if TCP_Hdr.Flag_ACK then
                     Conn.Send_Unack := TCP_Hdr.Ack_Number;
                     Conn.Send_Window := TCP_Hdr.Window;
                  end if;

                  if Data_Len > 0 then
                     declare
                        Seq_Start : constant Unsigned_32 :=
                           TCP_Hdr.Sequence_Number;
                        Seq_End   : constant Unsigned_32 :=
                           Seq_Start + Unsigned_32 (Data_Len);
                        Skip      : Natural := 0;
                        New_Len   : Natural;
                     begin
                        if Seq_End > Conn.Recv_Next and then
                           Seq_Start <= Conn.Recv_Next
                        then
                           Skip := Natural (Conn.Recv_Next - Seq_Start);
                           New_Len := Data_Len - Skip;
                           Copy_Len := Natural'Min
                              (New_Len,
                               Conn.Recv_Buffer'Length - Conn.Recv_Len);
                           if Copy_Len > 0 then
                              Conn.Recv_Buffer (Conn.Recv_Len + 1 ..
                                 Conn.Recv_Len + Copy_Len) :=
                                 Payload (Payload'First + Skip ..
                                    Payload'First + Skip + Copy_Len - 1);
                              Conn.Recv_Len := Conn.Recv_Len + Copy_Len;
                              Conn.Recv_Next := Conn.Recv_Next +
                                 Unsigned_32 (Copy_Len);
                           end if;

                           Ack_Hdr := TCP.Generate_ACK
                              (Conn.Local_Port, Conn.Remote_Port,
                               Conn.Send_Next, Conn.Recv_Next,
                               Conn.Recv_Window,
                               Conn.Local_IP, Conn.Remote_IP);
                           Send_TCP_Packet
                              (Conn, Dev, Ack_Hdr, Empty, Send_Ok);
                        end if;
                     end;
                  end if;

                  if TCP_Hdr.Flag_FIN then
                     Conn.Recv_Next := Conn.Recv_Next + 1;
                     Conn.State := TCP.State_Close_Wait;
                     Ack_Hdr := TCP.Generate_ACK
                        (Conn.Local_Port, Conn.Remote_Port,
                         Conn.Send_Next, Conn.Recv_Next,
                         Conn.Recv_Window,
                         Conn.Local_IP, Conn.Remote_IP);
                     Send_TCP_Packet (Conn, Dev, Ack_Hdr, Empty, Send_Ok);
                  end if;

               when others =>
                  null;
            end case;

            Synchronization.Release (Conn.Mutex);
            return;
         end if;
      end loop;
      Synchronization.Release (TCP_Mutex);
   exception
      when Constraint_Error =>
         return;
   end Deliver_TCP_Packet;
end Networking.Stack;
