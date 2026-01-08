--  networking-dns.adb: DNS protocol support.
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

with Networking.Ethernet;
with Networking.IPv4;
with Networking.UDP;
with Networking.Stack;
with Networking.Interfaces;
with Scheduler;

package body Networking.DNS with SPARK_Mode => Off is
   use type Devices.Dev_Status;
   use type Devices.Device_Handle;

   --  DNS header constants.
   DNS_FLAG_RD    : constant Unsigned_16 := 16#0100#;  --  Recursion Desired.
   DNS_TYPE_A     : constant Unsigned_16 := 1;         --  IPv4 address.
   DNS_CLASS_IN   : constant Unsigned_16 := 1;         --  Internet class.

   --  Transaction ID counter.
   Transaction_ID : Unsigned_16 := 16#1234# with Volatile;

   procedure Resolve
      (Hostname : String;
       Result   : out IPv4_Address;
       Success  : out Boolean)
   is
      Query_Buf   : Devices.Operation_Data (1 .. 512);
      Query_Len   : Natural;
      Trans_ID    : Unsigned_16;
      Local_IP    : IPv4_Address;
      Dev         : Devices.Device_Handle;
      Recv_Buf    : Devices.Operation_Data (1 .. 1518);
      Recv_Cnt    : Natural;
      Recv_Stat   : Devices.Dev_Status;
      Eth_Hdr     : Ethernet.Ethernet_Header;
      Ip_Hdr      : IPv4.IPv4_Packet_Header;
      Udp_Hdr     : UDP.UDP_Header;
      Parse_Ok    : Boolean;
      Payload_Start : Natural;
      Send_Ok     : Boolean;
      Retries     : Natural := 3;
   begin
      Result := [0, 0, 0, 0];
      Success := False;

      if Hostname'Length = 0 or Hostname'Length > Max_Hostname_Length then
         return;
      end if;

      --  Check if DNS server is configured.
      if DNS_Server = IPv4_Address'(0, 0, 0, 0) then
         return;
      end if;

      --  Get local IP and device.
      Interfaces.Get_Suitable_Interface
         (IP         => DNS_Server,
          Interfaced => Dev);

      if Dev = Devices.Error_Handle then
         return;
      end if;

      Interfaces.Get_Interface_Address (Dev, Local_IP);
      if Local_IP = IPv4_Address'(0, 0, 0, 0) then
         return;
      end if;

      --  Generate transaction ID.
      Trans_ID := Transaction_ID;
      Transaction_ID := Transaction_ID + 1;

      --  Build query.
      Query_Len := Build_Query (Hostname, Query_Buf, Trans_ID);

      while Retries > 0 loop
         --  Send DNS query via UDP.
         Stack.UDP_Send
            (Src_IP    => Local_IP,
             Src_Port  => 53000,  --  Source port.
             Dest_IP   => DNS_Server,
             Dest_Port => DNS_Port,
             Data      => Query_Buf (1 .. Query_Len),
             Success   => Send_Ok);

         if not Send_Ok then
            Retries := Retries - 1;
            goto Continue;
         end if;

         --  Wait for response (polling with timeout).
         for Wait_Iter in 1 .. 2000 loop
            Devices.Read (Dev, 0, Recv_Buf, Recv_Cnt, Recv_Stat);
            if Recv_Stat = Devices.Dev_Success and then
               Recv_Cnt >= Ethernet.Header_Size + IPv4.Header_Size +
                           UDP.Header_Size + 12
            then
               Ethernet.Parse_Header (Recv_Buf, Eth_Hdr, Parse_Ok);
               if Parse_Ok and then
                  Eth_Hdr.EtherType = Ethernet.EtherType_IPv4
               then
                  IPv4.Parse_Header
                     (Recv_Buf (Ethernet.Header_Size + 1 .. Recv_Cnt),
                      Ip_Hdr, Parse_Ok);
                  if Parse_Ok and then
                     Ip_Hdr.Protocol = IPv4.Protocol_UDP and then
                     Ip_Hdr.Source_IP = DNS_Server
                  then
                     UDP.Parse_Header
                        (Recv_Buf (Ethernet.Header_Size + IPv4.Header_Size +
                                   1 .. Recv_Cnt),
                         Udp_Hdr, Parse_Ok);
                     if Parse_Ok and then Udp_Hdr.Source_Port = DNS_Port then
                        Payload_Start := Ethernet.Header_Size +
                                        IPv4.Header_Size + UDP.Header_Size + 1;
                        if Parse_Response
                              (Recv_Buf (Payload_Start .. Recv_Cnt),
                               Recv_Cnt - Payload_Start + 1,
                               Trans_ID, Result)
                        then
                           Success := True;
                           return;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
            Scheduler.Yield_If_Able;
         end loop;

      <<Continue>>
         Retries := Retries - 1;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Resolve;

   procedure Set_DNS_Server (Server : IPv4_Address) is
   begin
      DNS_Server := Server;
   end Set_DNS_Server;
   ----------------------------------------------------------------------------
   function Build_Query
      (Hostname : String;
       Buffer   : out Devices.Operation_Data;
       Trans_ID : Unsigned_16) return Natural
   is
      Idx       : Natural;
      Label_Len : Natural;
      Label_Start : Natural;
   begin
      Idx := Buffer'First;

      --  DNS Header (12 bytes):
      --  Transaction ID (2).
      Buffer (Idx) := Unsigned_8 (Shift_Right (Trans_ID, 8));
      Buffer (Idx + 1) := Unsigned_8 (Trans_ID and 16#FF#);

      --  Flags: RD=1 (recursion desired).
      Buffer (Idx + 2) := Unsigned_8 (Shift_Right (DNS_FLAG_RD, 8));
      Buffer (Idx + 3) := Unsigned_8 (DNS_FLAG_RD and 16#FF#);

      --  Questions: 1.
      Buffer (Idx + 4) := 0;
      Buffer (Idx + 5) := 1;

      --  Answer RRs: 0.
      Buffer (Idx + 6) := 0;
      Buffer (Idx + 7) := 0;

      --  Authority RRs: 0.
      Buffer (Idx + 8) := 0;
      Buffer (Idx + 9) := 0;

      --  Additional RRs: 0.
      Buffer (Idx + 10) := 0;
      Buffer (Idx + 11) := 0;

      Idx := Idx + 12;

      --  Question section: encode hostname as DNS labels.
      --  "www.example.com" -> 3www7example3com0
      Label_Start := Hostname'First;
      for I in Hostname'Range loop
         if Hostname (I) = '.' or I = Hostname'Last then
            if Hostname (I) = '.' then
               Label_Len := I - Label_Start;
            else
               Label_Len := I - Label_Start + 1;
            end if;

            Buffer (Idx) := Unsigned_8 (Label_Len);
            Idx := Idx + 1;

            for J in 0 .. Label_Len - 1 loop
               Buffer (Idx + J) := Character'Pos (Hostname (Label_Start + J));
            end loop;
            Idx := Idx + Label_Len;

            Label_Start := I + 1;
         end if;
      end loop;

      --  Null terminator for labels.
      Buffer (Idx) := 0;
      Idx := Idx + 1;

      --  Type: A (IPv4).
      Buffer (Idx) := Unsigned_8 (Shift_Right (DNS_TYPE_A, 8));
      Buffer (Idx + 1) := Unsigned_8 (DNS_TYPE_A and 16#FF#);
      Idx := Idx + 2;

      --  Class: IN (Internet).
      Buffer (Idx) := Unsigned_8 (Shift_Right (DNS_CLASS_IN, 8));
      Buffer (Idx + 1) := Unsigned_8 (DNS_CLASS_IN and 16#FF#);
      Idx := Idx + 2;

      return Idx - Buffer'First;
   exception
      when Constraint_Error =>
         Buffer := [others => 0];
         return 0;
   end Build_Query;

   function Parse_Response
      (Buffer     : Devices.Operation_Data;
       Length     : Natural;
       Trans_ID   : Unsigned_16;
       Result     : out IPv4_Address) return Boolean
   is
      Idx         : Natural;
      Recv_ID     : Unsigned_16;
      Flags       : Unsigned_16;
      Ans_Count   : Unsigned_16;
      Rtype       : Unsigned_16;
      Rdlength    : Unsigned_16;
   begin
      Idx := Buffer'First;
      Result := [0, 0, 0, 0];

      if Length < 12 then
         return False;
      end if;

      --  Check transaction ID.
      Recv_ID := Shift_Left (Unsigned_16 (Buffer (Idx)), 8) or
                 Unsigned_16 (Buffer (Idx + 1));
      if Recv_ID /= Trans_ID then
         return False;
      end if;

      --  Check flags: QR=1 (response), RCODE=0 (no error).
      Flags := Shift_Left (Unsigned_16 (Buffer (Idx + 2)), 8) or
               Unsigned_16 (Buffer (Idx + 3));
      if (Flags and 16#8000#) = 0 then  --  Not a response.
         return False;
      end if;
      if (Flags and 16#000F#) /= 0 then  --  Error code.
         return False;
      end if;

      --  Get answer count.
      Ans_Count := Shift_Left (Unsigned_16 (Buffer (Idx + 6)), 8) or
                   Unsigned_16 (Buffer (Idx + 7));
      if Ans_Count = 0 then
         return False;
      end if;

      Idx := Idx + 12;  --  Skip header.

      --  Skip question section (hostname labels + type + class).
      while Idx <= Buffer'First + Length - 1 and then Buffer (Idx) /= 0 loop
         if (Buffer (Idx) and 16#C0#) = 16#C0# then
            --  Compression pointer.
            Idx := Idx + 2;
            exit;
         else
            Idx := Idx + Natural (Buffer (Idx)) + 1;
         end if;
      end loop;

      if Idx <= Buffer'First + Length - 1 and then Buffer (Idx) = 0 then
         Idx := Idx + 1;  --  Skip null terminator.
      end if;
      Idx := Idx + 4;  --  Skip type and class.

      --  Parse answer records.
      for Ans in 1 .. Natural (Ans_Count) loop
         exit when Idx >= Buffer'First + Length - 10;

         --  Skip name (could be compressed).
         if (Buffer (Idx) and 16#C0#) = 16#C0# then
            Idx := Idx + 2;  --  Compression pointer.
         else
            while Idx <= Buffer'First + Length - 1 and then
                  Buffer (Idx) /= 0
            loop
               Idx := Idx + Natural (Buffer (Idx)) + 1;
            end loop;
            Idx := Idx + 1;  --  Skip null terminator.
         end if;

         exit when Idx >= Buffer'First + Length - 10;

         --  Type.
         Rtype := Shift_Left (Unsigned_16 (Buffer (Idx)), 8) or
                  Unsigned_16 (Buffer (Idx + 1));
         Idx := Idx + 2;

         --  Class (skip).
         Idx := Idx + 2;

         --  TTL (skip).
         Idx := Idx + 4;

         --  RDLength.
         Rdlength := Shift_Left (Unsigned_16 (Buffer (Idx)), 8) or
                     Unsigned_16 (Buffer (Idx + 1));
         Idx := Idx + 2;

         --  If Type A (IPv4) and length 4, extract address.
         if Rtype = DNS_TYPE_A and then Rdlength = 4 and then
            Idx + 3 <= Buffer'First + Length - 1
         then
            Result (1) := Buffer (Idx);
            Result (2) := Buffer (Idx + 1);
            Result (3) := Buffer (Idx + 2);
            Result (4) := Buffer (Idx + 3);
            return True;
         end if;

         Idx := Idx + Natural (Rdlength);
      end loop;

      return False;
   exception
      when Constraint_Error =>
         Result := [others => 0];
         return False;
   end Parse_Response;
end Networking.DNS;
