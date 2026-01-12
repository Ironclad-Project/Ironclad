--  networking-arp.adb: Address resolution, from MAC to IP (be it 4 or 6).
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

package body Networking.ARP is
   pragma Suppress (All_Checks);

   procedure Initialize is
   begin
      if Interface_Entries = null then
         Interface_Entries := new ARP_Entries'(others =>
            (MAC        => [others => 0],
             IP4        => [others => 0],
             IP4_Subnet => [others => 0]));
      end if;
   end Initialize;

   procedure Add_Static
      (MAC        : MAC_Address;
       IP4        : IPv4_Address;
       IP4_Subnet : IPv4_Address)
   is
   begin
      if Interface_Entries = null then
         return;
      end if;

      for E of Interface_Entries.all loop
         if E.MAC = [0, 0, 0, 0, 0, 0] then
            E := (MAC, IP4, IP4_Subnet);
            return;
         end if;
      end loop;
   end Add_Static;

   procedure Modify_Static
      (MAC        : MAC_Address;
       IP4        : IPv4_Address;
       IP4_Subnet : IPv4_Address)
   is
   begin
      if Interface_Entries = null then
         return;
      end if;

      for E of Interface_Entries.all loop
         if E.MAC = MAC then
            E.IP4 := IP4;
            E.IP4_Subnet := IP4_Subnet;
            return;
         end if;
      end loop;
   end Modify_Static;

   procedure Lookup (MAC : MAC_Address; IP, Subnet : out IPv4_Address) is
   begin
      if Interface_Entries = null then
         goto Cleanup;
      end if;

      for E of Interface_Entries.all loop
         if E.MAC = MAC then
            IP     := E.IP4;
            Subnet := E.IP4_Subnet;
            return;
         end if;
      end loop;

   <<Cleanup>>
      IP     := [others => 0];
      Subnet := [others => 0];
   end Lookup;

   procedure Lookup (IP : IPv4_Address; MAC : out MAC_Address) is
   begin
      if Interface_Entries = null then
         goto Cleanup;
      end if;

      for E of Interface_Entries.all loop
         if E.IP4 = IP then
            MAC := E.MAC;
            return;
         end if;
      end loop;

   <<Cleanup>>
      MAC := [others => 0];
   end Lookup;
   ----------------------------------------------------------------------------
   function Create_Request
      (Sender_MAC : MAC_Address;
       Sender_IP  : IPv4_Address;
       Target_IP  : IPv4_Address) return ARP_Packet
   is
   begin
      return
         (Hardware_Type => ARP_HW_Ethernet,
          Protocol_Type => ARP_Proto_IPv4,
          Hardware_Size => 6,
          Protocol_Size => 4,
          Operation     => ARP_OP_Request,
          Sender_MAC    => Sender_MAC,
          Sender_IP     => Sender_IP,
          Target_MAC    => [0, 0, 0, 0, 0, 0],
          Target_IP     => Target_IP);
   end Create_Request;

   function Create_Reply
      (Sender_MAC : MAC_Address;
       Sender_IP  : IPv4_Address;
       Target_MAC : MAC_Address;
       Target_IP  : IPv4_Address) return ARP_Packet
   is
   begin
      return
         (Hardware_Type => ARP_HW_Ethernet,
          Protocol_Type => ARP_Proto_IPv4,
          Hardware_Size => 6,
          Protocol_Size => 4,
          Operation     => ARP_OP_Reply,
          Sender_MAC    => Sender_MAC,
          Sender_IP     => Sender_IP,
          Target_MAC    => Target_MAC,
          Target_IP     => Target_IP);
   end Create_Reply;

   procedure Parse_Packet
      (Data    : Devices.Operation_Data;
       Packet  : out ARP_Packet;
       Success : out Boolean)
   is
      pragma Warnings (Off, "storage order");
      Packet_Bytes : Devices.Operation_Data (1 .. ARP_Packet_Size)
         with Import, Address => Packet'Address;
      pragma Warnings (On, "storage order");
   begin
      Packet_Bytes := Data (Data'First .. Data'First + ARP_Packet_Size - 1);
      Success :=
         Packet.Hardware_Type = ARP_HW_Ethernet and then
         Packet.Protocol_Type = ARP_Proto_IPv4 and then
         Packet.Hardware_Size = 6 and then
         Packet.Protocol_Size = 4;
   end Parse_Packet;

   procedure To_Bytes
      (Packet : ARP_Packet;
       Data   : out Devices.Operation_Data)
   is
      pragma Warnings (Off, "storage order");
      Packet_Bytes : constant Devices.Operation_Data (1 .. ARP_Packet_Size)
         with Import, Address => Packet'Address;
      pragma Warnings (On, "storage order");
   begin
      Data (Data'First .. Data'First + ARP_Packet_Size - 1) := Packet_Bytes;
   end To_Bytes;
end Networking.ARP;
