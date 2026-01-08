--  networking-dns.ads: DNS protocol support.
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

package Networking.DNS with SPARK_Mode => Off is
   --  DNS resolver for hostname to IPv4 address resolution.

   --  DNS server address (set by DHCP).
   DNS_Server : IPv4_Address := [0, 0, 0, 0];
   DNS_Port   : constant Unsigned_16 := 53;

   --  Maximum hostname length for DNS queries.
   Max_Hostname_Length : constant := 253;

   --  Resolve a hostname to an IPv4 address.
   --  @param Hostname  Hostname to resolve (e.g., "example.com").
   --  @param Result    Resolved IPv4 address.
   --  @param Success   True if resolution succeeded.
   procedure Resolve
      (Hostname : String;
       Result   : out IPv4_Address;
       Success  : out Boolean);

   --  Set the DNS server address.
   --  @param Server New DNS server address.
   procedure Set_DNS_Server (Server : IPv4_Address);

private

   function Build_Query
      (Hostname : String;
       Buffer   : out Devices.Operation_Data;
       Trans_ID : Unsigned_16) return Natural;

   function Parse_Response
      (Buffer     : Devices.Operation_Data;
       Length     : Natural;
       Trans_ID   : Unsigned_16;
       Result     : out IPv4_Address) return Boolean;
end Networking.DNS;
