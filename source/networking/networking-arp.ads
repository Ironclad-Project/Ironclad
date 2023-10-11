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

package Networking.ARP is
   --  While ARP stands for the IPv4 address resolution protocol, this module
   --  abstracts both IPv4 ARP and other ARPs for IPv6 and others.
   --
   --  Addresses will be cached if discovery is needed, which can lead to
   --  inconsistent lookup times if requests are needed after an eviction.

   --  Initialize the module.
   procedure Initialize;

   --  Add a static address for an interface's MAC address.
   --  These addresses will never be evicted from cache, so use of these
   --  addresses can be assumed to always be consistent.
   procedure Add_Static
      (MAC        : MAC_Address;
       IP4        : IPv4_Address;
       IP4_Subnet : IPv4_Address;
       IP6        : IPv6_Address;
       IP6_Subnet : IPv6_Address);

   procedure Modify_Static
      (MAC        : MAC_Address;
       IP4        : IPv4_Address;
       IP4_Subnet : IPv4_Address);

   procedure Modify_Static
      (MAC        : MAC_Address;
       IP6        : IPv6_Address;
       IP6_Subnet : IPv6_Address);

   --  Lookup the associated IPv4 addresses for a MAC address.
   procedure Lookup (MAC : MAC_Address; IP, Subnet : out IPv4_Address);

   --  Lookup the associated IPv6 addresses for a MAC address.
   procedure Lookup (MAC : MAC_Address; IP, Subnet : out IPv6_Address);

   --  Lookup the associated MAC address for an IPv4 one.
   procedure Lookup (IP : IPv4_Address; MAC : out MAC_Address);

   --  Lookup the associated MAC address for an IPv6 one.
   procedure Lookup (IP : IPv6_Address; MAC : out MAC_Address);
end Networking.ARP;
