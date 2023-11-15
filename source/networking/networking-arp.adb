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
   pragma Suppress (All_Checks); --  Unit passes GNATprove AoRTE.

   procedure Initialize is
   begin
      Interface_Entries := new ARP_Entries'(others =>
         (MAC        => (others => 0),
          IP4        => (others => 0),
          IP4_Subnet => (others => 0),
          IP6        => (others => 0),
          IP6_Subnet => (others => 0)));
   end Initialize;

   procedure Add_Static
      (MAC        : MAC_Address;
       IP4        : IPv4_Address;
       IP4_Subnet : IPv4_Address;
       IP6        : IPv6_Address;
       IP6_Subnet : IPv6_Address)
   is
   begin
      for E of Interface_Entries.all loop
         if E.MAC = (0, 0, 0, 0, 0, 0) then
            E := (MAC, IP4, IP4_Subnet, IP6, IP6_Subnet);
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
      for E of Interface_Entries.all loop
         if E.MAC = MAC then
            E.IP4 := IP4;
            E.IP4_Subnet := IP4_Subnet;
            return;
         end if;
      end loop;
   end Modify_Static;

   procedure Modify_Static
      (MAC        : MAC_Address;
       IP6        : IPv6_Address;
       IP6_Subnet : IPv6_Address)
   is
   begin
      for E of Interface_Entries.all loop
         if E.MAC = MAC then
            E.IP6 := IP6;
            E.IP6_Subnet := IP6_Subnet;
            return;
         end if;
      end loop;
   end Modify_Static;

   procedure Lookup (MAC : MAC_Address; IP, Subnet : out IPv4_Address) is
   begin
      for E of Interface_Entries.all loop
         if E.MAC = MAC then
            IP     := E.IP4;
            Subnet := E.IP4_Subnet;
            return;
         end if;
      end loop;

      IP     := (others => 0);
      Subnet := (others => 0);
   end Lookup;

   procedure Lookup (MAC : MAC_Address; IP, Subnet : out IPv6_Address) is
   begin
      for E of Interface_Entries.all loop
         if E.MAC = MAC then
            IP     := E.IP6;
            Subnet := E.IP6_Subnet;
            return;
         end if;
      end loop;

      IP     := (others => 0);
      Subnet := (others => 0);
   end Lookup;

   procedure Lookup (IP : IPv4_Address; MAC : out MAC_Address) is
   begin
      for E of Interface_Entries.all loop
         if E.IP4 = IP then
            MAC := E.MAC;
            return;
         end if;
      end loop;

      MAC := (others => 0);
   end Lookup;

   procedure Lookup (IP : IPv6_Address; MAC : out MAC_Address) is
   begin
      for E of Interface_Entries.all loop
         if E.IP6 = IP then
            MAC := E.MAC;
            return;
         end if;
      end loop;

      MAC := (others => 0);
   end Lookup;
end Networking.ARP;
