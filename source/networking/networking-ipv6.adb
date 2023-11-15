--  networking-ipv6.adb: IPv6 support.
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

package body Networking.IPv6 is
   pragma Suppress (All_Checks); --  Unit passes GNATprove AoRTE.

   function Generate_Header
      (Source_IP, Desto_IP : IPv6_Address;
       Data_Length         : Natural) return IPv6_Packet_Header
   is
   begin
      return
         (Version         => 6,
          Trafic_Class    => 0,
          Flow_Label      => 0,
          Payload_Length  => Unsigned_16 (Data_Length),
          Next_Header     => 6, --  Placeholder.
          Hop_Limit       => Unsigned_8'Last,
          Source_IP       => Source_IP,
          Destination_IP  => Desto_IP);
   end Generate_Header;
end Networking.IPv6;
