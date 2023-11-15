--  networking-ipv4.adb: IPv4 support.
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

package body Networking.IPv4 is
   pragma Suppress (All_Checks); --  Unit passes GNATprove AoRTE.

   function Generate_Header
      (Source_IP, Desto_IP : IPv4_Address;
       Data_Length         : Natural) return IPv4_Packet_Header
   is
      Size : constant Unsigned_16 := IPv4_Packet_Header'Size;
   begin
      return
         (Version         => 4,
          IHL             => 5,
          DSCP            => 0,
          ECN             => 0,
          Total_Length    => (Size / 8) + Unsigned_16 (Data_Length),
          Identification  => 0,
          Flags           => 0,
          Fragment_Offset => 0,
          Time_To_Live    => Unsigned_8'Last,
          Protocol        => 0,
          Header_Checksum => 0,
          Source_IP       => Source_IP,
          Destination_IP  => Desto_IP);
   end Generate_Header;
end Networking.IPv4;
