--  networking-ipv6.ads: IPv6 support.
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

with System;

package Networking.IPv6 is
   --  Structure of an IPv6 header.
   type Unsigned_4  is mod 2**4;
   type Unsigned_20 is mod 2**20;

   --  Little does GNAT know, byte arrays dont have endianness...
   pragma Warnings (Off, "scalar storage order specified");
   type IPv6_Packet_Header is record
      Version         : Unsigned_4;
      Trafic_Class    : Unsigned_8;
      Flow_Label      : Unsigned_20;
      Payload_Length  : Unsigned_16;
      Next_Header     : Unsigned_8;
      Hop_Limit       : Unsigned_8;
      Source_IP       : IPv6_Address;
      Destination_IP  : IPv6_Address;
   end record with Size => 320, Bit_Order => System.High_Order_First,
      Scalar_Storage_Order => System.High_Order_First;
   for IPv6_Packet_Header use record
      Version         at  0 range 0 .. 3;
      Trafic_Class    at  0 range 4 .. 11;
      Flow_Label      at  0 range 12 .. 31;
      Payload_Length  at  4 range 0 .. 15;
      Next_Header     at  6 range 0 .. 7;
      Hop_Limit       at  7 range 0 .. 7;
      Source_IP       at  8 range 0 .. 127;
      Destination_IP  at 24 range 0 .. 127;
   end record;
   pragma Warnings (On, "scalar storage order specified");

   function Generate_Header
      (Source_IP, Desto_IP : IPv6_Address;
       Data_Length         : Natural) return IPv6_Packet_Header
      with Pre => Data_Length <=
                  Natural (Unsigned_16'Last - (IPv6_Packet_Header'Size / 8));
end Networking.IPv6;
