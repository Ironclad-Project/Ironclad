--  networking-ipv4.ads: IPv4 support.
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

package Networking.IPv4 is
   --  Structure of an IPv4 header.
   type Unsigned_2  is mod 2**2;
   type Unsigned_3  is mod 2**3;
   type Unsigned_4  is mod 2**4;
   type Unsigned_6  is mod 2**6;
   type Unsigned_13 is mod 2**13;

   --  Little does GNAT know, byte arrays dont have endianness...
   pragma Warnings (Off, "scalar storage order specified");
   type IPv4_Packet_Header is record
      Version         : Unsigned_4;
      IHL             : Unsigned_4;
      DSCP            : Unsigned_6;
      ECN             : Unsigned_2;
      Total_Length    : Unsigned_16;
      Identification  : Unsigned_16;
      Flags           : Unsigned_3;
      Fragment_Offset : Unsigned_13;
      Time_To_Live    : Unsigned_8;
      Protocol        : Unsigned_8;
      Header_Checksum : Unsigned_16;
      Source_IP       : IPv4_Address;
      Destination_IP  : IPv4_Address;
   end record with Size => 160, Bit_Order => System.High_Order_First,
      Scalar_Storage_Order => System.High_Order_First;
   for IPv4_Packet_Header use record
      Version         at  0 range 0 .. 3;
      IHL             at  0 range 4 .. 7;
      DSCP            at  1 range 0 .. 5;
      ECN             at  1 range 6 .. 7;
      Total_Length    at  2 range 0 .. 15;
      Identification  at  4 range 0 .. 15;
      Flags           at  6 range 0 .. 2;
      Fragment_Offset at  6 range 3 .. 15;
      Time_To_Live    at  8 range 0 .. 7;
      Protocol        at  9 range 0 .. 7;
      Header_Checksum at 10 range 0 .. 15;
      Source_IP       at 12 range 0 .. 31;
      Destination_IP  at 16 range 0 .. 31;
   end record;
   pragma Warnings (On, "scalar storage order specified");

   function Generate_Header
      (Source_IP, Desto_IP : IPv4_Address;
       Data_Length         : Natural) return IPv4_Packet_Header
      with Pre => Data_Length <=
                  Natural (Unsigned_16'Last - (IPv4_Packet_Header'Size / 8));
end Networking.IPv4;
