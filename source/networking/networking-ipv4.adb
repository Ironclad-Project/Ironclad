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
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   function Calculate_Checksum
      (Header : IPv4_Packet_Header) return Unsigned_16
   is
      --  Convert header to array of 16-bit words for checksumming.
      pragma Warnings (Off, "storage order");
      Header_Bytes : constant Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Header'Address;
      pragma Warnings (On, "storage order");

      Sum : Unsigned_32 := 0;
      Word : Unsigned_16;
   begin
      --  Sum all 16-bit words in the header.
      --  Header is 20 bytes = 10 words.
      for I in 0 .. 9 loop
         Word := Unsigned_16 (Header_Bytes (I * 2 + 1)) * 256 +
                 Unsigned_16 (Header_Bytes (I * 2 + 2));
         Sum := Sum + Unsigned_32 (Word);
      end loop;

      --  Fold 32-bit sum to 16 bits (add carry).
      while Sum > 16#FFFF# loop
         Sum := (Sum and 16#FFFF#) + Shift_Right (Sum, 16);
      end loop;

      --  Return one's complement.
      return not Unsigned_16 (Sum and 16#FFFF#);
   end Calculate_Checksum;

   function Generate_Header
      (Source_IP, Desto_IP : IPv4_Address;
       Data_Length         : Natural;
       Protocol            : Unsigned_8) return IPv4_Packet_Header
   is
      Size : constant Unsigned_16 := IPv4_Packet_Header'Size / 8;
      Hdr  : IPv4_Packet_Header;
   begin
      Hdr := (Version         => 4,
              IHL             => 5,
              DSCP            => 0,
              ECN             => 0,
              Total_Length    => Size + Unsigned_16 (Data_Length),
              Identification  => 0,
              Flags           => 2,  --  Don't fragment.
              Fragment_Offset => 0,
              Time_To_Live    => 64,
              Protocol        => Protocol,
              Header_Checksum => 0,
              Source_IP       => Source_IP,
              Destination_IP  => Desto_IP);

      --  Calculate and set the checksum.
      Hdr.Header_Checksum := Calculate_Checksum (Hdr);

      return Hdr;
   end Generate_Header;

   procedure Parse_Header
      (Data    : Devices.Operation_Data;
       Header  : out IPv4_Packet_Header;
       Success : out Boolean)
   is
      pragma Warnings (Off, "storage order");
      Header_Bytes : Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Header'Address;
      pragma Warnings (On, "storage order");
   begin
      Header_Bytes := Data (Data'First .. Data'First + Header_Size - 1);
      --  Basic validation: check version is 4.
      Success := Header.Version = 4;
   end Parse_Header;

   function Verify_Checksum (Header : IPv4_Packet_Header) return Boolean is
      --  When calculating checksum over a header that already has a checksum,
      --  the result should be 0 if valid.
      pragma Warnings (Off, "storage order");
      Header_Bytes : constant Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Header'Address;
      pragma Warnings (On, "storage order");

      Sum : Unsigned_32 := 0;
      Word : Unsigned_16;
   begin
      for I in 0 .. 9 loop
         Word := Unsigned_16 (Header_Bytes (I * 2 + 1)) * 256 +
                 Unsigned_16 (Header_Bytes (I * 2 + 2));
         Sum := Sum + Unsigned_32 (Word);
      end loop;

      while Sum > 16#FFFF# loop
         Sum := (Sum and 16#FFFF#) + Shift_Right (Sum, 16);
      end loop;

      return Unsigned_16 (Sum and 16#FFFF#) = 16#FFFF#;
   end Verify_Checksum;
end Networking.IPv4;
