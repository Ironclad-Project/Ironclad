--  networking-udp.adb: UDP protocol support.
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

with Networking.IPv4;

package body Networking.UDP is
   function Calculate_Checksum
      (Hdr     : UDP_Header;
       Src_IP  : IPv4_Address;
       Dest_IP : IPv4_Address;
       Payload : Devices.Operation_Data) return Unsigned_16
   is
      --  UDP pseudo-header for checksum:
      --  Source IP + Dest IP + Zero + Protocol + UDP Length
      --  Then UDP header + data.
      pragma Warnings (Off, "storage order");
      Hdr_Bytes : constant Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Hdr'Address;
      pragma Warnings (On, "storage order");

      Sum  : Unsigned_32 := 0;
      Word : Unsigned_16;
      Pld_Len : Natural;
      Udp_Len : Unsigned_16;
   begin
      Pld_Len := Payload'Length;
      Udp_Len := Unsigned_16 (Header_Size + Pld_Len);

      --  Add pseudo-header: Source IP.
      Sum := Sum + Unsigned_32 (Src_IP (1)) * 256 + Unsigned_32 (Src_IP (2));
      Sum := Sum + Unsigned_32 (Src_IP (3)) * 256 + Unsigned_32 (Src_IP (4));

      --  Add pseudo-header: Destination IP.
      Sum := Sum + Unsigned_32 (Dest_IP (1)) * 256 + Unsigned_32 (Dest_IP (2));
      Sum := Sum + Unsigned_32 (Dest_IP (3)) * 256 + Unsigned_32 (Dest_IP (4));

      --  Add pseudo-header: Protocol (UDP = 17) and length.
      Sum := Sum + Unsigned_32 (IPv4.Protocol_UDP);
      Sum := Sum + Unsigned_32 (Udp_Len);

      --  Add UDP header (4 words = 8 bytes), but zero out checksum field.
      --  Word 0: Source port.
      Sum := Sum + Unsigned_32 (Hdr_Bytes (1)) * 256 +
             Unsigned_32 (Hdr_Bytes (2));
      --  Word 1: Dest port.
      Sum := Sum + Unsigned_32 (Hdr_Bytes (3)) * 256 +
             Unsigned_32 (Hdr_Bytes (4));
      --  Word 2: Length.
      Sum := Sum + Unsigned_32 (Hdr_Bytes (5)) * 256 +
             Unsigned_32 (Hdr_Bytes (6));
      --  Word 3: Checksum - skip (treat as 0).

      --  Add payload.
      for I in 0 .. (Payload'Length / 2) - 1 loop
         Word := Unsigned_16 (Payload (Payload'First + I * 2)) * 256 +
                 Unsigned_16 (Payload (Payload'First + I * 2 + 1));
         Sum := Sum + Unsigned_32 (Word);
      end loop;

      --  Handle odd byte at end.
      if Payload'Length mod 2 = 1 then
         Sum := Sum + Unsigned_32 (Payload (Payload'Last)) * 256;
      end if;

      --  Fold to 16 bits.
      while Sum > 16#FFFF# loop
         Sum := (Sum and 16#FFFF#) + Shift_Right (Sum, 16);
      end loop;

      --  Return one's complement. If result is 0, use 0xFFFF per RFC 768.
      Word := not Unsigned_16 (Sum and 16#FFFF#);
      if Word = 0 then
         return 16#FFFF#;
      end if;
      return Word;
   exception
      when Constraint_Error =>
         return 0;
   end Calculate_Checksum;

   function Generate_Header
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Data_Len  : Natural;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address;
       Payload   : Devices.Operation_Data) return UDP_Header
   is
      pragma Suppress (All_Checks);
      Hdr : UDP_Header;
   begin
      Hdr :=
         (Source_Port      => Src_Port,
          Destination_Port => Dest_Port,
          Length           => Unsigned_16 (Header_Size + Data_Len),
          Checksum         => 0);
      Hdr.Checksum := Calculate_Checksum (Hdr, Src_IP, Dest_IP, Payload);
      return Hdr;
   end Generate_Header;

   procedure Parse_Header
      (Data    : Devices.Operation_Data;
       Header  : out UDP_Header;
       Success : out Boolean)
   is
      pragma Warnings (Off, "storage order");
      Header_Bytes : Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Header'Address;
      pragma Warnings (On, "storage order");
   begin
      Header_Bytes := Data (Data'First .. Data'First + Header_Size - 1);
      Success := Header.Length >= 8;
   exception
      when Constraint_Error =>
         Success := False;
   end Parse_Header;

   function Verify_Checksum
      (Hdr     : UDP_Header;
       Src_IP  : IPv4_Address;
       Dest_IP : IPv4_Address;
       Payload : Devices.Operation_Data) return Boolean
   is
   begin
      --  Zero checksum means checksum was not computed (allowed for UDP).
      if Hdr.Checksum = 0 then
         return True;
      end if;

      --  Calculate and compare (should match).
      --  Actually for verification, we include the checksum field and the
      --  result should be 0xFFFF.
      declare
         pragma Warnings (Off, "storage order");
         Hdr_Bytes : constant Devices.Operation_Data (1 .. Header_Size)
            with Import, Address => Hdr'Address;
         pragma Warnings (On, "storage order");

         Sum  : Unsigned_32 := 0;
         Word : Unsigned_16;
         Udp_Len : constant Unsigned_16 := Hdr.Length;
      begin
         --  Pseudo-header.
         Sum := Sum + Unsigned_32 (Src_IP (1)) * 256 +
                Unsigned_32 (Src_IP (2));
         Sum := Sum + Unsigned_32 (Src_IP (3)) * 256 +
                Unsigned_32 (Src_IP (4));
         Sum := Sum + Unsigned_32 (Dest_IP (1)) * 256 +
                Unsigned_32 (Dest_IP (2));
         Sum := Sum + Unsigned_32 (Dest_IP (3)) * 256 +
                Unsigned_32 (Dest_IP (4));
         Sum := Sum + Unsigned_32 (IPv4.Protocol_UDP);
         Sum := Sum + Unsigned_32 (Udp_Len);

         --  Full UDP header including checksum.
         for I in 0 .. 3 loop
            Word := Unsigned_16 (Hdr_Bytes (I * 2 + 1)) * 256 +
                    Unsigned_16 (Hdr_Bytes (I * 2 + 2));
            Sum := Sum + Unsigned_32 (Word);
         end loop;

         --  Payload.
         for I in 0 .. (Payload'Length / 2) - 1 loop
            Word := Unsigned_16 (Payload (Payload'First + I * 2)) * 256 +
                    Unsigned_16 (Payload (Payload'First + I * 2 + 1));
            Sum := Sum + Unsigned_32 (Word);
         end loop;
         if Payload'Length mod 2 = 1 then
            Sum := Sum + Unsigned_32 (Payload (Payload'Last)) * 256;
         end if;

         --  Fold.
         while Sum > 16#FFFF# loop
            Sum := (Sum and 16#FFFF#) + Shift_Right (Sum, 16);
         end loop;

         return Unsigned_16 (Sum and 16#FFFF#) = 16#FFFF#;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Verify_Checksum;
end Networking.UDP;
