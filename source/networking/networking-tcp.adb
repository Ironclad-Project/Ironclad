--  networking-tcp.adb: TCP protocol support.
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
with Arch.Clocks;

package body Networking.TCP is
   --  Counter for ISN generation (simple approach).
   ISN_Counter : Unsigned_32 := 0 with Volatile;

   function Calculate_Checksum
      (Hdr     : TCP_Header;
       Src_IP  : IPv4_Address;
       Dest_IP : IPv4_Address;
       Payload : Devices.Operation_Data) return Unsigned_16
   is
      --  TCP pseudo-header for checksum:
      --  Source IP (4) + Dest IP (4) + Zero + Protocol + TCP Length (2)
      --  Then TCP header + data.

      pragma Warnings (Off, "storage order");
      Hdr_Bytes : Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Hdr'Address;
      pragma Warnings (On, "storage order");

      Sum     : Unsigned_32 := 0;
      Word    : Unsigned_16;
      Tcp_Len : Natural;
   begin
      Tcp_Len := Header_Size + Payload'Length;

      --  Add pseudo-header: Source IP.
      Sum := Sum + Unsigned_32 (Src_IP (1)) * 256 + Unsigned_32 (Src_IP (2));
      Sum := Sum + Unsigned_32 (Src_IP (3)) * 256 + Unsigned_32 (Src_IP (4));

      --  Add pseudo-header: Destination IP.
      Sum := Sum + Unsigned_32 (Dest_IP (1)) * 256 + Unsigned_32 (Dest_IP (2));
      Sum := Sum + Unsigned_32 (Dest_IP (3)) * 256 + Unsigned_32 (Dest_IP (4));

      --  Add pseudo-header: Protocol (TCP = 6) and length.
      Sum := Sum + Unsigned_32 (IPv4.Protocol_TCP);
      Sum := Sum + Unsigned_32 (Tcp_Len);

      --  Add TCP header (10 words = 20 bytes), but zero out checksum field.
      for I in 0 .. 9 loop
         Word := Unsigned_16 (Hdr_Bytes (I * 2 + 1)) * 256 +
                 Unsigned_16 (Hdr_Bytes (I * 2 + 2));
         --  Skip checksum field (bytes 17-18, index 8).
         if I = 8 then
            Word := 0;
         end if;
         Sum := Sum + Unsigned_32 (Word);
      end loop;

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

      --  Return one's complement.
      return not Unsigned_16 (Sum and 16#FFFF#);
   exception
      when Constraint_Error =>
         return 0;
   end Calculate_Checksum;

   procedure Parse_Header
      (Data    : Devices.Operation_Data;
       Header  : out TCP_Header;
       Success : out Boolean)
   is
      pragma Warnings (Off, "storage order");
      Header_Bytes : Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Header'Address;
      pragma Warnings (On, "storage order");
   begin
      Header_Bytes := Data (Data'First .. Data'First + Header_Size - 1);
      --  Basic validation: data offset should be at least 5 (20 bytes).
      Success := Header.Data_Offset >= 5;
   exception
      when Constraint_Error =>
         Success := False;
   end Parse_Header;

   function Generate_Header_Base
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Window    : Unsigned_16;
       Flags_ACK : Boolean;
       Flags_SYN : Boolean;
       Flags_FIN : Boolean;
       Flags_RST : Boolean;
       Flags_PSH : Boolean) return TCP_Header
   is
   begin
      return (Source_Port      => Src_Port,
              Destination_Port => Dest_Port,
              Sequence_Number  => Seq_Num,
              Ack_Number       => Ack_Num,
              Data_Offset      => 5,  --  20 bytes, no options.
              Reserved         => 0,
              Flag_URG         => False,
              Flag_ACK         => Flags_ACK,
              Flag_PSH         => Flags_PSH,
              Flag_RST         => Flags_RST,
              Flag_SYN         => Flags_SYN,
              Flag_FIN         => Flags_FIN,
              Window           => Window,
              Checksum         => 0,
              Urgent_Pointer   => 0);
   end Generate_Header_Base;

   function Generate_SYN
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header
   is
      Hdr     : TCP_Header;
      Payload : constant Devices.Operation_Data (1 .. 0) := [others => 0];
   begin
      Hdr := Generate_Header_Base
         (Src_Port, Dest_Port, Seq_Num, 0, Default_Window_Size,
          Flags_ACK => False, Flags_SYN => True, Flags_FIN => False,
          Flags_RST => False, Flags_PSH => False);
      Hdr.Checksum := Calculate_Checksum (Hdr, Src_IP, Dest_IP, Payload);
      return Hdr;
   end Generate_SYN;

   function Generate_SYN_ACK
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header
   is
      Hdr     : TCP_Header;
      Payload : constant Devices.Operation_Data (1 .. 0) := [others => 0];
   begin
      Hdr := Generate_Header_Base
         (Src_Port, Dest_Port, Seq_Num, Ack_Num, Default_Window_Size,
          Flags_ACK => True, Flags_SYN => True, Flags_FIN => False,
          Flags_RST => False, Flags_PSH => False);
      Hdr.Checksum := Calculate_Checksum (Hdr, Src_IP, Dest_IP, Payload);
      return Hdr;
   end Generate_SYN_ACK;

   function Generate_ACK
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Window    : Unsigned_16;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header
   is
      Hdr     : TCP_Header;
      Payload : constant Devices.Operation_Data (1 .. 0) := [others => 0];
   begin
      Hdr := Generate_Header_Base
         (Src_Port, Dest_Port, Seq_Num, Ack_Num, Window,
          Flags_ACK => True, Flags_SYN => False, Flags_FIN => False,
          Flags_RST => False, Flags_PSH => False);
      Hdr.Checksum := Calculate_Checksum (Hdr, Src_IP, Dest_IP, Payload);
      return Hdr;
   end Generate_ACK;

   function Generate_Data
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Window    : Unsigned_16;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address;
       Payload   : Devices.Operation_Data) return TCP_Header
   is
      Hdr : TCP_Header;
   begin
      Hdr := Generate_Header_Base
         (Src_Port, Dest_Port, Seq_Num, Ack_Num, Window,
          Flags_ACK => True, Flags_SYN => False, Flags_FIN => False,
          Flags_RST => False, Flags_PSH => True);
      Hdr.Checksum := Calculate_Checksum (Hdr, Src_IP, Dest_IP, Payload);
      return Hdr;
   end Generate_Data;

   function Generate_FIN
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Ack_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header
   is
      Hdr     : TCP_Header;
      Payload : constant Devices.Operation_Data (1 .. 0) := [others => 0];
   begin
      Hdr := Generate_Header_Base
         (Src_Port, Dest_Port, Seq_Num, Ack_Num, Default_Window_Size,
          Flags_ACK => True, Flags_SYN => False, Flags_FIN => True,
          Flags_RST => False, Flags_PSH => False);
      Hdr.Checksum := Calculate_Checksum (Hdr, Src_IP, Dest_IP, Payload);
      return Hdr;
   end Generate_FIN;

   function Generate_RST
      (Src_Port  : Unsigned_16;
       Dest_Port : Unsigned_16;
       Seq_Num   : Unsigned_32;
       Src_IP    : IPv4_Address;
       Dest_IP   : IPv4_Address) return TCP_Header
   is
      Hdr     : TCP_Header;
      Payload : constant Devices.Operation_Data (1 .. 0) := [others => 0];
   begin
      Hdr := Generate_Header_Base
         (Src_Port, Dest_Port, Seq_Num, 0, 0,
          Flags_ACK => False, Flags_SYN => False, Flags_FIN => False,
          Flags_RST => True, Flags_PSH => False);
      Hdr.Checksum := Calculate_Checksum (Hdr, Src_IP, Dest_IP, Payload);
      return Hdr;
   end Generate_RST;

   procedure To_Bytes
      (Hdr  : TCP_Header;
       Data : out Devices.Operation_Data)
   is
      pragma Warnings (Off, "storage order");
      Hdr_Bytes : Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Hdr'Address;
      pragma Warnings (On, "storage order");
   begin
      Data (Data'First .. Data'First + Header_Size - 1) := Hdr_Bytes;
   exception
      when Constraint_Error =>
         Data := [others => 0];
   end To_Bytes;

   function Generate_ISN return Unsigned_32 is
      Stamp : Time.Timestamp;
   begin
      --  Simple ISN generation: use clock ticks plus a counter.
      --  A more secure implementation would use a hash of connection tuple.
      Arch.Clocks.Get_Monotonic_Time (Stamp);
      ISN_Counter := ISN_Counter + 1;
      return Unsigned_32 (Stamp.Seconds and 16#FFFF#) * 64000 +
             Unsigned_32 (Stamp.Nanoseconds and 16#FFFF#) +
             ISN_Counter * 64000;
   exception
      when Constraint_Error =>
         return 0;
   end Generate_ISN;
end Networking.TCP;
