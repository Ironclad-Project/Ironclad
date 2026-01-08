--  networking-ethernet.adb: Ethernet frame handling.
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

package body Networking.Ethernet is
   function Create_Header
      (Dest      : MAC_Address;
       Src       : MAC_Address;
       Ether_Typ : Unsigned_16) return Ethernet_Header
   is
   begin
      return (Destination => Dest,
              Source      => Src,
              EtherType   => Ether_Typ);
   end Create_Header;

   procedure Create_Frame
      (Dest      : MAC_Address;
       Src       : MAC_Address;
       Ether_Typ : Unsigned_16;
       Payload   : Devices.Operation_Data;
       Frame     : out Devices.Operation_Data;
       Frame_Len : out Natural)
   is
      Hdr : constant Ethernet_Header := Create_Header (Dest, Src, Ether_Typ);

      pragma Warnings (Off, "storage order");
      Hdr_Data : Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Hdr'Address;
      pragma Warnings (On, "storage order");
   begin
      --  Copy header.
      Frame (Frame'First .. Frame'First + Header_Size - 1) := Hdr_Data;

      --  Copy payload.
      Frame (Frame'First + Header_Size ..
             Frame'First + Header_Size + Payload'Length - 1) := Payload;

      Frame_Len := Header_Size + Payload'Length;
   exception
      when Constraint_Error =>
         Frame := [others => 0];
         Frame_Len := 0;
   end Create_Frame;

   procedure Parse_Header
      (Data    : Devices.Operation_Data;
       Header  : out Ethernet_Header;
       Success : out Boolean)
   is
      pragma Warnings (Off, "storage order");
      Hdr_Data : Devices.Operation_Data (1 .. Header_Size)
         with Import, Address => Header'Address;
      pragma Warnings (On, "storage order");
   begin
      Hdr_Data := Data (Data'First .. Data'First + Header_Size - 1);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Parse_Header;

   procedure Get_Payload
      (Frame       : Devices.Operation_Data;
       Payload     : out Devices.Operation_Data;
       Payload_Len : out Natural)
   is
      Actual_Len : Natural;
   begin
      Actual_Len := Frame'Length - Header_Size;
      Payload_Len := Actual_Len;
      Payload (Payload'First .. Payload'First + Actual_Len - 1) :=
         Frame (Frame'First + Header_Size .. Frame'Last);
   exception
      when Constraint_Error =>
         Payload := [others => 0];
         Payload_Len := 0;
   end Get_Payload;
end Networking.Ethernet;
