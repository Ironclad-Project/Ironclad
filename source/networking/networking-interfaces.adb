--  networking-interfaces.adb: Networking interface management.
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

with Devices.NetInter; use Devices.NetInter;

package body Networking.Interfaces is
   pragma Suppress (All_Checks); --  Unit passes GNATprove AoRTE.

   type Inner_Interface is record
      Handle     : Device_Handle;
      MAC        : MAC_Address;
      Is_Blocked : Boolean;
   end record;
   type Inner_Arr is array (1 .. Max_Interface_Count) of Inner_Interface;

   Interfaces_Lock : aliased Binary_Semaphore := Unlocked_Semaphore;
   Interfaces      : Inner_Arr :=
      (others => (Devices.Error_Handle, (others => 0), True));

   procedure Register_Interface
      (Interfaced  : Devices.Device_Handle;
       IPv4        : IPv4_Address;
       IPv4_Subnet : IPv4_Address;
       IPv6        : IPv6_Address;
       IPv6_Subnet : IPv6_Address;
       Success     : out Boolean)
   is
      pragma SPARK_Mode (Off); --  We need the 'Address here.
   begin
      Success := False;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Devices.Error_Handle then
            Int.Handle := Interfaced;
            Int.Is_Blocked := True;
            Success := IO_Control (Interfaced, NET_GETMAC, Int.MAC'Address);
            ARP.Add_Static (Int.MAC, IPv4, IPv4_Subnet, IPv6, IPv6_Subnet);
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Register_Interface;

   procedure Get_Interface_Address
      (Interfaced : Devices.Device_Handle;
       IP         : out IPv4_Address)
   is
      IP_Sub : IPv4_Address;
   begin
      IP := (others => 0);
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Interfaced then
            ARP.Lookup (Int.MAC, IP, IP_Sub);
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Get_Interface_Address;

   procedure Get_Interface_Address
      (Interfaced : Devices.Device_Handle;
       IP         : out IPv6_Address)
   is
      IP_Sub : IPv6_Address;
   begin
      IP := (others => 0);
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Interfaced then
            ARP.Lookup (Int.MAC, IP, IP_Sub);
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Get_Interface_Address;

   procedure Block
      (Interfaced : Devices.Device_Handle;
       Is_Blocked : Boolean;
       Success    : out Boolean)
   is
   begin
      Success := False;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Interfaced then
            Int.Is_Blocked := Is_Blocked;
            Success := True;
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Block;

   procedure Get_Suitable_Interface
      (IP         : IPv4_Address;
       Interfaced : out Devices.Device_Handle)
   is
      IP_Sub : IPv4_Address;
      IP2    : IPv4_Address;
   begin
      Interfaced := Devices.Error_Handle;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle /= Devices.Error_Handle and not Int.Is_Blocked then
            ARP.Lookup (Int.MAC, IP2, IP_Sub);
            for I in IP_Sub'Range loop
               if IP_Sub (I) /= 0 and then IP (I) /= IP2 (I) then
                  goto End_Iter;
               end if;
            end loop;
            Interfaced := Int.Handle;
            exit;
         <<End_Iter>>
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Get_Suitable_Interface;

   procedure Get_Suitable_Interface
      (IP         : IPv6_Address;
       Interfaced : out Devices.Device_Handle)
   is
      IP_Sub : IPv6_Address;
      IP2    : IPv6_Address;
   begin
      Interfaced := Devices.Error_Handle;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle /= Devices.Error_Handle and not Int.Is_Blocked then
            ARP.Lookup (Int.MAC, IP2, IP_Sub);
            for I in IP_Sub'Range loop
               if IP_Sub (I) /= 0 and then IP (I) /= IP2 (I) then
                  goto End_Iter;
               end if;
            end loop;
            Interfaced := Int.Handle;
            exit;
         <<End_Iter>>
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Get_Suitable_Interface;

   procedure Modify_Addresses
      (Interfaced : Devices.Device_Handle;
       IP         : IPv4_Address;
       IP_Subnet  : IPv4_Address;
       Success    : out Boolean)
   is
   begin
      Success := False;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Interfaced then
            ARP.Modify_Static (Int.MAC, IP, IP_Subnet);
            Success := True;
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Modify_Addresses;

   procedure Modify_Addresses
      (Interfaced : Devices.Device_Handle;
       IP         : IPv6_Address;
       IP_Subnet  : IPv6_Address;
       Success    : out Boolean)
   is
   begin
      Success := False;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Interfaced then
            ARP.Modify_Static (Int.MAC, IP, IP_Subnet);
            Success := True;
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Modify_Addresses;

   procedure List_Interfaces (Buffer : out Interface_Arr; Len : out Natural) is
      pragma Annotate (GNATprove, False_Positive,
         "cannot prove upper bound for Buffer'Length",
         "not entirely sure how that happens");
      Curr_Index : Natural := 0;
   begin
      Len := 0;
      Buffer := (others =>
         (Handle      => Devices.Error_Handle,
          Is_Blocked  => True,
          MAC         => (others => 0),
          IPv4        => (others => 0),
          IPv4_Subnet => (others => 0),
          IPv6        => (others => 0),
          IPv6_Subnet => (others => 0)));

      Seize (Interfaces_Lock);
      for I in Interfaces'Range loop
         if Interfaces (I).Handle /= Devices.Error_Handle then
            Len := Len + 1;
            if Curr_Index < Buffer'Length then
               Buffer (Buffer'First + Curr_Index).Handle :=
                  Interfaces (I).Handle;
               Buffer (Buffer'First + Curr_Index).Is_Blocked :=
                  Interfaces (I).Is_Blocked;
               Buffer (Buffer'First + Curr_Index).MAC := Interfaces (I).MAC;

               ARP.Lookup
                  (Buffer (Buffer'First + Curr_Index).MAC,
                   Buffer (Buffer'First + Curr_Index).IPv4,
                   Buffer (Buffer'First + Curr_Index).IPv4_Subnet);
               ARP.Lookup
                  (Buffer (Buffer'First + Curr_Index).MAC,
                   Buffer (Buffer'First + Curr_Index).IPv6,
                   Buffer (Buffer'First + Curr_Index).IPv6_Subnet);

               Curr_Index := Curr_Index + 1;
            end if;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end List_Interfaces;
end Networking.Interfaces;
