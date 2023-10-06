--  networking.adb: Specification of networking library.
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

with Ada.Characters.Latin_1;
with Lib.Synchronization; use Lib.Synchronization;
with Devices.NetInter; use Devices.NetInter;
with Networking.ARP;

package body Networking is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Initialize is
   begin
      ARP.Initialize;
   end Initialize;
   ----------------------------------------------------------------------------
   Hostname_Lock   : aliased Binary_Semaphore := Unlocked_Semaphore;
   Hostname_Length : Natural := 4;
   Hostname : String (1 .. Hostname_Max_Len) := "none" & (1 .. 251 => ' ');

   procedure Get_Hostname (Name : out String; Length : out Natural) is
   begin
      Name := (others => Ada.Characters.Latin_1.NUL);

      if Name'Length < Hostname_Length then
         Length := Name'Length;
      else
         Length := Hostname_Length;
      end if;

      Seize (Hostname_Lock);
      Name (Name'First .. Name'First + Length - 1) := Hostname (1 .. Length);
      Release (Hostname_Lock);
   end Get_Hostname;

   procedure Set_Hostname (Name : String; Success : out Boolean) is
   begin
      if Name'Length = 0 or Name'Length > Hostname'Length then
         Success := False;
         return;
      end if;

      Seize (Hostname_Lock);
      Hostname_Length := Name'Length;
      Hostname (1 .. Name'Length) := Name;
      Release (Hostname_Lock);
      Success := True;
   end Set_Hostname;
   ----------------------------------------------------------------------------
   type Inner_Interface is record
      Handle     : Device_Handle;
      Is_Blocked : Boolean;
   end record;
   type Inner_Arr is array (1 .. Max_Interface_Count) of Inner_Interface;

   Interfaces_Lock : aliased Binary_Semaphore := Unlocked_Semaphore;
   Interfaces      : Inner_Arr := (others => (Devices.Error_Handle, True));

   procedure Register_Interface
      (Interfaced : Devices.Device_Handle;
       Success    : out Boolean)
   is
   begin
      Success := False;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Devices.Error_Handle then
            Int := (Interfaced, True);
            Success := True;
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Register_Interface;

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

   procedure Set_Addresses
      (Interfaced  : Devices.Device_Handle;
       IPv4        : IPv4_Address;
       IPv4_Subnet : IPv4_Address;
       IPv6        : IPv6_Address;
       IPv6_Subnet : IPv6_Address;
       Success     : out Boolean)
   is
      MAC : MAC_Address;
   begin
      Success := False;
      Seize (Interfaces_Lock);
      for Int of Interfaces loop
         if Int.Handle = Interfaced then
            Success := IO_Control (Interfaced, NET_GETMAC, MAC'Address);
            ARP.Add_Static (MAC, IPv4, IPv4_Subnet, IPv6, IPv6_Subnet);
            Success := True;
            exit;
         end if;
      end loop;
      Release (Interfaces_Lock);
   end Set_Addresses;

   procedure List_Interfaces (Buffer : out Interface_Arr; Len : out Natural) is
      Curr_Index : Natural := 0;
      Discard    : Boolean;
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

               Discard := IO_Control
                  (Interfaces (I).Handle,
                   NET_GETMAC,
                   Buffer (Buffer'First + Curr_Index).MAC'Address);

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
end Networking;
