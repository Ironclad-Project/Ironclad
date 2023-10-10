--  networking.ads: Networking library.
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

with Devices;    use Devices;
with Interfaces; use Interfaces;

package Networking is
   --  Networking types.
   type MAC_Address  is array (1 ..  6) of Unsigned_8;
   type IPv4_Address is array (1 ..  4) of Unsigned_8;
   type IPv6_Address is array (1 .. 16) of Unsigned_8;
   type IPv4_Port is new Unsigned_16;
   type IPv6_Port is new Unsigned_16;

   --  Standard submasks.
   IPv4_8_Submask   : constant IPv4_Address := (255, 0, 0, 0);
   IPv6_128_Submask : constant IPv6_Address := (others => 16#FF#);
   ----------------------------------------------------------------------------
   --  Initialize the networking subsystem.
   procedure Initialize;
   ----------------------------------------------------------------------------
   --  Maximum size of a hostname.
   Hostname_Max_Len : constant Natural;

   --  Fetch the system's hostname.
   --  @param Name   Buffer to write the hostname.
   --  @param Length Length of the hostname, even if it doesn't fit.
   procedure Get_Hostname (Name : out String; Length : out Natural);

   --  Set the system's hostname.
   --  @param Name Hostname to set;
   --  @return True on success, False on failure.
   procedure Set_Hostname (Name : String; Success : out Boolean);
   ----------------------------------------------------------------------------
   --  Ironclad depends on userland to register networking interfaces for
   --  sockets to be created with them.

   --  Maximum number of interfaces.
   Max_Interface_Count : constant Natural;

   --  Register a device as a networking interface.
   --  The kernel will figure out the supported protocols and addresses by
   --  using the standard low-level networking interface.
   --  All interfaces are added blocked, must be unblocked before use.
   --  @param Interfaced Device to try to use a a networking interface.
   --  @param Success    True in success, False in failure.
   procedure Register_Interface
      (Interfaced : Devices.Device_Handle;
       Success    : out Boolean);

   --  Fetch the registered address of an interface.
   procedure Get_Interface_Address
      (Interfaced : Devices.Device_Handle;
       IP         : out IPv4_Address);

   procedure Get_Interface_Address
      (Interfaced : Devices.Device_Handle;
       IP         : out IPv6_Address);

   --  Block or unblock an interface.
   --  @param Interfaced Device to modify.
   --  @param Is_Blocked True for blocking broadcast, False for not doing so.
   --  @param Success    True in success, False in failure (not found).
   procedure Block
      (Interfaced : Devices.Device_Handle;
       Is_Blocked : Boolean;
       Success    : out Boolean);

   --  Fetch a registered interface device by address.
   procedure Get_Suitable_Interface
      (IP         : IPv4_Address;
       Interfaced : out Devices.Device_Handle);

   procedure Get_Suitable_Interface
      (IP         : IPv6_Address;
       Interfaced : out Devices.Device_Handle);

   --  Set the IP sets of an interface.
   --  0 addresses for either ipv4 or ipv6 will mean the stack will search
   --  for its own address, using DHCP for example.
   --  @param Interfaced Device to modify.
   --  @param IP4        IPv6 static address, 0 for the stack to acquire it.
   --  @param IP4_Subnet IPv4 static submask, 0 for the stack to acquire it.
   --  @param IP6        IPv6 static address, 0 for the stack to acquire it.
   --  @param IP6_Subnet IPv6 static submask, 0 for the stack to acquire it.
   --  @param Success    True in success, False in failure (not found).
   procedure Set_Addresses
      (Interfaced  : Devices.Device_Handle;
       IPv4        : IPv4_Address;
       IPv4_Subnet : IPv4_Address;
       IPv6        : IPv6_Address;
       IPv6_Subnet : IPv6_Address;
       Success     : out Boolean);

   --  Information reported about an interface.
   type Interface_Info is record
      Handle      : Devices.Device_Handle;
      Is_Blocked  : Boolean;
      MAC         : MAC_Address;
      IPv4        : IPv4_Address;
      IPv4_Subnet : IPv4_Address;
      IPv6        : IPv6_Address;
      IPv6_Subnet : IPv6_Address;
   end record;
   type Interface_Arr is array (Natural range <>) of Interface_Info;

   --  List all registered interfaces as well as their attributes.
   --  @param Buffer Buffer to write to.
   --  @param Len    Written count, even if it doesnt fit.
   procedure List_Interfaces (Buffer : out Interface_Arr; Len : out Natural);

private

   Hostname_Max_Len : constant Natural := 255;
   ----------------------------------------------------------------------------
   Max_Interface_Count : constant Natural := 5;
end Networking;
