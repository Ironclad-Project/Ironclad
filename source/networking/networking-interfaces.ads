--  networking-interfaces.ads: Networking interface management.
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

with Devices; use Devices;
with Networking.ARP;

package Networking.Interfaces is
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
      (Interfaced  : Devices.Device_Handle;
       IPv4        : IPv4_Address;
       IPv4_Subnet : IPv4_Address;
       IPv6        : IPv6_Address;
       IPv6_Subnet : IPv6_Address;
       Success     : out Boolean);

   --  Fetch the registered address of an interface.
   procedure Get_Interface_Address
      (Interfaced : Devices.Device_Handle;
       IP         : out IPv4_Address)
      with Pre => ARP.Is_Initialized;

   procedure Get_Interface_Address
      (Interfaced : Devices.Device_Handle;
       IP         : out IPv6_Address)
      with Pre => ARP.Is_Initialized;

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
       Interfaced : out Devices.Device_Handle)
      with Pre => ARP.Is_Initialized;

   procedure Get_Suitable_Interface
      (IP         : IPv6_Address;
       Interfaced : out Devices.Device_Handle)
      with Pre => ARP.Is_Initialized;

   procedure Modify_Addresses
      (Interfaced : Devices.Device_Handle;
       IP         : IPv4_Address;
       IP_Subnet  : IPv4_Address;
       Success    : out Boolean)
      with Pre => ARP.Is_Initialized;

   procedure Modify_Addresses
      (Interfaced : Devices.Device_Handle;
       IP         : IPv6_Address;
       IP_Subnet  : IPv6_Address;
       Success    : out Boolean)
      with Pre => ARP.Is_Initialized;

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
   procedure List_Interfaces (Buffer : out Interface_Arr; Len : out Natural)
      with Pre => ARP.Is_Initialized;

private

   Max_Interface_Count : constant Natural := 5;
end Networking.Interfaces;
