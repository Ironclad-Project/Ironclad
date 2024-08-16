--  arch-pci.ads: PCI bus driver.
--  Copyright (C) 2021 streaksu
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

with Interfaces; use Interfaces;

package Arch.PCI is
   --  A PCI device is defined by a class, subclass, and prog if. And is
   --  addressed by its bus, slot, function, and parent.
   --
   --  Each PCI device has an associated set of base address registers, this
   --  structures hold MMIO addresses used by the device.
   type Base_Address_Register is record
      Base            : Integer_Address;
      Size            : Unsigned_64;
      Is_MMIO         : Boolean;
      Is_Prefetchable : Boolean;
   end record;
   type PCI_Device is private;

   --  Initialize a device with its physical locations.
   --  True if found.
   procedure Fetch_Device
      (Bus     : Unsigned_8;
       Slot    : Unsigned_8;
       Func    : Unsigned_8;
       Result  : out PCI_Device;
       Success : out Boolean);
   procedure Search_Device
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8;
       Result       : out PCI_Device;
       Success      : out Boolean);
   ----------------------------------------------------------------------------
   --  Enable bus mastering for the passed PCI device.
   procedure Enable_Bus_Mastering (Dev : PCI_Device);

   --  Get a BAR from the PCI device configuration space, return True if it
   --  exists and was successful, or false if not.
   type BAR_Index is range 0 .. 5;
   procedure Get_BAR
      (Dev     : PCI_Device;
       Index   : BAR_Index;
       BAR     : out Base_Address_Register;
       Success : out Boolean);
   ----------------------------------------------------------------------------
   --  Read and write data into the PCI device configuration space.
   function Read8  (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_8;
   function Read16 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_16;
   function Read32 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_32;
   procedure Write8  (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_8);
   procedure Write16 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_16);
   procedure Write32 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_32);

private

   type PCI_Device is record
      Bus          : Unsigned_8;
      Func         : Unsigned_8;
      Slot         : Unsigned_8;
      Device_ID    : Unsigned_16;
      Vendor_ID    : Unsigned_16;
      Revision_ID  : Unsigned_8;
      Subclass     : Unsigned_8;
      Device_Class : Unsigned_8;
      Prog_If      : Unsigned_8;
   end record;

   procedure Get_Address (Dev : PCI_Device; Offset : Unsigned_16);

   procedure Check_Bus
      (Bus                  : Unsigned_8;
       Desired_Device_Class : Unsigned_8;
       Desired_Subclass     : Unsigned_8;
       Desired_Prog_If      : Unsigned_8;
       Result               : out PCI_Device;
       Success              : out Boolean);

   procedure Check_Function
      (Bus                  : Unsigned_8;
       Slot                 : Unsigned_8;
       Func                 : Unsigned_8;
       Desired_Device_Class : Unsigned_8;
       Desired_Subclass     : Unsigned_8;
       Desired_Prog_If      : Unsigned_8;
       Result               : out PCI_Device;
       Success              : out Boolean);
end Arch.PCI;
