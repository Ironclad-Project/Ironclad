--  arch-pci.ads: PCI bus driver.
--  Copyright (C) 2024 streaksu
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
   --  Generic PCI driver for the Ironclad kernel.

   --  Check whether PCI is supported.
   function Is_Supported return Boolean;

   --  Scan all the system's PCI and store it for future use.
   procedure Scan_PCI;
   ----------------------------------------------------------------------------
   --  A PCI device is defined by a class, subclass, and prog if. And is
   --  addressed by its bus, slot, function, and parent.
   --
   --  Each PCI device has an associated set of base address registers, this
   --  structures hold MMIO addresses used by the device.
   type PCI_Device is private;

   --  Get how many devices are available on the system with the passed
   --  information.
   --  @param Device_Class Device to search for.
   --  @param Subclass     Subclass to search for.
   --  @param Prog_If      Prog if to search for.
   --  @return Number of devices with these characteristics.
   function Enumerate_Devices
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8) return Natural;

   --  Get a device's information with the passed information.
   --  @param Device_Class Device to search for.
   --  @param Subclass     Subclass to search for.
   --  @param Prog_If      Prog if to search for.
   --  @param Idx          Index of the device among others like it.
   --  @param Result       Result device to write to.
   --  @param Success      True in success, False in failure.
   procedure Search_Device
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8;
       Idx          : Natural;
       Result       : out PCI_Device;
       Success      : out Boolean);

   --  Get a device's information with the passed location.
   --  When not found, a dummy device will be returned, for rationale, check
   --  the function body.
   --  @param Bus     Device to search for.
   --  @param Slot    Subclass to search for.
   --  @param Func    Prog if to search for.
   --  @param Result  Result device to write to.
   --  @param Success True in success, False in failure.
   procedure Search_Device
      (Bus     : Unsigned_8;
       Slot    : Unsigned_8;
       Func    : Unsigned_8;
       Result  : out PCI_Device;
       Success : out Boolean);
   ----------------------------------------------------------------------------
   --  Enable bus mastering for the passed PCI device.
   procedure Enable_Bus_Mastering (Dev : PCI_Device);

   --  Get a BAR from the PCI device configuration space, return True if it
   --  exists and was successful, or false if not.
   type BAR_Index is range 0 .. 5;

   type Base_Address_Register is record
      Base            : Integer_Address;
      Size            : Unsigned_64;
      Is_MMIO         : Boolean;
      Is_Prefetchable : Boolean;
   end record;

   procedure Get_BAR
      (Dev     : PCI_Device;
       Index   : BAR_Index;
       BAR     : out Base_Address_Register;
       Success : out Boolean);

   --  Set the MSI or MSIX vector.
   procedure Get_MSI_Support
      (Dev               : PCI_Device;
       Has_MSI, Has_MSIX : out Boolean);

   procedure Set_MSI_Vector (Dev : PCI_Device; Vector : Unsigned_8);
   ----------------------------------------------------------------------------
   --  Read and write data into the PCI device configuration space.
   function Read8  (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_8;
   function Read16 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_16;
   function Read32 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_32;
   procedure Write8  (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_8);
   procedure Write16 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_16);
   procedure Write32 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_32);
   ----------------------------------------------------------------------------
   type PCI_Listing is record
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
   type PCI_Listing_Arr is array (Natural range <>) of PCI_Listing;

   --  List all PCI devices on the system.
   --  @param Buffer Where to write all the information.
   --  @param Length Total count of devices, even if it is > Buffer'Length.
   procedure List_All (Buffer : out PCI_Listing_Arr; Length : out Natural);

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
      MSI_Support  : Boolean;
      MSIX_Support : Boolean;
      MSI_Offset   : Unsigned_8;
      MSIX_Offset  : Unsigned_8;
      Using_PCIe   : Boolean;
   end record;

   #if ArchName = """x86_64-limine"""
      type PCI_Registry_Entry;
      type PCI_Registry_Entry_Acc is access PCI_Registry_Entry;
      type PCI_Registry_Entry is record
         Dev  : PCI_Device;
         Next : PCI_Registry_Entry_Acc;
      end record;

      function Get_ECAM_Addr (Bus, Slot, Func : Unsigned_8) return Unsigned_64;
      procedure Get_Address (Dev : PCI_Device; Offset : Unsigned_16);
      procedure Check_Bus (Bus : Unsigned_8);
      procedure Check_Function (Bus, Slot, Func : Unsigned_8);
      procedure Fetch_Device
         (Bus     : Unsigned_8;
          Slot    : Unsigned_8;
          Func    : Unsigned_8;
          Result  : out PCI_Device;
          Success : out Boolean);
   #end if;
end Arch.PCI;
