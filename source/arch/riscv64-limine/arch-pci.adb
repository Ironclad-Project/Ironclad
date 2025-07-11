--  arch-pci.adb: PCI bus driver.
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

package body Arch.PCI is
   function Is_Supported return Boolean is
   begin
      return False;
   end Is_Supported;

   procedure Scan_PCI is
   begin
      null;
   end Scan_PCI;
   ----------------------------------------------------------------------------
   function Enumerate_Devices
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8) return Natural
   is
      pragma Unreferenced (Device_Class, Subclass, Prog_If);
   begin
      return 0;
   end Enumerate_Devices;

   procedure Search_Device
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8;
       Idx          : Natural;
       Result       : out PCI_Device;
       Success      : out Boolean)
   is
      pragma Unreferenced (Device_Class, Subclass, Prog_If, Idx, Result);
   begin
      Success := False;
   end Search_Device;

   procedure Search_Device
      (Bus     : Unsigned_8;
       Slot    : Unsigned_8;
       Func    : Unsigned_8;
       Result  : out PCI_Device;
       Success : out Boolean)
   is
      pragma Unreferenced (Bus, Slot, Func, Result);
   begin
      Success := False;
   end Search_Device;
   ----------------------------------------------------------------------------
   procedure Enable_Bus_Mastering (Dev : PCI_Device) is
      pragma Unreferenced (Dev);
   begin
      null;
   end Enable_Bus_Mastering;

   procedure Get_BAR
      (Dev     : PCI_Device;
       Index   : BAR_Index;
       BAR     : out Base_Address_Register;
       Success : out Boolean)
   is
      pragma Unreferenced (Dev, Index, BAR);
   begin
      Success := False;
   end Get_BAR;

   procedure Get_MSI_Support
      (Dev               : PCI_Device;
       Has_MSI, Has_MSIX : out Boolean)
   is
      pragma Unreferenced (Dev);
   begin
      Has_MSI  := False;
      Has_MSIX := False;
   end Get_MSI_Support;

   procedure Set_MSI_Vector (Dev : PCI_Device; Vector : Unsigned_8) is
      pragma Unreferenced (Dev, Vector);
   begin
      null;
   end Set_MSI_Vector;
   ----------------------------------------------------------------------------
   function Read8 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_8 is
      pragma Unreferenced (Dev, Off);
   begin
      return 0;
   end Read8;

   function Read16 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_16 is
      pragma Unreferenced (Dev, Off);
   begin
      return 0;
   end Read16;

   function Read32 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_32 is
      pragma Unreferenced (Dev, Off);
   begin
      return 0;
   end Read32;

   procedure Write8 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_8) is
      pragma Unreferenced (Dev, Off, D);
   begin
      null;
   end Write8;

   procedure Write16 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_16) is
      pragma Unreferenced (Dev, Off, D);
   begin
      null;
   end Write16;

   procedure Write32 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_32) is
      pragma Unreferenced (Dev, Off, D);
   begin
      null;
   end Write32;
   ----------------------------------------------------------------------------
   procedure List_All (Buffer : out PCI_Listing_Arr; Length : out Natural) is
   begin
      Buffer := [others => (0, 0, 0, 0, 0, 0, 0, 0, 0)];
      Length := 0;
   end List_All;
end Arch.PCI;
