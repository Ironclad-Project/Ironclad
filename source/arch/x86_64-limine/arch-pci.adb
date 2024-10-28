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

with Arch.Snippets;
with Lib.Panic;
with Arch.CPU;

package body Arch.PCI is
   --  PCI configuration space starts at this IO Port addresess.
   PCI_Config_Address : constant := 16#CF8#;
   PCI_Config_Data    : constant := 16#CFC#;

   --  Maximum number of different PCI entities.
   PCI_Max_Function : constant Unsigned_8 := 7;
   PCI_Max_Slot     : constant Unsigned_8 := 31;

   PCI_Registry : PCI_Registry_Entry_Acc := null;

   procedure Scan_PCI is
      Root_Bus, Host_Bridge : PCI_Device;
      Success               : Boolean;
   begin
      Fetch_Device (0, 0, 0, Root_Bus, Success);
      if not Success then
         Lib.Panic.Hard_Panic ("Could not read root bus");
      end if;

      if (Read32 (Root_Bus, 16#C#) and 16#800000#) = 0 then
         Check_Bus (0);
      else
         for I in 0 .. PCI_Max_Function loop
            Fetch_Device (0, 0, I, Host_Bridge, Success);
            if Success then
               if Read32 (Host_Bridge, 0) /= 16#FFFFFFFF# then
                  Check_Bus (I);
               end if;
            end if;
         end loop;
      end if;
   end Scan_PCI;

   function Enumerate_Devices
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8) return Natural
   is
      Idx  :                Natural := 0;
      Temp : PCI_Registry_Entry_Acc := PCI_Registry;
   begin
      loop
         if Temp.Dev.Device_Class = Device_Class and
            Temp.Dev.Subclass     = Subclass     and
            Temp.Dev.Prog_If      = Prog_If
         then
            Idx := Idx + 1;
         end if;

         if Temp.Next /= null then
            Temp := Temp.Next;
         else
            exit;
         end if;
      end loop;

      return Idx;
   end Enumerate_Devices;

   procedure Search_Device
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8;
       Idx          : Natural;
       Result       : out PCI_Device;
       Success      : out Boolean)
   is
      FIdx :                Natural := 0;
      Temp : PCI_Registry_Entry_Acc := PCI_Registry;
   begin
      loop
         if Temp.Dev.Device_Class = Device_Class and
            Temp.Dev.Subclass     = Subclass     and
            Temp.Dev.Prog_If      = Prog_If
         then
            FIdx := FIdx + 1;
            if Idx = FIdx then
               Result  := Temp.Dev;
               Success := True;
               return;
            end if;
         end if;

         if Temp.Next /= null then
            Temp := Temp.Next;
         else
            exit;
         end if;
      end loop;

      Success := False;
   end Search_Device;
   ----------------------------------------------------------------------------
   procedure Enable_Bus_Mastering (Dev : PCI_Device) is
      Bus_Master_Bit : constant := 2#100#;
      Config4 : constant Unsigned_32 := Read32 (Dev, 4);
   begin
      if (Config4 and Bus_Master_Bit) = 0 then
         Write32 (Dev, 4, Config4 or Bus_Master_Bit);
      end if;
   end Enable_Bus_Mastering;

   procedure Get_BAR
      (Dev     : PCI_Device;
       Index   : BAR_Index;
       BAR     : out Base_Address_Register;
       Success : out Boolean)
   is
      Reg_Index : constant Unsigned_16 := 16#10# + Unsigned_16 (Index) * 4;
      BAR_Low, BAR_Size_Low, BAR_High, BAR_Size_High : Unsigned_32;
      Is_MMIO, Is_Prefetchable, Is_64_Bits : Boolean;
      Base, Size : Unsigned_64;
   begin
      --  Check if the BAR exists first of all.
      if Read32 (Dev, Reg_Index) = 0 then
         Success := False;
         return;
      end if;

      --  Fetch the rest.
      BAR_Low         := Read32 (Dev, Reg_Index);
      BAR_High        := 0;
      Is_MMIO         := (BAR_Low and 1) = 0;
      Is_Prefetchable := Is_MMIO and ((BAR_Low and 2#1000#) /= 0);
      Is_64_Bits := Is_MMIO and ((Shift_Right (BAR_Low, 1) and 2#11#) = 2#10#);
      if Is_64_Bits then
         BAR_High := Read32 (Dev, Reg_Index + 4);
      end if;

      Base := Shift_Left (Unsigned_64 (BAR_High), 32) or Unsigned_64 (BAR_Low);
      if Is_MMIO then
         Base := Base and not 2#1111#;
      else
         Base := Base and not 2#11#;
      end if;

      Write32 (Dev, Reg_Index, 16#FFFFFFFF#);
      BAR_Size_Low := Read32 (Dev, Reg_Index);
      Write32 (Dev, Reg_Index, BAR_Low);

      if Is_64_Bits then
         Write32 (Dev, Reg_Index + 4, 16#FFFFFFFF#);
         BAR_Size_High := Read32 (Dev, Reg_Index + 4);
         Write32 (Dev, Reg_Index + 4, BAR_High);
      else
         BAR_Size_High := 16#FFFFFFFF#;
      end if;

      Size := Shift_Left (Unsigned_64 (BAR_Size_High), 32) or
              Unsigned_64 (BAR_Size_Low);
      if Is_MMIO then
         Base := Base and not 2#1111#;
      else
         Base := Base and not 2#11#;
      end if;
      Size := not Size + 1;

      BAR :=
         (Base            => Integer_Address (Base),
          Size            => Size,
          Is_MMIO         => Is_MMIO,
          Is_Prefetchable => Is_Prefetchable);
      Success := True;
   end Get_BAR;

   procedure Get_MSI_Support
      (Dev               : PCI_Device;
       Has_MSI, Has_MSIX : out Boolean)
   is
   begin
      Has_MSI  := Dev.MSI_Support;
      Has_MSIX := Dev.MSIX_Support;
   end Get_MSI_Support;

   procedure Set_MSI_Vector (Dev : PCI_Device; Vector : Unsigned_8) is
      BSP_LAPIC       : constant Unsigned_32 := CPU.Core_Locals (1).LAPIC_ID;
      MSI_Off         : constant Unsigned_16 := Unsigned_16 (Dev.MSI_Offset);
      Message_Control : Unsigned_16;
      Reg0            : Unsigned_16;
      Reg1            : Unsigned_16;
      Addr            : Unsigned_32;
   begin
      --  XXX: Support MSI-X, this so far only uses MSI.
      if not Dev.MSI_Support then
         return;
      end if;

      Message_Control := Read16 (Dev, MSI_Off + 2);
      Reg0 := 4;
      Reg1 := (if (Shift_Right (Message_Control, 7) and 1) = 1 then 12 else 8);
      Addr := Shift_Left (16#FEE#, 20) or Shift_Left (BSP_LAPIC, 12);

      Write32 (Dev, MSI_Off + Reg0, Addr);
      Write32 (Dev, MSI_Off + Reg1, Unsigned_32 (Vector));

      Message_Control := (Message_Control or 1) and not Shift_Left (2#111#, 4);
      Write16 (Dev, MSI_Off + 1, Message_Control);
   end Set_MSI_Vector;
   ----------------------------------------------------------------------------
   function Read8 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_8 is
   begin
      Get_Address (Dev, Off);
      return Snippets.Port_In (PCI_Config_Data + (Off and 3));
   end Read8;

   function Read16 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_16 is
   begin
      Get_Address (Dev, Off);
      return Snippets.Port_In16 (PCI_Config_Data + (Off and 3));
   end Read16;

   function Read32 (Dev : PCI_Device; Off : Unsigned_16) return Unsigned_32 is
   begin
      Get_Address (Dev, Off);
      return Snippets.Port_In32 (PCI_Config_Data + (Off and 3));
   end Read32;

   procedure Write8 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_8) is
   begin
      Get_Address (Dev, Off);
      Snippets.Port_Out (PCI_Config_Data + (Off and 3), D);
   end Write8;

   procedure Write16 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_16) is
   begin
      Get_Address (Dev, Off);
      Snippets.Port_Out16 (PCI_Config_Data + (Off and 3), D);
   end Write16;

   procedure Write32 (Dev : PCI_Device; Off : Unsigned_16; D : Unsigned_32) is
   begin
      Get_Address (Dev, Off);
      Snippets.Port_Out32 (PCI_Config_Data + (Off and 3), D);
   end Write32;
   ----------------------------------------------------------------------------
   procedure Get_Address (Dev : PCI_Device; Offset : Unsigned_16) is
      Addr : constant Unsigned_32 :=
         Shift_Left (Unsigned_32 (Dev.Bus),  16) or
         Shift_Left (Unsigned_32 (Dev.Slot), 11) or
         Shift_Left (Unsigned_32 (Dev.Func),  8) or
         (Unsigned_32 (Offset) and not 3)        or
         16#80000000#;
   begin
      Snippets.Port_Out32 (PCI_Config_Address, Addr);
   end Get_Address;

   procedure Check_Bus (Bus : Unsigned_8) is
   begin
      for Slot in 0 .. PCI_Max_Slot loop
         for Func in 0 .. PCI_Max_Function loop
            Check_Function (Bus, Slot, Func);
         end loop;
      end loop;
   end Check_Bus;

   procedure Check_Function (Bus, Slot, Func : Unsigned_8) is
      Success : Boolean;
      Config8 : Unsigned_32;
      Temp    : PCI_Registry_Entry_Acc := null;
      Result  : PCI_Device;
   begin
      Fetch_Device (Bus, Slot, Func, Result, Success);
      if not Success then
         return;
      end if;

      --  Check for placeholder devices.
      if Result.Device_ID = 16#FFFF# and Result.Vendor_ID = 16#FFFF# then
         return;
      end if;

      --  Check for PCI bridge, and take it.
      if Result.Device_Class = 6 and Result.Subclass = 4 then
         Config8 := Read32 (Result, 16#18#);
         Check_Bus (Unsigned_8 (Shift_Right (Config8, 8) and 16#FF#));
         return;
      end if;

      --  Add the device to the list.
      if PCI_Registry = null then
         PCI_Registry := new PCI_Registry_Entry'(Result, null);
      else
         Temp := PCI_Registry;
         while Temp.Next /= null loop
            Temp := Temp.Next;
         end loop;
         Temp.Next := new PCI_Registry_Entry'(Result, null);
      end if;
   end Check_Function;

   procedure Fetch_Device
      (Bus     : Unsigned_8;
       Slot    : Unsigned_8;
       Func    : Unsigned_8;
       Result  : out PCI_Device;
       Success : out Boolean)
   is
      Config0, Config8 : Unsigned_32;
      Config6  : Unsigned_16;
      Config34 : Unsigned_8;
   begin
      --  Assign the needed values for reading from the PCI config space.
      Result.Bus  := Bus;
      Result.Slot := Slot;
      Result.Func := Func;

      --  Read additional data and fill the device record.
      Config0 := Read32 (Result, 0);
      Config8 := Read32 (Result, 8);
      Result  :=
         (Bus          => Bus,
          Func         => Func,
          Slot         => Slot,
          Device_ID    => Unsigned_16 (Shift_Right (Config0, 16) and 16#FFFF#),
          Vendor_ID    => Unsigned_16 (Config0 and 16#FFFF#),
          Revision_ID  => Unsigned_8 (Config8 and 16#FF#),
          Subclass     => Unsigned_8 (Shift_Right (Config8, 16) and 16#FF#),
          Device_Class => Unsigned_8 (Shift_Right (Config8, 24) and 16#FF#),
          Prog_If      => Unsigned_8 (Shift_Right (Config8,  8) and 16#FF#),
          MSI_Support  => False,
          MSIX_Support => False,
          MSI_Offset   => 0,
          MSIX_Offset  => 0);

      --  Check for MSI/MSIX by reading the capabilities list.
      Config6 := Read16 (Result, 6);
      if (Config6 and Shift_Right (1, 4)) /= 0 then
         Config34 := Read8 (Result, 34);
         while Config34 /= 0 loop
            case Read8 (Result, Unsigned_16 (Config34)) is
               when 16#05# =>
                  Result.MSI_Support := True;
                  Result.MSI_Offset  := Config34;
               when 16#11# =>
                  Result.MSIX_Support := True;
                  Result.MSIX_Offset  := Config34;
               when others =>
                  null;
            end case;

            Config34 := Read8 (Result, Unsigned_16 (Config34) + 1);
         end loop;
      end if;

      Success := True;
   end Fetch_Device;
end Arch.PCI;
