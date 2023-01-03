--  arch-pci.adb: PCI bus driver.
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

with Arch.Snippets;

package body Arch.PCI with SPARK_Mode => Off is
   --  PCI configuration space starts at this IO Port addresess.
   PCI_Config_Address : constant := 16#CF8#;
   PCI_Config_Data    : constant := 16#CFC#;

   --  Maximum number of different PCI entities.
   PCI_Max_Function : constant Unsigned_8 := 7;
   PCI_Max_Slot     : constant Unsigned_8 := 31;

   function Fetch_Device
      (Bus    : Unsigned_8;
       Slot   : Unsigned_8;
       Func   : Unsigned_8;
       Result : out PCI_Device) return Boolean
   is
      Config0, Config8 : Unsigned_32;
   begin
      --  Assign the needed values for reading from the PCI config space.
      Result.Bus  := Bus;
      Result.Slot := Slot;
      Result.Func := Func;

      --  Read additional data.
      Config0 := Read32 (Result, 0);
      Config8 := Read32 (Result, 8);
      Result  := (
         Bus          => Bus,
         Func         => Func,
         Slot         => Slot,
         Device_ID    => Unsigned_16 (Shift_Right (Config0, 16) and 16#FFFF#),
         Vendor_ID    => Unsigned_16 (Config0 and 16#FFFF#),
         Revision_ID  => Unsigned_8 (Config8 and 16#FF#),
         Subclass     => Unsigned_8 (Shift_Right (Config8, 16) and 16#FF#),
         Device_Class => Unsigned_8 (Shift_Right (Config8, 24) and 16#FF#),
         Prog_If      => Unsigned_8 (Shift_Right (Config8,  8) and 16#FF#)
      );
      return True;
   end Fetch_Device;

   function Search_Device
      (Device_Class : Unsigned_8;
       Subclass     : Unsigned_8;
       Prog_If      : Unsigned_8;
       Result       : out PCI_Device) return Boolean
   is
      Root_Bus, Host_Bridge : PCI_Device;
   begin
      if not Fetch_Device (0, 0, 0, 0, Root_Bus) then
         goto Error;
      end if;

      if (Read32 (Root_Bus, 16#C#) and 16#800000#) = 0 then
         return Check_Bus (
            Bus                  => 0,
            Desired_Device_Class => Device_Class,
            Desired_Subclass     => Subclass,
            Desired_Prog_If      => Prog_If,
            Result               => Result
         );
      else
         for I in 0 .. PCI_Max_Function loop
            if Fetch_Device (0, 0, I, 0, Host_Bridge) then
               if Read32 (Host_Bridge, 0) /= 16#FFFFFFFF# then
                  if Check_Bus (
                     Bus                  => I,
                     Desired_Device_Class => Device_Class,
                     Desired_Subclass     => Subclass,
                     Desired_Prog_If      => Prog_If,
                     Result               => Result
                  )
                  then
                     return True;
                  end if;
               end if;
            end if;
         end loop;
      end if;

   <<Error>>
      return False;
   end Search_Device;

   function Check_Bus
      (Bus                  : Unsigned_8;
       Desired_Device_Class : Unsigned_8;
       Desired_Subclass     : Unsigned_8;
       Desired_Prog_If      : Unsigned_8;
       Result               : out PCI_Device) return Boolean
   is
   begin
      for Slot in 0 .. PCI_Max_Slot loop
         for Func in 0 .. PCI_Max_Function loop
            if Check_Function (
               Bus                  => Bus,
               Slot                 => Slot,
               Func                 => Func,
               Desired_Device_Class => Desired_Device_Class,
               Desired_Subclass     => Desired_Subclass,
               Desired_Prog_If      => Desired_Prog_If,
               Result               => Result
            )
            then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Check_Bus;

   function Check_Function
      (Bus                  : Unsigned_8;
       Slot                 : Unsigned_8;
       Func                 : Unsigned_8;
       Desired_Device_Class : Unsigned_8;
       Desired_Subclass     : Unsigned_8;
       Desired_Prog_If      : Unsigned_8;
       Result               : out PCI_Device) return Boolean
   is
      Fetch_Success : constant Boolean := Fetch_Device (
         Bus    => Bus,
         Slot   => Slot,
         Func   => Func,
         Result => Result
      );
      Config8 : Unsigned_32;
   begin
      if not Fetch_Success then
         return False;
      end if;

      --  Check for placeholder devices.
      if Result.Device_ID = 16#FFFF# and Result.Vendor_ID = 16#FFFF# then
         return False;
      end if;

      --  Check for PCI bridge, and take it.
      if Result.Device_Class = 6 and Result.Subclass = 4 then
         Config8 := Read32 (Result, 16#18#);
         return Check_Bus (
            Bus    => Unsigned_8 (Shift_Right (Config8, 8) and 16#FF#),
            Desired_Device_Class => Desired_Device_Class,
            Desired_Subclass     => Desired_Subclass,
            Desired_Prog_If      => Desired_Prog_If,
            Result               => Result
         );
      end if;

      return Result.Device_Class = Desired_Device_Class and
             Result.Subclass     = Desired_Subclass     and
             Result.Prog_If      = Desired_Prog_If;
   end Check_Function;
   ----------------------------------------------------------------------------
   procedure Enable_Bus_Mastering (Dev : PCI_Device) is
      Bus_Master_Bit : constant := 2#100#;
      Config4 : constant Unsigned_32 := Read32 (Dev, 4);
   begin
      if (Config4 and Bus_Master_Bit) = 0 then
         Write32 (Dev, 4, Config4 or Bus_Master_Bit);
      end if;
   end Enable_Bus_Mastering;

   function Get_BAR
      (Dev   : PCI_Device;
       Index : BAR_Index;
       BAR   : out Base_Address_Registers) return Boolean
   is
      Reg_Index : constant Unsigned_16 := 16#10# + Unsigned_16 (Index) * 4;
      BAR_Low, BAR_Size_Low, BAR_High, BAR_Size_High : Unsigned_32;
      Is_MMIO, Is_Prefetchable, Is_64_Bits : Boolean;
      Base, Size : Unsigned_64;
   begin
      --  Check if the BAR exists first of all.
      if Read32 (Dev, Reg_Index) = 0 then
         return False;
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

      BAR := (
         Base            => To_Address (Integer_Address (Base)),
         Size            => Size,
         Is_MMIO         => Is_MMIO,
         Is_Prefetchable => Is_Prefetchable
      );
      return True;
   end Get_BAR;
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
end Arch.PCI;
