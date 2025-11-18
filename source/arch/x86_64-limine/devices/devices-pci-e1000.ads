--  devices-pci-e1000.ads: Intel e1000 Gigabit Ethernet driver.
--  Copyright (C) 2025 Alexander Richards
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

with Memory;

package Devices.PCI.E1000 with SPARK_Mode => Off is
   procedure Init (Success : out Boolean);

private
   --  e1000 PCI Vendor/Device IDs for 82540EM (emulated by QEMU)
   E1000_VENDOR_ID : constant := 16#8086#;
   E1000_DEVICE_ID : constant := 16#100E#;

   --  e1000e PCI Vendor/Device IDs for 82574L (also emulated by QEMU)
   E1000E_VENDOR_ID : constant := 16#8086#;
   E1000E_DEVICE_ID : constant := 16#10D3#;

   type Device_Model is (E1000, E1000E);

   --  Descriptor Ring Sizes
   RX_RING_SIZE : constant := 16;
   TX_RING_SIZE : constant := 16;

   --  Descriptor Index Types
   type RX_Desc_Idx_Type is mod RX_RING_SIZE;
   type TX_Desc_Idx_Type is mod TX_RING_SIZE;

   --  Packet Buffer Types
   type RX_Packet_Buffers is array (RX_Desc_Idx_Type)
      of Memory.Virtual_Address;
   type TX_Packet_Buffers is array (TX_Desc_Idx_Type)
      of Memory.Virtual_Address;

   --  Maximum packet size (without CRC)
   MAX_PACKET_SIZE : constant := 1520;

   --  Descriptor Structures
   type RX_Descriptor is record
      Address    : Unsigned_64;  --  Buffer address
      Length     : Unsigned_16;  --  Length of received packet
      Checksum   : Unsigned_16;  --  Packet checksum (if enabled)
      Status     : Unsigned_8;   --  Descriptor status
      Errors     : Unsigned_8;   --  Error bits
      Reserved   : Unsigned_16;
   end record;
   for RX_Descriptor'Size use 128;

   type RX_Descriptor_Array is array (RX_Desc_Idx_Type) of RX_Descriptor;
   type RX_Descriptor_Array_Acc is access all RX_Descriptor_Array;

   type TX_Descriptor is record
      Address    : Unsigned_64;  --  Buffer address
      Length     : Unsigned_16;  --  Length of packet
      CSO        : Unsigned_8;   --  Checksum offset
      CMD        : Unsigned_8;   --  Command
      Status     : Unsigned_8;   --  Descriptor status
      CSS        : Unsigned_8;   --  Checksum start field
      VLAN       : Unsigned_16;  --  VLAN tag
   end record;
   for TX_Descriptor'Size use 128;

   type TX_Descriptor_Array is array (TX_Desc_Idx_Type) of TX_Descriptor;
   type TX_Descriptor_Array_Acc is access all TX_Descriptor_Array;

   --  Descriptor Status bits
   DESC_STATUS_DD : constant Unsigned_8 := 16#01#;  --  Descriptor Done

   --  Transmit Command bits
   TX_CMD_EOP : constant Unsigned_8 := 16#01#;  --  End Of Packet
   TX_CMD_IFCS : constant Unsigned_8 := 16#02#;  --  Insert FCS (CRC)
   TX_CMD_RS : constant Unsigned_8 := 16#08#;  --  Report Status

   --  Receive Descriptor Status bits
   RX_STATUS_DD : constant Unsigned_8 := 16#01#;  --  Descriptor Done
   RX_STATUS_EOP : constant Unsigned_8 := 16#02#;  --  End Of Packet

   subtype Packet_Buffer is Operation_Data (1 .. MAX_PACKET_SIZE);

   type MMIO_Registers is record
      --  Device Control Registers
      Ctrl         : Unsigned_32;  --  0x0000 - Device Control
      Status       : Unsigned_32;  --  0x0008 - Device Status
      Eeprom       : Unsigned_32;  --  0x0014 - EEPROM/Flash Control
      CtrlExt      : Unsigned_32;  --  0x0018 - Extended Device Control

      --  Interrupt Registers
      ICR          : Unsigned_32;  --  0x00C0 - Interrupt Cause Read
      IMask        : Unsigned_32;  --  0x00D0 - Interrupt Mask Set/Read

      --  Receive Control Registers
      RCTL         : Unsigned_32;  --  0x0100 - Receive Control

      --  Transmit Control Registers
      TCTL         : Unsigned_32;  --  0x0400 - Transmit Control
      TIPG         : Unsigned_32;  --  0x0410 - Transmit IPG

      --  Receive Descriptor Registers
      RDBAL        : Unsigned_32;  --  0x2800 - RX Descriptor Base Low
      RDBAH        : Unsigned_32;  --  0x2804 - RX Descriptor Base High
      RDLEN        : Unsigned_32;  --  0x2808 - RX Descriptor Length
      RDH          : Unsigned_32;  --  0x2810 - RX Descriptor Head
      RDT          : Unsigned_32;  --  0x2818 - RX Descriptor Tail
      RDTR         : Unsigned_32;  --  0x281C - RX Delay Timer

      --  Transmit Descriptor Registers
      TDBAL        : Unsigned_32;  --  0x3800 - TX Descriptor Base Low
      TDBAH        : Unsigned_32;  --  0x3804 - TX Descriptor Base High
      TDLEN        : Unsigned_32;  --  0x3808 - TX Descriptor Length
      TDH          : Unsigned_32;  --  0x3810 - TX Descriptor Head
      TDT          : Unsigned_32;  --  0x3818 - TX Descriptor Tail

      --  Receive Address Registers
      RA_Low       : Unsigned_32;  --  0x5400 - Receive Address Low
      RA_High      : Unsigned_32;  --  0x5404 - Receive Address High
   end record
      with Volatile;

   for MMIO_Registers use record
      Ctrl    at 16#0000# range 0 .. 31;
      Status  at 16#0008# range 0 .. 31;
      Eeprom  at 16#0014# range 0 .. 31;
      CtrlExt at 16#0018# range 0 .. 31;
      ICR     at 16#00C0# range 0 .. 31;
      IMask   at 16#00D0# range 0 .. 31;
      RCTL    at 16#0100# range 0 .. 31;
      TCTL    at 16#0400# range 0 .. 31;
      TIPG    at 16#0410# range 0 .. 31;
      RDBAL   at 16#2800# range 0 .. 31;
      RDBAH   at 16#2804# range 0 .. 31;
      RDLEN   at 16#2808# range 0 .. 31;
      RDH     at 16#2810# range 0 .. 31;
      RDT     at 16#2818# range 0 .. 31;
      RDTR    at 16#281C# range 0 .. 31;
      TDBAL   at 16#3800# range 0 .. 31;
      TDBAH   at 16#3804# range 0 .. 31;
      TDLEN   at 16#3808# range 0 .. 31;
      TDH     at 16#3810# range 0 .. 31;
      TDT     at 16#3818# range 0 .. 31;
      RA_Low  at 16#5400# range 0 .. 31;
      RA_High at 16#5404# range 0 .. 31;
   end record;

   type MMIO_Registers_Acc is access all MMIO_Registers;

   type Controller_Data is record
      --  MMIO Base address (for register access)
      MMIO_Regs           : MMIO_Registers_Acc;

      --  Descriptor rings
      RX_Descriptors      : RX_Descriptor_Array_Acc;
      TX_Descriptors      : TX_Descriptor_Array_Acc;

      --  Packet buffers
      RX_Buffers          : RX_Packet_Buffers;
      TX_Buffers          : TX_Packet_Buffers;

      --  Ring indices
      RX_Next             : RX_Desc_Idx_Type;
      TX_Next             : TX_Desc_Idx_Type;

      --  Interrupt info
      Interrupt           : Unsigned_8;
      Interrupt_IDT_Index : Unsigned_16;
      
      --  Device model
      Model               : Device_Model;
   end record;
   pragma Volatile (Controller_Data);

   type Controller_Data_Acc is access all Controller_Data;

   --  Interrupt handler
   procedure Generic_E1000_Interrupt_Handler (Num : Integer);

   function Install_Interrupt_Handler (CD : Controller_Data_Acc)
      return Boolean;

   procedure Handle_Interrupt (CD : Controller_Data_Acc);

   --  Initialization helpers
   function Initialize_Descriptors (CD : Controller_Data_Acc) return Boolean;

   function Enable_Receiver (CD : Controller_Data_Acc) return Boolean;

   function Enable_Transmitter (CD : Controller_Data_Acc) return Boolean;

   --  Device I/O callbacks
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   --  Internal device init
   --  Since e1000 and e1000e are similar enough, we can use basically the same
   --  init code, just with different device IDs.
   procedure Init_Device
      (PCI_Dev : Devices.PCI.PCI_Device;
       Model   : Device_Model;
       Idx     : Natural;
       Success : out Boolean);

end Devices.PCI.E1000;
