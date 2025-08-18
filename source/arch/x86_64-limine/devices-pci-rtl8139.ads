--  devices-rtl8139.ads: RTL8139 driver.
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

package Devices.PCI.RTL8139 with SPARK_Mode => Off is
   procedure Init (Success : out Boolean);

private
   --  There are 4 hardware transmit descriptors, 0 to 3
   type Transmit_Desc_Idx_Type is mod 4;
   --  We preallocate the 4 transmit buffers since they must be in low memory
   type Transmit_Buffers_Arr is array (Transmit_Desc_Idx_Type)
      of Memory.Virtual_Address;
   --  In order to write to the transmit buffer, we need to define a suitable
   --  array
   subtype Transmit_Buffer is Operation_Data (1 .. 16#700#);

   --  The size we set for the receive buffer.
   RECEIVE_BUFFER_LOGICAL_SIZE : constant := (1024 * 16) + 16;
   --  The size we allocate for the receive buffer. We enable WRAP, so we need
   --  to allocate 1500 more bytes.
   RECEIVE_BUFFER_SIZE : constant := RECEIVE_BUFFER_LOGICAL_SIZE + 1500;

   --  In order to access the receive buffer, we also need to define a suitable
   --  array
   subtype Receive_Buffer is Operation_Data (1 .. RECEIVE_BUFFER_SIZE);

   type Controller_Data is record
      Receive_Buffer_Start : Memory.Virtual_Address;
      IO_Base              : Unsigned_16;
      Interrupt            : Unsigned_8;
      Transmit_Desc_Idx    : Transmit_Desc_Idx_Type;
      Transmit_Buffers     : Transmit_Buffers_Arr;
   end record;
   type Controller_Data_Acc is access all Controller_Data;

   REG_ID     : constant := 16#00#;
   REG_RBSTART  : constant := 16#30#;
   REG_CMD      : constant := 16#37#;
   REG_CAPR     : constant := 16#38#;
   REG_CBR      : constant := 16#3A#;
   REG_IMR      : constant := 16#3C#;
   REG_ISR      : constant := 16#3E#;
   REG_RCR      : constant := 16#44#;
   REG_CONFIG_1 : constant := 16#52#;

   type Transmit_Descriptor_Registers_Arr is array (Transmit_Desc_Idx_Type)
      of Unsigned_8;
   Transmit_Descriptor_Status_Registers :
      constant Transmit_Descriptor_Registers_Arr :=
         [16#10#, 16#14#, 16#18#, 16#1C#];
   Transmit_Descriptor_Address_Registers :
      constant Transmit_Descriptor_Registers_Arr :=
         [16#20#, 16#24#, 16#28#, 16#2C#];

   procedure Generic_RTL8139_Interrupt_Handler (Num : Integer);

   function Install_Interrupt_Handler (CD : Controller_Data_Acc)
      return Boolean;

   procedure Handle_Interrupt (CD : Controller_Data_Acc);

   function Set_Receive_Buffer (CD : Controller_Data_Acc) return Boolean;

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

   procedure Put_IO_8
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8;
       Value   : Unsigned_8);

   procedure Put_IO_16
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8;
       Value   : Unsigned_16);

   procedure Put_IO_32
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8;
       Value  : Unsigned_32);

   function Get_IO_8
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8) return Unsigned_8;

   function Get_IO_16
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8) return Unsigned_16;

   function Get_IO_32
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8) return Unsigned_32;
end Devices.PCI.RTL8139;
