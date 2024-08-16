--  devices-serial.ads: Serial driver specification.
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

with Lib.Synchronization;

package Devices.Serial is
   --  Initialize the serial devices.
   function Init return Boolean;

   --  Little functions for debug reading and printing.
   procedure Init_COM1;
   procedure Read_COM1 (S : out Operation_Data);
   procedure Write_COM1 (C : Character);
   procedure Write_COM1 (S : String);

private

   type COM_Root is record
      Mutex : aliased Lib.Synchronization.Binary_Semaphore;
      Port  : Unsigned_16;
      Baud  : Unsigned_32;
   end record;
   type COM_Root_Acc is access all COM_Root;

   --  COM ports, the first 2 ones are almost sure to be at that address, the
   --  rest are a bit spoty, so we must not forget to test all of them.
   COM_Ports : constant array (1 .. 8) of Unsigned_16 :=
      (16#3F8#, 16#2F8#, 16#3E8#, 16#2E8#, 16#5F8#, 16#4F8#, 16#5E8#, 16#4E8#);

   --  Offsets of several registers.
   Data_Register    : constant := 0;
   Interrupt_Enable : constant := 1;
   DLAB_0           : constant := 0;
   DLAB_1           : constant := 1;
   Interrupt_ID     : constant := 2;
   Line_Control     : constant := 3;
   Modem_Control    : constant := 4;
   Line_Status      : constant := 5;
   Modem_Status     : constant := 6;
   Scratch          : constant := 7;

   --  Default settings.
   Default_Baud : constant := 115200;

   --  COM1 is initialized statically in order for it to be shared easily
   --  with debug outputs, while sharing baud, lock, etc...
   COM1 : aliased COM_Root :=
      (Lib.Synchronization.Unlocked_Semaphore, COM_Ports (1), Default_Baud);

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;

   procedure Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean);

   function Can_Receive  (Port : Unsigned_16) return Boolean;
   function Can_Transmit (Port : Unsigned_16) return Boolean;
   procedure Set_Baud    (Port : Unsigned_16; Baud : Unsigned_32);
end Devices.Serial;
