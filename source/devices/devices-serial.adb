--  devices-serial.adb: Serial driver.
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

with Arch.Wrappers;
with Lib.Synchronization;

package body Devices.Serial is
   --  COM ports, the first 2 ones are almost sure to be at that address, the
   --  rest are a bit spoty, so we must not forget to test all of them.
   COM_Ports : constant array (1 .. 4) of Unsigned_16 :=
      (16#3F8#, 16#2F8#, 16#3E8#, 16#2E8#);

   --  Inner COM port root data.
   type COM_Root is record
      Mutex : aliased Lib.Synchronization.Binary_Semaphore;
      Port  : Unsigned_16;
   end record;
   type COM_Root_Acc is access COM_Root;

   function Init return Boolean is
   begin
      for I in COM_Ports'Range loop
         --  Check if the drive exists by writting a value and checking.
         Arch.Wrappers.Port_Out (COM_Ports (I) + 7, 16#55#);
         if Arch.Wrappers.Port_In (COM_Ports (I) + 7) /= 16#55# then
            goto End_Port;
         end if;

         --  Disable all interrupts and set DLAB.
         Arch.Wrappers.Port_Out (COM_Ports (I) + 1, 16#00#);
         Arch.Wrappers.Port_Out (COM_Ports (I) + 3, 16#80#);

         --  Set divisor to low 1 hi 0 (115200 baud).
         Arch.Wrappers.Port_Out (COM_Ports (I) + 0, 16#01#);
         Arch.Wrappers.Port_Out (COM_Ports (I) + 1, 16#00#);

         --  Enable FIFO and interrupts.
         Arch.Wrappers.Port_Out (COM_Ports (I) + 3, 16#03#);
         Arch.Wrappers.Port_Out (COM_Ports (I) + 2, 16#C7#);
         Arch.Wrappers.Port_Out (COM_Ports (I) + 4, 16#0B#);

         --  Add the device.
         declare
            Data        : COM_Root_Acc := new COM_Root;
            Device_Name : Root_Name    := "serial0";
            Discard     : Boolean      := False;
         begin
            Data.Port := COM_Ports (I);
            Device_Name (7) := Character'Val (I + Character'Pos ('0'));
            Discard := Register_Root ((
               Name     => Device_Name,
               Data     => Data.all'Address,
               Init     => null,
               Unload   => null,
               Sync     => null,
               Create   => null,
               Open     => null,
               Close    => null,
               Read     => Serial_Read'Access,
               Write    => Serial_Write'Access,
               Get_Size => null
            ));
            Lib.Synchronization.Release (Data.Mutex'Access);
         end;
      <<End_Port>>
      end loop;
      return True;
   end Init;

   function Serial_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : System.Address;
       Count  : Positive;
       Desto  : System.Address) return Natural is
      COM    : COM_Root with Address => Data;
      Result : array (1 .. Count) of Unsigned_8 with Address => Desto;
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
   begin
      Lib.Synchronization.Seize (COM.Mutex'Access);
      for I of Result loop
         while not Is_Data_Received (COM.Port) loop null; end loop;
         I := Arch.Wrappers.Port_In (COM.Port);
      end loop;
      Lib.Synchronization.Release (COM.Mutex'Access);
      return Count;
   end Serial_Read;

   function Serial_Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : System.Address;
       Count    : Positive;
       To_Write : System.Address) return Natural is
      COM        : COM_Root with Address => Data;
      Write_Data : array (1 .. Count) of Unsigned_8 with Address => To_Write;
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
   begin
      Lib.Synchronization.Seize (COM.Mutex'Access);
      for I of Write_Data loop
         while not Is_Transmitter_Empty (COM.Port) loop null; end loop;
         Arch.Wrappers.Port_Out (COM.Port, I);
      end loop;
      Lib.Synchronization.Release (COM.Mutex'Access);
      return Count;
   end Serial_Write;

   function Is_Transmitter_Empty (Port : Unsigned_16) return Boolean is
   begin
      return (Arch.Wrappers.Port_In (Port + 5) and 2#01000000#) /= 0;
   end Is_Transmitter_Empty;

   function Is_Data_Received (Port : Unsigned_16) return Boolean is
   begin
      return (Arch.Wrappers.Port_In (Port + 5) and 2#00000001#) /= 0;
   end Is_Data_Received;
end Devices.Serial;
