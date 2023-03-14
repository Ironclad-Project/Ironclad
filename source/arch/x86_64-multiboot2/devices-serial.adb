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

with Lib.Synchronization;
with Scheduler;
with Devices.TermIOs;
with Arch.Snippets;

package body Devices.Serial with SPARK_Mode => Off is
   --  COM ports, the first 2 ones are almost sure to be at that address, the
   --  rest are a bit spoty, so we must not forget to test all of them.
   COM_Ports : constant array (1 .. 4) of Unsigned_16 :=
      (16#3F8#, 16#2F8#, 16#3E8#, 16#2E8#);

   --  Inner COM port root data.
   type COM_Root is record
      Mutex : aliased Lib.Synchronization.Binary_Semaphore;
      Port  : Unsigned_16;
      Baud  : Unsigned_32;
   end record;
   type COM_Root_Acc is access COM_Root;

   function Init return Boolean is
      Default_Baud : constant := 115200;
   begin
      for I in COM_Ports'Range loop
         --  Check if the port exists by writting a value and checking.
         Arch.Snippets.Port_Out (COM_Ports (I) + 7, 16#55#);
         if Arch.Snippets.Port_In (COM_Ports (I) + 7) /= 16#55# then
            goto End_Port;
         end if;

         --  Disable all interrupts, set baud enable interrupts and FIFO.
         Arch.Snippets.Port_Out (COM_Ports (I) + 1, 16#00#);
         Set_Baud (COM_Ports (I), Default_Baud);
         Arch.Snippets.Port_Out (COM_Ports (I) + 2, 16#C7#);
         Arch.Snippets.Port_Out (COM_Ports (I) + 4, 16#0B#);

         --  Add the device.
         declare
            Data        : constant COM_Root_Acc := new COM_Root;
            Device_Name : String (1 .. 7)       := "serial0";
            Discard     : Boolean               := False;
            Device      : Resource;
         begin
            Device_Name (7) := Character'Val (I + Character'Pos ('0'));
            Data.all := (
               Mutex => Lib.Synchronization.Unlocked_Semaphore,
               Port  => COM_Ports (I),
               Baud  => Default_Baud
            );

            Device := (
               Data        => Data.all'Address,
               Is_Block    => False,
               Block_Size  => 4096,
               Block_Count => 0,
               Sync        => null,
               Read        => Read'Access,
               Write       => Write'Access,
               IO_Control  => IO_Control'Access,
               Mmap        => null,
               Munmap      => null
            );

            Register (Device, Device_Name, Discard);
         end;
      <<End_Port>>
      end loop;
      return True;
   end Init;

   procedure Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      Did_Seize : Boolean;
      COM       : COM_Root with Address => Key;
      pragma Unreferenced (Offset);
   begin
      loop
         Lib.Synchronization.Try_Seize (COM.Mutex, Did_Seize);
         exit when Did_Seize;
         Scheduler.Yield;
      end loop;

      for I of Data loop
         I := Fetch_Data (COM.Port);
      end loop;

      Lib.Synchronization.Release (COM.Mutex);
      Ret_Count := Data'Length;
      Success   := True;
   end Read;

   procedure Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      Did_Seize  : Boolean;
      COM        : COM_Root with Address => Key;
      pragma Unreferenced (Offset);
   begin
      loop
         Lib.Synchronization.Try_Seize (COM.Mutex, Did_Seize);
         exit when Did_Seize;
         Scheduler.Yield;
      end loop;

      for I of Data loop
         Transmit_Data (COM.Port, I);
      end loop;
      Lib.Synchronization.Release (COM.Mutex);
      Ret_Count := Data'Length;
      Success   := True;
   end Write;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      COM      : COM_Root          with Import, Address => Data;
      Returned : TermIOs.Main_Data with Import, Address => Argument;
      Success  : Boolean := False;
   begin
      Lib.Synchronization.Seize (COM.Mutex);
      case Request is
         when TermIOs.TCGETS =>
            Returned := (
               Input_Modes   => <>,
               Output_Modes  => <>,
               Control_Modes => <>,
               Local_Mode    => <>,
               Special_Chars => <>,
               Input_Baud    => COM.Baud,
               Output_Baud   => COM.Baud
            );
            Success := True;
         when TermIOs.TCSETS | TermIOs.TCSETSW | TermIOs.TCSETSF =>
            Set_Baud (COM.Port, Returned.Output_Baud);
            COM.Baud := Returned.Output_Baud;
            Success := True;
         when others =>
            null;
      end case;
      Lib.Synchronization.Release (COM.Mutex);
      return Success;
   end IO_Control;

   procedure Transmit_Data (Port : Unsigned_16; Data : Unsigned_8) is
   begin
      while not ((Arch.Snippets.Port_In (Port + 5) and 2#01000000#) /= 0) loop
         Arch.Snippets.Pause;
      end loop;
      Arch.Snippets.Port_Out (Port, Data);
   end Transmit_Data;

   function Fetch_Data (Port : Unsigned_16) return Unsigned_8 is
   begin
      while not ((Arch.Snippets.Port_In (Port + 5) and 2#00000001#) /= 0) loop
         Arch.Snippets.Pause;
      end loop;
      return Arch.Snippets.Port_In (Port);
   end Fetch_Data;

   procedure Set_Baud (Port : Unsigned_16; Baud : Unsigned_32) is
      New_Divisor : constant Unsigned_32 := 115200 / Baud;
      Low_Divisor : constant Unsigned_32 := Shift_Right (New_Divisor, 8);

      Low8  : constant Unsigned_8 := Unsigned_8 (New_Divisor and 16#FF#);
      High8 : constant Unsigned_8 := Unsigned_8 (Low_Divisor and 16#FF#);
   begin
      --  Enable DLAB and set the low and high parts of the divisor.
      Arch.Snippets.Port_Out (Port + 3, 16#80#);
      Arch.Snippets.Port_Out (Port + 0, Low8);
      Arch.Snippets.Port_Out (Port + 1, High8);
      Arch.Snippets.Port_Out (Port + 3, 16#03#);
   end Set_Baud;
end Devices.Serial;
