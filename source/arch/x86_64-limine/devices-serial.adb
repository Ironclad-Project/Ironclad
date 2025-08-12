--  devices-serial.adb: Serial driver.
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

with System.Address_To_Access_Conversions;
with Devices.TermIOs;
with Arch.Snippets;

package body Devices.Serial with SPARK_Mode => Off is
   package C1 is new System.Address_To_Access_Conversions (COM_Root);

   function Init return Boolean is
      Device_Name : String (1 .. 7) := "serial0";
      Discard     : Boolean;
      Device      : Resource;
      Data        : COM_Root_Acc;
   begin
      for I in COM_Ports'Range loop
         --  Check if the port exists by writing a value and checking.
         Arch.Snippets.Port_Out (COM_Ports (I) + Scratch, 16#55#);
         if Arch.Snippets.Port_In (COM_Ports (I) + Scratch) = 16#55# then
            --  Disable all interrupts, set baud and FIFO.
            Arch.Snippets.Port_Out (COM_Ports (I) + Interrupt_Enable, 16#00#);
            Set_Baud (COM_Ports (I), Default_Baud);
            Arch.Snippets.Port_Out (COM_Ports (I) + Interrupt_ID,  16#C7#);
            Arch.Snippets.Port_Out (COM_Ports (I) + Modem_Control, 2#11#);

            --  Add the device.
            if (I = 1) then
               Data := COM1'Access;
            else
               Data := new COM_Root'(Synchronization.Unlocked_Semaphore,
                  COM_Ports (I), Default_Baud);
            end if;
            Device_Name (7) := Character'Val (I + Character'Pos ('0'));
            Device :=
               (Data        => C1.To_Address (C1.Object_Pointer (Data)),
                ID          => [others => 0],
                Is_Block    => False,
                Block_Size  => 4096,
                Block_Count => 0,
                Sync        => null,
                Sync_Range  => null,
                Read        => Read'Access,
                Write       => Write'Access,
                IO_Control  => IO_Control'Access,
                Mmap        => null,
                Poll        => Poll'Access,
                Remove      => null);
            Register (Device, Device_Name, Discard);
         end if;
      end loop;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Init;

   procedure Init_COM1 is
   begin
      Arch.Snippets.Port_Out (COM_Ports (1) + Interrupt_Enable, 16#00#);
      Set_Baud (COM_Ports (1), Default_Baud);
      Arch.Snippets.Port_Out (COM_Ports (1) + Interrupt_ID,  16#C7#);
      Arch.Snippets.Port_Out (COM_Ports (1) + Modem_Control, 2#11#);
   end Init_COM1;

   procedure Read_COM1 (S : out Operation_Data) is
      Count : Natural;
      Succ  : Dev_Status;
   begin
      Read (COM1'Address, 0, S, Count, Succ, True);
   end Read_COM1;

   procedure Write_COM1 (C : Character) is
      Count : Natural;
      Succ  : Dev_Status;
      Data  : Operation_Data (1 .. 1) with Import, Address => C'Address;
   begin
      Write (COM1'Address, 0, Data, Count, Succ, True);
   end Write_COM1;

   procedure Write_COM1 (S : String) is
   begin
      if S'Length > 0 then
         declare
            Count : Natural;
            Succ  : Dev_Status;
            Data  : Operation_Data (1 .. S'Length)
               with Import, Address => S (S'First)'Address;
         begin
            Write (COM1'Address, 0, Data, Count, Succ, True);
         end;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Write_COM1;
   ----------------------------------------------------------------------------
   --  We will not yield instead of pausing here to avoid issues printing
   --  inside the scheduler.

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset, Is_Blocking);
      COM : COM_Root with Import, Address => Key;
   begin
      Ret_Count := 0;
      Synchronization.Seize (COM.Mutex);
      for I of Data loop
         while not Can_Receive (COM.Port) loop
            Arch.Snippets.Pause;
         end loop;
         I := Arch.Snippets.Port_In (COM.Port);
         Ret_Count := Ret_Count + 1;
      end loop;
      Synchronization.Release (COM.Mutex);
      Success := Dev_Success;
   exception
      when Constraint_Error =>
         Data      := [others => 0];
         Ret_Count := 0;
         Success   := Dev_IO_Failure;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset, Is_Blocking);
      COM : COM_Root with Import, Address => Key;
   begin
      Ret_Count := 0;
      Synchronization.Seize (COM.Mutex);
      for I of Data loop
         while not Can_Transmit (COM.Port) loop
            Arch.Snippets.Pause;
         end loop;
         Arch.Snippets.Port_Out (COM.Port, I);
         Ret_Count := Ret_Count + 1;
      end loop;
      Synchronization.Release (COM.Mutex);
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   exception
      when Constraint_Error =>
         Ret_Count := 0;
         Success   := Dev_IO_Failure;
   end Write;

   procedure IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      COM      : COM_Root          with Import, Address => Key;
      Returned : TermIOs.Main_Data with Import, Address => Argument;
   begin
      Has_Extra := False;
      Extra     := 0;

      Synchronization.Seize (COM.Mutex);
      case Request is
         when TermIOs.TCGETS =>
            Returned :=
               (Input_Modes   => <>,
                Output_Modes  => <>,
                Control_Modes => <>,
                Local_Mode    => <>,
                Special_Chars => <>,
                Input_Baud    => COM.Baud,
                Output_Baud   => COM.Baud);
            Success := True;
         when TermIOs.TCSETS | TermIOs.TCSETSW | TermIOs.TCSETSF =>
            Set_Baud (COM.Port, Returned.Output_Baud);
            COM.Baud := Returned.Output_Baud;
            Success  := True;
         when others =>
            Success := False;
      end case;
      Synchronization.Release (COM.Mutex);
   end IO_Control;

   procedure Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      COM : COM_Root with Import, Address => Data;
   begin
      Synchronization.Seize (COM.Mutex);
      Can_Write := Can_Transmit (COM.Port);
      Can_Read  := Can_Receive  (COM.Port);
      Is_Error  := False;
      Synchronization.Release (COM.Mutex);
   end Poll;
   ----------------------------------------------------------------------------
   function Can_Receive (Port : Unsigned_16) return Boolean is
   begin
      return (Arch.Snippets.Port_In (Port + Line_Status) and 2#00000001#) /= 0;
   end Can_Receive;

   function Can_Transmit (Port : Unsigned_16) return Boolean is
   begin
      return (Arch.Snippets.Port_In (Port + Line_Status) and 2#01000000#) /= 0;
   end Can_Transmit;

   procedure Set_Baud (Port : Unsigned_16; Baud : Unsigned_32) is
      New_Div, Low_Div : Unsigned_32;
   begin
      --  Enable DLAB and set the low and high parts of the divisor.
      New_Div := 115200 / Baud;
      Low_Div := Shift_Right (New_Div, 8);
      Arch.Snippets.Port_Out (Port + Line_Control, 16#80#);
      Arch.Snippets.Port_Out (Port + DLAB_0, Unsigned_8 (New_Div and 16#FF#));
      Arch.Snippets.Port_Out (Port + DLAB_1, Unsigned_8 (Low_Div and 16#FF#));
      Arch.Snippets.Port_Out (Port + Line_Control, 16#03#);
   exception
      when Constraint_Error =>
         null;
   end Set_Baud;
end Devices.Serial;
