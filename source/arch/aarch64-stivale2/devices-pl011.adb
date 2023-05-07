--  devices-pl011.adb: PL011-compatible driver.
--  Copyright (C) 2023 streaksu
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

with Devices.TermIOs;
with Arch.Snippets;
with Lib.Synchronization;

package body Devices.PL011 with SPARK_Mode => Off is
   type PL011_Info is record
      Baud : Unsigned_32;
   end record;
   type PL011_Info_Acc is access PL011_Info;

   Global_Mutex : aliased Lib.Synchronization.Binary_Semaphore;

   procedure Configure is
   begin
      Set_Baud (Default_Baud);
      Lib.Synchronization.Release (Global_Mutex);
   end Configure;

   procedure Print (Message : Character) is
   begin
      Print (Character'Pos (Message));
   end Print;

   procedure Print (Message : Unsigned_8) is
   begin
      Lib.Synchronization.Seize (Global_Mutex);
      while (PL011_Status and 16#100000#) /= 0 loop
         Arch.Snippets.Pause;
      end loop;
      PL011_Data := Unsigned_32 (Message);
      Lib.Synchronization.Release (Global_Mutex);
   end Print;

   function Init return Boolean is
      Data : constant PL011_Info_Acc := new PL011_Info'(Baud => Default_Baud);
      Succ : Boolean;
      Dev  : constant Resource :=
         (Data        => Data.all'Address,
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Sync        => null,
          Sync_Range  => null,
          Read        => null,
          Write       => Write'Access,
          IO_Control  => IO_Control'Access,
          Mmap        => null,
          Munmap      => null);
   begin
      Register (Dev, "pl011", Succ);
      return Succ;
   end Init;
   ----------------------------------------------------------------------------
   procedure Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
   begin
      for I of Data loop
         Print (I);
      end loop;
      Ret_Count := Data'Length;
      Success   := True;
   end Write;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      Info     : PL011_Info        with Import, Address => Data;
      Returned : TermIOs.Main_Data with Import, Address => Argument;
      Success  : Boolean := True;
   begin
      case Request is
         when TermIOs.TCGETS =>
            Returned :=
               (Input_Modes   => <>,
                Output_Modes  => <>,
                Control_Modes => <>,
                Local_Mode    => <>,
                Special_Chars => <>,
                Input_Baud    => Info.Baud,
                Output_Baud   => Info.Baud);
         when TermIOs.TCSETS | TermIOs.TCSETSW | TermIOs.TCSETSF =>
            Set_Baud (Returned.Output_Baud);
            Info.Baud := Returned.Output_Baud;
         when others =>
            Success := False;
      end case;
      return Success;
   end IO_Control;

   procedure Set_Baud (Baud : Unsigned_32) is
      Clock  : constant Unsigned_64 := 24000000;
      A_Baud : constant Unsigned_64 := Unsigned_64 (16 * Baud);
      I_Part : constant Unsigned_64 := Clock / A_Baud;
      F_Part : constant Unsigned_64 := (((Clock * 1000) /
         A_Baud - (I_Part * 1000)) * 64 + 500) / 1000;
   begin
      Lib.Synchronization.Seize (Global_Mutex);
      PL011_I_Baud := Unsigned_32 (I_Part and 16#FFFFFFFF#);
      PL011_F_Baud := Unsigned_32 (F_Part and 16#FFFFFFFF#);
      Lib.Synchronization.Release (Global_Mutex);
   end Set_Baud;
end Devices.PL011;
