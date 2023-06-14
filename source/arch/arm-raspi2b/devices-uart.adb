--  devices-uart.adb: UART driver.
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

with Arch.Snippets;

package body Devices.UART with SPARK_Mode => Off is
   procedure Configure is
   begin
      UART0_CR   := 0;
      GPPUD      := 0;
      GPPUDCLK0  := Shift_Left (1, 14) or Shift_Left (1, 15);
      GPPUDCLK0  := 0;
      UART0_ICR  := 16#7FF#;
      UART0_IBRD := 1;
      UART0_FBRD := 40;
      UART0_LCRH := 2#1110000#;
      UART0_IMSC := 2#11111111110#;
      UART0_CR   := 2#1100000000#;
   end Configure;

   procedure Print (Message : Character) is
   begin
      Write (Character'Pos (Message));
   end Print;

   function Init return Boolean is
      Succ : Boolean;
      Dev  : constant Resource :=
         (Data        => System.Null_Address,
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Sync        => null,
          Sync_Range  => null,
          Read        => Read'Access,
          Write       => Write'Access,
          IO_Control  => null,
          Mmap        => null,
          Munmap      => null);
   begin
      Register (Dev, "uart", Succ);
      return Succ;
   end Init;
   ----------------------------------------------------------------------------
   function Read return Unsigned_8 is
   begin
      while (UART0_FR and 16#010000#) /= 0 loop
         Arch.Snippets.Pause;
      end loop;
      return Unsigned_8 (UART0_DR and 16#FF#);
   end Read;

   procedure Write (Message : Unsigned_8) is
   begin
      while (UART0_FR and 16#100000#) /= 0 loop
         Arch.Snippets.Pause;
      end loop;
      UART0_DR := Unsigned_32 (Message);
   end Write;

   procedure Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
   begin
      for I of Data loop
         I := Read;
      end loop;
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
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
   begin
      for I of Data loop
         Write (I);
      end loop;
      Ret_Count := Data'Length;
      Success   := True;
   end Write;
end Devices.UART;
