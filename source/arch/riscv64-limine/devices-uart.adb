--  devices-uart.adb:  Provides basic UART functionality for RISC-V64 systems.
--  Copyright (C) 2025 Sean C. Weeks, streaksu
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
   --  UART Register Bit Definitions
   --  Line Status Register (LSR) bits
   THR_Empty_Bit : constant Unsigned_8 := 32;  -- Bit 5 = THR Empty (THRE)
   DATA_Ready_Bit : constant Unsigned_8 := 1;  -- Bit 0 = Data Ready

   --  Line Control Register (LCR) bits
   LCR_DLAB_Bit : constant Unsigned_8 := 128; -- Bit 7 = Divisor Latch Access
   LCR_8N1 : constant Unsigned_8 := 16#03#;   -- 8 data bits, No parity, 1 stop

   --  FIFO Control Register (FCR) bits
   FCR_Enable_FIFO : constant Unsigned_8 := 1; -- Bit 0 = FIFO enable
   FCR_Clear_Rx_FIFO : constant Unsigned_8 := 2; -- Bit 1 = Clear Rx FIFO
   FCR_Clear_Tx_FIFO : constant Unsigned_8 := 4; -- Bit 2 = Clear Tx FIFO

   --  Modem Control Register (MCR) bits
   MCR_DTR : constant Unsigned_8 := 1;  -- Data Terminal Ready
   MCR_RTS : constant Unsigned_8 := 2;  -- Request to Send

   procedure Init_UART0 is
      Divisor_Low  : constant Unsigned_8 := 2;
      Divisor_High : constant Unsigned_8 := 0;
   begin
      IER := 0; --  Disable all UART interrupts
      LCR := LCR_DLAB_Bit; --  Enable DLAB to set the baud rate divisor
      THR := Divisor_Low; --  Write divisor (DLL, then DLM)
      IER := Divisor_High; --  Write divisor (DLL, then DLM)
      LCR := LCR_8N1; --  Clear DLAB, set 8N1
      FCR := FCR_Enable_FIFO or FCR_Clear_Rx_FIFO or FCR_Clear_Tx_FIFO;
      MCR := MCR_DTR or MCR_RTS;  --  Set Modem Control (DTR, RTS set)

      --  (Optional) Read LSR to clear any existing status
      declare
         Dummy : Unsigned_8 := LSR;
      begin
         null;
      end;
   end Init_UART0;

   procedure Write_UART0 (Message : Character) is
   begin
      while (LSR and THR_Empty_Bit) = 0 loop
         Arch.Snippets.Pause;
      end loop;

      THR := Unsigned_8 (Character'Pos (Message));
   end Write_UART0;

   procedure Write_UART0 (Message : String) is
   begin
      for C of Message loop
         Write_UART0 (C);
      end loop;
   end Write_UART0;

   function Read_UART0 return Unsigned_8 is
   begin
      while (LSR and DATA_Ready_Bit) = 0 loop
         Arch.Snippets.Pause;
      end loop;

      return RBR;
   end Read_UART0;

   function Remap_UART return Boolean is
      Success : Boolean;
   begin
      Arch.MMU.Map_Range
         (Map              => Arch.MMU.Kernel_Table,
          Virtual_Start    => To_Address (Base),
          Physical_Start   => To_Address (Orig),
          Length           => Storage_Count (Arch.MMU.Page_Size),
          Permissions      =>
            (Is_User_Accessible => False,
             Can_Read          => True,
             Can_Write         => True,
             Can_Execute       => False,
             Is_Global         => True),
          Success          => Success,
          Caching          => Arch.MMU.Uncacheable);
      return Success;
   end Remap_UART;
end Devices.UART;
