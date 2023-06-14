--  devices-uart.ads: UART driver.
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

with System.Storage_Elements; use System.Storage_Elements;
with System;

package Devices.UART with SPARK_Mode => Off is
   --  Early initialization of PL011 for kernel output.
   procedure Configure;

   --  Print a character.
   procedure Print (Message : Character);

   --  Register the already configured device.
   function Init return Boolean;

private

   GPPUD        : Unsigned_32 with Import, Volatile;
   GPPUDCLK0    : Unsigned_32 with Import, Volatile;
   UART0_DR     : Unsigned_32 with Import, Volatile;
   UART0_RSRECR : Unsigned_32 with Import, Volatile;
   UART0_FR     : Unsigned_32 with Import, Volatile;
   UART0_ILPR   : Unsigned_32 with Import, Volatile;
   UART0_IBRD   : Unsigned_32 with Import, Volatile;
   UART0_FBRD   : Unsigned_32 with Import, Volatile;
   UART0_LCRH   : Unsigned_32 with Import, Volatile;
   UART0_CR     : Unsigned_32 with Import, Volatile;
   UART0_IFLS   : Unsigned_32 with Import, Volatile;
   UART0_IMSC   : Unsigned_32 with Import, Volatile;
   UART0_RIS    : Unsigned_32 with Import, Volatile;
   UART0_MIS    : Unsigned_32 with Import, Volatile;
   UART0_ICR    : Unsigned_32 with Import, Volatile;
   UART0_DMACR  : Unsigned_32 with Import, Volatile;
   UART0_ITCR   : Unsigned_32 with Import, Volatile;
   UART0_ITIP   : Unsigned_32 with Import, Volatile;
   UART0_ITOP   : Unsigned_32 with Import, Volatile;
   UART0_TDR    : Unsigned_32 with Import, Volatile;

   for GPPUD'Address        use To_Address (16#3F200094#);
   for GPPUDCLK0'Address    use To_Address (16#3F200098#);
   for UART0_DR'Address     use To_Address (16#3F201000#);
   for UART0_RSRECR'Address use To_Address (16#3F201004#);
   for UART0_FR'Address     use To_Address (16#3F201018#);
   for UART0_ILPR'Address   use To_Address (16#3F201020#);
   for UART0_IBRD'Address   use To_Address (16#3F201024#);
   for UART0_FBRD'Address   use To_Address (16#3F201028#);
   for UART0_LCRH'Address   use To_Address (16#3F20102C#);
   for UART0_CR'Address     use To_Address (16#3F201030#);
   for UART0_IFLS'Address   use To_Address (16#3F201034#);
   for UART0_IMSC'Address   use To_Address (16#3F201038#);
   for UART0_RIS'Address    use To_Address (16#3F20103C#);
   for UART0_MIS'Address    use To_Address (16#3F201040#);
   for UART0_ICR'Address    use To_Address (16#3F201044#);
   for UART0_DMACR'Address  use To_Address (16#3F201048#);
   for UART0_ITCR'Address   use To_Address (16#3F201080#);
   for UART0_ITIP'Address   use To_Address (16#3F201084#);
   for UART0_ITOP'Address   use To_Address (16#3F201088#);
   for UART0_TDR'Address    use To_Address (16#3F20108C#);

   function Read return Unsigned_8;
   procedure Write (Message : Unsigned_8);

   procedure Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);

   procedure Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);
end Devices.UART;
