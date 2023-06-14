--  devices-watchdog.ads: Watchdog driver.
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

package Devices.Watchdog with SPARK_Mode => Off is
   --  Register the device.
   function Init return Boolean;

private

   PM_WDOG_RESET            : constant := 16#00000000#;
   PM_PASSWORD              : constant := 16#5A000000#;
   PM_WDOG_TIME_SET         : constant := 16#000FFFFF#;
   PM_RSTC_WRCFG_CLR        : constant := 16#FFFFFFCF#;
   PM_RSTC_WRCFG_SET        : constant := 16#00000030#;
   PM_RSTC_WRCFG_FULL_RESET : constant := 16#00000030#;
   PM_RSTC_RESET            : constant := 16#00000102#;

   PM_RSTC : Unsigned_32 with Import, Volatile;
   PM_WDOG : Unsigned_32 with Import, Volatile;
   for PM_RSTC'Address use To_Address (16#3F10001C#);
   for PM_WDOG'Address use To_Address (16#3F100024#);

   function Get_Remaining_Count return Unsigned_32;
   procedure Reset_Count;
   procedure Stop_Count;

   procedure Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean;
end Devices.Watchdog;
