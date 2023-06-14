--  devices-watchdog.adb: Watchdog driver.
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

package body Devices.Watchdog with SPARK_Mode => Off is
   function Init return Boolean is
      Succ : Boolean;
      Dev  : constant Resource :=
         (Data        => System.Null_Address,
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
      Register (Dev, "watchdog", Succ);
      return Succ;
   end Init;
   ----------------------------------------------------------------------------
   function Get_Remaining_Count return Unsigned_32 is
   begin
      return PM_WDOG and PM_WDOG_TIME_SET;
   end Get_Remaining_Count;

   procedure Reset_Count is
   begin
      PM_WDOG := PM_PASSWORD or (3000000 and PM_WDOG_TIME_SET);
      PM_RSTC := PM_PASSWORD or (PM_RSTC and PM_RSTC_WRCFG_CLR) or
                 PM_RSTC_WRCFG_FULL_RESET;
   end Reset_Count;

   procedure Stop_Count is
   begin
      PM_RSTC := PM_PASSWORD or PM_RSTC_RESET;
   end Stop_Count;

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
      Reset_Count;
      Ret_Count := Data'Length;
      Success   := True;
   end Write;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      pragma Unreferenced (Data);
      WDOG_GET_REMAINING : constant := 1;
      WDOG_ENABLE        : constant := 2;
      WDOG_DISABLE       : constant := 3;

      Arg : Unsigned_32 with Import, Address => Argument;
   begin
      case Request is
         when WDOG_GET_REMAINING => Arg := Get_Remaining_Count;
         when WDOG_ENABLE        => Reset_Count;
         when WDOG_DISABLE       => Stop_Count;
         when others             => return False;
      end case;
      return True;
   end IO_Control;

end Devices.Watchdog;
