--  devices-rtc.adb: RTC driver.
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

package body Devices.RTC with SPARK_Mode => Off is
   CMOS_Base_Port   : constant := 16#70#;
   CMOS_Data_Port   : constant := 16#71#;

   CMOS_Update_Flag : constant := 16#0A#;
   CMOS_Seconds     : constant := 16#00#;
   CMOS_Minute      : constant := 16#02#;
   CMOS_Hour        : constant := 16#04#;
   CMOS_Day         : constant := 16#07#;
   CMOS_Month       : constant := 16#08#;
   CMOS_Year        : constant := 16#09#;
   CMOS_Config      : constant := 16#0B#;

   function Init return Boolean is
      Stat : constant VFS.File_Stat := (
         Unique_Identifier => 0,
         Type_Of_File      => VFS.File_Character_Device,
         Mode              => 8#660#,
         Hard_Link_Count   => 1,
         Byte_Size         => 0,
         IO_Block_Size     => 4096,
         IO_Block_Count    => 0
      );
      Device : constant VFS.Resource := (
         Data       => System.Null_Address,
         Mutex      => Lib.Synchronization.Unlocked_Semaphore,
         Stat       => Stat,
         Sync       => null,
         Read       => null,
         Write      => null,
         IO_Control => IO_Control'Access,
         Mmap       => null,
         Munmap     => null
      );
   begin
      return VFS.Register (Device, "rtc");
   end Init;

   type RTC_Time is record
      TM_Sec   : Unsigned_32;
      TM_Min   : Unsigned_32;
      TM_Hour  : Unsigned_32;
      TM_MDay  : Unsigned_32;
      TM_Mon   : Unsigned_32;
      TM_Year  : Unsigned_32;
      TM_WDay  : Unsigned_32;
      TM_YDay  : Unsigned_32;
      TM_IsDST : Unsigned_32;
   end record;

   IO_Control_RTC_RD_TIME : constant := 1;
   function IO_Control
      (Data     : VFS.Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      pragma Unreferenced (Data);
      Temp : constant Unsigned_8 := Get_RTC_Data (CMOS_Config);
      Re   : RTC_Time with Import, Address => Argument;
   begin
      case Request is
         when IO_Control_RTC_RD_TIME =>
            --  Check we are not updating.
            loop
               if (Get_RTC_Data (CMOS_Update_Flag) and 16#80#) = 0 then
                  exit;
               end if;
            end loop;

            Re := (
               TM_Sec   => Unsigned_32 (Get_RTC_Data (CMOS_Seconds)),
               TM_Min   => Unsigned_32 (Get_RTC_Data (CMOS_Minute)),
               TM_Hour  => Unsigned_32 (Get_RTC_Data (CMOS_Hour)),
               TM_MDay  => Unsigned_32 (Get_RTC_Data (CMOS_Day)),
               TM_Mon   => Unsigned_32 (Get_RTC_Data (CMOS_Month)),
               TM_Year  => Unsigned_32 (Get_RTC_Data (CMOS_Year)),
               TM_WDay  => 0,
               TM_YDay  => 0,
               TM_IsDST => 0
            );

            --  Convert BCD to binary values.
            if (Temp and 4) = 0 then
               Re.TM_Sec  := (Re.TM_Sec  and 16#F#) + (Re.TM_Sec  / 16 * 10);
               Re.TM_Min  := (Re.TM_Min  and 16#F#) + (Re.TM_Min  / 16 * 10);
               Re.TM_Hour := ((Re.TM_Hour and 16#F#) +
               ((Re.TM_Hour and 16#70#) / 16 * 10)) or (Re.TM_Hour and 16#80#);
               Re.TM_MDay := (Re.TM_MDay and 16#F#) + (Re.TM_MDay / 16 * 10);
               Re.TM_Mon  := (Re.TM_Mon  and 16#F#) + (Re.TM_Mon  / 16 * 10);
               Re.TM_Year := (Re.TM_Year and 16#F#) + (Re.TM_Year / 16 * 10);
            end if;

            --  Convert 12 hour clock to 24 hour clock if necessary.
            if ((Temp and 2) = 0) and ((Re.TM_Hour and 16#80#) /= 0) then
               Re.TM_Hour := ((Re.TM_Hour and 16#7F#) + 12) mod 24;
            end if;

            --  Calculate the full (4-digit) year.
            --  If you are using ironclad on x86_64 in 2100, im sorry.
            Re.TM_Year := Re.TM_Year + (20 * 100);
            return True;
         when others =>
            return False;
      end case;
   end IO_Control;

   function Get_RTC_Data (Register : Unsigned_8) return Unsigned_8 is
   begin
      Arch.Wrappers.Port_Out (CMOS_Base_Port, Register);
      return Arch.Wrappers.Port_In (CMOS_Data_Port);
   end Get_RTC_Data;
end Devices.RTC;
