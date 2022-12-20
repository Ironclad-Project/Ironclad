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
      Device : constant Resource := (
         Data              => System.Null_Address,
         Mutex             => Lib.Synchronization.Unlocked_Semaphore,
         Is_Block          => False,
         Block_Size        => 4096,
         Block_Count       => 0,
         Unique_Identifier => 0,
         Sync              => null,
         Read              => null,
         Write             => null,
         IO_Control        => IO_Control'Access,
         Mmap              => null,
         Munmap            => null
      );
   begin
      return Register (Device, "rtc");
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

   IO_Control_RTC_RD_TIME  : constant := 1;
   IO_Control_RTC_SET_TIME : constant := 2;
   function IO_Control
      (Data     : Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      pragma Unreferenced (Data);
      Temp  : constant Unsigned_8 := Get_RTC_Data (CMOS_Config);
      Is_PM : Boolean;
      Re    : RTC_Time with Import, Address => Argument;
   begin
      --  Wait for clear before we operate.
      loop
         if (Get_RTC_Data (CMOS_Update_Flag) and 16#80#) = 0 then
            exit;
         end if;
      end loop;

      case Request is
         when IO_Control_RTC_RD_TIME =>
            Re := (
               TM_Sec  => Unsigned_32 (Get_RTC_Data (CMOS_Seconds)),
               TM_Min  => Unsigned_32 (Get_RTC_Data (CMOS_Minute)),
               TM_Hour => Unsigned_32 (Get_RTC_Data (CMOS_Hour)),
               TM_MDay => Unsigned_32 (Get_RTC_Data (CMOS_Day)),
               TM_Mon  => Unsigned_32 (Get_RTC_Data (CMOS_Month)),
               TM_Year => Unsigned_32 (Get_RTC_Data (CMOS_Year)),
               others  => 0
            );

            --  Convert BCD to binary values.
            --  For hour, we must maintain the highest bit for 24h conversion.
            if (Temp and 4) = 0 then
               Re := (
                  TM_Sec  => From_BCD (Re.TM_Sec),
                  TM_Min  => From_BCD (Re.TM_Min),
                  TM_Hour => From_BCD (Re.TM_Hour and 16#7F#) or
                             (Re.TM_Hour and 16#80#),
                  TM_MDay => From_BCD (Re.TM_MDay),
                  TM_Mon  => From_BCD (Re.TM_Mon),
                  TM_Year => From_BCD (Re.TM_Year),
                  others  => <>
               );
            end if;

            --  Convert 12 hour clock to 24 hour clock if necessary.
            if ((Temp and 2) = 0) and ((Re.TM_Hour and 16#80#) /= 0) then
               Re.TM_Hour := ((Re.TM_Hour and 16#7F#) + 12) mod 24;
            end if;

            --  Calculate the full (4-digit) year.
            --  If you are using ironclad on x86_64 in 1900/2100, im sorry.
            Re.TM_Year := Re.TM_Year + 2000;
         when IO_Control_RTC_SET_TIME =>
            --  Adjust year.
            Re.TM_Year := Re.TM_Year - 2000;

            --  Convert 24 hour clock to 12 hour clock if necessary.
            if ((Temp and 2) = 0) and Re.TM_Hour > 11 then
               Is_PM      := True;
               Re.TM_Hour := 16#80# + (Re.TM_Hour - 12);
            else
               Is_PM := False;
            end if;

            --  Convert BCD to binary values and set the PM flag.
            if (Temp and 4) = 0 then
               Re := (
                  TM_Sec  => To_BCD (Re.TM_Sec),
                  TM_Min  => To_BCD (Re.TM_Min),
                  TM_Hour => To_BCD (Re.TM_Hour),
                  TM_MDay => To_BCD (Re.TM_MDay),
                  TM_Mon  => To_BCD (Re.TM_Mon),
                  TM_Year => To_BCD (Re.TM_Year),
                  others  => <>
               );
            end if;
            Re.TM_Hour := Re.TM_Hour or Shift_Left (Boolean'Pos (Is_PM), 8);

            Set_RTC_Data (CMOS_Seconds, Unsigned_8 (Re.TM_Sec));
            Set_RTC_Data (CMOS_Minute,  Unsigned_8 (Re.TM_Min));
            Set_RTC_Data (CMOS_Hour,    Unsigned_8 (Re.TM_Hour));
            Set_RTC_Data (CMOS_Day,     Unsigned_8 (Re.TM_MDay));
            Set_RTC_Data (CMOS_Month,   Unsigned_8 (Re.TM_Mon));
            Set_RTC_Data (CMOS_Year,    Unsigned_8 (Re.TM_Year));
         when others =>
            return False;
      end case;
      return True;
   end IO_Control;

   function Get_RTC_Data (Register : Unsigned_8) return Unsigned_8 is
   begin
      Arch.Wrappers.Port_Out (CMOS_Base_Port, Register);
      return Arch.Wrappers.Port_In (CMOS_Data_Port);
   end Get_RTC_Data;

   procedure Set_RTC_Data (Register, Data : Unsigned_8) is
   begin
      Arch.Wrappers.Port_Out (CMOS_Base_Port, Register);
      Arch.Wrappers.Port_Out (CMOS_Data_Port, Data);
   end Set_RTC_Data;

   function To_BCD (Num : Unsigned_32) return Unsigned_32 is
   begin
      return (Num / 10 * 16) + (Num mod 10);
   end To_BCD;

   function From_BCD (Num : Unsigned_32) return Unsigned_32 is
   begin
      return (Num and 16#F#) + (Num / 16 * 10);
   end From_BCD;
end Devices.RTC;
