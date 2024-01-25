--  arch-rtc.adb: RTC driver.
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

package body Arch.RTC is
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

   procedure Get_RTC_Date (Epoch_Seconds : out Unsigned_64) is
      Temp : constant Unsigned_8 := Get_RTC_Data (CMOS_Config);
      Seconds : Unsigned_32;
      Minutes : Unsigned_32;
      Hours   : Unsigned_32;
      Days    : Unsigned_32;
      Months  : Unsigned_32;
      Years   : Unsigned_32;
      Curr    : Unsigned_64;
      J_1970  : Unsigned_64;
   begin
      --  Wait for clear before we operate.
      loop
         if (Get_RTC_Data (CMOS_Update_Flag) and 16#80#) = 0 then
            exit;
         end if;
      end loop;

      --  Get date.
      Seconds := Unsigned_32 (Get_RTC_Data (CMOS_Seconds));
      Minutes := Unsigned_32 (Get_RTC_Data (CMOS_Minute));
      Hours   := Unsigned_32 (Get_RTC_Data (CMOS_Hour));
      Days    := Unsigned_32 (Get_RTC_Data (CMOS_Day));
      Months  := Unsigned_32 (Get_RTC_Data (CMOS_Month));
      Years   := Unsigned_32 (Get_RTC_Data (CMOS_Year));

      --  Convert BCD to binary values.
      --  For hour, we must maintain the highest bit for 24h conversion.
      if (Temp and 4) = 0 then
         Seconds := From_BCD (Seconds);
         Minutes := From_BCD (Minutes);
         Hours   := From_BCD (Hours and 16#7F#) or (Hours and 16#80#);
         Days    := From_BCD (Days);
         Months  := From_BCD (Months);
         Years   := From_BCD (Years);
      end if;

      --  Convert 12 hour clock to 24 hour clock if necessary.
      if ((Temp and 2) = 0) and ((Hours and 16#80#) /= 0) then
         Hours := ((Hours and 16#7F#) + 12) mod 24;
      end if;

      --  Calculate the full (4-digit) year.
      --  If you are using ironclad on x86_64 in 1900/2100, im sorry.
      Years := Years + 2000;

      Curr   := Get_Julian_Day (Integer_64 (Days), Integer_64 (Months),
                                Integer_64 (Years));
      J_1970 := Get_Julian_Day (1, 1, 1970);

      Epoch_Seconds := ((Curr - J_1970) * (60 * 60 * 24)) +
                       (Unsigned_64 (Hours) * 3600)       +
                       (Unsigned_64 (Minutes) * 60)       +
                       Unsigned_64 (Seconds);
   end Get_RTC_Date;

   procedure Set_RTC_Date (Epoch_Seconds : Unsigned_64) is
   begin
      --  TODO: Implement.
      null;
   end Set_RTC_Date;
   ----------------------------------------------------------------------------
   function Get_Julian_Day
      (Days, Months, Years : Integer_64) return Unsigned_64 is
   begin
      return Unsigned_64
         ((1461 * (Years + 4800 + (Months - 14)        / 12))  / 4  +
          (367  * (Months - 2 - 12 * ((Months - 14)    / 12))) / 12 -
          (3    * ((Years + 4900 + (Months - 14) / 12) / 100)) / 4 +
          Days - 32075);
   end Get_Julian_Day;

   function Get_RTC_Data (Register : Unsigned_8) return Unsigned_8 is
   begin
      Arch.Snippets.Port_Out (CMOS_Base_Port, Register);
      return Arch.Snippets.Port_In (CMOS_Data_Port);
   end Get_RTC_Data;

   procedure Set_RTC_Data (Register, Data : Unsigned_8) is
   begin
      Arch.Snippets.Port_Out (CMOS_Base_Port, Register);
      Arch.Snippets.Port_Out (CMOS_Data_Port, Data);
   end Set_RTC_Data;

   function To_BCD (Num : Unsigned_32) return Unsigned_32 is
   begin
      return (Num / 10 * 16) + (Num mod 10);
   end To_BCD;

   function From_BCD (Num : Unsigned_32) return Unsigned_32 is
   begin
      return (Num and 16#F#) + (Num / 16 * 10);
   end From_BCD;
end Arch.RTC;
