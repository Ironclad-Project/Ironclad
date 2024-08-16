--  devices-rtc.ads: RTC driver.
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

with Interfaces; use Interfaces;

package Arch.RTC is
   procedure Get_RTC_Date (Epoch_Seconds : out Unsigned_64);
   procedure Set_RTC_Date (Epoch_Seconds : Unsigned_64);

private

   function Get_Julian_Day
      (Days, Months, Years : Integer_64) return Unsigned_64;
   function Get_RTC_Data (Register : Unsigned_8) return Unsigned_8;
   procedure Set_RTC_Data (Register, Data : Unsigned_8);
   function To_BCD (Num : Unsigned_32) return Unsigned_32;
   function From_BCD (Num : Unsigned_32) return Unsigned_32;
end Arch.RTC;
