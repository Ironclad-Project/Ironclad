--  arch-pic.adb: PIC driver.
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

package body Arch.PIC with SPARK_Mode => Off is
   procedure Mask_All is
   begin
      Wrappers.Port_Out (16#A1#, 16#FF#);
      Wrappers.Port_Out (16#21#, 16#FF#);
      Flush;
   end Mask_All;

   procedure Flush is
   begin
      for I in 0 .. 15 loop
         EOI (I);
      end loop;
   end Flush;

   procedure EOI (IRQ : Natural) is
   begin
      if IRQ >= 8 then
         Wrappers.Port_Out (16#A0#, 16#20#);
      end if;
      Wrappers.Port_Out (16#20#, 16#20#);
   end EOI;
end Arch.PIC;
