--  arch-pit.adb: Programmable Interval Timer driver.
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

with Arch.Snippets;

package body Arch.PIT is
   PIT_Divisor       : constant := 1193180;
   PIT_Frequency     : constant := 1000;
   PIT_Channel0_Port : constant := 16#40#;
   PIT_Command_Port  : constant := 16#43#;

   function Init return Boolean is
      New_Divisor : constant Unsigned_32 := PIT_Divisor / PIT_Frequency;
      Low_Divisor : constant Unsigned_32 := Shift_Right (New_Divisor, 8);

      Low8  : constant Unsigned_8 := Unsigned_8 (New_Divisor and 16#FF#);
      High8 : constant Unsigned_8 := Unsigned_8 (Low_Divisor and 16#FF#);
   begin
      --  Setup the PIT.
      Arch.Snippets.Port_Out (PIT_Command_Port,  16#36#);
      Arch.Snippets.Port_Out (PIT_Channel0_Port, Low8);
      Arch.Snippets.Port_Out (PIT_Channel0_Port, High8);
      return True;
   end Init;

   function Get_Current_Count return Unsigned_16 is
      Low_8, High_8 : Unsigned_8;
   begin
      Arch.Snippets.Port_Out (PIT_Command_Port, 0);
      Low_8 := Arch.Snippets.Port_In (PIT_Channel0_Port);
      High_8 := Arch.Snippets.Port_In (PIT_Channel0_Port);
      return Unsigned_16 (Shift_Left (High_8, 8)) or Unsigned_16 (Low_8);
   end Get_Current_Count;

   procedure Set_Current_Count (Value : Unsigned_16) is
      Low_8  : constant Unsigned_16 := Value and 16#FF#;
      High_8 : constant Unsigned_16 := Shift_Right (Value, 8) and 16#FF#;
   begin
      Arch.Snippets.Port_Out (PIT_Command_Port, 16#34#);
      Arch.Snippets.Port_Out (PIT_Channel0_Port, Unsigned_8 (Low_8));
      Arch.Snippets.Port_Out (PIT_Channel0_Port, Unsigned_8 (High_8));
   end Set_Current_Count;

   procedure Sleep_1MS is
      Value : Unsigned_16;
   begin
      Set_Current_Count (Unsigned_16 (PIT_Divisor / PIT_Frequency));
      loop
         Value := Get_Current_Count;
         exit when Value = 0;
      end loop;
   end Sleep_1MS;
end Arch.PIT;
