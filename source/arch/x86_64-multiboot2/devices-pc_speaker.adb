--  devices-pc_speaker.adb: PC speaker driver.
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

with Arch.Snippets; use Arch.Snippets;

package body Devices.PC_Speaker is
   function Init return Boolean is
      Success : Boolean;
   begin
      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => null,
           Write       => null,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => IO_Control'Access,
           Mmap        => null,
           Poll        => null), "pcspeaker", Success);
      return Success;
   end Init;

   procedure Beep (Frequency : Unsigned_32 := 1000) is
      Divisor : constant Unsigned_32 := 1193180 / Frequency;
      Tmp     : Unsigned_8;
   begin

      --  Set the PIT to the frequency we want.
      Port_Out (16#43#, 16#B6#);
      Port_Out (16#42#, Unsigned_8 (Divisor and 16#FF#));
      Port_Out (16#42#, Unsigned_8 (Shift_Right (Divisor, 8) and 16#FF#));

      --  Play.
      Tmp := Port_In (16#61#);
      if Tmp /= (Tmp or 3) then
         Port_Out (16#61#, Tmp or 3);
      end if;

      --  Wait for a few cycles.
      Delay_Execution (300_000_000);

      --  Make it stop.
      Port_Out (16#61#, Port_In (16#61#) and 16#FC#);
   end Beep;
   ----------------------------------------------------------------------------
   procedure Delay_Execution (Cycles : Unsigned_64) is
      Next_Stop : constant Unsigned_64 := Read_Cycles + Cycles;
   begin
      while Read_Cycles < Next_Stop loop null; end loop;
   end Delay_Execution;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Request);
      Frequency : Unsigned_32 with Import, Address => Argument;
   begin
      Beep (Frequency);
      return True;
   end IO_Control;
end Devices.PC_Speaker;
