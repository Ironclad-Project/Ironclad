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

with Arch.Clocks;
with Arch.Snippets; use Arch.Snippets;

package body Devices.PC_Speaker with SPARK_Mode => Off is
   procedure Init (Success : out Boolean) is
   begin
      Register
         ((Data        => System.Null_Address,
           ID          => [others => 0],
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => null,
           Write       => null,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => IO_Control'Access,
           Mmap        => null,
           Poll        => null,
           Remove      => null), "pcspeaker", Success);
   end Init;

   procedure Beep (Frequency : Unsigned_32 := 1000) is
      MS_In_NS : constant := 1_000_000;
      Divisor  : Unsigned_32;
      Tmp      : Unsigned_8;
   begin
      --  Set the PIT to the frequency we want.
      Divisor := 1193180 / Frequency;
      Port_Out (16#43#, 16#B6#);
      Port_Out (16#42#, Unsigned_8 (Divisor and 16#FF#));
      Port_Out (16#42#, Unsigned_8 (Shift_Right (Divisor, 8) and 16#FF#));

      --  Play.
      Tmp := Port_In (16#61#);
      if Tmp /= (Tmp or 3) then
         Port_Out (16#61#, Tmp or 3);
      end if;

      Arch.Clocks.Busy_Monotonic_Sleep (333 * MS_In_NS);

      --  Make it stop.
      Port_Out (16#61#, Port_In (16#61#) and 16#FC#);
   exception
      when Constraint_Error =>
         null;
   end Beep;
   ----------------------------------------------------------------------------
   procedure IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Request);
      Frequency : Unsigned_32 with Import, Address => Argument;
   begin
      Beep (Frequency);
      Has_Extra := False;
      Extra     := 0;
      Success   := True;
   end IO_Control;
end Devices.PC_Speaker;
