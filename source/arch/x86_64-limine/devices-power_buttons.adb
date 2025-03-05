--  devices-power_buttons.adb: Power button virtual devices.
--  Copyright (C) 2025 streaksu
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

package body Devices.Power_Buttons is
   --  Event variables.
   Power_Button_Triggered : Boolean := False;
   Sleep_Button_Triggered : Boolean := False;

   function Init return Boolean is
      Success : Boolean;
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
           IO_Control  => null,
           Mmap        => null,
           Poll        => Poll_Power_Button'Access,
           Remove      => null), "pwrbutton", Success);
      if not Success then
         return False;
      end if;

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
           IO_Control  => null,
           Mmap        => null,
           Poll        => Poll_Sleep_Button'Access,
           Remove      => null), "sleepbutton", Success);
      return Success;
   end Init;

   procedure Trigger_Power_Button is
   begin
      Power_Button_Triggered := True;
   end Trigger_Power_Button;

   procedure Trigger_Sleep_Button is
   begin
      Sleep_Button_Triggered := True;
   end Trigger_Sleep_Button;
   ----------------------------------------------------------------------------
   procedure Poll_Power_Button
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Data);
   begin
      Can_Read  := Power_Button_Triggered;
      Can_Write := False;
      Is_Error  := False;
      Power_Button_Triggered := False;
   end Poll_Power_Button;

   procedure Poll_Sleep_Button
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Data);
   begin
      Can_Read  := Sleep_Button_Triggered;
      Can_Write := False;
      Is_Error  := False;
      Sleep_Button_Triggered := False;
   end Poll_Sleep_Button;
end Devices.Power_Buttons;
