--  lib-messages.adb: Utilities for reporting messages to the user.
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

with Arch.Debug;
with Arch.Interrupts;
with Arch.Stivale2;

package body Lib.Messages is
   use ASCII;

   New_Line : constant String := "" & LF;

   procedure Put_Line (Message : String) is
   begin
      Put (Message);
      Put (New_Line);
   end Put_Line;

   procedure Panic (Message : String) is
   begin
      --  Print the error.
      Put ("PANIC: ");
      Put (Message);
      Put (New_Line);

      --  Disable interrupts and loop forever, effectively killing the core.
      Arch.Interrupts.Set_Interrupt_Flag (False);
      loop null; end loop;
   end Panic;

   procedure Put (Message : String) is
   begin
      Arch.Debug.Print (Message);
      Arch.Stivale2.Print_Terminal (Message);
   end Put;
end Lib.Messages;
