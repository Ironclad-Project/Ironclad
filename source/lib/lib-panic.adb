--  lib-panic.adb: Soft and hard panic functions.
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

with Arch.Interrupts;
with Lib.Messages;

package body Lib.Panic is
   Already_Soft_Panicked : Boolean := False;

   procedure Soft_Panic (Message : String) is
   begin
      --  Check whether it makes sense to go on.
      if Already_Soft_Panicked then
         Hard_Panic (Message);
      end if;

      --  Print the error and try to recover.
      Already_Soft_Panicked := True;
      Lib.Messages.Put ("Soft panic requested: ");
      Lib.Messages.Put (Message);
   end Soft_Panic;

   procedure Hard_Panic (Message : String) is
   begin
      --  Print the error and lights out.
      Lib.Messages.Put      ("Hard panic requested: ");
      Lib.Messages.Put_Line (Message);
      Arch.Interrupts.Set_Interrupt_Flag (False);
      loop null; end loop;
   end Hard_Panic;
end Lib.Panic;
