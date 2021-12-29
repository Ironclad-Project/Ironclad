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
   Maximum_Soft_Tries : constant := 3;
   Soft_Tries         : Natural  := 0;

   procedure Soft_Panic (Message : String; Err_Hint : Hint := Unknown_Error) is
   begin
      --  Check whether it makes sense to go on.
      if Soft_Tries >= Maximum_Soft_Tries then
         Hard_Panic (Message);
      end if;

      --  Print the error and try to recover.
      Soft_Tries := Soft_Tries + 1;
      Lib.Messages.Put ("Soft panic requested: ");
      Lib.Messages.Put (Message);

      --  TODO: Add subsystem-specific sanitizing.
      --  (Not like there is much subsystem to sanitize).
      case Err_Hint is
         when Arch_Error    => Lib.Messages.Put_Line (" (Arch error)");
         when Lib_Error     => Lib.Messages.Put_Line (" (Lib error)");
         when Memory_Error  => Lib.Messages.Put_Line (" (Memory error)");
         when Unknown_Error => Lib.Messages.Put_Line (" (Unknown error)");
      end case;
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
