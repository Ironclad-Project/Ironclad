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

with Ada.Characters.Latin_1;
with Arch.Hooks;
with Arch.Snippets;
with Lib.Messages;
with Lib.Synchronization;

package body Lib.Panic with SPARK_Mode => Off is
   Already_Soft_Panicked : Boolean := False;
   Panic_Mutex           : aliased Synchronization.Binary_Semaphore;

   Soft_Panic_Color : constant String := Ada.Characters.Latin_1.ESC & "[35m";
   Hard_Panic_Color : constant String := Ada.Characters.Latin_1.ESC & "[31m";
   Reset_Color      : constant String := Ada.Characters.Latin_1.ESC & "[0m";

   procedure Soft_Panic (Message : String) is
   begin
      --  Check whether it makes sense to go on.
      if Already_Soft_Panicked then
         Hard_Panic (Message);
      end if;

      --  Print the error and try to recover.
      Already_Soft_Panicked := True;
      Lib.Messages.Put      (Soft_Panic_Color & "Soft panic: " & Reset_Color);
      Lib.Messages.Put_Line (Message);
   end Soft_Panic;

   procedure Hard_Panic (Message : String) is
   begin
      --  Ensure only this core panics.
      Synchronization.Seize (Panic_Mutex);

      --  Tell the rest of the cores to go take a nap, forever.
      Arch.Hooks.Panic_SMP_Hook;

      --  Print the error and lights out.
      Lib.Messages.Put_Line ("                   --:::-+*.            ");
      Lib.Messages.Put_Line ("  ....             =%%%%%%:++==..:+#:   ");
      Lib.Messages.Put_Line ("+*+++++**++=-.      +:%%%%%=+%%%:.-%%.  ");
      Lib.Messages.Put_Line ("--=+++=-----=**+.   .+:%%%%*-%%%:.-%%   ");
      Lib.Messages.Put_Line ("-=++++++----#@@=#-   -:=%%%%:%%%-.:%%:  ");
      Lib.Messages.Put_Line ("++++++++*@@==%%-+#=   +.%%%#:%%%-.:%%#  ");
      Lib.Messages.Put_Line ("+++++++--+==*%%**-#   .+*-   *%%=..#%%+ ");
      Lib.Messages.Put_Line ("+++++++=----====-=%    -:            .- ");
      Lib.Messages.Put_Line ("++++++++=---=+++++%     +               ");
      Lib.Messages.Put      (Hard_Panic_Color & "Hard panic: " & Reset_Color);
      Lib.Messages.Put_Line (Message);
      Arch.Snippets.HCF;
   end Hard_Panic;
end Lib.Panic;
