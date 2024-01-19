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

with Arch.Hooks;
with Arch.Snippets;
with Lib.Synchronization;

package body Lib.Panic with
   Refined_State => (Panic_State => Panic_Mutex)
is
   Panic_Mutex : aliased Synchronization.Binary_Semaphore :=
      Synchronization.Unlocked_Semaphore;

   procedure Hard_Panic (Message : String) is
   begin
      --  Ensure only this core panics.
      Synchronization.Seize (Panic_Mutex);

      --  Tell the rest of the cores to go take a nap, forever.
      Arch.Hooks.Panic_SMP_Hook;

      --  Print the error and lights out.
      Messages.Put_Line (HP & "                   --:::-+*.            " & RC);
      Messages.Put_Line (HP & "  ....             =%%%%%%:++==..:+#:   " & RC);
      Messages.Put_Line (HP & "+*+++++**++=-.      +:%%%%%=+%%%:.-%%.  " & RC);
      Messages.Put_Line (HP & "--=+++=-----=**+.   .+:%%%%*-%%%:.-%%   " & RC);
      Messages.Put_Line (HP & "-=++++++----#@@=#-   -:=%%%%:%%%-.:%%:  " & RC);
      Messages.Put_Line (HP & "++++++++*@@==%%-+#=   +.%%%#:%%%-.:%%#  " & RC);
      Messages.Put_Line (HP & "+++++++--+==*%%**-#   .+*-   *%%=..#%%+ " & RC);
      Messages.Put_Line (HP & "+++++++=----====-=%    -:            .- " & RC);
      Messages.Put_Line (HP & "++++++++=---=+++++%     +               " & RC);
      Messages.Put_Line (Panic_Header & Message & RC);
      Arch.Snippets.HCF;
   end Hard_Panic;
end Lib.Panic;
