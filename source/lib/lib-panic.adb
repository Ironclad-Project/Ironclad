--  lib-panic.adb: For when recovering is not an option!
--  Copyright (C) 2024 streaksu
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
with Lib.Synchronization;

package body Lib.Panic with
   Refined_State => (Panic_State => Panic_Mutex)
is
   --  String header and ending to be added to passed message strings.
   HP           : constant String := Ada.Characters.Latin_1.ESC & "[31m";
   RC           : constant String := Ada.Characters.Latin_1.ESC & "[0m";
   Panic_Header : constant String := HP & "Panic: ";

   Panic_Mutex : aliased Synchronization.Binary_Semaphore :=
      Synchronization.Unlocked_Semaphore;

   procedure Hard_Panic (Message : String) is
   begin
      Panic_Hook;
      Print_Seal_And_Goodnight (Message);
   end Hard_Panic;

   procedure Hard_Panic (Message : String; Ctx : Arch.Context.GP_Context) is
   begin
      Panic_Hook;

      #if ArchName = """x86_64-limine"""
         Print_Triple ("RAX", "RBX", "RCX", Ctx.RAX, Ctx.RBX, Ctx.RCX);
         Print_Triple ("RDX", "RSI", "RDI", Ctx.RDX, Ctx.RSI, Ctx.RDI);
         Print_Triple ("RBP", " R8", " R9", Ctx.RBP, Ctx.R8,  Ctx.R9);
         Print_Triple ("R10", "R11", "R12", Ctx.R10, Ctx.R11, Ctx.R12);
         Print_Triple ("R13", "R14", "R15", Ctx.R13, Ctx.R14, Ctx.R15);
         Print_Triple ("RIP", "RSP", "CR2", Ctx.RIP, Ctx.RSP,
                       Arch.Snippets.Read_CR2);
         Messages.Put_Line ("Error code: " & Ctx.Error_Code'Image);
      #end if;

      Print_Seal_And_Goodnight (Message);
   end Hard_Panic;
   ----------------------------------------------------------------------------
   procedure Panic_Hook is
   begin
      Arch.Snippets.Disable_Interrupts;    --  Never be preempted outside here.
      Synchronization.Seize (Panic_Mutex); --  Ensure only this core panics.
      Arch.Hooks.Panic_SMP_Hook;           --  Tell the system to nap.
   end Panic_Hook;

   procedure Print_Seal_And_Goodnight (Message : String) is
   begin
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
   end Print_Seal_And_Goodnight;

   procedure Print_Triple (N1, N2, N3 : String; V1, V2, V3 : Unsigned_64) is
   begin
      Messages.Put_Line
         (N1 & " " & V1'Image & " " &
          N2 & " " & V2'Image & " " &
          N3 & " " & V3'Image);
   end Print_Triple;
end Lib.Panic;
