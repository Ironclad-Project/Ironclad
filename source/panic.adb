--  panic.adb: For when recovering is not an option!
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

with Arch.Hooks;
with Arch.Snippets;
with Config;
with Synchronization;

package body Panic with
   Refined_State => (Panic_State => Panic_Mutex)
is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   Panic_Mutex : aliased Synchronization.Binary_Semaphore :=
      Synchronization.Unlocked_Semaphore;

   procedure Hard_Panic (Message : String) is
   begin
      Panic_Common (Message);
      Arch.Snippets.HCF;
   end Hard_Panic;

   procedure Hard_Panic (Message : String; Ctx : Arch.Context.GP_Context) is
   begin
      Panic_Common (Message);

      #if ArchName = """riscv64-limine"""
         Print_Triple ("R0", "R1", "R2", Ctx.R0, Ctx.R1, Ctx.R2);
         Print_Triple ("R3", "R4", "R5", Ctx.R3, Ctx.R4, Ctx.R5);
         Print_Triple ("R6", " R7", " R8", Ctx.R6, Ctx.R7,  Ctx.R8);
         Print_Triple ("R9", "R10", "R11", Ctx.R9, Ctx.R10, Ctx.R11);
         Print_Triple ("R12", "R13", "R14", Ctx.R12, Ctx.R13, Ctx.R14);
         Print_Triple ("R15", "RSP", "CR2", Ctx.R15, 0, 0);
      #elsif ArchName = """x86_64-limine"""
         Print_Triple ("RAX", "RBX", "RCX", Ctx.RAX, Ctx.RBX, Ctx.RCX);
         Print_Triple ("RDX", "RSI", "RDI", Ctx.RDX, Ctx.RSI, Ctx.RDI);
         Print_Triple ("RBP", " R8", " R9", Ctx.RBP, Ctx.R8,  Ctx.R9);
         Print_Triple ("R10", "R11", "R12", Ctx.R10, Ctx.R11, Ctx.R12);
         Print_Triple ("R13", "R14", "R15", Ctx.R13, Ctx.R14, Ctx.R15);
         Print_Triple ("RIP", "RSP", "CR2", Ctx.RIP, Ctx.RSP,
                       Arch.Snippets.Read_CR2);
         Messages.Put_Line ("Error code: " & Ctx.Error_Code'Image);
      #end if;

      Arch.Snippets.HCF;
   end Hard_Panic;
   ----------------------------------------------------------------------------
   procedure Panic_Common (Message : String) is
   begin
      --  Common preparations.
      Arch.Snippets.Disable_Interrupts;
      Synchronization.Seize (Panic_Mutex);
      Arch.Hooks.Panic_SMP_Hook;

      --  Print something nice.
      Messages.Put_Line ("                   --:::-+*.            ");
      Messages.Put_Line ("  ....             =%%%%%%:++==..:+#:   ");
      Messages.Put_Line ("+*+++++**++=-.      +:%%%%%=+%%%:.-%%.  ");
      Messages.Put_Line ("--=+++=-----=**+.   .+:%%%%*-%%%:.-%%   ");
      Messages.Put_Line ("-=++++++----#@@=#-   -:=%%%%:%%%-.:%%:  ");
      Messages.Put_Line ("++++++++*@@==%%-+#=   +.%%%#:%%%-.:%%#  ");
      Messages.Put_Line ("+++++++--+==*%%**-#   .+*-   *%%=..#%%+ ");
      Messages.Put_Line ("+++++++=----====-=%    -:            .- ");
      Messages.Put_Line ("++++++++=---=+++++%     +               ");
      Messages.Put_Line ("");
      Messages.Put_Line ("Kernel Panic: " & Message);
      Messages.Put_Line ("");
      Messages.Put_Line ("Please reboot your computer! State will be lost");
      Messages.Put_Line ("");
      Messages.Put_Line ("Consider reporting this issue at:");
      Messages.Put_Line (Config.Bug_Site);
   end Panic_Common;

   procedure Print_Triple (N1, N2, N3 : String; V1, V2, V3 : Unsigned_64) is
   begin
      Messages.Put_Line
         (N1 & " " & V1'Image & " " &
          N2 & " " & V2'Image & " " &
          N3 & " " & V3'Image);
   end Print_Triple;
end Panic;
