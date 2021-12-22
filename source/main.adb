--  main.adb: Main function and its closest utilities.
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

with System.Address_To_Access_Conversions;
with Arch.GDT;
with Arch.IDT;
with Arch.Stivale2;
with Lib.Messages;
with Config;

procedure Main (Protocol : access Arch.Stivale2.Header) is
   package Convert is new System.Address_To_Access_Conversions
     (Arch.Stivale2.TerminalTag);
   Term : constant access Arch.Stivale2.TerminalTag := Convert.To_Pointer
     (Arch.Stivale2.Get_Tag (Protocol, Arch.Stivale2.TerminalID));
begin
   Arch.Stivale2.Init_Terminal (Term);
   Lib.Messages.Put      (Config.Package_Name);
   Lib.Messages.Put      (" ");
   Lib.Messages.Put      (Config.Package_Version);
   Lib.Messages.Put      (" booted by ");
   Lib.Messages.Put      (Protocol.BootloaderBrand);
   Lib.Messages.Put      (" ");
   Lib.Messages.Put_Line (Protocol.BootloaderVersion);
   Lib.Messages.Put      ("Please report errors and issues to ");
   Lib.Messages.Put_Line (Config.Package_BugReport);

   Arch.GDT.Init;
   Arch.IDT.Init;
   Lib.Messages.Panic ("End of kernel");
end Main;
