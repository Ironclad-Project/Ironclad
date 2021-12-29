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

with Interfaces; use Interfaces;
with System.Address_To_Access_Conversions;
with Arch.GDT;
with Arch.IDT;
with Arch.Stivale2;
with Lib.Messages;
with Memory.Physical;
with Config;

procedure Main (Protocol : access Arch.Stivale2.Header) is
   package ST renames Arch.Stivale2;

   package C1 is new System.Address_To_Access_Conversions (ST.Terminal_Tag);
   package C2 is new System.Address_To_Access_Conversions (ST.Memmap_Tag);

   Term : constant access ST.Terminal_Tag :=
     C1.To_Pointer (ST.Get_Tag (Protocol, ST.TerminalID));
   Memmap : constant access ST.Memmap_Tag :=
     C2.To_Pointer (ST.Get_Tag (Protocol, ST.MemmapID));

   Total_Memory, Free_Memory, Used_Memory : Memory.Size;
begin
   ST.Init_Terminal (Term);
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

   Lib.Messages.Put_Line ("Initializing allocators");
   Memory.Physical.Init_Allocator (Memmap);
   Memory.Physical.Get_Info (Total_Memory, Free_Memory, Used_Memory);
   Lib.Messages.Put      (Unsigned_64 (Used_Memory));
   Lib.Messages.Put      (" used + ");
   Lib.Messages.Put      (Unsigned_64 (Free_Memory));
   Lib.Messages.Put      (" free / ");
   Lib.Messages.Put      (Unsigned_64 (Total_Memory));
   Lib.Messages.Put_Line (" memory used");
   for E of Memmap.Entries loop
      Lib.Messages.Put      ('[');
      Lib.Messages.Put      (E.Base, True);
      Lib.Messages.Put      ('+');
      Lib.Messages.Put      (E.Length, True, True);
      Lib.Messages.Put      ("] ");
      Lib.Messages.Put      (Integer (E.EntryType), False, True);
      Lib.Messages.Put_Line ("");
   end loop;

   Lib.Messages.Panic ("End of kernel");
end Main;
