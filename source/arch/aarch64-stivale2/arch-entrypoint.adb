--  entrypoint.adb: Specification of the main function's package.
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

with Lib.Messages;
with Lib.Panic;
with Config;
with Memory.Physical;
with Memory.Virtual;

package body Arch.Entrypoint is
   procedure Bootstrap_Main (Protocol : access Arch.Stivale2.Header) is
      --  TODO: This is a placeholder because I am lazy to read the DTB.
      --  This sucks, and is only for testing purposes, please fix this asap.
      Memory_Map : Boot_Memory_Map (1 .. 1) := (
         1 => (System'To_Address (16#45000000#), 16#15000000#, True)
      );
   begin
      Lib.Messages.Put (Config.Name & " " & Config.Version & " booted by ");
      Lib.Messages.Put (Protocol.BootloaderBrand & " ");
      Lib.Messages.Put_Line (Protocol.BootloaderVersion);
      Lib.Messages.Put ("Please report errors and issues to ");
      Lib.Messages.Put_Line (Config.Bug_Site);

      Memory.Physical.Init_Allocator (Memory_Map);
      if not Memory.Virtual.Init (Memory_Map) then
         Lib.Panic.Hard_Panic ("Could not start the VMM");
      end if;

      Lib.Panic.Hard_Panic ("End of kernel");
   end Bootstrap_Main;
end Arch.Entrypoint;
