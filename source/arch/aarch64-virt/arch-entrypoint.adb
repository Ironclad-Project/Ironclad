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
with Config;

package body Arch.Entrypoint is
   procedure Bootstrap_Main (Tree : System.Address) is
   begin
      Lib.Messages.Put      (Config.Name & " " & Config.Version & " ");
      Lib.Messages.Put_Line ("booted from aarch64-virt");
      Lib.Messages.Put      ("Please report errors and issues to ");
      Lib.Messages.Put_Line (Config.Bug_Site);

      Lib.Messages.Put      ("Device tree at ");
      Lib.Messages.Put      (Tree);
      Lib.Messages.Put_Line ("");
   end Bootstrap_Main;
end Arch.Entrypoint;
