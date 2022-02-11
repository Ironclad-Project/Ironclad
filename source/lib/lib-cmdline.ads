--  lib-messages.ads: Library for parsing command line options.
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

with System;

package Lib.Cmdline is
   --  Get the value of a key from a C-style cmdline, and return it.
   --  Returns null if not found.
   function Get_Parameter
      (Cmdline_Addr : System.Address;
       Key          : String) return access String;
end Lib.Cmdline;
