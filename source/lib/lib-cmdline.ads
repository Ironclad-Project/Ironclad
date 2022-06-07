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

package Lib.Cmdline is
   type String_Acc is access String;

   --  Get the value of a key from a C-style cmdline, and return it.
   --  Returns null if not found.
   function Get_Parameter
      (Address : System.Address;
       Key     : String) return String_Acc;

   --  Get whether an option is present on a C-style cmdline.
   function Is_Key_Present
      (Address : System.Address;
       Key     : String) return Boolean;
end Lib.Cmdline;
