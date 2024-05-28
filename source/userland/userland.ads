--  userland.ads: Specification of userland library.
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

package Userland is
   --  Arguments passed to a program when created.
   type String_Acc is access String;
   type Argument_Arr     is array (Positive range <>) of String_Acc;
   type Environment_Arr  is array (Positive range <>) of String_Acc;
   type Argument_Arr_Acc is access Argument_Arr;
end Userland;
