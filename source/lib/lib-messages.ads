--  lib-messages.ads: Specification of the messages package.
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

package Lib.Messages is
   --  Non formatting general purpose printing function.
   procedure Print (Message : String);

   --  Panic function of the kernel, prints the message and then halts.
   procedure Panic (Message : String);

private
   procedure Basic_Print (Message : String);
end Lib.Messages;
