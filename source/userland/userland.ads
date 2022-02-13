--  userland.ads: Specification of the program loader.
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

with Scheduler;

package Userland is
   --  Load a file with an absolute path and set it up for execution.
   --  Return the TID, or 0 if failure.
   type Argument_Arr    is array (Positive range <>) of access String;
   type Environment_Arr is array (Positive range <>) of access String;
   function Start_User_ELF
      (Path        : String;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       StdIn       : String;
       StdOut      : String;
       StdErr      : String) return Scheduler.TID;
end Userland;
