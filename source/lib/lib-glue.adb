--  lib-glue.adb: Calls generated for the compiler that we must fulfill.
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

package body Lib.Glue is
   procedure Access_Check (File : System.Address; Line : Integer) is
   begin
      Lib.Messages.Panic ("Access check failure");
   end Access_Check;

   procedure Index_Check (File : System.Address; Line : Integer) is
   begin
      Lib.Messages.Panic ("Index check failure");
   end Index_Check;

   procedure Range_Check (File : System.Address; Line : Integer) is
   begin
      Lib.Messages.Panic ("Range check failure");
   end Range_Check;

   procedure Accessib_Check (File : System.Address; Line : Integer) is
   begin
      Lib.Messages.Panic ("Accessibility check failure");
   end Accessib_Check;

   procedure Overflow_Check (File : System.Address; Line : Integer) is
   begin
      Lib.Messages.Panic ("Overflow check failure");
   end Overflow_Check;
end Lib.Glue;
