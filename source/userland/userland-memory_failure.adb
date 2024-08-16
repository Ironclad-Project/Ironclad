--  userland-memoryfailure.adb: Code to manage memory failures.
--  Copyright (C) 2024 streaksu
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

package body Userland.Memory_Failure is
   procedure Get_System_Policy (Pol : out Policy) is
   begin
      Pol := Hard_Panic;
   end Get_System_Policy;

   procedure Set_System_Policy (Pol : Policy) is
   begin
      null;
   end Set_System_Policy;
end Userland.Memory_Failure;
