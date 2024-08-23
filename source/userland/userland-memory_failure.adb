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

with Lib.Panic;

package body Userland.Memory_Failure is
   procedure Get_System_Policy (Pol : out Policy) is
   begin
      Seize (Config_Mutex);
      Pol := Mem_Policy;
      Release (Config_Mutex);
   end Get_System_Policy;

   procedure Set_System_Policy (Pol : Policy) is
   begin
      Seize (Config_Mutex);
      Mem_Policy := Pol;
      Release (Config_Mutex);
   end Set_System_Policy;

   procedure Handle_Failure is
   begin
      Lib.Panic.Hard_Panic ("Hardware memory failure unhandled!");
   end Handle_Failure;
end Userland.Memory_Failure;
