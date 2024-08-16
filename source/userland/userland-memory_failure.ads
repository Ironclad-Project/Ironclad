--  userland-memoryfailure.ads: Code to manage memory failures.
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

package Userland.Memory_Failure is
   --  Policy for the handlers to follow.
   type Policy is
      (Hard_Panic, --  Not do any recovery, just panic the system, simple as.
       Soft_Kill,  --  Kill the affected processes when they step on it.
       Hard_Kill); --  Kill the processes eagerly.

   --  Get the system-wide policy.
   procedure Get_System_Policy (Pol : out Policy);

   --  Set the system-wide policy.
   procedure Set_System_Policy (Pol : Policy);
end Userland.Memory_Failure;
