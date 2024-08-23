--  userland-oom_failure.ads: Code to manage OOM failures.
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

with Lib.Synchronization; use Lib.Synchronization;

package Userland.OOM_Failure is
   --  Set whether process killing during OOM failures is enabled or not.
   --  @param Enabled True if enabled, False if disabled.
   procedure Get_Killing_Config (Enabled : out Boolean);

   --  Set whether process killing during OOM failures is enabled or not.
   --  @param Enabled True to enable, False to disable.
   procedure Configure_Killing (Enabled : Boolean);

   --  To be called by the allocator to handle OOM conditions.
   procedure Handle_Failure;

private

   Config_Mutex       : aliased Binary_Semaphore := Unlocked_Semaphore;
   Is_Killing_Allowed :                  Boolean := True;
end Userland.OOM_Failure;
