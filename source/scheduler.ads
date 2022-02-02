--  scheduler.ads: Specification of the scheduler.
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

package Scheduler is
   --  True if the scheduler is initialized.
   Is_Initialized : Boolean with Volatile;

   --  Initialize the scheduler, return true on success, false on failure.
   function Init return Boolean;

   --  Function for all cores to use when doing nothing, doubles as the
   --  function to initialize core locals.
   procedure Idle_Core;

private
   procedure Scheduler_ISR;
end Scheduler;
