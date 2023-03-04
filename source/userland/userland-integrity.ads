--  userland-integrity.ads: Integrity checks and backbone of the watchdog.
--  Copyright (C) 2023 streaksu
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

with Memory; use Memory;
with Userland.Process;

package Userland.Integrity with SPARK_Mode => Off is
   --  This unit configures and launches checks of everything pertaining to
   --  userland, from memory to process counts, to everything we make it to.
   --  This is only for userland, kernel integrity neednt be checked here.

   --  Which action to take in the event of a check failure.
   type Policy is (Policy_Warn, Policy_Panic);
   procedure Set_Policy (P : Policy);

   --  How much free memory to require to pass the test.
   procedure Set_Free_Memory (Amount : Memory.Size);

   --  How many processes to allow before we fail.
   procedure Set_Max_Processes_Check (Num : Natural);

   --  Actually run the battery of tests, if they fail, we will not be here
   --  to tell the story (it will panic).
   procedure Run_Checks;

private

   Failure_Policy  :      Policy := Policy_Panic;
   Min_Free_Memory : Memory.Size := 0;
   Max_Processes   :     Natural := Process.Max_Process_Count;
end Userland.Integrity;
