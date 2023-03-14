--  userland-integrity.adb: Integrity checks and backbone of the watchdog.
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

with Lib.Messages;
with Lib.Panic;
with Memory.Physical;

package body Userland.Integrity is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Set_Policy (P : Policy) is
   begin
      Failure_Policy := P;
   end Set_Policy;

   procedure Set_Free_Memory (Amount : Size) is
   begin
      Min_Free_Memory := Amount;
   end Set_Free_Memory;

   procedure Set_Max_Processes_Check (Num : Natural) is
   begin
      Max_Processes := Num;
   end Set_Max_Processes_Check;

   procedure Run_Checks is
      Failure_Message : constant String := "Integrity check failed!";
      Memory_Stats    : Memory.Physical.Statistics;
      Proc_Count      : Natural;
   begin
      Memory.Physical.Get_Statistics (Memory_Stats);
      if Memory_Stats.Free < Min_Free_Memory then
         goto Failure;
      end if;

      Proc_Count := Process.Get_Process_Count;
      if Proc_Count >= Max_Processes then
         goto Failure;
      end if;

      return;

   <<Failure>>
      case Failure_Policy is
         when Policy_Warn  => Lib.Messages.Warn (Failure_Message);
         when Policy_Panic => Lib.Panic.Hard_Panic (Failure_Message);
      end case;
   end Run_Checks;
end Userland.Integrity;
