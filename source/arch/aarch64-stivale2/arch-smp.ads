--  arch-smp.ads: SMP initialization.
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

with Arch.Stivale2;
with Userland.Process;
with Scheduler;

package Arch.SMP with SPARK_Mode => Off is
   --  Core-local data, that each core holds an own version of.
   type Core_Local;
   type Core_Local_Acc is access all Core_Local;
   type Core_Local is record
      Self            : Core_Local_Acc; --  Here for performance reasons.
      Number          : Positive;       --  Core number, 1 based.
      Current_Thread  : Scheduler.TID;
      Current_Process : Userland.Process.PID;
   end record;

   --  Core locals and the number of cores, used as an index for the former.
   type Core_Local_Arr is array (Positive range <>) of aliased Core_Local;
   type Core_Local_Arr_Acc is access Core_Local_Arr;
   Core_Count  : Positive;
   Core_Locals : Core_Local_Arr_Acc;

   --  Init the cores and BSP.
   procedure Init_Cores (SMP_Info : access Stivale2.SMP_Tag);

   --  Get the core local structure of the passed core.
   function Get_Local return not null Core_Local_Acc;

private

   procedure Init_Core (Core_Info : access Stivale2.SMP_Core)
      with Convention => C;
   procedure Init_Common (Core_Number : Positive);
end Arch.SMP;
