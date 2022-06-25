--  arch-local.adb: Architecture-specific CPU-local storage.
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

with Arch.CPU;

package body Arch.Local is
   function Get_Current_Thread return Scheduler.TID is
   begin
      return CPU.Get_Local.Current_Thread;
   end Get_Current_Thread;

   function Get_Current_Process return Userland.Process.Process_Data_Acc is
   begin
      return CPU.Get_Local.Current_Process;
   end Get_Current_Process;

   procedure Set_Current_Thread (Thread : Scheduler.TID) is
   begin
      CPU.Get_Local.Current_Thread := Thread;
   end Set_Current_Thread;

   procedure Set_Current_Process (Proc : Userland.Process.Process_Data_Acc) is
   begin
      CPU.Get_Local.Current_Process := Proc;
   end Set_Current_Process;
end Arch.Local;
