--  scheduler.adb: Specification of the scheduler.
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

package body Scheduler is
   function Init return Boolean is
   begin
      return False;
   end Init;

   procedure Idle_Core is
   begin
      return;
   end Idle_Core;

   function Create_Kernel_Thread
      (Address  : Virtual_Address;
       Argument : Unsigned_64) return TID
   is
      pragma Unreferenced (Address);
      pragma Unreferenced (Argument);
   begin
      return TID (0);
   end Create_Kernel_Thread;

   function Create_User_Thread
      (Address   : Virtual_Address;
       Args      : Userland.Argument_Arr;
       Env       : Userland.Environment_Arr;
       Map       : Memory.Virtual.Page_Map_Acc;
       Vector    : Userland.ELF.Auxval;
       Stack_Top : Unsigned_64;
       PID       : Natural) return TID
   is
      pragma Unreferenced (Address);
      pragma Unreferenced (Args);
      pragma Unreferenced (Env);
      pragma Unreferenced (Map);
      pragma Unreferenced (Vector);
      pragma Unreferenced (Stack_Top);
      pragma Unreferenced (PID);
   begin
      return TID (0);
   end Create_User_Thread;

   function Create_User_Thread
      (State : access ISR_GPRs;
       Map   : Memory.Virtual.Page_Map_Acc;
       PID   : Natural) return TID
   is
      pragma Unreferenced (State);
      pragma Unreferenced (Map);
      pragma Unreferenced (PID);
   begin
      return TID (0);
   end Create_User_Thread;

   procedure Delete_Thread (Thread : TID) is
      pragma Unreferenced (Thread);
   begin
      return;
   end Delete_Thread;

   procedure Ban_Thread (Thread : TID; Is_Banned : Boolean) is
      pragma Unreferenced (Thread);
      pragma Unreferenced (Is_Banned);
   begin
      return;
   end Ban_Thread;

   function Get_Thread_Priority (Thread : TID) return Integer is
      pragma Unreferenced (Thread);
   begin
      return 0;
   end Get_Thread_Priority;

   procedure Set_Thread_Priority (Thread : TID; Priority : Integer) is
      pragma Unreferenced (Thread);
      pragma Unreferenced (Priority);
   begin
      return;
   end Set_Thread_Priority;

   procedure Yield is
   begin
      return;
   end Yield;

   procedure Bail is
   begin
      return;
   end Bail;
end Scheduler;
