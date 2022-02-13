--  userland.adb: Program loader.
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

with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with FS.File; use FS.File;
with Memory.Virtual; use Memory.Virtual;
with Memory; use Memory;
with Userland.ELF;

package body Userland is
   --  Virtual offsets for different kinds of programs to load.
   Program_Offset : constant := 16#00000000#;
   LD_Offset      : constant := 16#40000000#;

   function Start_User_ELF
      (Path        : String;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       StdIn       : String;
       StdOut      : String;
       StdErr      : String) return Scheduler.TID is
      Loaded_ELF, LD_ELF : ELF.Parsed_ELF;
      Entrypoint : Virtual_Address;
      User_Map   : constant access Page_Map := Fork_Map (Kernel_Map.all);
      Sched_Args : Scheduler.Arguments (1 .. 0);
      Sched_Envp : Scheduler.Environment (1 .. 0);

      --  Load an ELF Path into the user map.
      function Load (P : String; Base : Unsigned_64) return ELF.Parsed_ELF is
         FD : constant FS.File.FD    := FS.File.Open (P, FS.File.Access_R);
         Loaded_ELF : ELF.Parsed_ELF := (False, System.Null_Address, null);
      begin
         if FD = FS.File.Error_FD then
            return Loaded_ELF;
         end if;

         Loaded_ELF := ELF.Load_ELF (FD, User_Map.all, Base);
         FS.File.Close (FD);
         return Loaded_ELF;
      end Load;
   begin
      --  Create the pagemap and load the executable.
      Loaded_ELF := Load (Path, Program_Offset);

      --  Load the interpreter if it's present, and set entrypoint.
      if Loaded_ELF.Linker_Path /= null then
         LD_ELF     := Load (Path, LD_Offset);
         Entrypoint := To_Integer (LD_ELF.Entrypoint);
      else
         Entrypoint := To_Integer (Loaded_ELF.Entrypoint);
      end if;

      return Scheduler.Create_User_Thread
         (Entrypoint, Sched_Args, Sched_Envp, User_Map.all);
   end Start_User_ELF;
end Userland;
