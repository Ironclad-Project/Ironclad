--  userland-loader.adb: Program loader.
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
with Memory.Virtual; use Memory.Virtual;
with Memory; use Memory;
with Userland.ELF;
with Scheduler; use Scheduler;

package body Userland.Loader is
   --  Virtual offsets for different kinds of programs to load.
   Program_Offset : constant := 16#00000000#;
   LD_Offset      : constant := 16#40000000#;

   function Start_User_ELF
      (Path        : String;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       StdIn       : String;
       StdOut      : String;
       StdErr      : String) return Process.PID
   is
      Loaded_ELF, LD_ELF : ELF.Parsed_ELF;
      Entrypoint   : Virtual_Address;
      Returned_PID : constant Process.PID := Process.Create_Process;
      LD_Path      : String (1 .. 100);
   begin
      --  Check if we created the PID and load memmap.
      if Returned_PID = Process.Error_PID then
         goto Error;
      end if;
      Process.Set_Memmap (Returned_PID, Fork_Map (Kernel_Map));

      --  Load the executable.
      Loaded_ELF := ELF.Open_And_Load_ELF
         (Path,
          Process.Get_Memmap (Returned_PID),
          Program_Offset);
      if not Loaded_ELF.Was_Loaded then
         goto Error;
      end if;

      --  Load the interpreter if it's present, and set entrypoint.
      if Loaded_ELF.Linker_Path /= null then
         --  Interpreter paths are relative, so we build an absolute one on
         --  the spot using Path, which is absolute.
         LD_Path (1 .. 8) := Path (Path'First .. Path'First + 7);
         LD_Path (9 .. Loaded_ELF.Linker_Path.all'Length + 8) :=
            Loaded_ELF.Linker_Path (1 .. Loaded_ELF.Linker_Path.all'Length);

         LD_ELF := ELF.Open_And_Load_ELF
            (LD_Path (1 .. 6 + Loaded_ELF.Linker_Path.all'Length),
             Process.Get_Memmap (Returned_PID), LD_Offset);
         Entrypoint := To_Integer (LD_ELF.Entrypoint);
         if not LD_ELF.Was_Loaded then
            goto Error;
         end if;
      else
         Entrypoint := To_Integer (Loaded_ELF.Entrypoint);
      end if;

      --  Create the main thread and load it to the process.
      declare
         Returned_TID : constant Scheduler.TID := Scheduler.Create_User_Thread
            (Address   => Entrypoint,
             Args      => Arguments,
             Env       => Environment,
             Map       => Process.Get_Memmap (Returned_PID),
             Vector    => Loaded_ELF.Vector,
             --  TODO: Do not hardcode stack size.
             Stack_Top => Process.Bump_Stack (Returned_PID, 16#200000#));
      begin
         if Returned_TID = 0 then
            goto Error_Process;
         end if;

         if not Process.Add_Thread (Returned_PID, Returned_TID) then
            Scheduler.Delete_Thread (Returned_TID);
            goto Error_Process;
         end if;

         Process.Set_Current_Root
            (Returned_PID,
             Path (Path'First + 1 .. Path'First + 7));
         return Returned_PID;
      end;

   <<Error_Process>>
      Process.Delete_Process (Returned_PID);
   <<Error>>
      return Error_PID;
   end Start_User_ELF;
end Userland.Loader;
