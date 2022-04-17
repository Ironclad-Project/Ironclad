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

with Ada.Unchecked_Deallocation;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Memory.Virtual; use Memory.Virtual;
with Memory; use Memory;
with Userland.ELF;
with Scheduler; use Scheduler;

package body Userland.Loader is
   procedure Free_File is new Ada.Unchecked_Deallocation (File, File_Acc);

   --  Virtual offsets for different kinds of programs to load.
   Program_Offset        : constant := 16#00000000#;
   Dynamic_Linker_Offset : constant := 16#40000000#;

   function Start_Program
      (FD          : File_Acc;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       StdIn_Path  : String;
       StdOut_Path : String;
       StdErr_Path : String) return Process.PID
   is
      Returned_PID : constant PID      := Process.Create_Process;
      Stdin        : constant File_Acc := Open (StdIn_Path,  Access_R);
      StdOut       : constant File_Acc := Open (StdOut_Path, Access_W);
      StdErr       : constant File_Acc := Open (StdErr_Path, Access_W);
      Discard      : Natural;
   begin
      if Returned_PID = Error_PID then
         goto Error;
      end if;
      Process.Set_Memmap (Returned_PID, Fork_Map (Kernel_Map));
      Process.Set_Current_Root (Returned_PID, FD.Root.Name);

      if not Start_Program (FD, Arguments, Environment, Returned_PID) then
         goto Error_Process;
      end if;

      if not Process.Add_File (Returned_PID, Stdin,  Discard) or else
         not Process.Add_File (Returned_PID, StdOut, Discard) or else
         not Process.Add_File (Returned_PID, StdErr, Discard)
      then
         goto Error_Process;
      end if;
      return Returned_PID;

   <<Error_Process>>
      Process.Delete_Process (Returned_PID);
   <<Error>>
      return Error_PID;
   end Start_Program;

   function Start_Program
      (FD          : File_Acc;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID) return Boolean
   is
      Loaded_ELF, LD_ELF : ELF.Parsed_ELF;
      Entrypoint   : Virtual_Address;
      LD_Path : String (1 .. 100);
      LD_File : File_Acc;
   begin
      --  Load the executable.
      Loaded_ELF := ELF.Load_ELF
         (FD,
          Process.Get_Memmap (Proc),
          Program_Offset);
      if not Loaded_ELF.Was_Loaded then
         goto Error;
      end if;

      --  Load the interpreter if it's present, and set entrypoint.
      if Loaded_ELF.Linker_Path /= null then
         --  Interpreter paths are relative, so we build an absolute one on
         --  the spot using Path, which is absolute.
         LD_Path (1) := '@';
         LD_Path (2 .. 8) := FD.Root.Name;
         LD_Path (9 .. Loaded_ELF.Linker_Path.all'Length + 8) :=
            Loaded_ELF.Linker_Path (1 .. Loaded_ELF.Linker_Path.all'Length);
         LD_File := Open
            (LD_Path (1 .. 6 + Loaded_ELF.Linker_Path.all'Length), Access_R);
         if LD_File = null then
            goto Error;
         end if;
         LD_ELF := ELF.Load_ELF
            (LD_File,
             Process.Get_Memmap (Proc), Dynamic_Linker_Offset);
         Entrypoint := To_Integer (LD_ELF.Entrypoint);
         if not LD_ELF.Was_Loaded then
            goto Error;
         end if;
         Free_File (LD_File);
      else
         Entrypoint := To_Integer (Loaded_ELF.Entrypoint);
      end if;

      --  Create the main thread and load it to the process.
      declare
         Returned_TID : constant Scheduler.TID := Scheduler.Create_User_Thread
            (Address   => Entrypoint,
             Args      => Arguments,
             Env       => Environment,
             Map       => Process.Get_Memmap (Proc),
             Vector    => Loaded_ELF.Vector,
             --  TODO: Do not hardcode stack size.
             Stack_Top => Process.Bump_Stack (Proc, 16#200000#));
      begin
         if Returned_TID = 0 then
            goto Error;
         end if;

         if not Process.Add_Thread (Proc, Returned_TID) then
            Scheduler.Delete_Thread (Returned_TID);
            goto Error;
         end if;

         Process.Set_Current_Root (Proc, FD.Root.Name);
         return True;
      end;

   <<Error>>
      return False;
   end Start_Program;
end Userland.Loader;
