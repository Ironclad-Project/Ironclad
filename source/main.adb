--  main.adb: Main function of the freestanding kernel.
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
with Arch;
with Devices.Ramdev;
with Devices;
with VFS.File; use VFS.File;
with VFS;
with Lib.Cmdline; use Lib.Cmdline;
with Lib.Messages;
with Lib.Panic;
with Userland.Loader;
with Userland.Process; use Userland.Process;
with Userland;
with Scheduler;
with Lib.Runtime;
pragma Unreferenced (Lib.Runtime);
with Config;
with Userland.Syscall;

procedure Main is
   pragma SPARK_Mode (Off);

   Init_Arguments   : Userland.Argument_Arr (1 .. 1);
   Init_Environment : Userland.Environment_Arr (1 .. 0);

   Root_Value : Lib.Cmdline.String_Acc;
   Init_Value : Lib.Cmdline.String_Acc;
   Init_File  : File_Acc;

   Init_Stdin  : constant String := "/dev/null";
   Init_Stdout : constant String := "/dev/debug";
   Init_Stderr : constant String := "/dev/debug";

   procedure Free_F is new Ada.Unchecked_Deallocation (File, File_Acc);
   procedure Free_S is new Ada.Unchecked_Deallocation
      (String, Lib.Cmdline.String_Acc);

   Proto   : constant Arch.Boot_Information := Arch.Get_Info;
   Cmdline : constant String := Proto.Cmdline (1 .. Proto.Cmdline_Len);
begin
   Lib.Messages.Put_Line (Config.Name & " version " & Config.Version);
   Lib.Messages.Put_Line ("Please report bugs/issues at " & Config.Bug_Site);
   Lib.Messages.Put_Line ("Command line: '" & Cmdline & "'");

   --  Initialize several subsystems.
   Userland.Process.Init;
   Devices.Init;
   VFS.Init;

   --  Load RAM files.
   for I in 1 .. Proto.RAM_Files_Len loop
      declare
         Name : String := "ramdev0";
      begin
         Name (7) := Character'Val (I + Character'Pos ('0'));
         if not Devices.Register
            (Devices.Ramdev.Init_Module (Proto.RAM_Files (I)), Name)
         then
            Lib.Panic.Hard_Panic ("Could not load a boot ramdev");
         end if;
      end;
   end loop;

   --  Initialize the scheduler.
   if not Scheduler.Init then
      Lib.Panic.Hard_Panic ("Could not initialize the scheduler");
   end if;

   --  Fetch commandline parameters, and load the specified root and init.
   Root_Value := Lib.Cmdline.Get_Parameter (Cmdline, "root");
   Init_Value := Lib.Cmdline.Get_Parameter (Cmdline, "init");
   Userland.Syscall.Set_Tracing
      (Lib.Cmdline.Is_Key_Present (Cmdline, "syscalltracing"));

   if Root_Value /= null then
      if not VFS.Mount (Root_Value.all, "/", VFS.FS_USTAR) then
         Lib.Panic.Hard_Panic ("Could not mount root " & Root_Value.all);
      end if;
      Free_S (Root_Value);
   end if;

   if Init_Value /= null then
      Lib.Messages.Put_Line ("Booting init " & Init_Value.all);
      Init_Arguments (1) := new String'(Init_Value.all);
      Init_File := Open (Init_Value.all, Read_Only);
      if Init_File = null or else Userland.Loader.Start_Program
         (Init_File, Init_Arguments, Init_Environment, Init_Stdin,
          Init_Stdout, Init_Stderr) = null
      then
         Lib.Panic.Hard_Panic ("Could not start init");
      end if;
      Free_S (Init_Value);
      Free_F (Init_File);
   else
      Lib.Panic.Hard_Panic ("No init was specified");
   end if;

   --  Going idle into the scheduler.
   Scheduler.Idle_Core;
end Main;
