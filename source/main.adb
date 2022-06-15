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
with Arch.Syscall;
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

procedure Main is
   Init_Arguments   : Userland.Argument_Arr (1 .. 1);
   Init_Environment : Userland.Environment_Arr (1 .. 0);

   Root_Value : Lib.Cmdline.String_Acc;
   Init_Value : Lib.Cmdline.String_Acc;
   Init_File  : File_Acc;

   Init_Stdin  : constant String :=
      (if Config.Is_Embedded then "/dev/null" else "/dev/ttydev");
   Init_Stdout : constant String :=
      (if Config.Is_Embedded then "/dev/debug" else "/dev/ttydev");
   Init_Stderr : constant String :=
      (if Config.Is_Embedded then "/dev/debug" else "/dev/ttydev");

   procedure Free_F is new Ada.Unchecked_Deallocation (File, File_Acc);
   procedure Free_S is new Ada.Unchecked_Deallocation
      (String, Lib.Cmdline.String_Acc);

   Proto   : constant Arch.Boot_Information := Arch.Get_Info;
   Cmdline : constant String := Proto.Cmdline (1 .. Proto.Cmdline_Len);
begin
   Lib.Messages.Put_Line ("Initializing VFS subsystem");
   VFS.Init;

   Lib.Messages.Put_Line ("Initializing processes");
   Userland.Process.Init;

   Lib.Messages.Put_Line ("Initializing devices");
   Devices.Init;

   Lib.Messages.Put_Line ("Mounting boot modules as ramdevs");
   for I in 1 .. Proto.RAM_Files_Len loop
      declare
         Name : String := "ramdev0";
      begin
         Name (7) := Character'Val (I + Character'Pos ('0'));
         if not VFS.Register
            (Devices.Ramdev.Init_Module (Proto.RAM_Files (I)), Name)
         then
            Lib.Panic.Hard_Panic ("Could not load a stivale2 ramdev");
         end if;
      end;
   end loop;

   Lib.Messages.Put_Line ("Fetching kernel cmdline options");
   Root_Value := Lib.Cmdline.Get_Parameter (Cmdline, "root");
   Init_Value := Lib.Cmdline.Get_Parameter (Cmdline, "init");

   Arch.Syscall.Set_Tracing
      (Lib.Cmdline.Is_Key_Present (Cmdline, "syscalltracing"));

   if Root_Value /= null then
      if not VFS.Mount (Root_Value.all, "/", VFS.FS_USTAR) then
         Lib.Panic.Soft_Panic ("Could not mount " & Root_Value.all & " /");
      end if;
      Free_S (Root_Value);
   end if;

   if Init_Value /= null then
      Lib.Messages.Put_Line ("Booting init " & Init_Value.all);
      Init_Arguments (1) := new String'(Init_Value.all);
      Init_File := Open (Init_Value.all, Access_R);
      if Init_File = null or else Userland.Loader.Start_Program
         (Init_File, Init_Arguments, Init_Environment, Init_Stdin,
          Init_Stdout, Init_Stderr) = null
      then
         Lib.Panic.Soft_Panic ("Could not start init");
      end if;
      Free_S (Init_Value);
      Free_F (Init_File);
   end if;

   Lib.Messages.Put_Line ("Bailing main thread");
   Scheduler.Bail;
end Main;
