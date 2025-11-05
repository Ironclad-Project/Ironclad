--  main.adb: Freestanding kernel main function.
--  Copyright (C) 2025 streaksu
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

with Arch.Entrypoint;
with Runtime;
pragma Unreferenced (Arch.Entrypoint);
pragma Unreferenced (Runtime);

with Arch;
with Arch.ACPI;
with Devices; use Devices;
with VFS; use VFS;
with Cmdline;
with Messages;
with Panic;
with Userland.Loader;
with Userland.Process; use Userland.Process;
with Userland;
with Scheduler;
with Networking.ARP;
with Cryptography.Random;
with Config;
with Virtualization;

procedure Main is
   pragma SPARK_Mode (Off);

   package CL renames Cmdline;
   Init_Path   : Userland.String_Acc;
   Init_Args   : Userland.Argument_Arr_Acc;
   Init_Env    : Userland.Environment_Arr (1 .. 0);
   Value       : String (1 .. 64);
   Value_Len   : Natural;
   Found       : Boolean;
   Init_Dev    : Devices.Device_Handle;
   Init_FS     : VFS.FS_Handle;
   Init_Ino    : VFS.File_Inode_Number;
   Init_PID    : PID;
   Success     : VFS.FS_Status;
   Init_Stdin  : constant String := "null";
   Init_Stdout : constant String := "console";
begin
   Messages.Put_Line (Config.Name & " " & Config.Version);
   Messages.Put_Line ("Please report bugs at " & Config.Bug_Site);

   --  Initialize ACPI if present.
   if Arch.ACPI.Is_Supported then
      Arch.ACPI.Initialize (Found);
      if not Found then
         Panic.Hard_Panic ("Failed to initialize ACPI");
      end if;
   end if;

   --  Initialize several subsystems.
   Cryptography.Random.Init;
   Userland.Process.Init;
   Networking.ARP.Initialize;
   Devices.Init;
   VFS.Init;
   Virtualization.Initialize;

   --  Initialize the scheduler.
   Scheduler.Init (Found);
   if not Found then
      Panic.Hard_Panic ("Could not initialize the scheduler");
   end if;

   --  Before loading init, check if the user wants us to enable thingies.
   declare
      Cmdline : String renames Arch.Cmdline (1 .. Arch.Cmdline_Len);
   begin
      if CL.Is_Key_Present (Cmdline, CL.No_Location_ASLR) then
         Messages.Put_Line ("Disabled process ASLR");
         Userland.Process.Disable_ASLR;
      end if;

      --  Mount a root if specified.
      CL.Get_Key_Value (Cmdline, CL.Root_UUID_Key, Value, Found, Value_Len);
      if Found and Value_Len = Devices.UUID_String'Length then
         Messages.Put_Line ("Searching root " & Value (1 .. Value_Len));
         Init_Dev := Devices.Fetch_By_Part_UUID (Value (1 .. Value_Len));
         if Init_Dev /= Devices.Error_Handle then
            Devices.Fetch_Name (Init_Dev, Value, Value_Len);
         else
            Messages.Put_Line ("Failed to find " & Value (1 .. Value_Len));
         end if;
      else
         CL.Get_Key_Value (Cmdline, CL.Root_Key, Value, Found, Value_Len);
      end if;
      if Found and Value_Len /= 0 then
         Messages.Put_Line ("Mounting root " & Value (1 .. Value_Len));
         Mount (Value (1 .. Value_Len), "/", False, Relative_Update, Success);
         if Success /= FS_Success then
            Panic.Hard_Panic
               ("Failed to mount '" & Value (1 .. Value_Len) & "' (" &
                VFS.FS_Status'Image (Success) & ")");
         end if;
      end if;

      --  Fetch an init or panic.
      CL.Get_Key_Value (Cmdline, CL.Init_Key, Value, Found, Value_Len);
      if Found and Value_Len /= 0 then
         Init_Path := new String'(Value (1 .. Value_Len));
      else
         Panic.Hard_Panic ("No init was found");
      end if;

      --  Prepare arguments.
      CL.Get_Key_Value (Cmdline, CL.Init_Arg_Key, Value, Found, Value_Len);
      if Found and Value_Len /= 0 then
         declare
            Arg_Count : Natural := 0;
            First_Idx, Last_Idx, Arg_Len, List_Idx : Natural;
         begin
            for C of Value (1 .. Value_Len) loop
               if C = ' ' then
                  Arg_Count := Arg_Count + 1;
               end if;
            end loop;
            Arg_Count := Arg_Count + 1;

            Init_Args     := new Userland.Argument_Arr (1 .. 1 + Arg_Count);
            Init_Args (1) := Init_Path;
            List_Idx      := 2;
            First_Idx     := 1;
            Last_Idx      := Value (1 .. Value_Len)'First;
            while Value (1 .. Value_Len)'Last >= Last_Idx loop
               if First_Idx > Value (1 .. Value_Len)'Last then
                  goto END_PARSE;
               end if;

               Arg_Len  := 0;
               Last_Idx := First_Idx;
               while Value (1 .. Value_Len)'Last >= Last_Idx and then
                     Value (Last_Idx) /= ' '
               loop
                  Arg_Len  := Arg_Len  + 1;
                  Last_Idx := Last_Idx + 1;
               end loop;

               Init_Args (List_Idx) :=
                  new String'(Value (First_Idx .. Last_Idx - 1));
               List_Idx  := List_Idx + 1;
               First_Idx := Last_Idx + 1;
            end loop;
         <<END_PARSE>>
         end;

         Messages.Put_Line
            ("Booting init: " & Init_Path.all & " " & Value (1 .. Value_Len));
      else
         Init_Args := new Userland.Argument_Arr'(1 => Init_Path);
         Messages.Put_Line ("Booting init: " & Init_Args.all (1).all);
      end if;

      --  Mount /dev.
      Mount ("zero", "/dev/", VFS.FS_DEV, False, Relative_Update, Success);
      if Success /= FS_Success then
         Panic.Hard_Panic
            ("Failed to mount /dev (" & VFS.FS_Status'Image (Success) & ")");
      end if;

      --  Init an init or panic.
      CL.Get_Key_Value (Cmdline, CL.Init_Key, Value, Found, Value_Len);
      if Found and Value_Len /= 0 then
         Init_Args (1) := new String'(Value (1 .. Value_Len));

         Open
            (Path       => Value (1 .. Value_Len),
             Key        => Init_FS,
             Ino        => Init_Ino,
             Success    => Success,
             User       => 0,
             Want_Read  => True,
             Want_Write => False);

         if Success = VFS.FS_Success then
            Userland.Loader.Start_Program
               (Init_Args.all (1).all, Init_FS, Init_Ino, Init_Args.all,
                Init_Env, Init_Stdin, Init_Stdout, Init_Stdout, Init_PID);
            if Init_PID /= Error_PID then
               Userland.Process.Set_Identifier
                  (Init_PID, Init_Args.all (1).all);

               Open
                  (Path       => "/",
                   Key        => Init_FS,
                   Ino        => Init_Ino,
                   Success    => Success,
                   User       => 0,
                   Want_Read  => True,
                   Want_Write => False);

               Userland.Process.Set_CWD (Init_PID, Init_FS, Init_Ino);
            else
               Panic.Hard_Panic ("Could not start init");
            end if;
         else
            Panic.Hard_Panic ("Init could not be opened");
         end if;
      else
         Panic.Hard_Panic ("No init was specified");
      end if;
   end;

   --  Going idle into the scheduler.
   Scheduler.Idle_Core;
exception
   when Constraint_Error =>
      Panic.Hard_Panic ("Exception in freestanding main");
end Main;
