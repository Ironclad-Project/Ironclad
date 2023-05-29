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

with Ada.Characters.Latin_1;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Memory.Virtual;
with Memory; use Memory;
with Userland.ELF;
with Scheduler; use Scheduler;
with Userland.Memory_Locations;
with Lib.Alignment;
with Cryptography.Random;
with Devices;

package body Userland.Loader with SPARK_Mode => Off is
   function Start_Program
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       StdIn_Path  : String;
       StdOut_Path : String;
       StdErr_Path : String) return PID
   is
      Returned_PID : constant PID := Process.Create_Process;
      Discard      : Natural;
      User_Stdin, User_StdOut, User_StdErr : File_Description_Acc;
   begin
      if Returned_PID = Error_PID then
         goto Error;
      end if;
      Process.Set_Common_Map (Returned_PID, Memory.Virtual.New_Map);
      if not Start_Program (Exec_Path, FS, Ino, Arguments, Environment,
                            Returned_PID)
      then
         goto Error_Process;
      end if;

      User_Stdin := new File_Description'(
         Children_Count  => 0,
         Description     => Description_Device,
         Inner_Dev_Read  => True,
         Inner_Dev_Write => False,
         Inner_Dev_Pos   => 0,
         Inner_Dev       => Devices.Fetch (StdIn_Path)
      );
      User_StdOut := new File_Description'(
         Children_Count  => 0,
         Description     => Description_Device,
         Inner_Dev_Read  => False,
         Inner_Dev_Write => True,
         Inner_Dev_Pos   => 0,
         Inner_Dev       => Devices.Fetch (StdOut_Path)
      );
      User_StdErr := new File_Description'(
         Children_Count  => 0,
         Description     => Description_Device,
         Inner_Dev_Read  => False,
         Inner_Dev_Write => True,
         Inner_Dev_Pos   => 0,
         Inner_Dev       => Devices.Fetch (StdErr_Path)
      );

      if not Process.Add_File (Returned_PID, User_Stdin,  Discard) or else
         not Process.Add_File (Returned_PID, User_StdOut, Discard) or else
         not Process.Add_File (Returned_PID, User_StdErr, Discard)
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
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID) return Boolean
   is
   begin
      return Start_ELF     (FS, Ino, Arguments, Environment, Proc) or else
             Start_Shebang (Exec_Path, FS, Ino, Arguments, Environment, Proc);
   end Start_Program;

   function Start_ELF
      (FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID) return Boolean
   is
      package Aln is new Lib.Alignment (Unsigned_64);

      Loaded_ELF, LD_ELF : ELF.Parsed_ELF;
      Entrypoint   : Virtual_Address;
      LD_Slide : Unsigned_64;
      LD_Path  : String (1 .. 100);
      LD_FS    : FS_Handle;
      LD_Ino   : File_Inode_Number;
      Success  : FS_Status;
   begin
      --  Load the executable.
      Loaded_ELF := ELF.Load_ELF (FS, Ino, Process.Get_Common_Map (Proc),
         Memory_Locations.Program_Offset);
      if not Loaded_ELF.Was_Loaded then
         goto Error;
      end if;

      if Loaded_ELF.Linker_Path /= null then
         --  Interpreter paths are relative, so we build an absolute one on
         --  the spot using Path, which is absolute.
         LD_Path (9 .. Loaded_ELF.Linker_Path.all'Length + 8) :=
            Loaded_ELF.Linker_Path (1 .. Loaded_ELF.Linker_Path.all'Length);
         Open (LD_Path (9 .. 7 + Loaded_ELF.Linker_Path.all'Length), LD_FS,
               LD_Ino, Success, 0);
         if Success /= VFS.FS_Success then
            goto Error;
         end if;
         LD_Slide := Cryptography.Random.Get_Integer
            (Memory_Locations.LD_Offset_Min,
             Memory_Locations.LD_Offset_Max);
         LD_Slide := Aln.Align_Up (LD_Slide, Memory.Virtual.Page_Size);
         LD_ELF := ELF.Load_ELF (LD_FS, LD_Ino, Process.Get_Common_Map (Proc),
                                 LD_Slide);
         Entrypoint := To_Integer (LD_ELF.Entrypoint);
         if not LD_ELF.Was_Loaded then
            goto Error;
         end if;
         Close (LD_FS, LD_Ino);
      else
         Entrypoint := To_Integer (Loaded_ELF.Entrypoint);
      end if;

      declare
         Returned_TID : constant Scheduler.TID := Scheduler.Create_User_Thread
            (Address    => Entrypoint,
             Args       => Arguments,
             Env        => Environment,
             Map        => Process.Get_Common_Map (Proc),
             Vector     => Loaded_ELF.Vector,
             Stack_Top  => Process.Get_Stack_Base (Proc),
             PID        => Process.Convert (Proc),
             Exec_Stack => Loaded_ELF.Exec_Stack);
      begin
         --  TODO: Do not hardcode stack size.
         Process.Set_Stack_Base (Proc, Process.Get_Stack_Base (Proc) +
                                 16#200000#);

         if Returned_TID = 0 then
            goto Error;
         end if;

         if not Process.Add_Thread (Proc, Returned_TID) then
            Scheduler.Delete_Thread (Returned_TID);
            goto Error;
         end if;
         return True;
      end;

   <<Error>>
      return False;
   end Start_ELF;

   function Start_Shebang
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID) return Boolean
   is
      Path_Len  :     Natural := 0;
      Arg_Len   :     Natural := 0;
      Pos       : Unsigned_64 := 0;
      Path      : String (1 .. 100);
      Path_Data : Devices.Operation_Data (1 .. 100)
         with Import, Address => Path'Address;
      Arg       : String (1 .. 100);
      Char      : Character;
      Char_Data : Devices.Operation_Data (1 .. 1)
         with Import, Address => Char'Address;
      Char_Len  : Natural;
      Success   : VFS.FS_Status;
   begin
      Read (FS, Ino, 0, Path_Data (1 .. 2), Path_Len, Success, 0);
      if Success /= VFS.FS_Success or Path_Len /= 2 or Path (1 .. 2) /= "#!"
      then
         return False;
      end if;
      Pos := Pos + Unsigned_64 (Path_Len);

      --  Format of a shebang: #!path [arg]newline
      Path_Len := 0;
      loop
         Read (FS, Ino, Pos, Char_Data, Char_Len, Success, 0);
         Pos := Pos + Unsigned_64 (Char_Len);
         if Success /= VFS.FS_Success or Char_Len /= 1 then
            return False;
         end if;
         case Char is
            when ' '                       => exit;
            when Ada.Characters.Latin_1.LF => goto Return_Shebang;
            when others => Path_Len := Path_Len + 1; Path (Path_Len) := Char;
         end case;
      end loop;
      loop
         Read (FS, Ino, Pos, Char_Data, Char_Len, Success, 0);
         Pos := Pos + Unsigned_64 (Char_Len);
         if Success /= VFS.FS_Success or Char_Len /= 1 then
            return False;
         end if;
         case Char is
            when Ada.Characters.Latin_1.LF | ' ' => exit;
            when others => Arg_Len := Arg_Len + 1; Arg (Arg_Len) := Char;
         end case;
      end loop;

   <<Return_Shebang>>
      declare
         Arg_Diff   : constant Natural := (if Arg_Len = 0 then 1 else 2);
         New_Args   : Argument_Arr (1 .. Arguments'Length + Arg_Diff);
         I          : Positive := 1;
         Banged_FS  : FS_Handle;
         Banged_Ino : File_Inode_Number;
      begin
         New_Args (I) := new String'(Path (1 .. Path_Len));
         I := I + 1;
         if Arg_Len /= 0 then
            New_Args (I) := new String'(Arg (1 .. Arg_Len));
            I := I + 1;
         end if;
         New_Args (I .. New_Args'Length) := Arguments;
         New_Args (I) := new String'(Exec_Path);
         Open (Path (1 .. Path_Len), Banged_FS, Banged_Ino, Success, 0);
         if Success /= FS_Success then
            return False;
         end if;
         return Start_Program (
            Exec_Path,
            Banged_FS,
            Banged_Ino,
            New_Args,
            Environment,
            Proc
         );
      end;
   end Start_Shebang;
end Userland.Loader;
