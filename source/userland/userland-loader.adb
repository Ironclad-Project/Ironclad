--  userland-loader.adb: Program loader.
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

with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.MMU;
with Memory; use Memory;
with Userland.ELF; use Userland.ELF;
with Scheduler; use Scheduler;
with Userland.Memory_Locations;
with Lib.Alignment;
with Cryptography.Random;
with Devices;
with Userland.MAC; use Userland.MAC;

package body Userland.Loader is
   Do_ASLR : Boolean := True;

   procedure Disable_ASLR is
   begin
      Do_ASLR := False;
   end Disable_ASLR;

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
      Returned_PID : PID;
      Discard      : Natural;
      Success      : VFS.FS_Status;
      Success1, Success2, Success3         : Boolean;
      User_Stdin, User_StdOut, User_StdErr : File_Description_Acc;
   begin
      Process.Create_Process (Error_PID, Returned_PID);
      if Returned_PID = Error_PID then
         goto Error;
      end if;
      Process.Set_Common_Map
         (Returned_PID,
          Arch.MMU.Fork_Table (Arch.MMU.Kernel_Table));
      if not Start_Program (Exec_Path, FS, Ino, Arguments, Environment,
                            Returned_PID)
      then
         goto Error_Process;
      end if;

      User_Stdin := new File_Description'
         (Children_Count    => 0,
          Description       => Description_Inode,
          Inner_Is_Locked   => False,
          Inner_Is_Blocking => True,
          Inner_Ino_Read    => True,
          Inner_Ino_Write   => False,
          Inner_Ino_Pos     => 0,
          Inner_Ino_FS      => VFS.Error_Handle,
          Inner_Ino         => 0);
      User_StdOut := new File_Description'
         (Children_Count    => 0,
          Description       => Description_Inode,
          Inner_Is_Locked   => False,
          Inner_Is_Blocking => True,
          Inner_Ino_Read    => False,
          Inner_Ino_Write   => True,
          Inner_Ino_Pos     => 0,
          Inner_Ino_FS      => VFS.Error_Handle,
          Inner_Ino         => 0);
      User_StdErr := new File_Description'
         (Children_Count    => 0,
          Description       => Description_Inode,
          Inner_Is_Locked   => False,
          Inner_Is_Blocking => True,
          Inner_Ino_Read    => False,
          Inner_Ino_Write   => True,
          Inner_Ino_Pos     => 0,
          Inner_Ino_FS      => VFS.Error_Handle,
          Inner_Ino         => 0);

      VFS.Open
         (Path       => "/dev/" & StdIn_Path,
          Key        => User_Stdin.Inner_Ino_FS,
          Ino        => User_Stdin.Inner_Ino,
          Success    => Success,
          User       => 0,
          Want_Read  => True,
          Want_Write => False);
      if Success /= FS_Success then
         return Error_PID;
      end if;
      VFS.Open
         (Path       => "/dev/" & StdOut_Path,
          Key        => User_StdOut.Inner_Ino_FS,
          Ino        => User_StdOut.Inner_Ino,
          Success    => Success,
          User       => 0,
          Want_Read  => False,
          Want_Write => True);
      if Success /= FS_Success then
         return Error_PID;
      end if;
      VFS.Open
         (Path       => "/dev/" & StdErr_Path,
          Key        => User_StdErr.Inner_Ino_FS,
          Ino        => User_StdErr.Inner_Ino,
          Success    => Success,
          User       => 0,
          Want_Read  => False,
          Want_Write => True);
      if Success /= FS_Success then
         return Error_PID;
      end if;

      Process.Add_File (Returned_PID, User_Stdin,  Discard, Success1);
      Process.Add_File (Returned_PID, User_StdOut, Discard, Success2);
      Process.Add_File (Returned_PID, User_StdErr, Discard, Success3);

      if Success1 and Success2 and Success3 then
         return Returned_PID;
      end if;

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
      Success2 : Boolean;
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
               LD_Ino, Success, 0, True, False);
         if Success /= VFS.FS_Success then
            goto Error;
         end if;
         if Do_ASLR then
            Cryptography.Random.Get_Integer
               (Memory_Locations.LD_Offset_Min,
                Memory_Locations.LD_Offset_Max,
                LD_Slide);
            LD_Slide := Aln.Align_Up (LD_Slide, Arch.MMU.Page_Size);
         else
            LD_Slide := Memory_Locations.LD_Offset_Min;
         end if;
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
             Cluster    => Scheduler.Convert (1),
             Stack_Size => Unsigned_64 (Get_Limit (Proc, Stack_Size_Limit)),
             PID        => Process.Convert (Proc));
      begin
         if Returned_TID = Error_TID then
            goto Error;
         end if;

         Process.Add_Thread (Proc, Returned_TID, Success2);
         if not Success2 then
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
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Acc);

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
      Returned  : Boolean;
      Banged_FS  : FS_Handle;
      Banged_Ino : File_Inode_Number;
      Path_Acc   : String_Acc;
      Arg_Acc    : String_Acc;
   begin
      Read (FS, Ino, 0, Path_Data (1 .. 2), Path_Len, True, Success);
      if Success /= VFS.FS_Success or Path_Len /= 2 or Path (1 .. 2) /= "#!"
      then
         return False;
      end if;
      Pos := Pos + Unsigned_64 (Path_Len);

      --  Format of a shebang: #![maybe spaces]path [arg]newline
      Path_Len := 0;
      loop
         Read (FS, Ino, Pos, Char_Data, Char_Len, True, Success);
         if Success /= VFS.FS_Success or Char_Len /= 1 then
            return False;
         end if;
         case Char is
            when ' '    => Pos := Pos + 1;
            when others => exit;
         end case;
      end loop;
      loop
         Read (FS, Ino, Pos, Char_Data, Char_Len, True, Success);
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
         Read (FS, Ino, Pos, Char_Data, Char_Len, True, Success);
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
      Open (Path (1 .. Path_Len), Banged_FS, Banged_Ino, Success, 0, True,
         False);
      if Success /= FS_Success then
         return False;
      end if;

      Path_Acc := new String'(Path (1 .. Path_Len));
      Arg_Acc  := new String'(Arg  (1 .. Arg_Len));

      if Arg_Len /= 0 then
         Returned := Start_Program
            (Exec_Path   => Exec_Path,
             FS          => Banged_FS,
             Ino         => Banged_Ino,
             Arguments   => (Path_Acc, Arg_Acc) & Arguments,
             Environment => Environment,
             Proc        => Proc);
      else
         Returned := Start_Program
            (Exec_Path   => Exec_Path,
             FS          => Banged_FS,
             Ino         => Banged_Ino,
             Arguments   => (Path_Acc) & Arguments,
             Environment => Environment,
             Proc        => Proc);
      end if;

      Free (Path_Acc);
      Free (Arg_Acc);
      return Returned;
   end Start_Shebang;
end Userland.Loader;
