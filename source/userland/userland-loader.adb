--  userland-loader.adb: Program loader.
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

with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Memory; use Memory;
with Userland.ELF; use Userland.ELF;
with Scheduler; use Scheduler;
with Userland.Memory_Locations;
with Alignment;
with Cryptography.Random;
with Devices;
with Userland.MAC; use Userland.MAC;

package body Userland.Loader is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   Do_ASLR : Boolean := True;

   procedure Disable_ASLR is
   begin
      Do_ASLR := False;
   end Disable_ASLR;

   procedure Start_Program
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       StdIn_Path  : String;
       StdOut_Path : String;
       StdErr_Path : String;
       Result      : out PID)
   is
      Discard      : Natural;
      Success      : VFS.FS_Status;
      Success1, Success2, Success3         : Boolean;
      User_Stdin, User_StdOut, User_StdErr : File_Description_Acc;
      Table : Memory.MMU.Page_Table_Acc;
   begin
      Process.Create_Process (Error_PID, Result);
      if Result = Error_PID then
         goto Error;
      end if;

      Memory.MMU.Create_Table (Table);
      Process.Set_Common_Map (Result, Table);

      Start_Program (Exec_Path, FS, Ino, Arguments, Environment, Result,
                     Success1);
      if not Success1 then
         goto Error_Process;
      end if;

      User_Stdin := new File_Description'
         (Children_Count    => 0,
          Is_Blocking       => True,
          Description       => Description_Inode,
          Inner_Is_Locked   => False,
          Inner_Ino_Read    => True,
          Inner_Ino_Write   => False,
          Inner_Ino_Pos     => 0,
          Inner_Ino_FS      => VFS.Error_Handle,
          Inner_Ino         => 0);
      User_StdOut := new File_Description'
         (Children_Count    => 0,
          Is_Blocking       => True,
          Description       => Description_Inode,
          Inner_Is_Locked   => False,
          Inner_Ino_Read    => False,
          Inner_Ino_Write   => True,
          Inner_Ino_Pos     => 0,
          Inner_Ino_FS      => VFS.Error_Handle,
          Inner_Ino         => 0);
      User_StdErr := new File_Description'
         (Children_Count    => 0,
          Is_Blocking       => True,
          Description       => Description_Inode,
          Inner_Is_Locked   => False,
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
         Result := Error_PID;
         return;
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
         Result := Error_PID;
         return;
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
         Result := Error_PID;
         return;
      end if;

      Process.Add_File (Result, User_Stdin,  Discard, Success1);
      Process.Add_File (Result, User_StdOut, Discard, Success2);
      Process.Add_File (Result, User_StdErr, Discard, Success3);

      if Success1 and Success2 and Success3 then
         return;
      end if;

   <<Error_Process>>
      Process.Delete_Process (Result);
   <<Error>>
      Result := Error_PID;
   end Start_Program;

   procedure Start_Program
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID;
       Success     : out Boolean)
   is
   begin
      Start_ELF (FS, Ino, Arguments, Environment, Proc, Success);
      if Success then
         return;
      end if;
      Start_Shebang (Exec_Path, FS, Ino, Arguments, Environment, Proc,
                     Success);
   end Start_Program;

   procedure Start_ELF
      (FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID;
       Success     : out Boolean)
   is
      package Aln is new Alignment (Unsigned_64);

      Loaded_ELF, LD_ELF : ELF.Parsed_ELF;
      Entrypoint   : Virtual_Address;
      Base_Slide   : Unsigned_64;
      LD_Slide     : Unsigned_64;
      Stack_Size   : Unsigned_64;
      LD_Path      : String (1 .. 100);
      LD_FS        : FS_Handle;
      LD_Ino       : File_Inode_Number;
      Success2     : FS_Status;
      Returned_TID : Scheduler.TID;
      Table        : Memory.MMU.Page_Table_Acc;
   begin
      --  Calculate the binary slides.
      if Do_ASLR then
         Cryptography.Random.Get_Integer
            (Memory_Locations.Offset_Min,
             Memory_Locations.LD_Offset_Max,
             Base_Slide);
         Cryptography.Random.Get_Integer
            (Memory_Locations.LD_Offset_Min,
             Memory_Locations.LD_Offset_Max,
             LD_Slide);
         Base_Slide := Aln.Align_Down (Base_Slide, Memory.MMU.Page_Size);
         LD_Slide   := Aln.Align_Down (LD_Slide,   Memory.MMU.Page_Size);
      else
         Base_Slide := Memory_Locations.Offset_Min;
         LD_Slide   := Memory_Locations.LD_Offset_Min;
      end if;

      --  Load the executable.
      Process.Get_Common_Map (Proc, Table);
      ELF.Load_ELF (FS, Ino, Table, Base_Slide, Loaded_ELF);
      if not Loaded_ELF.Was_Loaded then
         Success := False;
         return;
      end if;

      if Loaded_ELF.Linker_Path /= null then
         --  Interpreter paths are relative, so we build an absolute one on
         --  the spot using Path, which is absolute.
         LD_Path (9 .. Loaded_ELF.Linker_Path.all'Length + 8) :=
            Loaded_ELF.Linker_Path (1 .. Loaded_ELF.Linker_Path.all'Length);
         Open (LD_Path (9 .. 7 + Loaded_ELF.Linker_Path.all'Length), LD_FS,
               LD_Ino, Success2, 0, True, False);
         if Success2 /= VFS.FS_Success then
            Success := False;
            return;
         end if;

         ELF.Load_ELF (LD_FS, LD_Ino, Table, LD_Slide, LD_ELF);
         Entrypoint := To_Integer (LD_ELF.Entrypoint);
         if not LD_ELF.Was_Loaded then
            Success := False;
            return;
         end if;
      else
         Entrypoint := To_Integer (Loaded_ELF.Entrypoint);
      end if;

      Stack_Size := Get_Limit (Proc, Stack_Size_Limit).Soft_Limit;

      Scheduler.Create_User_Thread
         (Address    => Entrypoint,
          Args       => Arguments,
          Env        => Environment,
          Map        => Table,
          Vector     => Loaded_ELF.Vector,
          Pol        => Scheduler.Policy_Other,
          Stack_Size => Stack_Size,
          PID        => Process.Convert (Proc),
          New_TID    => Returned_TID);
      if Returned_TID = Error_TID then
         Success := False;
         return;
      end if;

      Process.Add_Thread (Proc, Returned_TID, Success);
      if not Success then
         Scheduler.Delete_Thread (Returned_TID);
         return;
      end if;
   end Start_ELF;

   procedure Start_Shebang
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID;
       Success     : out Boolean)
   is
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Acc);

      Path_Len  : Natural;
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
      Success2   : VFS.FS_Status;
      Banged_FS  : FS_Handle;
      Banged_Ino : File_Inode_Number;
      Path_Acc   : String_Acc;
      Arg_Acc    : String_Acc;
   begin
      Read (FS, Ino, 0, Path_Data (1 .. 2), Path_Len, True, Success2);
      if Success2 /= VFS.FS_Success or Path_Len /= 2 or Path (1 .. 2) /= "#!"
      then
         Success := False;
         return;
      end if;
      Pos := Pos + Unsigned_64 (Path_Len);

      --  Format of a shebang: #![maybe spaces]path [arg]newline
      Path_Len := 0;
      loop
         Read (FS, Ino, Pos, Char_Data, Char_Len, True, Success2);
         if Success2 /= VFS.FS_Success or Char_Len /= 1 then
            Success := False;
            return;
         end if;
         case Char is
            when ' '    => Pos := Pos + 1;
            when others => exit;
         end case;
      end loop;
      loop
         Read (FS, Ino, Pos, Char_Data, Char_Len, True, Success2);
         Pos := Pos + Unsigned_64 (Char_Len);
         if Success2 /= VFS.FS_Success or Char_Len /= 1 then
            Success := False;
            return;
         end if;
         case Char is
            when ' '                       => exit;
            when Ada.Characters.Latin_1.LF => goto Return_Shebang;
            when others => Path_Len := Path_Len + 1; Path (Path_Len) := Char;
         end case;
      end loop;
      loop
         Read (FS, Ino, Pos, Char_Data, Char_Len, True, Success2);
         Pos := Pos + Unsigned_64 (Char_Len);
         if Success2 /= VFS.FS_Success or Char_Len /= 1 then
            Success := False;
            return;
         end if;
         case Char is
            when Ada.Characters.Latin_1.LF | ' ' => exit;
            when others => Arg_Len := Arg_Len + 1; Arg (Arg_Len) := Char;
         end case;
      end loop;

   <<Return_Shebang>>
      Open (Path (1 .. Path_Len), Banged_FS, Banged_Ino, Success2, 0, True,
         False);
      if Success2 /= FS_Success then
         Success := False;
         return;
      end if;

      Path_Acc := new String'(Path (1 .. Path_Len));
      Arg_Acc  := new String'(Arg  (1 .. Arg_Len));

      --  Some applications, like python, will rely on us passing the original
      --  executable path instead of whatever the user invoked with, so we must
      --  overwrite the first argument with the raw original path.
      --  This relies on the first arg being the path to the program.
      declare
         Touched_Arguments : Argument_Arr (Arguments'First .. Arguments'Last);
      begin
         for I in Touched_Arguments'Range loop
            Touched_Arguments (I) := new String'(Arguments (I).all);
         end loop;
         if Touched_Arguments'Length /= 0 then
            Free (Touched_Arguments (Arguments'First));
            Touched_Arguments (Arguments'First) := new String'(Exec_Path);
         end if;

         if Arg_Len /= 0 then
            Start_Program
               (Exec_Path   => Exec_Path,
                FS          => Banged_FS,
                Ino         => Banged_Ino,
                Arguments   => [Path_Acc, Arg_Acc] & Touched_Arguments,
                Environment => Environment,
                Proc        => Proc,
                Success     => Success);
         else
            Start_Program
               (Exec_Path   => Exec_Path,
                FS          => Banged_FS,
                Ino         => Banged_Ino,
                Arguments   => (Path_Acc) & Touched_Arguments,
                Environment => Environment,
                Proc        => Proc,
                Success     => Success);
         end if;

         for I in Touched_Arguments'Range loop
            Free (Touched_Arguments (I));
         end loop;
      end;

      Free (Path_Acc);
      Free (Arg_Acc);
   end Start_Shebang;
end Userland.Loader;
