--  userland-loader.ads: Specification of the program loader.
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

with Userland.Process; use Userland.Process;
with VFS;              use VFS;

package Userland.Loader is
   --  By default, the offsets of loaded programs are randomized, this
   --  behaviour can be disabled at boottime.
   procedure Disable_ASLR;

   --  Start a program from a passed file, and create a process for it with
   --  1 thread running it.
   --  The format of the file is guessed.
   --  Return the PID, or 0 if failure.
   function Start_Program
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       StdIn_Path  : String;
       StdOut_Path : String;
       StdErr_Path : String) return PID;

   --  Same as above but with an existing process instead.
   --  Returns true on success, false on failure.
   function Start_Program
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID) return Boolean;

   --  Start specifically an ELF file.
   function Start_ELF
      (FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID) return Boolean;

   --  Start specifically a shebang.
   function Start_Shebang
      (Exec_Path   : String;
       FS          : FS_Handle;
       Ino         : File_Inode_Number;
       Arguments   : Argument_Arr;
       Environment : Environment_Arr;
       Proc        : PID) return Boolean;
end Userland.Loader;
