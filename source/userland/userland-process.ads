--  userland-process.ads: Specification of the process registry and handler.
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

with VFS.File; use VFS.File;
with VFS;
with Memory.Virtual;
with Scheduler; use Scheduler;
with Interfaces; use Interfaces;
with Userland.MAC;
with IPC.Pipe; use IPC.Pipe;
with IPC.PTY;  use IPC.PTY;

package Userland.Process with SPARK_Mode => Off is
   --  A process is a collection of threads (handled by the scheduler) and
   --  their shared resources, like memory map, and open files.
   --  It is the main unit of program handling. When a program is opened, a
   --  process will be created for it.
   --  Userland will know the process by a PID, a parent PID of 0 means no
   --  parent.
   type File_Description_Type is (
      Description_Reader_Pipe,
      Description_Writer_Pipe,
      Description_Primary_PTY,
      Description_Secondary_PTY,
      Description_File
   );
   type File_Description (Description : File_Description_Type) is record
      Close_On_Exec : Boolean;
      case Description is
         when Description_Reader_Pipe =>
            Inner_Reader_Pipe : IPC.Pipe.Reader_Acc;
         when Description_Writer_Pipe =>
            Inner_Writer_Pipe : IPC.Pipe.Writer_Acc;
         when Description_Primary_PTY =>
            Inner_Primary_PTY : IPC.PTY.Primary_Acc;
         when Description_Secondary_PTY =>
            Inner_Secondary_PTY : IPC.PTY.Secondary_Acc;
         when Description_File =>
            Inner_File : VFS.File.File_Acc;
      end case;
   end record;
   type File_Description_Acc is access all File_Description;

   type Process_Data_Threads   is array (1 .. 20) of Scheduler.TID;
   type Process_File_Table     is array (0 .. 99) of File_Description_Acc;
   type Process_Children_Table is array (1 .. 10) of Natural;
   type Process_Data is record
      Identifier      : String (1 .. 20);
      Process_PID     : Positive;
      Parent_PID      : Natural;
      Tracer_PID      : Natural;
      Tracer_FD       : Natural range 0 .. 99;
      Children        : Process_Children_Table;
      Current_Dir_Len : Natural;
      Current_Dir     : String (1 .. 100);
      Thread_List     : Process_Data_Threads;
      File_Table      : Process_File_Table;
      Common_Map      : Memory.Virtual.Page_Map_Acc;
      Stack_Base      : Unsigned_64;
      Alloc_Base      : Unsigned_64;
      Is_MAC_Locked   : Boolean;
      Perms           : MAC.Permissions;

      --  Returns for waiting.
      Did_Exit  : Boolean    with Volatile;
      Exit_Code : Unsigned_8 with Volatile;
   end record;
   type Process_Data_Acc is access Process_Data;

   Max_Process_Count : constant Natural;

   --  Initialize the process registry.
   procedure Init;

   --  Get a count of all the processes in the system.
   function Get_Process_Count return Natural;

   --  Information of a process.
   type Process_Info is record
      Identifier      : String (1 .. 20);
      Process_PID     : Positive;
      Parent_PID      : Natural;
      Is_Being_Traced : Boolean;
      Is_MAC_Locked   : Boolean;
      Has_Exited      : Boolean;
   end record;
   type Process_Info_Arr is array (Natural range <>) of Process_Info;

   --  List all processes on the system.
   --  @param List  Where to write all the process information.
   --  @param Total Total count of processes, even if it is > List'Length.
   procedure List_All (List : out Process_Info_Arr; Total : out Natural);

   --  Created a vanilla process, or remove a process, or fetch processes.
   function Create_Process
      (Parent : Process_Data_Acc := null) return Process_Data_Acc;
   procedure Delete_Process (Process : Process_Data_Acc);
   function Get_By_PID (Process : Positive) return Process_Data_Acc;
   function Get_By_Thread (Thread : Scheduler.TID) return Process_Data_Acc;

   --  Check whether a process is a child of the passed one.
   function Is_Child (P : Process_Data_Acc; Proc : Positive) return Boolean;

   --  From here on, these are convenience functions for handling data in the
   --  process record.

   --  Add or remove an threads and files to a process, or remove them all.
   function Add_Thread
      (Process : Process_Data_Acc;
       Thread  : Scheduler.TID) return Boolean;
   procedure Remove_Thread
      (Process : Process_Data_Acc;
       Thread  : Scheduler.TID);
   procedure Flush_Threads (Process : Process_Data_Acc);

   --  Reroll the ASLR addresses given to a process at creation.
   procedure Reroll_ASLR (Process : Process_Data_Acc);

   --  Add and remove files to the process file table, or remove them all.
   function Is_Valid_File
      (Process : Process_Data_Acc;
       FD      : Unsigned_64) return Boolean;
   function Add_File
      (Process : Process_Data_Acc;
       File    : File_Description_Acc;
       FD      : out Natural) return Boolean;
   function Duplicate (F : File_Description_Acc) return File_Description_Acc;
   procedure Close (F : in out File_Description_Acc);
   function Get_File
      (Process : Process_Data_Acc;
       FD      : Unsigned_64) return File_Description_Acc;
   function Replace_File
      (Process : Process_Data_Acc;
       File    : File_Description_Acc;
       Old_FD  : Natural) return Boolean;
   procedure Remove_File (Process : Process_Data_Acc; FD : Natural);
   procedure Flush_Files (Process : Process_Data_Acc);
   procedure Flush_Exec_Files (Process : Process_Data_Acc);

private

   Max_Process_Count : constant Natural := 256;
end Userland.Process;
