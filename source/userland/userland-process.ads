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

package Userland.Process with SPARK_Mode => Off is
   --  A process is a collection of threads (handled by the scheduler) and
   --  their shared resources, like memory map, and open files.
   --  It is the main unit of program handling. When a program is opened, a
   --  process will be created for it.
   --  Userland will know the process by a PID, a parent PID of 0 means no
   --  parent.
   type Process_Data_Threads   is array (1 .. 20) of Scheduler.TID;
   type Process_File_Table     is array (0 .. 99) of VFS.File.File_Acc;
   type Process_Children_Table is array (1 .. 10) of Natural;
   type Process_Data is record
      Process_PID     : Positive;
      Parent_PID      : Natural;
      Children        : Process_Children_Table;
      Current_Dir_Len : Natural;
      Current_Dir     : String (1 .. 100);
      Thread_List     : Process_Data_Threads;
      File_Table      : Process_File_Table;
      Common_Map      : Memory.Virtual.Page_Map_Acc;
      Stack_Base      : Unsigned_64;
      Alloc_Base      : Unsigned_64;

      --  Returns for waiting.
      Did_Exit  : Boolean    with Volatile;
      Exit_Code : Unsigned_8 with Volatile;
   end record;
   type Process_Data_Acc is access Process_Data;

   --  Initialize the process registry.
   procedure Init;

   --  Created a vanilla process, or remove a process, or fetch processes.
   function Create_Process
      (Parent : Process_Data_Acc := null) return Process_Data_Acc;
   procedure Delete_Process (Process : Process_Data_Acc);
   function Get_By_PID (Process : Positive) return Process_Data_Acc;
   function Get_By_Thread (Thread : Scheduler.TID) return Process_Data_Acc;

   --  Fork a process.
   --  This clones a process in all regards, but without running threads.
   function Fork (Parent : Process_Data_Acc) return Process_Data_Acc;

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

   --  Add and remove files to the process file table, or remove them all.
   function Add_File
      (Process : Process_Data_Acc;
       File    : VFS.File.File_Acc;
       FD      : out Natural) return Boolean;
   function Replace_File
      (Process : Process_Data_Acc;
       File    : VFS.File.File_Acc;
       Old_FD  : Natural) return Boolean;
   procedure Remove_File (Process : Process_Data_Acc; FD : Natural);
   procedure Flush_Files (Process : Process_Data_Acc);
end Userland.Process;
