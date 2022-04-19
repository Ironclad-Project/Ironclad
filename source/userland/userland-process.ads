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

package Userland.Process is
   --  A process is a collection of threads (handled by the scheduler) and
   --  their shared resources, like memory map, and open files.
   --  It is the main unit of program handling. When a program is opened, a
   --  process will be created for it.
   --  A process is identified by an integer, 0 is reserved for error.
   type PID is new Natural;
   Error_PID : constant PID := 0;

   --  Initialize the process registry.
   procedure Init;

   --  Created a vanilla process, or remove a process, or fetch processes.
   function Create_Process (Parent : PID := Error_PID) return PID;
   procedure Delete_Process (Process : PID);
   function Get_Process_By_Thread (Thread : Scheduler.TID) return PID;

   --  Add or remove an threads and files to a process.
   function Add_Thread (Process : PID; Threa : Scheduler.TID) return Boolean;
   procedure Remove_Thread (Process : PID; Thread : Scheduler.TID);
   procedure Flush_Threads (Process : PID);

   --  Add and remove files to the process file descriptor table.
   function Add_File
      (Process : PID;
       File    : VFS.File.File_Acc;
       FD      : out Natural) return Boolean;
   function Get_File (Process : PID; FD : Natural) return VFS.File.File_Acc;
   procedure Remove_File (Process : PID; FD : Natural);
   procedure Flush_Files (Process : PID);

   --  Get or set the current root of the process.
   procedure Set_Current_Root (Process : PID; Root : VFS.Root_Name);
   function Get_Current_Root (Process : PID) return VFS.Root_Name;

   --  Get and set different properties of the process.
   function Get_Parent_Process (Process : PID) return PID;
   procedure Set_Memmap (Process : PID; Map : Memory.Virtual.Page_Map_Acc);
   function Get_Memmap (Process : PID) return Memory.Virtual.Page_Map_Acc;

   --  Get the stack base and increment it by the passed stack size.
   function Bump_Stack (Process : PID; Val : Unsigned_64) return Unsigned_64;

   --  Get the allocation base and increment it by the passed size.
   function Bump_Alloc (Process : PID; Val : Unsigned_64) return Unsigned_64;
private

   function Is_Valid_Process (Process : PID) return Boolean;
end Userland.Process;
