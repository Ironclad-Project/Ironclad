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
with Lib.Synchronization;
with Scheduler; use Scheduler;
with Interfaces; use Interfaces;
with Userland.MAC;
with IPC.Pipe; use IPC.Pipe;
with IPC.PTY;  use IPC.PTY;

package Userland.Process with SPARK_Mode => Off is
   --  A process is identifier by a PID (process identifier).
   --  In order to obtain a classical integer that can be used for reporting
   --  or userland passing, PIDs can be converted to integers using functions
   --  of this module.
   type PID is private;
   Error_PID         : constant PID;
   Max_Process_Count : constant Natural;

   --  Processes can be assigned names, for identification purposes, current
   --  directories, threads, all of these things have maximum limits.
   Max_CWD_Length   : constant Natural;
   Max_Name_Length  : constant Natural;
   Max_Thread_Count : constant Natural;
   Max_File_Count   : constant Natural;

   --  Each file held by the process follows this format.
   type File_Description_Type is
      (Description_Reader_Pipe,
       Description_Writer_Pipe,
       Description_Primary_PTY,
       Description_Secondary_PTY,
       Description_File);
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

   --  Initialize the process registry.
   procedure Init;

   --  Get a count of all the processes registered in the system.
   function Get_Process_Count return Natural;

   --  Query children for the passed process.
   --  @param Proc Process to search the children for.
   --  @param Buf  Where to write the children.
   --  @return Count of children, even if it does not fit.
   type Children_Arr is array (Natural range <>) of PID;
   function Get_Children (Proc : PID; Buf : out Children_Arr) return Natural;

   --  Information of a process.
   type Process_Info is record
      Identifier      : String (1 .. 20);
      Process         : PID;
      Parent          : PID;
      Is_Being_Traced : Boolean;
      Is_MAC_Locked   : Boolean;
      Has_Exited      : Boolean;
   end record;
   type Process_Info_Arr is array (Natural range <>) of Process_Info;

   --  List all processes on the system.
   --  @param List  Where to write all the process information.
   --  @param Total Total count of processes, even if it is > List'Length.
   procedure List_All (List : out Process_Info_Arr; Total : out Natural);

   --  Create an empty process, a parent may be passed for getting some data.
   --  @param Parent Parent of the process, or Error_PID for none.
   --  @return PID if successful, or Error_PID if not successful.
   function Create_Process (Parent : PID := Error_PID) return PID;

   --  Delete a process.
   --  @param Process Process to delete.
   procedure Delete_Process (Process : PID) with Pre => Process /= Error_PID;

   --  Add a thread to the process.
   --  @param Proc   Process to add a thread.
   --  @param Thread Thread to add.
   --  @return True on success, False on failure.
   function Add_Thread (Proc : PID; Thread : Scheduler.TID) return Boolean
      with Pre => Proc /= Error_PID;

   --  Remove a thread from the process.
   --  @param Proc   Process to remove a thread from.
   --  @param Thread Thread to remove.
   procedure Remove_Thread (Proc : PID; Thread : Scheduler.TID)
      with Pre => Proc /= Error_PID;

   --  Flush all the threads from a process.
   --  @param Proc Process to remove all threads.
   procedure Flush_Threads (Proc : PID) with Pre => Proc /= Error_PID;

   --  Reroll the ASLR addresses given to a process at creation.
   --  @param Process Process to reroll.
   procedure Reroll_ASLR (Process : PID) with Pre => Process /= Error_PID;

   --  Check whether a file is registered in a process.
   --  @param Process Process to check.
   --  @param FD      FD to check.
   --  @return True if valid and registered, False if not.
   function Is_Valid_File (Process : PID; FD : Unsigned_64) return Boolean
      with Pre => Process /= Error_PID;

   --  Add a file to a process in the earliest available file descriptor slot.
   --  @param Process Process to add a file to.
   --  @param File    File to add.
   --  @param FD      FD registered.
   --  @return True on success, False, for example, if no slots are available.
   function Add_File
      (Process : PID;
       File    : File_Description_Acc;
       FD      : out Natural) return Boolean
      with Pre => Process /= Error_PID;

   --  Duplicate an individual file.
   --  @param File File to duplicate.
   --  @return Duplicated file, or null if error'd out.
   function Duplicate (F : File_Description_Acc) return File_Description_Acc;

   --  Duplicate an individual file that resides in a process FD.
   --  @param Proc Process to use.
   --  @param FD   Slot to duplicate.
   --  @return Duplicated file, or null if error'd out or the slot was empty.
   function Duplicate (Proc : PID; FD : Natural) return File_Description_Acc
      with Pre => Proc /= Error_PID;

   --  Duplicate an entire process table to another process.
   --  @param Process Process to use.
   --  @param Target  Target process.
   procedure Duplicate_FD_Table (Process, Target : PID)
      with Pre => (Process /= Error_PID) and (Target /= Error_PID);

   --  Close and free an individual file.
   --  @param F File to operate on.
   procedure Close (F : in out File_Description_Acc);

   --  Get a file held on a file descriptor.
   --  @param Process Process to operate on.
   --  @param FD      FD to fetch.
   --  @return Returned file, or null in failure.
   function Get_File
      (Process : PID;
       FD      : Unsigned_64) return File_Description_Acc
      with Pre => Process /= Error_PID;

   --  Replace a file in a descriptor slot for a new one, closing the old one.
   --  @param Process Process to operate on.
   --  @param File    File to replace with.
   --  @param Old_FD  FD to replace.
   --  @return True on success, or False if the replacement could not be done.
   function Replace_File
      (Process : PID;
       File    : File_Description_Acc;
       Old_FD  : Natural) return Boolean
      with Pre => Process /= Error_PID;

   --  Remove a file from a process.
   --  @param Process Process to operate on.
   --  @param FD      File to remove.
   procedure Remove_File (Process : PID; FD : Natural)
      with Pre => Process /= Error_PID;

   --  Remove all files from a process.
   --  @param Process Process to operate on.
   procedure Flush_Files (Process : PID) with Pre => Process /= Error_PID;

   --  Remove all cloexec files from a process.
   --  @param Process Process to operate on.
   procedure Flush_Exec_Files (Process : PID) with Pre => Process /= Error_PID;

   --  Set the virtual map associated with the process.
   --  @param Proc Process to operate on.
   --  @param Map  Map to assign.
   procedure Set_Common_Map (Proc : PID; Map : Memory.Virtual.Page_Map_Acc)
      with Pre => Proc /= Error_PID;

   --  Get the virtual map associated with the process.
   --  @param Proc Process to operate on.
   --  @return The map.
   function Get_Common_Map (Proc : PID) return Memory.Virtual.Page_Map_Acc
      with Pre => Proc /= Error_PID;

   --  Get the stack base of the process.
   --  @param Proc Process to operate on.
   --  @return The stack base.
   function Get_Stack_Base (Process : PID) return Unsigned_64
      with Pre => Process /= Error_PID;

   --  Set the stack base of the process.
   --  @param Proc Process to operate on.
   --  @param Base Base to set.
   procedure Set_Stack_Base (Process : PID; Base : Unsigned_64)
      with Pre => Process /= Error_PID;

   --  Get the alloc base of the process.
   --  @param Proc Process to operate on.
   --  @return The stack base.
   function Get_Alloc_Base (Process : PID) return Unsigned_64
      with Pre => Process /= Error_PID;

   --  Set the alloc base of the process.
   --  @param Proc Process to operate on.
   --  @param Base Base to set.
   procedure Set_Alloc_Base (Process : PID; Base : Unsigned_64)
      with Pre => Process /= Error_PID;

   --  Get whether a process is traced and its tracer FD.
   --  @param Proc      Process to operate on.
   --  @param Is_Traced True if traced, False if not.
   --  @param FD        FD to use for tracing.
   procedure Get_Traced_Info
      (Process   : PID;
       Is_Traced : out Boolean;
       FD        : out Natural)
      with Pre => Process /= Error_PID;

   --  Set whether a process is traced and its tracer FD.
   --  @param Proc      Process to operate on.
   --  @param Is_Traced True if traced, False if not.
   --  @param FD        FD to use for tracing.
   procedure Set_Traced_Info
      (Process   : PID;
       Is_Traced : Boolean;
       FD        : Natural)
      with Pre => Process /= Error_PID;

   --  Exit a process, and set an error code.
   --  @param Process Process to exit.
   --  @param Code    Exit code.
   procedure Issue_Exit (Process : PID; Code : Unsigned_8)
      with Pre => Process /= Error_PID;

   --  Check whether a process exited.
   --  @param Process  Process to check.
   --- @param Did_Exit True if exited, False if not.
   --  @param Code     Exit code, if it exited.
   procedure Check_Exit
      (Process  : PID;
       Did_Exit : out Boolean;
       Code     : out Unsigned_8)
      with Pre => Process /= Error_PID;

   --  Set the current working directory.
   --  @param Proc Process to set the CWD of.
   --  @param CWD  Path to set, it is not checked.
   --  @return True if succesful, False if it did not fit, between others.
   function Set_CWD (Proc : PID; CWD : String) return Boolean
      with Pre => Proc /= Error_PID;

   --  Get the current working directory.
   --  @param Proc Process to get the CWD of.
   --  @param CWD  Buffer to write the CWD.
   --  @param Len  Length of the CWD, even if bigger than CWD'Length.
   procedure Get_CWD
      (Proc : PID;
       CWD  : out String;
       Len  : out Natural)
      with Pre => Proc /= Error_PID;

   --  Check whether MAC is locked.
   --  @param Proc Process to get the info from.
   --  @return True if locked, False if not.
   function Is_MAC_Locked (Proc : PID) return Boolean
      with Pre => Proc /= Error_PID;

   --  Lock MAC.
   --  @param Proc Process to lock.
   procedure Lock_MAC (Proc : PID)
      with Pre => Proc /= Error_PID;

   --  Get MAC permissions.
   --  @param Proc Process to get the info from.
   --  @return Permissions.
   function Get_MAC (Proc : PID) return MAC.Permissions
      with Pre => Proc /= Error_PID;

   --  Get MAC permissions.
   --  @param Proc  Process to get the info from.
   --  @param Perms Permissions to set.
   procedure Set_MAC (Proc : PID; Perms : MAC.Permissions)
      with Pre => Proc /= Error_PID;

   --  Get the parent PID of the process.
   --  @param Proc Process to get the info from.
   --  @return Parent PID, or Error_PID if no parent.
   function Get_Parent (Proc : PID) return PID
      with Pre => Proc /= Error_PID;

   --  Set the identifier of the process, will be chopped if needed.
   --  @param Proc Process to set the name of.
   --  @param Name ID to set, it is not checked.
   procedure Set_Identifier (Proc : PID; Name : String)
      with Pre => Proc /= Error_PID;

   --  Get the process identifier.
   --  @param Proc Process to get the ID of.
   --  @param ID   Buffer to write the ID.
   --  @param Len  Length of the ID, even if bigger than ID'Length.
   procedure Get_Identifier
      (Proc : PID;
       ID   : out String;
       Len  : out Natural)
      with Pre => Proc /= Error_PID;

   --  Convert a PID to an integer. The results will be reproducible for the
   --  same PIDs.
   --  @param Proc PID to convert, can be Error_PID.
   --  @return The converted integer.
   function Convert (Proc : PID) return Natural with Pre => Proc /= Error_PID;

   --  Convert an integer back into a PID, invalid integers may be passed, in
   --  which case they will translate to Error_PID.
   --  @param Proc Integer to convert.
   --  @return The converted PID.
   function Convert (Proc : Natural) return PID;

private

   type PID is new Natural range 0 .. 256;
   Error_PID         : constant PID     :=   0;
   Max_Process_Count : constant Natural := 256;

   Max_CWD_Length   : constant Natural := 100;
   Max_Name_Length  : constant Natural := 100;
   Max_Thread_Count : constant Natural :=  20;
   Max_File_Count   : constant Natural := 100;

   type Thread_Arr is array (1 .. Max_Thread_Count)   of Scheduler.TID;
   type File_Arr   is array (0 .. Max_File_Count - 1) of File_Description_Acc;
   type Process_Data is record
      Identifier      : String (1 .. Max_Name_Length);
      Identifier_Len  : Natural range 0 .. Max_Name_Length;
      Parent          : PID;
      Is_Traced       : Boolean;
      Tracer_FD       : Natural range 0 .. Max_File_Count - 1;
      Current_Dir     : String (1 .. Max_CWD_Length);
      Current_Dir_Len : Natural range 0 .. Max_CWD_Length;
      Thread_List     : Thread_Arr;
      File_Table      : File_Arr;
      Common_Map      : Memory.Virtual.Page_Map_Acc;
      Stack_Base      : Unsigned_64;
      Alloc_Base      : Unsigned_64;
      Is_MAC_Locked   : Boolean;
      Perms           : MAC.Permissions;
      Did_Exit        : Boolean;
      Exit_Code       : Unsigned_8;
   end record;
   type Process_Data_Acc is access Process_Data;
   type Process_Arr     is array (PID range 1 .. PID'Last) of Process_Data_Acc;
   type Process_Arr_Acc  is access Process_Arr;

   Registry_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Registry       : Process_Arr_Acc;
end Userland.Process;
