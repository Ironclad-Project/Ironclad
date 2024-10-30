--  userland-process.ads: Process registry, PIDs, and all the fuzz.
--  Copyright (C) 2024 streaksu
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

with System;
with VFS;
with Arch.MMU;
with Lib.Synchronization;
with Scheduler; use Scheduler;
with Interfaces; use Interfaces;
with Userland.MAC;
with IPC.FIFO;   use IPC.FIFO;
with IPC.PTY;    use IPC.PTY;
with IPC.Socket; use IPC.Socket;

package Userland.Process is
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

   --  Each process has a umask, inherited from the parent, with a default mask
   --  for the first process.
   Default_Umask : constant VFS.File_Mode;

   --  Each file held by the process follows this format.
   type File_Description_Type is
      (Description_Reader_FIFO,
       Description_Writer_FIFO,
       Description_Primary_PTY,
       Description_Secondary_PTY,
       Description_Inode,
       Description_Socket);
   type File_Description;
   type File_Description_Acc is access File_Description;
   type File_Description (Description : File_Description_Type) is record
      Children_Count : Natural;
      case Description is
         when Description_Reader_FIFO =>
            Inner_Reader_FIFO : IPC.FIFO.Inner_Acc;
         when Description_Writer_FIFO =>
            Inner_Writer_FIFO : IPC.FIFO.Inner_Acc;
         when Description_Primary_PTY =>
            Inner_Primary_PTY : IPC.PTY.Inner_Acc;
         when Description_Secondary_PTY =>
            Inner_Secondary_PTY : IPC.PTY.Inner_Acc;
         when Description_Inode =>
            Inner_Is_Locked   : Boolean;
            Inner_Is_Blocking : Boolean;
            Inner_Ino_Read    : Boolean;
            Inner_Ino_Write   : Boolean;
            Inner_Ino_Pos     : Unsigned_64;
            Inner_Ino_FS      : VFS.FS_Handle;
            Inner_Ino         : VFS.File_Inode_Number;
         when Description_Socket =>
            Inner_Socket : IPC.Socket.Socket_Acc;
      end case;
   end record;

   type Signal is
      (Signal_Abort,
       Signal_Alarm,
       Signal_Bad_Memory,
       Signal_Child,
       Signal_Continue,
       Signal_FP_Exception,
       Signal_Hang_Up,
       Signal_Illegal_Instruction,
       Signal_Interrupted,
       Signal_Kill,
       Signal_Broken_Pipe,
       Signal_Quit,
       Signal_Segmentation_Fault,
       Signal_Stop,
       Signal_Terminated,
       Signal_Terminal_Stop,
       Signal_Terminal_In,
       Signal_Terminal_Out,
       Signal_User_1,
       Signal_User_2,
       Signal_Pollable,
       Signal_Profiling_Timer,
       Signal_Bad_Syscall,
       Signal_Tracepoint,
       Signal_Urgent,
       Signal_Virtual_Timer,
       Signal_CPU_Exceeded,
       Signal_File_Size_Exceeded);

   --  These values and the values of Signal_Bitmap are userland ABI, please
   --  dont touch them if you dont want to break them!
   for Signal use
      (Signal_Abort               =>  1,
       Signal_Alarm               =>  2,
       Signal_Bad_Memory          =>  3,
       Signal_Child               =>  4,
       Signal_Continue            =>  5,
       Signal_FP_Exception        =>  6,
       Signal_Hang_Up             =>  7,
       Signal_Illegal_Instruction =>  8,
       Signal_Interrupted         =>  9,
       Signal_Kill                => 10,
       Signal_Broken_Pipe         => 11,
       Signal_Quit                => 12,
       Signal_Segmentation_Fault  => 13,
       Signal_Stop                => 14,
       Signal_Terminated          => 15,
       Signal_Terminal_Stop       => 16,
       Signal_Terminal_In         => 17,
       Signal_Terminal_Out        => 18,
       Signal_User_1              => 19,
       Signal_User_2              => 20,
       Signal_Pollable            => 21,
       Signal_Profiling_Timer     => 22,
       Signal_Bad_Syscall         => 23,
       Signal_Tracepoint          => 24,
       Signal_Urgent              => 25,
       Signal_Virtual_Timer       => 26,
       Signal_CPU_Exceeded        => 27,
       Signal_File_Size_Exceeded  => 28);
   type Signal_Bitmap is array (Signal) of Boolean with Pack, Size => 28;

   --  Initialize the process registry.
   procedure Init;

   --  Disable location ASLR when creating processes.
   procedure Disable_ASLR;

   --  Query children for the passed process.
   --  @param Proc Process to search the children for.
   --  @param Buf  Where to write the children.
   --  @param Len  Count of children, even if it does not fit.
   type Children_Arr is array (Natural range <>) of PID;
   procedure Get_Children
      (Proc : PID;
       Buf  : out Children_Arr;
       Len  : out Natural);

   --  Information of a process.
   type Process_Info is record
      Identifier      : String (1 .. 20);
      Identifier_Len  : Natural;
      Process         : PID;
      Parent          : PID;
      User            : Unsigned_32;
      Is_Being_Traced : Boolean;
      Has_Exited      : Boolean;
   end record;
   type Process_Info_Arr is array (Natural range <>) of Process_Info;

   --  List all processes on the system.
   --  @param List  Where to write all the process information.
   --  @param Total Total count of processes, even if it is > List'Length.
   procedure List_All (List : out Process_Info_Arr; Total : out Natural);

   --  Create an empty process, a parent may be passed for getting some data.
   --  @param Parent   Parent of the process, or Error_PID for none.
   --  @param Returned PID if successful, or Error_PID if not successful.
   procedure Create_Process (Parent : PID; Returned : out PID);

   --  Delete a process.
   --  @param Process Process to delete.
   procedure Delete_Process (Process : PID) with Pre => Process /= Error_PID;

   --  Get runtime of all the threads owned by the process;
   procedure Get_Runtime_Times
      (Proc : PID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64);

   --  Get runtime of all children processes that have exited.
   procedure Get_Children_Runtimes
      (Proc : PID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64);

   --  Add a thread to the process.
   --  @param Proc    Process to add a thread.
   --  @param Thread  Thread to add.
   --  @param Success True on success, False on failure.
   procedure Add_Thread
      (Proc    : PID;
       Thread  : Scheduler.TID;
       Success : out Boolean)
      with Pre => Proc /= Error_PID;

   --  Get the count of tgreads held by the process.
   --  @param Process Process to check.
   --  @return Number of threads held by the process.
   function Get_Thread_Count (Process : PID) return Natural
      with Pre => Process /= Error_PID;

   --  Remove a thread from the process.
   --  @param Proc   Process to remove a thread from.
   --  @param Thread Thread to remove.
   procedure Remove_Thread (Proc : PID; Thread : Scheduler.TID)
      with Pre => Proc /= Error_PID;

   --  Flush all the threads from a process.
   --  @param Proc Process to remove all threads.
   procedure Flush_Threads (Proc : PID) with Pre => Proc /= Error_PID;

   --  Reset the trackers for memory addresses and locations for the process.
   --  If ASLR is enabled, it will be used here.
   --  @param Process Process to reroll.
   procedure Reassign_Process_Addresses (Process : PID)
      with Pre => Process /= Error_PID;

   --  Get the process's default niceness.
   function Get_Niceness (Process : PID) return Scheduler.Niceness
      with Pre => Process /= Error_PID;

   --  Set the process's default niceness.
   procedure Set_Niceness (Process : PID; Nice : Scheduler.Niceness)
      with Pre => Process /= Error_PID;

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
   --  @param Success True on success, False if no slots are available.
   --  @param Start   Start allocating by the passed FD.
   procedure Add_File
      (Process : PID;
       File    : File_Description_Acc;
       FD      : out Natural;
       Success : out Boolean;
       Start   : Natural := 0)
      with Pre => Process /= Error_PID;

   --  Get the count of file descriptors held by the process.
   --  @param Process Process to add a file to.
   --  @return Number of file descriptors held by the process.
   function Get_File_Count (Process : PID) return Natural
      with Pre => Process /= Error_PID;

   --  Duplicate an individual file.
   --  @param File   File to duplicate.
   --  @param Result Duplicated file, or null if error'd out.
   procedure Duplicate
      (F      : File_Description_Acc;
       Result : out File_Description_Acc);

   --  Duplicate an entire process table to another process.
   --  @param Process Process to use.
   --  @param Target  Target process.
   --  @param Max_FD  FDs to duplicate from the beginning, by default, all.
   procedure Duplicate_FD_Table
      (Process : PID;
       Target  : PID;
       Max_FD  : Natural := Max_File_Count)
      with Pre => (Process /= Error_PID) and
                  (Target /= Error_PID)  and
                  Max_FD > 0             and
                  Max_FD <= Max_File_Count;

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

   --  Get the Close on Exec flag for a file descriptor.
   --  @param Process  Process to operate on.
   --  @param FD       FD to check.
   --- @return True if close on exec, false if not.
   procedure Get_FD_Flags
      (Process       : PID;
       FD            : Unsigned_64;
       Close_On_Exec : out Boolean;
       Close_On_Fork : out Boolean)
      with Pre => Process /= Error_PID;

   --  Set the Close on Exec flag for a file descriptor.
   --  @param Process  Process to operate on.
   --  @param FD       FD to set.
   --- @param Is_Close True if close on exec, false if not.
   procedure Set_FD_Flags
      (Process       : PID;
       FD            : Unsigned_64;
       Close_On_Exec : Boolean;
       Close_On_Fork : Boolean)
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
   procedure Set_Common_Map (Proc : PID; Map : Arch.MMU.Page_Table_Acc)
      with Pre => Proc /= Error_PID;

   --  Get the virtual map associated with the process.
   --  @param Proc Process to operate on.
   --  @return The map.
   function Get_Common_Map (Proc : PID) return Arch.MMU.Page_Table_Acc
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

   --  Bump the alloc base of the process by the passed length.
   --  @param P      Process to operate on.
   --  @param Length Length to bump to.
   --  @return Previous base.
   function Bump_Alloc_Base (P : PID; Length : Unsigned_64) return Unsigned_64
      with Pre => P /= Error_PID;

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
   --  @param Signal  True if the exit was caused by a signal.
   procedure Issue_Exit (Process : PID; Code : Unsigned_8)
      with Pre => Process /= Error_PID;

   --  Exit a process with a signal, and set an error code.
   --  @param Process Process to exit.
   --  @param Code    Exit code.
   --  @param Sig     Signal to cause the termination.
   procedure Issue_Exit (Process : PID; Sig : Signal)
      with Pre => Process /= Error_PID;

   --  Check whether a process exited.
   --  @param Process    Process to check.
   --- @param Did_Exit   True if exited, False if not.
   --  @param Code       Exit code, if it exited.
   --  @param Was_Signal True if the exit was caused by a signal.
   --  @param Sig        The signal that caused termination, if any.
   procedure Check_Exit
      (Process    : PID;
       Did_Exit   : out Boolean;
       Code       : out Unsigned_8;
       Was_Signal : out Boolean;
       Sig        : out Signal)
      with Pre => Process /= Error_PID;

   --  Set the current working directory.
   --  @param Proc    Process to set the CWD of.
   --  @param CWD     Path to set, it is not checked.
   --  @param Success True if succesful, False if it did not fit, or others.
   procedure Set_CWD
      (Proc : PID;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number)
      with Pre => Proc /= Error_PID;

   --  Get the current working directory.
   --  @param Proc Process to get the CWD of.
   --  @param CWD  Buffer to write the CWD.
   --  @param Len  Length of the CWD, even if bigger than CWD'Length.
   procedure Get_CWD
      (Proc : PID;
       FS   : out VFS.FS_Handle;
       Ino  : out VFS.File_Inode_Number)
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

   --  Get the process user id.
   --  @param Proc Process to get the UID of.
   --  @param UID  Returned UID.
   procedure Get_UID (Proc : PID; UID : out Unsigned_32);

   --  Set the process user id.
   --  @param Proc Process to get the UID of.
   --  @param UID  UID to set.
   procedure Set_UID (Proc : PID; UID : Unsigned_32);

   --  Get the process effective user id.
   --  @param Proc Process to get the UID of.
   --  @param EUID Returned UID.
   procedure Get_Effective_UID (Proc : PID; EUID : out Unsigned_32);

   --  Set the process effective user id.
   --  @param Proc Process to get the UID of.
   --  @param EUID EUID to set.
   procedure Set_Effective_UID (Proc : PID; EUID : Unsigned_32);

   --  Get the process group id.
   --  @param Proc Process to get the GID of.
   --  @param GID  Returned GID.
   procedure Get_GID (Proc : PID; GID : out Unsigned_32);

   --  Set the process group id.
   --  @param Proc Process to set the GID of.
   --  @param GID  GID to set.
   procedure Set_GID (Proc : PID; GID : Unsigned_32);

   --  Get the process effective group id.
   --  @param Proc Process to get the GID of.
   --  @param EGID Returned GID.
   procedure Get_Effective_GID (Proc : PID; EGID : out Unsigned_32);

   --  Set the process effective group id.
   --  @param Proc Process to get the GID of.
   --  @param EGID EGID to set.
   procedure Set_Effective_GID (Proc : PID; EGID : Unsigned_32);

   Max_Supplementary_Groups : constant := 10;
   type Supplementary_GID_Arr is array (Natural range <>) of Unsigned_32;

   procedure Get_Supplementary_Groups
      (Proc   : PID;
       Groups : out Supplementary_GID_Arr;
       Length : out Natural);

   procedure Set_Supplementary_Groups
      (Proc    : PID;
       Groups  : Supplementary_GID_Arr;
       Success : out Boolean);

   procedure Empty_Supplementary_Groups (Proc : PID);

   --  Get the process umask.
   --  @param Proc  Process to get the UID of.
   --  @param Umask Returned umask.
   procedure Get_Umask (Proc : PID; Umask : out VFS.File_Mode);

   --  Set the process umask.
   --  @param Proc  Process to get the UID of.
   --  @param Umask umask to set without modification, no AND or anything.
   procedure Set_Umask (Proc : PID; Umask : VFS.File_Mode);

   --  Get masked signals for a process, those are signals that cannot be
   --  raised.
   --  @param Proc Process to set masked signals for.
   --  @param Sig  Signal bitmap to write.
   procedure Get_Masked_Signals (Proc : PID; Sig : out Signal_Bitmap);

   --  Set masked signals for a process, those are signals that cannot be
   --  raised.
   --  @param Proc Process to set masked signals for.
   --  @param Sig  Signal bitmap to mask, true means that signal will be mask.
   procedure Set_Masked_Signals (Proc : PID; Sig : Signal_Bitmap);

   --  Signal that a signal was raised.
   --  @param Proc Process to raise for.
   --  @param Sig  Signal to raise.
   procedure Raise_Signal (Proc : PID; Sig : Signal);

   --  Get the address assigned for a process to use as a signal handler.
   --  @param Proc Process to set the handler for.
   --  @param Sig  Signal to set the handler to.
   --  @param Addr Address set for the handler.
   procedure Get_Signal_Handler
      (Proc : PID;
       Sig  : Signal;
       Addr : out System.Address);

   --  Set an address for a process to use as a signal handler.
   --  @param Proc Process to set the handler for.
   --  @param Sig  Signal to set the handler to.
   --  @param Addr Address to set for the handler.
   procedure Set_Signal_Handler
      (Proc : PID;
       Sig  : Signal;
       Addr : System.Address);

   --  Get a raised signal for a process.
   --  @param Proc   Process to fetch signals for.
   --  @param Sig    Raised signal.
   --  @param Addr   Address of the handler, Null_Address if not registered.
   --  @param No_Sig The process has no more signals to process.
   --  @param Ignore If not registered, this signal can be ignored, else, kill.
   procedure Get_Raised_Signal_Actions
      (Proc   : PID;
       Sig    : out Signal;
       Addr   : out System.Address;
       No_Sig : out Boolean;
       Ignore : out Boolean);

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
   ----------------------------------------------------------------------------
   --  These functions are wrappers for MAC behaviour, getters and setters
   --  would be expensive given the nature of "hey imma just check this" of
   --  MAC and the size of the structures involved.
   --  It is a bit boilerplaty though, sorry.

   function Get_Enforcement (Proc : PID) return MAC.Enforcement;
   procedure Set_Enforcement (Proc : PID; Act : MAC.Enforcement);

   function Get_Capabilities (Proc : PID) return MAC.Capabilities;
   procedure Set_Capabilities (Proc : PID; Caps : MAC.Capabilities);

   function Check_Permissions
      (Proc : PID;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number) return MAC.Permissions;

   procedure Add_Entity
      (Proc   : PID;
       FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Perms  : MAC.Permissions;
       Status : out MAC.Addition_Status);

   function Get_Limit
      (Proc     : PID;
       Resource : MAC.Limit_Type) return MAC.Limit_Value;

   procedure Set_Limit
      (Proc      : PID;
       Resource  : MAC.Limit_Type;
       Limit     : MAC.Limit_Value;
       Could_Set : out Boolean);

private

   type PID is new Natural range 0 .. 256;
   Error_PID         : constant PID     :=   0;
   Max_Process_Count : constant Natural := 256;

   Max_CWD_Length   : constant Natural := 100;
   Max_Name_Length  : constant Natural :=  20;
   Max_Thread_Count : constant Natural :=  20;
   Max_File_Count   : constant Natural := 100;

   Default_Umask : constant VFS.File_Mode := 8#22#;

   type File_Descriptor is record
      Close_On_Exec : Boolean;
      Close_On_Fork : Boolean;
      Description   : File_Description_Acc;
   end record;

   type Thread_Arr is array (1 .. Max_Thread_Count)   of Scheduler.TID;
   type File_Arr   is array (0 .. Max_File_Count - 1) of File_Descriptor;
   type Handle_Arr is array (Signal)                  of System.Address;
   type Process_Data is record
      Data_Mutex      : aliased Lib.Synchronization.Binary_Semaphore;
      Masked_Signals  : Signal_Bitmap;
      Raised_Signals  : Signal_Bitmap;
      Signal_Handlers : Handle_Arr;
      Niceness        : Scheduler.Niceness;
      Umask           : VFS.File_Mode;
      User            : Unsigned_32;
      Effective_User  : Unsigned_32;
      Group           : Unsigned_32;
      Effective_Group : Unsigned_32;
      SGroup_Count    : Natural;
      SGroups         : Supplementary_GID_Arr (1 .. Max_Supplementary_Groups);
      Identifier      : String (1 .. Max_Name_Length);
      Identifier_Len  : Natural range 0 .. Max_Name_Length;
      Parent          : PID;
      Is_Traced       : Boolean;
      Tracer_FD       : Natural range 0 .. Max_File_Count - 1;
      Current_Dir_FS  : VFS.FS_Handle;
      Current_Dir_Ino : VFS.File_Inode_Number;
      Thread_List     : Thread_Arr;
      File_Table      : File_Arr;
      Common_Map      : Arch.MMU.Page_Table_Acc;
      Stack_Base      : Unsigned_64;
      Alloc_Base      : Unsigned_64;
      Perms           : MAC.Context;
      Signal_Exit     : Boolean;
      Which_Signal    : Signal;
      Did_Exit        : Boolean;
      Exit_Code       : Unsigned_8;
      Children_SSec   : Unsigned_64;
      Children_SNSec  : Unsigned_64;
      Children_USec   : Unsigned_64;
      Children_UNSec  : Unsigned_64;
   end record;
   type Process_Data_Acc is access Process_Data;
   type Process_Arr     is array (PID range 1 .. PID'Last) of Process_Data_Acc;
   type Process_Arr_Acc  is access Process_Arr;

   Registry_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Registry       : Process_Arr_Acc;
end Userland.Process;
