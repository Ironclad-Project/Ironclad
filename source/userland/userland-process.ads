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
with Time; use Time;
with VFS;
with Memory.MMU;
with Synchronization;
with Scheduler; use Scheduler;
with Interfaces; use Interfaces;
with Userland.MAC; use Userland.MAC;
with IPC.FIFO;     use IPC.FIFO;
with IPC.PTY;      use IPC.PTY;
with IPC.Socket;   use IPC.Socket;
with IPC.SHM;

package Userland.Process is
   --  FIXME: This unit is not preelaborate, and that generates certain binding
   --  code that generates exceptions. These functions are garbage collected at
   --  linking, so we can just mute the warning for the time being.
   pragma Warnings (Off, "Restrictions (No_Exception_Propagation) in effect");

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
   Max_File_Count   : constant Natural;
   Max_Thread_Count : constant Natural;

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
      Is_Blocking    : Boolean;

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
            Inner_Ino_Read  : Boolean;
            Inner_Ino_Write : Boolean;
            Inner_Ino_Pos   : Unsigned_64;
            Inner_Ino_FS    : VFS.FS_Handle;
            Inner_Ino       : VFS.File_Inode_Number;
         when Description_Socket =>
            Inner_Socket : IPC.Socket.Socket_Acc;
      end case;
   end record;

   type Signal is
      (Signal_Hang_Up,
       Signal_Interrupted,
       Signal_Quit,
       Signal_Continue,
       Signal_Bad_Memory,
       Signal_Abort,
       Signal_Child,
       Signal_FP_Exception,
       Signal_Kill,
       Signal_Illegal_Instruction,
       Signal_Broken_Pipe,
       Signal_Segmentation_Fault,
       Signal_Stop,
       Signal_Alarm,
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
       Signal_File_Size_Exceeded,
       Signal_Window_Resize,
       Signal_Power_Failure,
       Signal_RT_1,
       Signal_RT_2,
       Signal_RT_3,
       Signal_RT_4,
       Signal_RT_5,
       Signal_RT_6,
       Signal_RT_7,
       Signal_RT_8);

   --  These values and the values of Signal_Bitmap are userland ABI, please
   --  dont touch them if you dont want to break them!
   --
   --  The index of SIGHUP, SIGINT, SIGQUIT, SIGABRT, SIGKILL, SIGALRM, and
   --  SIGTERM is fixed due to POSIX, which requires the `kill` command to do
   --  these signals when issued like `kill -9`, and if we do not respect
   --  those numbers, userland needs to do weird translations.
   for Signal use
      (Signal_Hang_Up             =>  1,
       Signal_Interrupted         =>  2,
       Signal_Quit                =>  3,
       Signal_Continue            =>  4,
       Signal_Bad_Memory          =>  5,
       Signal_Abort               =>  6,
       Signal_Child               =>  7,
       Signal_FP_Exception        =>  8,
       Signal_Kill                =>  9,
       Signal_Illegal_Instruction => 10,
       Signal_Broken_Pipe         => 11,
       Signal_Segmentation_Fault  => 12,
       Signal_Stop                => 13,
       Signal_Alarm               => 14,
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
       Signal_File_Size_Exceeded  => 28,
       Signal_Window_Resize       => 29,
       Signal_Power_Failure       => 30,
       Signal_RT_1                => 31,
       Signal_RT_2                => 32,
       Signal_RT_3                => 33,
       Signal_RT_4                => 34,
       Signal_RT_5                => 35,
       Signal_RT_6                => 36,
       Signal_RT_7                => 37,
       Signal_RT_8                => 38);
   type Signal_Bitmap is array (Signal) of Boolean with Pack;

   --  Where a raised signal came from, as reported to SA_SIGINFO handlers.
   --  Code is the si_code: Origin_User for kill, Origin_Kernel for the
   --  kernel's own, or a CLD_* value for SIGCHLD, where PID and UID name the
   --  child and Status holds its exit code or the signal that killed it.
   --  Value is the application-defined si_value a signal queued with
   --  sigqueue carries, and is 0 for a signal raised any other way; it
   --  shares the union's slot with Status, as <signal.h> lays it out.
   type Signal_Origin is record
      Code   : Unsigned_32;
      PID    : Unsigned_32;
      UID    : Unsigned_32;
      Status : Unsigned_32;
      Value  : Unsigned_64;
   end record;

   Origin_User   : constant Unsigned_32 := 0;
   Origin_Kernel : constant Unsigned_32 := 16#80#;

   Origin_Thread : constant Unsigned_32 := 16#FFFF_FFFA#;
   Origin_Exited : constant Unsigned_32 := 1;
   Origin_Killed : constant Unsigned_32 := 2;

   Kernel_Origin : constant Signal_Origin := (Origin_Kernel, 0, 0, 0, 0);

   --  A signal sent with sigqueue is queued along with its value, several
   --  occurrences in FIFO order, where one raised any other way is only made
   --  pending and leaves the queue alone (POSIX.1-2024, XSH 2.4.2). The queue
   --  is the process's, which is what sigqueue names; a signal aimed at a
   --  thread carries no value, as no interface gives one to a thread. The
   --  depth is {_POSIX_SIGQUEUE_MAX} (limits.h), and a full queue is
   --  sigqueue's [EAGAIN].
   Max_Queued_Signals : constant := 32;

   --  Initialize the process registry.
   procedure Init;

   --  Disable location ASLR when creating processes.
   procedure Disable_ASLR;

   --  Information of a process.
   type Process_Info is record
      Identifier      : String (1 .. 20);
      Identifier_Len  : Natural range 0 .. 20;
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
   --  PIDs still in use as process group or session IDs are not reused.
   --  @param Parent   Parent of the process, or Error_PID for none.
   --  @param Returned PID if successful, or Error_PID if not successful.
   procedure Create_Process (Parent : PID; Returned : out PID);

   --  Delete a process that has no threads left. Descriptors it still holds,
   --  as a child whose fork failed half way does, are closed.
   --  @param Process Process to delete.
   procedure Delete_Process (Process : PID) with Pre => Process /= Error_PID;

   --  Get the controlling terminal of a process.
   procedure Get_Controlling_TTY (Proc : PID; TTY : out IPC.PTY.Inner_Acc);

   --  Clear the controlling terminal of a process.
   procedure Set_Controlling_TTY
      (Proc    : PID;
       TTY     : IPC.PTY.Inner_Acc;
       Success : out Boolean);

   --  Clear the controlling terminal of a process.
   procedure Clear_Controlling_TTY
      (Proc    : PID;
       TTY     : IPC.PTY.Inner_Acc;
       Success : out Boolean);

   --  Get runtime of all the threads owned by the process;
   procedure Get_Runtime_Times
      (Proc : PID; System, User : out Time.Timestamp);

   --  Get runtime of all children processes that have exited.
   procedure Get_Children_Runtimes
      (Proc : PID; System, User : out Time.Timestamp);

   --  Get elapsed time since creation of the process.
   procedure Get_Elapsed_Time (Proc : PID; Elapsed : out Time.Timestamp);

   --  Add a thread to the process and let it run, a user thread being made
   --  held (see Scheduler.Create_User_Thread). A thread refused is still
   --  held, and its maker deletes it. Threads are refused while those of the
   --  process are being flushed and once it exits.
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
   --  @param Count   Number of threads held by the process.
   procedure Get_Thread_Count (Process : PID; Count : out Natural)
      with Pre => Process /= Error_PID;

   --  Remove a thread from the process.
   --  @param Proc   Process to remove a thread from.
   --  @param Thread Thread to remove.
   procedure Remove_Thread (Proc : PID; Thread : Scheduler.TID)
      with Pre => Proc /= Error_PID;

   --  Remove a thread from the process and say how many it has left, counted
   --  in the same step, so that of threads leaving together exactly one sees
   --  none left.
   --  @param Proc      Process to remove a thread from.
   --  @param Thread    Thread to remove.
   --  @param Remaining Threads the process has once this one is gone.
   procedure Remove_Thread
      (Proc      : PID;
       Thread    : Scheduler.TID;
       Remaining : out Natural)
      with Pre => Proc /= Error_PID;

   --  Flush all the threads from a process but the caller, and wait for them
   --  to be gone, so nothing of the process is in use by them afterwards.
   --  Only one flush runs at a time, and one for an exit is the last: no
   --  thread can join the process after it.
   --  @param Proc     Process to remove all threads.
   --  @param For_Exit Whether the process is exiting, rather than exec'ing.
   --  @param Done     True once the threads are gone, False if the flush gave
   --                  way to another flush of the process.
   procedure Flush_Threads
      (Proc     : PID;
       For_Exit : Boolean;
       Done     : out Boolean)
      with Pre => Proc /= Error_PID;

   --  Reset the trackers for memory addresses and locations for the process.
   --  If ASLR is enabled, it will be used here.
   --  @param Process Process to reroll.
   procedure Reassign_Process_Addresses (Process : PID)
      with Pre => Process /= Error_PID;

   --  Get the process's default niceness.
   procedure Get_Niceness (Process : PID; Nice : out Scheduler.Niceness)
      with Pre => Process /= Error_PID;

   --  Set the process's default niceness.
   procedure Set_Niceness (Process : PID; Nice : Scheduler.Niceness)
      with Pre => Process /= Error_PID;

   --  Get a mark to know whether the process has been vforked.
   procedure Pop_VFork_Marker (Process : PID; Marked : out Boolean)
      with Pre => Process /= Error_PID;

   --  Set a mark to know whether the process has been vforked.
   procedure Set_VFork_Marker (Process : PID)
      with Pre => Process /= Error_PID;

   --  Get a mark to know whether the process has exec'd.
   procedure Get_Exec_Marker (Process : PID; Marked : out Boolean)
      with Pre => Process /= Error_PID;

   --  Set a mark to know whether the process has exec'd.
   procedure Set_Exec_Marker (Process : PID)
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

   --  Duplicate an individual file.
   --  @param File   File to duplicate.
   --  @param Result Duplicated file, or null if error'd out.
   procedure Duplicate
      (F      : File_Description_Acc;
       Result : out File_Description_Acc);

   --  Duplicate an entire process table to another process.
   --  @param Process Process to use.
   --  @param Target  Target process.
   procedure Duplicate_FD_Table (Process, Target : PID)
      with Pre => Process /= Error_PID and Target /= Error_PID;
   ----------------------------------------------------------------------------
   --  Shared memory segments attached to a process, by the address each is
   --  attached at, each counted in the segment's number of attachments. A
   --  vforked process that has not exec'd runs on its parent's map, so what it
   --  attaches and detaches there is its parent's.

   --  Record a segment attached at an address.
   --  @param Process Process to operate on.
   --  @param Address Address the segment is attached at.
   --  @param Segment Segment attached there.
   --  @param Success False if the process has as many attached as it may.
   procedure Add_SHM_Attachment
      (Process : PID;
       Address : Unsigned_64;
       Segment : IPC.SHM.Segment_ID;
       Success : out Boolean)
      with Pre => Process /= Error_PID and Segment /= IPC.SHM.Error_ID;

   --  Forget the segment attached at an address.
   --  @param Process Process to operate on.
   --  @param Address Address the segment is attached at.
   --  @param Segment Segment that was attached there, Error_ID if none was.
   procedure Remove_SHM_Attachment
      (Process : PID;
       Address : Unsigned_64;
       Segment : out IPC.SHM.Segment_ID)
      with Pre => Process /= Error_PID;

   --  Give the child of a fork the segments attached to its parent, which the
   --  copy of the parent's map keeps mapped, counting each attachment.
   --  @param Process Parent.
   --  @param Target  Child.
   procedure Duplicate_SHM_Attachments (Process, Target : PID)
      with Pre => Process /= Error_PID and Target /= Error_PID;

   --  Detach every segment attached to a process whose map is going away.
   --  @param Process Process to operate on.
   procedure Detach_All_SHM (Process : PID) with Pre => Process /= Error_PID;

   --  Close and free an individual file.
   --  @param F File to operate on.
   procedure Close (F : in out File_Description_Acc);

   --  Get a file held on a file descriptor. It comes with a reference of its
   --  own, for Put_File to drop once done with it, so that the descriptor
   --  being closed meanwhile does not free it.
   --  @param Process Process to operate on.
   --  @param FD      FD to fetch.
   --  @param File    Returned file, or null in failure.
   procedure Get_File
      (Process : PID;
       FD      : Unsigned_64;
       File    : out File_Description_Acc)
      with Pre => Process /= Error_PID;

   --  Drop the reference Get_File took, if any, closing the file if that
   --  was the last one.
   --  @param File File to drop, null afterwards.
   procedure Put_File (File : in out File_Description_Acc);

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
   --  @param Success True if FD held a file, which is now closed.
   procedure Remove_File
      (Process : PID;
       FD      : Unsigned_64;
       Success : out Boolean)
      with Pre => Process /= Error_PID;

   --  Remove all files from a process.
   --  @param Process Process to operate on.
   procedure Flush_Files (Process : PID) with Pre => Process /= Error_PID;

   --  Remove all cloexec files from a process.
   --  @param Process Process to operate on.
   procedure Flush_Exec_Files (Process : PID) with Pre => Process /= Error_PID;

   --  Destroy every virtual machine the process owns: at its exec, before
   --  the old image goes, as Exit_Process does at its exit.
   --  @param Process Process to operate on.
   procedure Destroy_Machines (Process : PID) with Pre => Process /= Error_PID;

   --  Set the virtual map associated with the process.
   --  @param Proc Process to operate on.
   --  @param Map  Map to assign.
   procedure Set_Common_Map (Proc : PID; Map : Memory.MMU.Page_Table_Acc)
      with Pre => Proc /= Error_PID;

   --  Get the virtual map associated with the process.
   --  @param Proc Process to operate on.
   --  @return The map.
   procedure Get_Common_Map (Proc : PID; Map : out Memory.MMU.Page_Table_Acc)
      with Pre => Proc /= Error_PID;

   --  Bump the alloc base of the process by the passed length.
   --  @param P        Process to operate on.
   --  @param Length   Length to bump to.
   --  @param Previous Previous base.
   procedure Bump_Alloc_Base
      (P        : PID;
       Length   : Unsigned_64;
       Previous : out Unsigned_64)
      with Pre => P /= Error_PID;

   --  Get the amount of mapped memory a process has.
   --  @param Proc   Process to operate on.
   --  @param Size   Mapped size;
   procedure Get_User_Mapped_Size (P : PID; Size : out Unsigned_64)
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


   --  Check whether a process' children exited.
   --  @param Process     Process to check.
   --  @param Exited_Proc Process that exited, or Error_PID if none.
   --- @param Did_Exit    True if exited, False if not.
   --  @param Code        Exit code, if it exited.
   --  @param Was_Signal  True if the exit was caused by a signal.
   --  @param Sig         The signal that caused termination, if any.
   procedure Check_Children_Exit
      (Process     : PID;
       Exited_Proc : out PID;
       Did_Exit    : out Boolean;
       Code        : out Unsigned_8;
       Was_Signal  : out Boolean;
       Sig         : out Signal)
      with Pre => Process /= Error_PID;

   --  Check whether a process' children exited.
   --  @param Process     Process to check.
   --  @param Group       Check for this group.
   --  @param Exited_Proc Process that exited, or Error_PID if none.
   --- @param Did_Exit    True if exited, False if not.
   --  @param Code        Exit code, if it exited.
   --  @param Was_Signal  True if the exit was caused by a signal.
   --  @param Sig         The signal that caused termination, if any.
   procedure Check_Children_Group_Exit
      (Process     : PID;
       Group       : Unsigned_32;
       Exited_Proc : out PID;
       Did_Exit    : out Boolean;
       Code        : out Unsigned_8;
       Was_Signal  : out Boolean;
       Sig         : out Signal)
      with Pre => Process /= Error_PID;

   --  Reassign children to init.
   --  @param Process Parent process to reassign from.
   procedure Reassign_Parent_To_Init (Process : PID)
      with Pre => Process /= Error_PID;

   --  Terminate process, if it is the caller one, this function will not
   --  return. Its other threads are waited for to leave the kernel first,
   --  and nothing is done if the process is already being terminated.
   --  @param Process Parent process to reassign from.
   --  @param Killer  Signal that ended up killing this process.
   procedure Exit_Process (Process : PID; Killer : Signal)
      with Pre => Process /= Error_PID;

   --  Terminate process, if it is the caller one, this function will not
   --  return. Its other threads are waited for to leave the kernel first,
   --  and nothing is done if the process is already being terminated.
   --  @param Process Parent process to reassign from.
   --  @param Code    Code to exit the process with.
   procedure Exit_Process (Process : PID; Code : Unsigned_8)
      with Pre => Process /= Error_PID;

   --  Set the current working directory.
   --  @param Proc    Process to set the CWD of.
   --  @param CWD     Path to set, it is not checked.
   --  @param Success True if successful, False if it did not fit, or others.
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

   --  Give back the address space of a process that has exited, which it
   --  can never touch again.
   --  @param Proc Process to operate on.
   procedure Drop_Address_Space (Proc : PID) with Pre => Proc /= Error_PID;

   --  A vforked child that has not called exec runs on its parent's table.
   --  The parent can be killed while the child still runs on it. This hands
   --  over the tables between processes.
   --  @param Map   Table its owner is done with.
   --  @param Taken True if a child took it, else it is the caller's to free.
   procedure Hand_Table_On
      (Map   : Memory.MMU.Page_Table_Acc;
       Taken : out Boolean);

   --  Get the parent PID of the process.
   --  @param Proc   Process to get the info from.
   --  @param Parent PID of the parent, or Error_PID if no parent.
   procedure Get_Parent (Proc : PID; Parent : out PID)
      with Pre => Proc /= Error_PID;

   --  Set the parent PID of the process.
   --  @param Proc   Process to get the info from.
   --  @param Parent PID of the parent, or Error_PID if no parent.
   procedure Set_Parent (Proc : PID; Parent : PID)
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

   --  Get the process group id.
   --  @param Proc Process to get the PGID of.
   --  @param PGID Group ID.
   procedure Get_PGID (Proc : PID; PGID : out Unsigned_32);

   --  Set the process group id.
   --  @param Proc Process to set the PGID of.
   --  @param PGID Group ID.
   procedure Set_PGID (Proc : PID; PGID : Unsigned_32);

   --  Get the process group id.
   --  @param Proc Process to get the PGID of.
   --  @param PGID Group ID.
   procedure Get_Session_ID (Proc : PID; ID : out Unsigned_32);

   --  Set the process group id.
   --  @param Proc Process to set the PGID of.
   --  @param PGID Group ID.
   procedure Create_Session (Proc : PID; Success : out Boolean);

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

   --  The signals a thread has blocked.
   --  @param Thread Thread to operate on.
   --  @param Sig    Signal bitmap, true meaning that signal is blocked.
   procedure Get_Thread_Mask (Thread : Scheduler.TID; Sig : out Signal_Bitmap);
   procedure Set_Thread_Mask (Thread : Scheduler.TID; Sig : Signal_Bitmap);


   --  A pending signal belongs to a thread as well as to a process: one sent
   --  to a thread waits for that thread alone, and one sent to the process
   --  may be taken by any thread that does not block it. Both sets are kept,
   --  and delivery reads the thread's first. A thread's set is emptied when
   --  its number is handed to a new thread, which is the one place a TID can
   --  start meaning something else.
   --  @param Thread Thread whose pending signals to forget.
   procedure Clear_Thread_Signals (Thread : Scheduler.TID);

   --  Hand a thread's pending signals over to its process, which is what
   --  exec does with the calling thread's: the initial thread of the new
   --  image inherits them (POSIX.1-2024, exec), and exec gives the new image
   --  a thread of its own, so what waits on the caller moves or is lost. It
   --  moves to the process, as the new image has one thread, the only one
   --  that can take it. The signals of a thread that only exits go with it,
   --  as no image of it continues.
   --  @param Proc   The thread's process.
   --  @param Thread Thread whose pending signals the process takes over.
   procedure Absorb_Thread_Signals (Proc : PID; Thread : Scheduler.TID);

   --  What sigpending answers: the signals blocked from delivery to this
   --  thread that are pending on it or on its process (POSIX.1-2024,
   --  sigpending).
   --  @param Proc   The thread's process.
   --  @param Thread Thread asking.
   --  @param Sig    Pending and blocked, true meaning that signal is both.
   procedure Get_Pending_Signals
      (Proc   : PID;
       Thread : Scheduler.TID;
       Sig    : out Signal_Bitmap);

   --  Whether the process has a signal pending that a thread blocking the
   --  passed set could take. It is what makes a wait give up: a caught
   --  signal is delivered on the way out of the call, so the call has to get
   --  there, and it answers EINTR.
   --  @param Proc   Process to ask about, which may be no process at all.
   --  @param Thread Thread asking, whose own pending signals count too.
   --  @param Masked What the asking thread blocks, as the bitmap userland
   --                passes, bit N - 1 standing for signal N.
   function Has_Deliverable_Signal
      (Proc   : PID;
       Thread : Scheduler.TID;
       Masked : Unsigned_64) return Boolean;

   --  Get the key to wait on for signals raised to a process.
   --  @param Proc Process to get the key of.
   --  @return The key, see Scheduler.Add_Wait_Key, or System.Null_Address if
   --          the process does not exist.
   function Get_Wait_Key (Proc : PID) return System.Address;

   --  Raise a signal to a process group.
   --  @param Proc   Process to raise for.
   --  @param Sig    Signal to raise.
   --  @param Origin Where the signal came from.
   procedure Raise_Signal
      (Proc   : PID;
       Sig    : Signal;
       Origin : Signal_Origin := Kernel_Origin);

   --  Raise a signal to a process group.
   --  @param Sig    Signal to raise.
   --  @param Group  Group to send the signal to.
   procedure Raise_Signal
      (Process    : PID;
       Sig        : Signal;
       Sender_UID : Unsigned_32;
       Bypass_UID : Boolean;
       Origin     : Signal_Origin := Kernel_Origin)
      with Pre => Process /= Error_PID;

   --  Raise a signal for one thread of a process rather than for the
   --  process, which is what pthread_kill does. The signal waits for that
   --  thread alone, but the action is still the process's: a default action
   --  that ends or stops ends or stops the whole of it (POSIX.1-2024,
   --  pthread_kill).
   --  @param Thread Thread to raise for.
   --  @param Sig    Signal to raise.
   --  @param Origin Where the signal came from.
   --  @param Found  False when there is no such thread, which is ESRCH.
   procedure Raise_Signal
      (Thread : Scheduler.TID;
       Sig    : Signal;
       Origin : Signal_Origin;
       Found  : out Boolean);

   procedure Raise_Signal
      (Sig        : Signal;
       Sender_UID : Unsigned_32;
       Bypass_UID : Boolean;
       Group      : Unsigned_32;
       Origin     : Signal_Origin := Kernel_Origin);

   --  Queue a signal to a process, which is what sigqueue does. It is raised
   --  as it would be otherwise, with the same handler and default action,
   --  and its siginfo is kept as well, so that several occurrences of one
   --  signal each reach the process with their own value, in the order sent.
   --  @param Proc    Process to queue for.
   --  @param Sig     Signal to queue.
   --  @param Origin  Where it came from, si_value included.
   --  @param Success False when the queue is full, which is [EAGAIN].
   procedure Queue_Signal
      (Proc    : PID;
       Sig     : Signal;
       Origin  : Signal_Origin;
       Success : out Boolean);

   --  Take a pending signal without running anything, which is what
   --  sigwaitinfo and sigtimedwait do: the caller has the signals blocked,
   --  so nothing can deliver them, and this accepts one and reports what it
   --  was sent with. The thread's own pending set is looked at first, as
   --  delivery does, and the queue is drained in order.
   --  @param Proc   Process of the asking thread.
   --  @param Thread Thread asking, whose own pending signals count too.
   --  @param Wanted Signals the caller will accept, true meaning accept.
   --  @param Sig    The signal accepted, meaningless unless Got.
   --  @param Origin What it was sent with, meaningless unless Got.
   --  @param Got    True if a signal was accepted, False if none was
   --                pending at all.
   procedure Accept_Signal
      (Proc   : PID;
       Thread : Scheduler.TID;
       Wanted : Signal_Bitmap;
       Sig    : out Signal;
       Origin : out Signal_Origin;
       Got    : out Boolean);

   --  Get the address assigned for a process to use as a signal handler.
   --  @param Proc       Process to get the handler for.
   --  @param Sig        Signal to get the handler for.
   --  @param Is_Default True if the handler is default action.
   --  @param Is_Ignored True if the handler is ignored.
   --  @param Handler    Address set for the handler.
   --  @param Restorer   Address set for the restorer function.
   --  @param Mask       sa_mask of the action.
   --  @param Flags      sa_flags of the action.
   procedure Get_Signal_Handlers
      (Proc       : PID;
       Sig        : Signal;
       Is_Default : out Boolean;
       Is_Ignored : out Boolean;
       Handler    : out System.Address;
       Restorer   : out System.Address;
       Mask       : out Unsigned_64;
       Flags      : out Unsigned_32);

   --  Set an address for a process to use as a signal handler.
   --  @param Proc       Process to set the handler for.
   --  @param Sig        Signal to set the handlers for.
   --  @param Is_Default Ignore the Handler field, use default actions.
   --  @param Is_Ignored Ignore the Handler field, never raise this signal.
   --  @param Handler    Address set for the handler.
   --  @param Restorer   Address set for the restorer function.
   --  @param Mask       sa_mask of the action.
   --  @param Flags      sa_flags of the action.
   --  @param Success    True if the passed signal was changed.
   procedure Set_Signal_Handlers
      (Proc       : PID;
       Sig        : Signal;
       Is_Default : Boolean;
       Is_Ignored : Boolean;
       Handler    : System.Address;
       Restorer   : System.Address;
       Mask       : Unsigned_64;
       Flags      : Unsigned_32;
       Success    : out Boolean);

   --  Get a raised signal for a process.
   --  Getting a signal will mask it.
   --  @param Proc     Process to fetch signals for.
   --  @param Thread   Thread taking the signal, whose own pending signals
   --                  count too.
   --  @param Masked   What the taking thread has blocked, which is what
   --                  decides whether a pending signal can be taken at all.
   --  @param Sig      Raised signal.
   --  @param Handler  Address of the handler, Null_Address if not registered.
   --  @param Restorer Address of the restorer.
   --  @param No_Sig   The process has no more signals to process.
   --  @param Sig_Mask The action's sa_mask, blocked while its handler runs.
   --  @param Flags    The action's sa_flags.
   procedure Get_Raised_Signal_Actions
      (Proc     : PID;
       Thread   : Scheduler.TID;
       Masked   : Signal_Bitmap;
       Sig      : out Signal;
       Handler  : out System.Address;
       Restorer : out System.Address;
       No_Sig   : out Boolean;
       Sig_Mask : out Unsigned_64;
       Flags    : out Unsigned_32;
       Origin   : out Signal_Origin);

   procedure Get_Default_Policy (Proc : PID; Pol : out Scheduler.Policy);
   procedure Set_Default_Policy (Proc : PID; Pol : Scheduler.Policy);

   procedure Get_Priority (Proc : PID; Prio : out Scheduler.Priority);
   procedure Set_Priority (Proc : PID; Prio : Scheduler.Priority);

   procedure Get_RR_Interval (Proc : PID; Sec, NS : out Unsigned_64);
   procedure Set_RR_Interval (Proc : PID; Sec, NS : Unsigned_64);

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

   Default_Umask : constant VFS.File_Mode := 8#22#;

   type File_Descriptor is record
      Close_On_Exec : Boolean;
      Close_On_Fork : Boolean;
      Description   : File_Description_Acc;
   end record;

   type Signal_Handlers is record
      Is_Ignored    : Boolean;
      Handler_Addr  : System.Address;
      Restorer_Addr : System.Address;
      Mask          : Unsigned_64;
      Flags         : Unsigned_32;
   end record;

   Max_File_Count : constant Natural := 1024;
   type File_Arr     is array (0 .. Max_File_Count - 1) of File_Descriptor;
   type File_Arr_Acc is access File_Arr;

   type Thread_Arr is array (1 .. Max_Thread_Count)   of Scheduler.TID;
   type Handle_Arr is array (Signal)                  of Signal_Handlers;
   type Origin_Arr is array (Signal)                  of Signal_Origin;

   --  One occurrence of a queued signal. The queue is kept in FIFO order
   --  as a plain array with a count, so a take is a shift of what is left:
   --  it holds at most 32 entries, and nothing about it is hot.
   type Queued_Signal is record
      Sig    : Signal;
      Origin : Signal_Origin;
   end record;
   type Queued_Signal_Arr is array (1 .. Max_Queued_Signals)
      of Queued_Signal;

   --  At most this many attachments at once (POSIX.1-2024, shmat [EMFILE]);
   --  a free slot names no segment.
   Max_SHM_Attachments : constant := 32;
   type SHM_Attachment is record
      Address : Unsigned_64;
      Segment : IPC.SHM.Segment_ID;
   end record;
   type SHM_Attachment_Arr is array (1 .. Max_SHM_Attachments)
      of SHM_Attachment;

   --  The process whose map a process runs on.
   function Map_Owner (Process : PID) return PID;

   --  Release the locks a process holds on the file of a description, as a
   --  descriptor for it is closed.
   procedure Release_Locks (Process : PID; F : File_Description_Acc);

   type Process_Data is record
      Data_Mutex      : aliased Synchronization.Mutex;
      Controlling_TTY : IPC.PTY.Inner_Acc;
      Raised_Signals  : Signal_Bitmap;
      --  The same set as a word, bit N - 1 standing for signal N, kept
      --  beside it so that it can be read without the mutex: the scheduler
      --  asks whether a sleeping thread has anything to take, and it asks
      --  from inside its own critical section.
      Pending_Bits    : Unsigned_64 with Atomic;
      Signal_Origins  : Origin_Arr;
      Queued_Signals  : Queued_Signal_Arr;
      Queued_Count    : Natural range 0 .. Max_Queued_Signals;
      Signal_Handlers : Handle_Arr;
      Prio            : Scheduler.Priority;
      Niceness        : Scheduler.Niceness;
      Pol             : Scheduler.Policy;
      Umask           : VFS.File_Mode;
      User            : Unsigned_32;
      Effective_User  : Unsigned_32;
      Group           : Unsigned_32;
      Effective_Group : Unsigned_32;
      Process_Group   : Unsigned_32;
      Session_ID      : Unsigned_32;
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
      File_Table      : File_Arr_Acc;
      SHM_Attachments : SHM_Attachment_Arr;
      Common_Map      : Memory.MMU.Page_Table_Acc;
      Alloc_Base      : Unsigned_64;
      Perms           : MAC.Context;
      Signal_Exit     : Boolean;
      Which_Signal    : Signal;
      VFork_Mark      : Boolean;
      Exec_Mark       : Boolean;
      Did_Exit        : Boolean;
      Exit_Code       : Unsigned_8;
      Is_Flushing     : Boolean;
      Is_Exiting      : Boolean;
      Children_System : Time.Timestamp;
      Children_User   : Time.Timestamp;
      Creation        : Time.Timestamp;
      RR_Sec          : Unsigned_64;
      RR_NS           : Unsigned_64;
   end record;
   type Process_Data_Acc is access Process_Data;
   type Process_Arr     is array (PID range 1 .. PID'Last) of Process_Data_Acc;
   type Process_Arr_Acc  is access Process_Arr;

   Default_Signal_Handlers : constant Handle_Arr :=
      [others => (False, System.Null_Address, System.Null_Address, 0, 0)];

   --  A thread's own pending signals, beside its process's. They live here
   --  and not in the scheduler because the scheduler must know nothing of
   --  what a signal is (this spec withs it, so a signal type there would be
   --  a cycle), and everything that decides what a pending signal means is
   --  here already.
   type Thread_Signals is record
      Raised  : Signal_Bitmap;
      Origins : Origin_Arr;
      --  The same set as a word, kept beside it so that it can be read
      --  with no lock at all, exactly as Process_Data.Pending_Bits is and
      --  for the same caller: the scheduler, from inside its own critical
      --  section, asking whether a sleeping thread has anything to take.
      Pending : Unsigned_64 with Atomic;
   end record;
   type Thread_Signal_Arr is array
      (Natural range 1 .. Scheduler.Max_Thread_Number) of Thread_Signals;
   type Thread_Signal_Arr_Acc is access Thread_Signal_Arr;

   Thread_Sigs    : Thread_Signal_Arr_Acc := null;

   --  Empty every thread's pending set, once, when the table is made. It is
   --  its own routine because the aggregate carries a check and Init has no
   --  handler to cover one.
   procedure Clear_All_Thread_Signals;

   --  Throw a pending signal away wherever it may be waiting: the process's
   --  own set and that of every thread of the process. The caller holds the
   --  process's Data_Mutex, which is what the thread list and every thread's
   --  pending set are written under everywhere.
   --  @param Proc Process whose threads are to forget the signal.
   --  @param Sig  Signal to forget.
   procedure Discard_Thread_Signal (Proc : PID; Sig : Signal);

   --  Take the first queued occurrence of a signal off the process's queue.
   --  The caller holds Data_Mutex.
   --  @param Proc   Process to take from.
   --  @param Sig    Signal to take an occurrence of.
   --  @param Origin What that occurrence was sent with.
   --  @param Found  True if there was one at all.
   --  @param More   True if another occurrence of the same signal is still
   --                queued, which keeps it pending (POSIX.1-2024, XSH 2.4.2).
   procedure Take_Queued
      (Proc   : PID;
       Sig    : Signal;
       Origin : out Signal_Origin;
       Found  : out Boolean;
       More   : out Boolean);

   --  Throw away every queued occurrence of a signal, which a discard must
   --  do as well as clearing the pending bit (POSIX.1-2024, XSH 2.4.3). The
   --  caller holds Data_Mutex.
   --  @param Proc Process to drop them from.
   --  @param Sig  Signal to drop every occurrence of.
   procedure Drop_Queued (Proc : PID; Sig : Signal);

   Registry       : Process_Arr_Acc := null;
   Registry_Mutex : aliased Synchronization.Mutex :=
      Synchronization.Unlocked_Mutex;
end Userland.Process;
