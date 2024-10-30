--  userland-process.adb: Process registry, PIDs, and all the fuzz.
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

with Lib.Time;
with Lib.Alignment;
with Lib.Messages;
with Ada.Unchecked_Deallocation;
with Arch.Local;
with Cryptography.Random;
with Userland.Memory_Locations;
with IPC.FileLock;

package body Userland.Process is
   procedure Free is new Ada.Unchecked_Deallocation
      (Process_Data, Process_Data_Acc);

   Do_ASLR : Boolean := True;

   procedure Init is
   begin
      Registry := new Process_Arr'(others => null);
      Lib.Synchronization.Release (Registry_Mutex);
   end Init;

   procedure Disable_ASLR is
   begin
      Do_ASLR := False;
   end Disable_ASLR;

   procedure Get_Children
      (Proc : PID;
       Buf  : out Children_Arr;
       Len  : out Natural)
   is
      Index : Natural := 0;
   begin
      Buf := (others => Error_PID);
      Len := 0;
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry.all'Range loop
         if Registry (I) /= null and then Registry (I).Parent = Proc
         then
            Len := Len + 1;
            if Index < Buf'Length then
               Buf (Buf'First + Index) := I;
               Index                   := Index + 1;
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
   end Get_Children;

   procedure List_All (List : out Process_Info_Arr; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
      List :=
         (others =>
            (Identifier      => (others => ' '),
             Identifier_Len  => 0,
             Process         => Error_PID,
             Parent          => 0,
             User            => 0,
             Is_Being_Traced => False,
             Has_Exited      => False));
      Total := 0;

      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry.all'Range loop
         if Registry (I) /= null then
            Total := Total + 1;
            if Curr_Index < List'Length then
               Lib.Synchronization.Seize (Registry (I).Data_Mutex);
               List (List'First + Curr_Index) :=
                  (Identifier      => Registry (I).Identifier (1 .. 20),
                   Identifier_Len  => Registry (I).Identifier_Len,
                   Process         => I,
                   Parent          => Registry (I).Parent,
                   User            => Registry (I).User,
                   Is_Being_Traced => Registry (I).Is_Traced,
                   Has_Exited      => Registry (I).Did_Exit);
               Curr_Index := Curr_Index + 1;
               Lib.Synchronization.Release (Registry (I).Data_Mutex);
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
   end List_All;

   procedure Create_Process (Parent : PID; Returned : out PID) is
      P : PID renames Parent;
   begin
      Returned := Error_PID;

      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry.all'Range loop
         if Registry (I) = null then
            Registry (I) := new Process_Data'
               (Data_Mutex      => Lib.Synchronization.Unlocked_Semaphore,
                Masked_Signals  => (others => False),
                Raised_Signals  => (others => False),
                Signal_Handlers => (others => System.Null_Address),
                Niceness        => Scheduler.Default_Niceness,
                Umask           => Default_Umask,
                User            => 0,
                Effective_User  => 0,
                Group           => 0,
                Effective_Group => 0,
                SGroup_Count    => 0,
                SGroups         => (others => 0),
                Identifier      => (others => ' '),
                Identifier_Len  => 0,
                Parent          => 0,
                Is_Traced       => False,
                Tracer_FD       => 0,
                Current_Dir_FS  => VFS.Error_Handle,
                Current_Dir_Ino => 0,
                Thread_List     => (others => Error_TID),
                File_Table      => (others => (False, False, null)),
                Common_Map      => null,
                Stack_Base      => 0,
                Alloc_Base      => 0,
                Perms           => MAC.Default_Context,
                Signal_Exit     => False,
                Which_Signal    => Signal_Kill,
                Did_Exit        => False,
                Exit_Code       => 0,
                others          => 0);

            if Parent /= Error_PID then
               Lib.Synchronization.Seize (Registry (P).Data_Mutex);
               Registry (I).Niceness        := Registry (P).Niceness;
               Registry (I).Masked_Signals  := Registry (P).Masked_Signals;
               Registry (I).Parent          := Parent;
               Registry (I).Stack_Base      := Registry (P).Stack_Base;
               Registry (I).Alloc_Base      := Registry (P).Alloc_Base;
               Registry (I).Current_Dir_FS  := Registry (P).Current_Dir_FS;
               Registry (I).Current_Dir_Ino := Registry (P).Current_Dir_Ino;
               Registry (I).Perms           := Registry (P).Perms;
               Registry (I).User            := Registry (P).User;
               Registry (I).Effective_User  := Registry (P).Effective_User;
               Registry (I).Group           := Registry (P).Group;
               Registry (I).Effective_Group := Registry (P).Effective_Group;
               Registry (I).SGroup_Count    := Registry (P).SGroup_Count;
               Registry (I).SGroups         := Registry (P).SGroups;
               Registry (I).Umask           := Registry (P).Umask;
               Lib.Synchronization.Release (Registry (P).Data_Mutex);
            else
               Reassign_Process_Addresses (PID (I));
            end if;

            Returned := PID (I);
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
   end Create_Process;

   procedure Delete_Process (Process : PID) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Lib.Synchronization.Seize (Registry_Mutex);
      Free (Registry (Process));
      Lib.Synchronization.Release (Registry_Mutex);
   end Delete_Process;

   procedure Get_Runtime_Times
      (Proc : PID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64)
   is
      Temp1, Temp2, Temp3, Temp4 : Unsigned_64;
   begin
      System_Seconds := 0;
      System_Nanoseconds := 0;
      User_Seconds := 0;
      User_Nanoseconds := 0;

      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);

      for Th of Registry (Proc).Thread_List loop
         if Th /= Error_TID then
            Scheduler.Get_Runtime_Times (Th, Temp1, Temp2, Temp3, Temp4);
            System_Seconds := System_Seconds + Temp1;
            System_Nanoseconds := System_Nanoseconds + Temp2;
            User_Seconds := User_Seconds + Temp3;
            User_Nanoseconds := User_Nanoseconds + Temp4;
         end if;
      end loop;

      Lib.Time.Normalize (System_Seconds, System_Nanoseconds);
      Lib.Time.Normalize (User_Seconds, User_Nanoseconds);

      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Runtime_Times;

   procedure Get_Children_Runtimes
      (Proc : PID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      System_Seconds := Registry (Proc).Children_SSec;
      System_Nanoseconds := Registry (Proc).Children_SNSec;
      User_Seconds := Registry (Proc).Children_USec;
      User_Nanoseconds := Registry (Proc).Children_UNSec;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Children_Runtimes;

   procedure Add_Thread
      (Proc    : PID;
       Thread  : Scheduler.TID;
       Success : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Success := False;
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = Error_TID then
            Registry (Proc).Thread_List (I) := Thread;
            Scheduler.Set_Niceness (Thread, Registry (Proc).Niceness);
            Success := True;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Add_Thread;

   function Get_Thread_Count (Process : PID) return Natural is
      Returned : Natural := 0;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      for I in Registry (Process).Thread_List'Range loop
         if Registry (Process).Thread_List (I) /= Error_TID then
            Returned := Returned + 1;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
      return Returned;
   end Get_Thread_Count;

   procedure Remove_Thread (Proc : PID; Thread : Scheduler.TID) is
      Temp1, Temp2, Temp3, Temp4 : Unsigned_64;
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = Thread then
            Registry (Proc).Thread_List (I) := Error_TID;
            Scheduler.Get_Runtime_Times (Thread, Temp1, Temp2, Temp3, Temp4);
            if Registry (Proc).Parent /= Error_PID and then
               Registry (Registry (Proc).Parent) /= null
            then
               Lib.Time.Increment
                  (Registry (Registry (Proc).Parent).Children_SSec,
                   Registry (Registry (Proc).Parent).Children_SNSec,
                   Temp1, Temp2);
               Lib.Time.Increment
                  (Registry (Registry (Proc).Parent).Children_USec,
                   Registry (Registry (Proc).Parent).Children_UNSec,
                   Temp3, Temp4);
            end if;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Remove_Thread;

   procedure Flush_Threads (Proc : PID) is
      Current_Thread : constant TID := Arch.Local.Get_Current_Thread;
      T1, T2, T3, T4 : Unsigned_64;
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      for Thread of Registry (Proc).Thread_List loop
         if Thread /= Error_TID then
            if Registry (Proc).Parent /= Error_PID and then
               Registry (Registry (Proc).Parent) /= null
            then
               Scheduler.Get_Runtime_Times (Thread, T1, T2, T3, T4);

               Lib.Time.Increment
                  (Registry (Registry (Proc).Parent).Children_SSec,
                   Registry (Registry (Proc).Parent).Children_SNSec, T1, T2);
               Lib.Time.Increment
                  (Registry (Registry (Proc).Parent).Children_USec,
                   Registry (Registry (Proc).Parent).Children_UNSec, T3, T4);
            end if;

            if Thread /= Current_Thread then
               Scheduler.Delete_Thread (Thread);
            end if;
         end if;
         Thread := Error_TID;
      end loop;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Flush_Threads;

   procedure Reassign_Process_Addresses (Process : PID) is
      package Aln is new Lib.Alignment (Unsigned_64);
      Rand_Addr, Rand_Jump : Unsigned_64;
   begin
      if Do_ASLR then
         Cryptography.Random.Get_Integer
            (Memory_Locations.Mmap_Anon_Min,
             Memory_Locations.Mmap_Anon_Max,
             Rand_Addr);
         Cryptography.Random.Get_Integer
            (Memory_Locations.Stack_Jump_Min,
             Memory_Locations.Stack_Jump_Max,
             Rand_Jump);

         Rand_Addr := Aln.Align_Up (Rand_Addr, Arch.MMU.Page_Size);
         Rand_Jump := Aln.Align_Up (Rand_Jump, Arch.MMU.Page_Size);
      else
         Rand_Addr := Memory_Locations.Mmap_Anon_Min;
         Rand_Jump := Memory_Locations.Stack_Jump_Min;
      end if;

      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Alloc_Base := Rand_Addr;
      Registry (Process).Stack_Base := Rand_Addr + Rand_Jump;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Reassign_Process_Addresses;

   function Get_Niceness (Process : PID) return Scheduler.Niceness is
      Result : Scheduler.Niceness;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Result := Registry (Process).Niceness;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
      return Result;
   end Get_Niceness;

   procedure Set_Niceness (Process : PID; Nice : Scheduler.Niceness) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Niceness := Nice;
      for Thread of Registry (Process).Thread_List loop
         if Thread /= Error_TID then
            Scheduler.Set_Niceness (Thread, Nice);
         end if;
      end loop;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Set_Niceness;

   function Is_Valid_File (Process : PID; FD : Unsigned_64) return Boolean is
      R : Boolean;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         R := Registry (Process).File_Table (Natural (FD)).Description /= null;
      else
         R := False;
      end if;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
      return R;
   end Is_Valid_File;

   procedure Add_File
      (Process : PID;
       File    : File_Description_Acc;
       FD      : out Natural;
       Success : out Boolean;
       Start   : Natural := 0)
   is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      for I in Start .. Registry (Process).File_Table'Last loop
         if Registry (Process).File_Table (I).Description = null then
            Registry (Process).File_Table (I).Description := File;
            FD      := I;
            Success := True;
            goto Cleanup;
         end if;
      end loop;
      FD      := 0;
      Success := False;
   <<Cleanup>>
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Add_File;

   function Get_File_Count (Process : PID) return Natural is
      Count_Of_FDs : Natural := 0;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      for Ent of Registry (Process).File_Table loop
         if Ent.Description = null then
            Count_Of_FDs := Count_Of_FDs + 1;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
      return Count_Of_FDs;
   end Get_File_Count;

   procedure Duplicate
      (F      : File_Description_Acc;
       Result : out File_Description_Acc)
   is
      pragma SPARK_Mode (Off);
   begin
      if F.Children_Count /= Natural'Last then
         F.Children_Count := F.Children_Count + 1;
         Result := F;
      else
         Result := null;
      end if;
   end Duplicate;

   procedure Duplicate_FD_Table
      (Process : PID;
       Target  : PID;
       Max_FD  : Natural := Max_File_Count)
   is
      Src : File_Arr renames Registry (Process).File_Table;
      Tgt : File_Arr renames Registry (Target).File_Table;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Lib.Synchronization.Seize (Registry (Target).Data_Mutex);

      for I in 0 .. Max_FD - 1 loop
         if not Src (I).Close_On_Fork then
            Tgt (I) := Src (I);
            if Src (I).Description /= null then
               Duplicate (Src (I).Description, Tgt (I).Description);
            end if;
         end if;
      end loop;

      Lib.Synchronization.Release (Registry (Target).Data_Mutex);
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Duplicate_FD_Table;

   procedure Close (F : in out File_Description_Acc) is
      procedure Free is new Ada.Unchecked_Deallocation
         (File_Description, File_Description_Acc);
      T : Boolean;
   begin
      if F.Children_Count = 0 then
         case F.Description is
            when Description_Reader_FIFO => Close_Reader (F.Inner_Reader_FIFO);
            when Description_Writer_FIFO => Close_Writer (F.Inner_Writer_FIFO);
            when Description_Primary_PTY => Close (F.Inner_Primary_PTY);
            when Description_Secondary_PTY => Close (F.Inner_Secondary_PTY);
            when Description_Inode =>
               if F.Inner_Is_Locked then
                  IPC.FileLock.Release_Lock (F.Inner_Ino_FS, F.Inner_Ino, T);
                  if not T then
                     Lib.Messages.Put_Line ("Missing file lock on closure!");
                  end if;
               end if;
               VFS.Close (F.Inner_Ino_FS, F.Inner_Ino);
            when Description_Socket => Close (F.Inner_Socket);
         end case;
         Free (F);
      else
         F.Children_Count := F.Children_Count - 1;
      end if;
      F := null;
   end Close;

   function Get_File
      (Process : PID;
       FD      : Unsigned_64) return File_Description_Acc
   is
      Res : File_Description_Acc;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         Res := Registry (Process).File_Table (Natural (FD)).Description;
      else
         Res := null;
      end if;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
      return Res;
   end Get_File;

   procedure Get_FD_Flags
      (Process       : PID;
       FD            : Unsigned_64;
       Close_On_Exec : out Boolean;
       Close_On_Fork : out Boolean)
   is
      Table : File_Arr renames Registry (Process).File_Table;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         Close_On_Exec := Table (Natural (FD)).Close_On_Exec;
         Close_On_Fork := Table (Natural (FD)).Close_On_Fork;
      else
         Close_On_Exec := False;
         Close_On_Fork := False;
      end if;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Get_FD_Flags;

   procedure Set_FD_Flags
      (Process       : PID;
       FD            : Unsigned_64;
       Close_On_Exec : Boolean;
       Close_On_Fork : Boolean)
   is
      Table : File_Arr renames Registry (Process).File_Table;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         Table (Natural (FD)).Close_On_Exec := Close_On_Exec;
         Table (Natural (FD)).Close_On_Fork := Close_On_Fork;
      end if;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Set_FD_Flags;

   procedure Remove_File (Process : PID; FD : Natural) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= File_Arr'Last then
         Close (Registry (Process).File_Table (FD).Description);
         Registry (Process).File_Table (FD).Close_On_Exec := False;
         Registry (Process).File_Table (FD).Close_On_Fork := False;
      end if;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Remove_File;

   procedure Flush_Files (Process : PID) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      for F of Registry (Process).File_Table loop
         if F.Description /= null then
            Close (F.Description);
         end if;
         F.Close_On_Exec := False;
         F.Close_On_Fork := False;
      end loop;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Flush_Files;

   procedure Flush_Exec_Files (Process : PID) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      for F of Registry (Process).File_Table loop
         if F.Description /= null and then F.Close_On_Exec then
            Close (F.Description);
            F.Close_On_Exec := False;
            F.Close_On_Fork := False;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Flush_Exec_Files;

   procedure Set_Common_Map (Proc : PID; Map : Arch.MMU.Page_Table_Acc) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Common_Map := Map;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Common_Map;

   function Get_Common_Map (Proc : PID) return Arch.MMU.Page_Table_Acc is
      Result : Arch.MMU.Page_Table_Acc;
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Result := Registry (Proc).Common_Map;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
      return Result;
   end Get_Common_Map;

   function Get_Stack_Base (Process : PID) return Unsigned_64 is
      Result : Unsigned_64;
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Result := Registry (Process).Stack_Base;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
      return Result;
   end Get_Stack_Base;

   procedure Set_Stack_Base (Process : PID; Base : Unsigned_64) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Stack_Base := Base;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Set_Stack_Base;

   procedure Get_Traced_Info
      (Process   : PID;
       Is_Traced : out Boolean;
       FD        : out Natural)
   is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Is_Traced := Registry (Process).Is_Traced;
      FD        := Registry (Process).Tracer_FD;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Get_Traced_Info;

   procedure Set_Traced_Info
      (Process   : PID;
       Is_Traced : Boolean;
       FD        : Natural)
   is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Is_Traced := Is_Traced;
      Registry (Process).Tracer_FD := FD;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Set_Traced_Info;

   procedure Issue_Exit (Process : PID; Code : Unsigned_8) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Did_Exit    := True;
      Registry (Process).Signal_Exit := False;
      Registry (Process).Exit_Code   := Code;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Issue_Exit;

   procedure Issue_Exit (Process : PID; Sig : Signal) is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Did_Exit     := True;
      Registry (Process).Signal_Exit  := True;
      Registry (Process).Which_Signal := Sig;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Issue_Exit;

   procedure Check_Exit
      (Process    : PID;
       Did_Exit   : out Boolean;
       Code       : out Unsigned_8;
       Was_Signal : out Boolean;
       Sig        : out Signal)
   is
   begin
      Lib.Synchronization.Seize (Registry (Process).Data_Mutex);
      Did_Exit   := Registry (Process).Did_Exit;
      Code       := Registry (Process).Exit_Code;
      Was_Signal := Registry (Process).Signal_Exit;
      Sig        := Registry (Process).Which_Signal;
      Lib.Synchronization.Release (Registry (Process).Data_Mutex);
   end Check_Exit;

   procedure Set_CWD
      (Proc : PID;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Current_Dir_FS  := FS;
      Registry (Proc).Current_Dir_Ino := Ino;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_CWD;

   procedure Get_CWD
      (Proc : PID;
       FS   : out VFS.FS_Handle;
       Ino  : out VFS.File_Inode_Number)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      FS  := Registry (Proc).Current_Dir_FS;
      Ino := Registry (Proc).Current_Dir_Ino;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_CWD;

   function Bump_Alloc_Base (P : PID; Length : Unsigned_64) return Unsigned_64
   is
      Result : Unsigned_64;
   begin
      Lib.Synchronization.Seize (Registry (P).Data_Mutex);
      Result := Registry (P).Alloc_Base;
      Registry (P).Alloc_Base := Result + Length;
      Lib.Synchronization.Release (Registry (P).Data_Mutex);
      return Result;
   end Bump_Alloc_Base;

   function Get_Parent (Proc : PID) return PID is
      Result : PID;
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Result := Registry (Proc).Parent;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
      return Result;
   end Get_Parent;

   procedure Set_Identifier (Proc : PID; Name : String) is
      Length : Natural;
   begin
      if Name'Length > Max_Name_Length then
         Length := Max_Name_Length;
      else
         Length := Name'Length;
      end if;

      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Identifier_Len           := Length;
      Registry (Proc).Identifier (1 .. Length) :=
         Name (Name'First .. Name'First + Length - 1);
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Identifier;

   procedure Get_Identifier
      (Proc : PID;
       ID   : out String;
       Len  : out Natural)
   is
      Length : Natural;
   begin
      ID := (others => ' ');

      if ID'Length > Registry (Proc).Identifier_Len then
         Length := Registry (Proc).Identifier_Len;
      else
         Length := ID'Length;
      end if;

      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      ID (ID'First .. ID'First + Length - 1) :=
         Registry (Proc).Identifier (1 .. Length);
      Len := Length;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Identifier;

   procedure Get_UID (Proc : PID; UID : out Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      UID := Registry (Proc).User;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_UID;

   procedure Set_UID (Proc : PID; UID : Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).User := UID;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_UID;

   procedure Get_Effective_UID (Proc : PID; EUID : out Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      EUID := Registry (Proc).Effective_User;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Effective_UID;

   procedure Set_Effective_UID (Proc : PID; EUID : Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Effective_User := EUID;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Effective_UID;

   procedure Get_GID (Proc : PID; GID : out Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      GID := Registry (Proc).Group;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_GID;

   procedure Set_GID (Proc : PID; GID : Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Group := GID;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_GID;

   procedure Get_Effective_GID (Proc : PID; EGID : out Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      EGID := Registry (Proc).Effective_Group;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Effective_GID;

   procedure Set_Effective_GID (Proc : PID; EGID : Unsigned_32) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Effective_Group := EGID;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Effective_GID;

   procedure Get_Supplementary_Groups
      (Proc   : PID;
       Groups : out Supplementary_GID_Arr;
       Length : out Natural)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      if Groups'Length <= Max_Supplementary_Groups then
         Groups := Registry (Proc).SGroups (1 .. Groups'Length);
      else
         Groups (Groups'First .. Groups'First + Max_Supplementary_Groups - 1)
            := Registry (Proc).SGroups;
      end if;
      Length := Registry (Proc).SGroup_Count;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Supplementary_Groups;

   procedure Set_Supplementary_Groups
      (Proc    : PID;
       Groups  : Supplementary_GID_Arr;
       Success : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      if Groups'Length > Max_Supplementary_Groups then
         Success := False;
      else
         Registry (Proc).SGroups (1 .. Groups'Length) := Groups;
         Registry (Proc).SGroup_Count := Groups'Length;
         Success := True;
      end if;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Supplementary_Groups;

   procedure Empty_Supplementary_Groups (Proc : PID) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).SGroup_Count := 0;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Empty_Supplementary_Groups;

   procedure Get_Umask (Proc : PID; Umask : out VFS.File_Mode) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Umask := Registry (Proc).Umask;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Umask;

   procedure Set_Umask (Proc : PID; Umask : VFS.File_Mode) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Umask := Umask;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Umask;

   procedure Get_Masked_Signals (Proc : PID; Sig : out Signal_Bitmap) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Sig := Registry (Proc).Masked_Signals;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Masked_Signals;

   procedure Set_Masked_Signals (Proc : PID; Sig : Signal_Bitmap) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);

      --  Set the signals and ensure that unmaskable signals are not masked.
      Registry (Proc).Masked_Signals                     := Sig;
      Registry (Proc).Masked_Signals (Signal_Kill)       := False;
      Registry (Proc).Masked_Signals (Signal_Stop)       := False;
      Registry (Proc).Masked_Signals (Signal_Tracepoint) := False;

      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Masked_Signals;

   procedure Raise_Signal (Proc : PID; Sig : Signal) is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      if not Registry (Proc).Masked_Signals (Sig) then
         Registry (Proc).Raised_Signals (Sig) := True;
      end if;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Raise_Signal;

   procedure Get_Signal_Handler
      (Proc : PID;
       Sig  : Signal;
       Addr : out System.Address)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      Addr := Registry (Proc).Signal_Handlers (Sig);
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Signal_Handler;

   procedure Set_Signal_Handler
      (Proc : PID;
       Sig  : Signal;
       Addr : System.Address)
   is
   begin
      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      if Sig /= Signal_Kill then
         Registry (Proc).Signal_Handlers (Sig) := Addr;
      end if;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Set_Signal_Handler;

   procedure Get_Raised_Signal_Actions
      (Proc   : PID;
       Sig    : out Signal;
       Addr   : out System.Address;
       No_Sig : out Boolean;
       Ignore : out Boolean)
   is
   begin
      Sig    := Signal_FP_Exception;
      Addr   := System.Null_Address;
      No_Sig := True;
      Ignore := False;

      Lib.Synchronization.Seize (Registry (Proc).Data_Mutex);
      for I in Registry (Proc).Raised_Signals'Range loop
         if Registry (Proc).Raised_Signals (I) then
            Sig    := I;
            Addr   := Registry (Proc).Signal_Handlers (I);
            No_Sig := False;

            --  POSIX established only certain signals are safely ignored, when
            --  not ignored, POSIX instructs us to terminate the process if not
            --  handled.
            Ignore := Sig = Signal_Child or Sig = Signal_Urgent;

            Registry (Proc).Raised_Signals (I) := False;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry (Proc).Data_Mutex);
   end Get_Raised_Signal_Actions;

   function Convert (Proc : PID) return Natural is
   begin
      return Natural (Proc);
   end Convert;

   function Convert (Proc : Natural) return PID is
   begin
      if Proc /= 0 and then Proc <= Max_Process_Count and then
         Registry (PID (Proc)) /= null
      then
         return PID (Proc);
      else
         return Error_PID;
      end if;
   end Convert;
   ----------------------------------------------------------------------------
   function Get_Enforcement (Proc : PID) return MAC.Enforcement is
   begin
      return MAC.Get_Enforcement (Registry (Proc).Perms);
   end Get_Enforcement;

   procedure Set_Enforcement (Proc : PID; Act : MAC.Enforcement) is
   begin
      MAC.Set_Enforcement (Registry (Proc).Perms, Act);
   end Set_Enforcement;

   function Get_Capabilities (Proc : PID) return MAC.Capabilities is
   begin
      return MAC.Get_Capabilities (Registry (Proc).Perms);
   end Get_Capabilities;

   procedure Set_Capabilities (Proc : PID; Caps : MAC.Capabilities) is
   begin
      MAC.Set_Capabilities (Registry (Proc).Perms, Caps);
   end Set_Capabilities;

   function Check_Permissions
      (Proc : PID;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number) return MAC.Permissions
   is
   begin
      return MAC.Check_Permissions (Registry (Proc).Perms, FS, Ino);
   end Check_Permissions;

   procedure Add_Entity
      (Proc   : PID;
       FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Perms  : MAC.Permissions;
       Status : out MAC.Addition_Status)
   is
   begin
      MAC.Add_Entity (Registry (Proc).Perms, FS, Ino, Perms, Status);
   end Add_Entity;

   function Get_Limit
      (Proc     : PID;
       Resource : MAC.Limit_Type) return MAC.Limit_Value
   is
   begin
      return MAC.Get_Limit (Registry (Proc).Perms, Resource);
   end Get_Limit;

   procedure Set_Limit
      (Proc      : PID;
       Resource  : MAC.Limit_Type;
       Limit     : MAC.Limit_Value;
       Could_Set : out Boolean)
   is
   begin
      MAC.Set_Limit (Registry (Proc).Perms, Resource, Limit, Could_Set);
   end Set_Limit;
end Userland.Process;
