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

with Time;
with Alignment;
with Messages;
with Ada.Unchecked_Deallocation;
with Arch.Local;
with Arch.Clocks;
with Cryptography.Random;
with Userland.Memory_Locations;
with IPC.FileLock;

package body Userland.Process with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation
      (Process_Data, Process_Data_Acc);

   Do_ASLR : Boolean := True;

   procedure Init is
   begin
      Registry := new Process_Arr'(others => null);
      Synchronization.Release (Registry_Mutex);
   end Init;

   procedure Disable_ASLR is
   begin
      Do_ASLR := False;
   end Disable_ASLR;

   procedure List_All (List : out Process_Info_Arr; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
      List :=
         [others =>
            (Identifier      => [others => ' '],
             Identifier_Len  => 0,
             Process         => Error_PID,
             Parent          => 0,
             User            => 0,
             Is_Being_Traced => False,
             Has_Exited      => False)];
      Total := 0;

      Synchronization.Seize (Registry_Mutex);
      for I in Registry.all'Range loop
         if Registry (I) /= null then
            Total := Total + 1;
            if Curr_Index < List'Length then
               Synchronization.Seize (Registry (I).Data_Mutex);
               List (List'First + Curr_Index) :=
                  (Identifier      => Registry (I).Identifier (1 .. 20),
                   Identifier_Len  => Registry (I).Identifier_Len,
                   Process         => I,
                   Parent          => Registry (I).Parent,
                   User            => Registry (I).User,
                   Is_Being_Traced => Registry (I).Is_Traced,
                   Has_Exited      => Registry (I).Did_Exit);
               Curr_Index := Curr_Index + 1;
               Synchronization.Release (Registry (I).Data_Mutex);
            end if;
         end if;
      end loop;
      Synchronization.Release (Registry_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Registry_Mutex);
         Total := 0;
   end List_All;

   procedure Create_Process (Parent : PID; Returned : out PID) is
      P : PID renames Parent;
   begin
      Returned := Error_PID;

      Synchronization.Seize (Registry_Mutex);
      for I in Registry.all'Range loop
         if Registry (I) = null then
            Registry (I) := new Process_Data'
               (Data_Mutex      => Synchronization.Unlocked_Mutex,
                Controlling_TTY => null,
                Masked_Signals  => [others => False],
                Raised_Signals  => [others => False],
                Signal_Handlers => [others => (others => System.Null_Address)],
                Prio            => Scheduler.Default_Priority,
                Niceness        => Scheduler.Default_Niceness,
                Pol             => Scheduler.Policy_Other,
                Umask           => Default_Umask,
                User            => 0,
                Effective_User  => 0,
                Group           => 0,
                Effective_Group => 0,
                Process_Group   => 0,
                Session_ID      => 0,
                SGroup_Count    => 0,
                SGroups         => [others => 0],
                Identifier      => [others => ' '],
                Identifier_Len  => 0,
                Parent          => 0,
                Is_Traced       => False,
                Tracer_FD       => 0,
                Current_Dir_FS  => VFS.Error_Handle,
                Current_Dir_Ino => 0,
                Thread_List     => [others => Error_TID],
                File_Table => new File_Arr'[others => (False, False, null)],
                Common_Map      => null,
                Stack_Base      => 0,
                Alloc_Base      => 0,
                Perms           => MAC.Default_Context,
                Signal_Exit     => False,
                Which_Signal    => Signal_Kill,
                Did_Exit        => False,
                Exit_Code       => 0,
                VFork_Mark      => False,
                RR_Sec          => 0,
                RR_NS        => Unsigned_64 (Scheduler.Default_RR_NS_Interval),
                others          => 0);

            if Parent /= Error_PID then
               Synchronization.Seize (Registry (P).Data_Mutex);
               Registry (I).Prio            := Registry (P).Prio;
               Registry (I).Niceness        := Registry (P).Niceness;
               Registry (I).Pol             := Registry (P).Pol;
               Registry (I).Masked_Signals  := Registry (P).Masked_Signals;
               Registry (I).Controlling_TTY := Registry (P).Controlling_TTY;
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
               Registry (I).Process_Group   := Registry (P).Process_Group;
               Registry (I).Session_ID      := Registry (P).Session_ID;
               Registry (I).SGroup_Count    := Registry (P).SGroup_Count;
               Registry (I).SGroups         := Registry (P).SGroups;
               Registry (I).Umask           := Registry (P).Umask;
               Registry (I).Signal_Handlers := Registry (P).Signal_Handlers;
               Registry (I).RR_Sec          := Registry (P).RR_Sec;
               Registry (I).RR_NS           := Registry (P).RR_NS;
               Synchronization.Release (Registry (P).Data_Mutex);
            else
               VFS.Get_Root
                  (Registry (I).Current_Dir_FS,
                   Registry (I).Current_Dir_Ino);
               Reassign_Process_Addresses (PID (I));
            end if;

            Arch.Clocks.Get_Monotonic_Time
               (Registry (I).Creation_Secs,
                Registry (I).Creation_NSecs);

            Returned := PID (I);
            exit;
         end if;
      end loop;
      Synchronization.Release (Registry_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Registry_Mutex);
         Returned := Error_PID;
   end Create_Process;

   procedure Delete_Process (Process : PID) is
      Var1, Var2, Var3 : Unsigned_64;
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Synchronization.Seize (Registry_Mutex);
      Var1 := Registry (Process).Creation_NSecs;
      Var2 := Registry (Process).Children_UNSec;
      Var3 := Registry (Process).Children_SNSec;
      Free (Registry (Process));
      Synchronization.Release (Registry_Mutex);

      declare
         CVar1 : Cryptography.Random.Crypto_Data (1 .. Var1'Size / 8)
            with Import, Address => Var1'Address;
         CVar2 : Cryptography.Random.Crypto_Data (1 .. Var2'Size / 8)
            with Import, Address => Var2'Address;
         CVar3 : Cryptography.Random.Crypto_Data (1 .. Var3'Size / 8)
            with Import, Address => Var3'Address;
      begin
         Cryptography.Random.Feed_Entropy (CVar1);
         Cryptography.Random.Feed_Entropy (CVar2);
         Cryptography.Random.Feed_Entropy (CVar3);
      end;
   exception
      when Constraint_Error =>
         null;
   end Delete_Process;

   procedure Get_Controlling_TTY (Proc : PID; TTY : out IPC.PTY.Inner_Acc) is
   begin
      TTY := Registry (Proc).Controlling_TTY;
   exception
      when Constraint_Error =>
         TTY := null;
   end Get_Controlling_TTY;

   procedure Set_Controlling_TTY
      (Proc    : PID;
       TTY     : IPC.PTY.Inner_Acc;
       Success : out Boolean)
   is
   begin
      if Registry (Proc).Controlling_TTY = null then
         Registry (Proc).Controlling_TTY := TTY;
         Success := True;
      else
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Set_Controlling_TTY;

   procedure Clear_Controlling_TTY
      (Proc    : PID;
       TTY     : IPC.PTY.Inner_Acc;
       Success : out Boolean)
   is
   begin
      if Registry (Proc).Controlling_TTY = TTY then
         Registry (Proc).Controlling_TTY := null;
         Success := True;
      else
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Clear_Controlling_TTY;

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

      Synchronization.Seize (Registry (Proc).Data_Mutex);

      for Th of Registry (Proc).Thread_List loop
         if Th /= Error_TID then
            Scheduler.Get_Runtime_Times (Th, Temp1, Temp2, Temp3, Temp4);
            System_Seconds := System_Seconds + Temp1;
            System_Nanoseconds := System_Nanoseconds + Temp2;
            User_Seconds := User_Seconds + Temp3;
            User_Nanoseconds := User_Nanoseconds + Temp4;
         end if;
      end loop;

      Time.Normalize (System_Seconds, System_Nanoseconds);
      Time.Normalize (User_Seconds, User_Nanoseconds);

      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         System_Seconds     := 0;
         System_Nanoseconds := 0;
         User_Seconds       := 0;
         User_Nanoseconds   := 0;
   end Get_Runtime_Times;

   procedure Get_Children_Runtimes
      (Proc : PID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      System_Seconds := Registry (Proc).Children_SSec;
      System_Nanoseconds := Registry (Proc).Children_SNSec;
      User_Seconds := Registry (Proc).Children_USec;
      User_Nanoseconds := Registry (Proc).Children_UNSec;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         System_Seconds     := 0;
         System_Nanoseconds := 0;
         User_Seconds       := 0;
         User_Nanoseconds   := 0;
   end Get_Children_Runtimes;

   procedure Get_Elapsed_Time
      (Proc        : PID;
       Seconds     : out Unsigned_64;
       Nanoseconds : out Unsigned_64)
   is
   begin
      Arch.Clocks.Get_Monotonic_Time (Seconds, Nanoseconds);
      Time.Subtract
         (Seconds, Nanoseconds,
          Registry (Proc).Creation_Secs, Registry (Proc).Creation_NSecs);
   exception
      when Constraint_Error =>
         Seconds     := 0;
         Nanoseconds := 0;
   end Get_Elapsed_Time;

   procedure Add_Thread
      (Proc    : PID;
       Thread  : Scheduler.TID;
       Success : out Boolean)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Success := False;
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = Error_TID then
            Registry (Proc).Thread_List (I) := Thread;
            Scheduler.Set_Niceness (Thread, Registry (Proc).Niceness);
            Scheduler.Set_Priority (Thread, Registry (Proc).Prio);
            Scheduler.Set_Policy (Thread, Registry (Proc).Pol);
            Scheduler.Set_RR_Interval (Thread,
               Registry (Proc).RR_Sec, Registry (Proc).RR_NS);
            Success := True;
            exit;
         end if;
      end loop;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Success := False;
   end Add_Thread;

   procedure Get_Thread_Count (Process : PID; Count : out Natural) is
   begin
      Count := 0;
      Synchronization.Seize (Registry (Process).Data_Mutex);
      for I in Registry (Process).Thread_List'Range loop
         if Registry (Process).Thread_List (I) /= Error_TID then
            Count := Count + 1;
         end if;
      end loop;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         Count := 0;
   end Get_Thread_Count;

   procedure Remove_Thread (Proc : PID; Thread : Scheduler.TID) is
      Temp1, Temp2, Temp3, Temp4 : Unsigned_64;
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = Thread then
            Registry (Proc).Thread_List (I) := Error_TID;
            Scheduler.Get_Runtime_Times (Thread, Temp1, Temp2, Temp3, Temp4);
            if Registry (Proc).Parent /= Error_PID and then
               Registry (Registry (Proc).Parent) /= null
            then
               Time.Increment
                  (Registry (Registry (Proc).Parent).Children_SSec,
                   Registry (Registry (Proc).Parent).Children_SNSec,
                   Temp1, Temp2);
               Time.Increment
                  (Registry (Registry (Proc).Parent).Children_USec,
                   Registry (Registry (Proc).Parent).Children_UNSec,
                   Temp3, Temp4);
            end if;
            exit;
         end if;
      end loop;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Remove_Thread;

   procedure Flush_Threads (Proc : PID) is
      Current_Thread : constant TID := Arch.Local.Get_Current_Thread;
      T1, T2, T3, T4 : Unsigned_64;
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      for Thread of Registry (Proc).Thread_List loop
         if Thread /= Error_TID then
            if Registry (Proc).Parent /= Error_PID and then
               Registry (Registry (Proc).Parent) /= null
            then
               Scheduler.Get_Runtime_Times (Thread, T1, T2, T3, T4);

               Time.Increment
                  (Registry (Registry (Proc).Parent).Children_SSec,
                   Registry (Registry (Proc).Parent).Children_SNSec, T1, T2);
               Time.Increment
                  (Registry (Registry (Proc).Parent).Children_USec,
                   Registry (Registry (Proc).Parent).Children_UNSec, T3, T4);
            end if;

            if Thread /= Current_Thread then
               Scheduler.Delete_Thread (Thread);
            end if;
         end if;
         Thread := Error_TID;
      end loop;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Flush_Threads;

   procedure Reassign_Process_Addresses (Process : PID) is
      package Aln is new Alignment (Unsigned_64);
      Rand_Addr, Rand_Jump : Unsigned_64;
   begin
      --  Reassign the memory addresses.
      if Do_ASLR then
         Cryptography.Random.Get_Integer
            (Memory_Locations.Mmap_Anon_Min,
             Memory_Locations.Mmap_Anon_Max,
             Rand_Addr);
         Cryptography.Random.Get_Integer
            (Memory_Locations.Stack_Jump_Min,
             Memory_Locations.Stack_Jump_Max,
             Rand_Jump);

         Rand_Addr := Aln.Align_Up (Rand_Addr, Memory.MMU.Page_Size);
         Rand_Jump := Aln.Align_Up (Rand_Jump, Memory.MMU.Page_Size);
      else
         Rand_Addr := Memory_Locations.Mmap_Anon_Min;
         Rand_Jump := Memory_Locations.Stack_Jump_Min;
      end if;

      Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Alloc_Base := Rand_Addr;
      Registry (Process).Stack_Base := Rand_Addr + Rand_Jump;
      Synchronization.Release (Registry (Process).Data_Mutex);

      --  Reassign signal information.
      Registry (Process).Masked_Signals  := [others => False];
      Registry (Process).Raised_Signals  := [others => False];
      Registry (Process).Signal_Handlers :=
         [others => (others => System.Null_Address)];
   exception
      when Constraint_Error =>
         null;
   end Reassign_Process_Addresses;

   procedure Get_Niceness (Process : PID; Nice : out Scheduler.Niceness) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Nice := Registry (Process).Niceness;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         Nice := 0;
   end Get_Niceness;

   procedure Set_Niceness (Process : PID; Nice : Scheduler.Niceness) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Niceness := Nice;
      for Thread of Registry (Process).Thread_List loop
         if Thread /= Error_TID then
            Scheduler.Set_Niceness (Thread, Nice);
         end if;
      end loop;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Niceness;

   procedure Pop_VFork_Marker (Process : PID; Marked : out Boolean) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Marked := Registry (Process).VFork_Mark;
      Registry (Process).VFork_Mark := False;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         Marked := False;
   end Pop_VFork_Marker;

   procedure Set_VFork_Marker (Process : PID) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).VFork_Mark := True;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_VFork_Marker;

   procedure Is_Valid_File
      (Process : PID;
       FD      : Unsigned_64;
       Success : out Boolean)
   is
      R : Boolean renames Success;
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         R := Registry (Process).File_Table (Natural (FD)).Description /= null;
      else
         R := False;
      end if;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         R := False;
   end Is_Valid_File;

   procedure Add_File
      (Process : PID;
       File    : File_Description_Acc;
       FD      : out Natural;
       Success : out Boolean;
       Start   : Natural := 0)
   is
      File_Limit   : Unsigned_64;
      Count_Of_FDs : Natural := 0;
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);

      FD      := 0;
      Success := False;

      for Ent of Registry (Process).File_Table.all loop
         if Ent.Description = null then
            Count_Of_FDs := Count_Of_FDs + 1;
         end if;
      end loop;

      File_Limit := MAC.Get_Limit
         (Registry (Process).Perms, MAC.Opened_File_Limit).Soft_Limit;
      if Unsigned_64 (Count_Of_FDs) >= File_Limit then
         goto Cleanup;
      end if;

      for I in Start .. Registry (Process).File_Table'Last loop
         if Registry (Process).File_Table (I).Description = null then
            Registry (Process).File_Table (I).Description := File;
            FD      := I;
            Success := True;
            goto Cleanup;
         end if;
      end loop;

   <<Cleanup>>
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         FD      := 0;
         Success := False;
   end Add_File;

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
   exception
      when Constraint_Error =>
         null;
   end Duplicate;

   procedure Duplicate_FD_Table (Process, Target : PID) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Synchronization.Seize (Registry (Target).Data_Mutex);

      for I in Registry (Process).File_Table'Range loop
         if not Registry (Process).File_Table (I).Close_On_Fork then
            Registry (Target).File_Table (I) :=
               Registry (Process).File_Table (I);
            if Registry (Process).File_Table (I).Description /= null then
               Duplicate
                  (Registry (Process).File_Table (I).Description,
                   Registry (Target).File_Table (I).Description);
            end if;
         end if;
      end loop;

      Synchronization.Release (Registry (Target).Data_Mutex);
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
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
                     Messages.Put_Line ("Missing file lock on closure!");
                  end if;
               end if;
            when Description_Socket => Close (F.Inner_Socket);
         end case;
         Free (F);
      else
         F.Children_Count := F.Children_Count - 1;
      end if;
      F := null;
   exception
      when Constraint_Error =>
         F := null;
   end Close;

   procedure Get_File
      (Process : PID;
       FD      : Unsigned_64;
       File    : out File_Description_Acc)
   is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         File := Registry (Process).File_Table (Natural (FD)).Description;
      else
         File := null;
      end if;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         File := null;
   end Get_File;

   procedure Get_FD_Flags
      (Process       : PID;
       FD            : Unsigned_64;
       Close_On_Exec : out Boolean;
       Close_On_Fork : out Boolean)
   is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         Close_On_Exec :=
            Registry (Process).File_Table (Natural (FD)).Close_On_Exec;
         Close_On_Fork :=
            Registry (Process).File_Table (Natural (FD)).Close_On_Fork;
      else
         Close_On_Exec := False;
         Close_On_Fork := False;
      end if;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         Close_On_Exec := False;
         Close_On_Fork := False;
   end Get_FD_Flags;

   procedure Set_FD_Flags
      (Process       : PID;
       FD            : Unsigned_64;
       Close_On_Exec : Boolean;
       Close_On_Fork : Boolean)
   is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= Unsigned_64 (File_Arr'Last) then
         Registry (Process).File_Table (Natural (FD)).Close_On_Exec :=
            Close_On_Exec;
         Registry (Process).File_Table (Natural (FD)).Close_On_Fork :=
            Close_On_Fork;
      end if;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_FD_Flags;

   procedure Remove_File (Process : PID; FD : Natural) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      if FD <= File_Arr'Last then
         Close (Registry (Process).File_Table (FD).Description);
         Registry (Process).File_Table (FD).Close_On_Exec := False;
         Registry (Process).File_Table (FD).Close_On_Fork := False;
      end if;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Remove_File;

   procedure Flush_Files (Process : PID) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      for F of Registry (Process).File_Table.all loop
         if F.Description /= null then
            Close (F.Description);
         end if;
         F.Close_On_Exec := False;
         F.Close_On_Fork := False;
      end loop;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Flush_Files;

   procedure Flush_Exec_Files (Process : PID) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      for F of Registry (Process).File_Table.all loop
         if F.Description /= null and then F.Close_On_Exec then
            Close (F.Description);
            F.Close_On_Exec := False;
            F.Close_On_Fork := False;
         end if;
      end loop;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Flush_Exec_Files;

   procedure Set_Common_Map (Proc : PID; Map : Memory.MMU.Page_Table_Acc) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Common_Map := Map;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Common_Map;

   procedure Get_Common_Map (Proc : PID; Map : out Memory.MMU.Page_Table_Acc)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Map := Registry (Proc).Common_Map;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Map := null;
   end Get_Common_Map;

   procedure Bump_Stack_Base
      (P        : PID;
       Length   : Unsigned_64;
       Previous : out Unsigned_64)
   is
   begin
      Synchronization.Seize (Registry (P).Data_Mutex);
      Previous := Registry (P).Stack_Base;
      Registry (P).Stack_Base := Previous + Length;
      Synchronization.Release (Registry (P).Data_Mutex);
   exception
      when Constraint_Error =>
         Previous := 0;
   end Bump_Stack_Base;

   procedure Get_Traced_Info
      (Process   : PID;
       Is_Traced : out Boolean;
       FD        : out Natural)
   is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Is_Traced := Registry (Process).Is_Traced;
      FD        := Registry (Process).Tracer_FD;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         Is_Traced := False;
         FD        := 0;
   end Get_Traced_Info;

   procedure Set_Traced_Info
      (Process   : PID;
       Is_Traced : Boolean;
       FD        : Natural)
   is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Registry (Process).Is_Traced := Is_Traced;
      Registry (Process).Tracer_FD := FD;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Traced_Info;

   procedure Issue_Exit (Process : PID; Code : Unsigned_8) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);

      Registry (Process).Did_Exit    := True;
      Registry (Process).Signal_Exit := False;
      Registry (Process).Exit_Code   := Code;

      if Registry (Process).Parent /= Error_PID then
         Raise_Signal (Registry (Process).Parent, Signal_Child);
      end if;

      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Issue_Exit;

   procedure Issue_Exit (Process : PID; Sig : Signal) is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);

      Registry (Process).Did_Exit     := True;
      Registry (Process).Signal_Exit  := True;
      Registry (Process).Which_Signal := Sig;

      if Registry (Process).Parent /= Error_PID then
         Raise_Signal (Registry (Process).Parent, Signal_Child);
      end if;

      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Issue_Exit;

   procedure Check_Exit
      (Process    : PID;
       Did_Exit   : out Boolean;
       Code       : out Unsigned_8;
       Was_Signal : out Boolean;
       Sig        : out Signal)
   is
   begin
      Synchronization.Seize (Registry (Process).Data_Mutex);
      Did_Exit   := Registry (Process).Did_Exit;
      Code       := Registry (Process).Exit_Code;
      Was_Signal := Registry (Process).Signal_Exit;
      Sig        := Registry (Process).Which_Signal;
      Synchronization.Release (Registry (Process).Data_Mutex);
   exception
      when Constraint_Error =>
         Did_Exit   := False;
         Code       := 0;
         Was_Signal := False;
         Sig        := Signal_Abort;
   end Check_Exit;

   procedure Check_Children_Exit
      (Process     : PID;
       Exited_Proc : out PID;
       Did_Exit    : out Boolean;
       Code        : out Unsigned_8;
       Was_Signal  : out Boolean;
       Sig         : out Signal)
   is
   begin
      Did_Exit    := False;
      Exited_Proc := Error_PID;
      Code        := 0;
      Was_Signal  := False;
      Sig         := Signal_Abort;

      for I in Registry'Range loop
         if Registry (I) /= null and then Registry (I).Parent = Process then
            Synchronization.Seize (Registry (I).Data_Mutex);
            Exited_Proc := I;
            Did_Exit    := Registry (I).Did_Exit;
            Code        := Registry (I).Exit_Code;
            Was_Signal  := Registry (I).Signal_Exit;
            Sig         := Registry (I).Which_Signal;
            Synchronization.Release (Registry (I).Data_Mutex);
            exit when Did_Exit;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Did_Exit    := False;
         Exited_Proc := Error_PID;
         Code        := 0;
         Was_Signal  := False;
         Sig         := Signal_Abort;
   end Check_Children_Exit;

   procedure Check_Children_Group_Exit
      (Process     : PID;
       Group       : Unsigned_32;
       Exited_Proc : out PID;
       Did_Exit    : out Boolean;
       Code        : out Unsigned_8;
       Was_Signal  : out Boolean;
       Sig         : out Signal)
   is
   begin
      Did_Exit    := False;
      Exited_Proc := Error_PID;
      Code        := 0;
      Was_Signal  := False;
      Sig         := Signal_Abort;

      for I in Registry'Range loop
         if Registry (I) /= null          and then
            Registry (I).Parent = Process and then
            Registry (I).Process_Group = Group
         then
            Synchronization.Seize (Registry (I).Data_Mutex);
            Exited_Proc := I;
            Did_Exit    := Registry (I).Did_Exit;
            Code        := Registry (I).Exit_Code;
            Was_Signal  := Registry (I).Signal_Exit;
            Sig         := Registry (I).Which_Signal;
            Synchronization.Release (Registry (I).Data_Mutex);
            exit when Did_Exit;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Did_Exit    := False;
         Exited_Proc := Error_PID;
         Code        := 0;
         Was_Signal  := False;
         Sig         := Signal_Abort;
   end Check_Children_Group_Exit;

   procedure Reassign_Parent_To_Init (Process : PID) is
   begin
      for Proc of Registry.all loop
         if Proc /= null and then Proc.Parent = Process then
            Synchronization.Seize (Proc.Data_Mutex);
            Proc.Parent := 1;
            Synchronization.Release (Proc.Data_Mutex);
         end if;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Reassign_Parent_To_Init;

   procedure Exit_Process (Process : PID; Killer : Signal) is
      Exiting_Ourselves : constant Boolean :=
         Process = Arch.Local.Get_Current_Process;
   begin
      if Exiting_Ourselves then
         --  Switch to the kernel page table to make us immune to having it
         --  swept from under out feet by process cleanup.
         if not Memory.MMU.Make_Active (Memory.MMU.Kernel_Table) then
            Messages.Put_Line ("Could not switch table on thread exit");
         end if;

         Remove_Thread (Process, Arch.Local.Get_Current_Thread);
      end if;

      --  Inherit all our children to init, who will take care of them.
      Reassign_Parent_To_Init (Process);

      --  Remove all state but the return value and keep the zombie around
      --  until we are waited.
      Flush_Threads (Process);
      Flush_Files   (Process);
      Issue_Exit (Process, Killer);

      if Exiting_Ourselves then
         Scheduler.Bail;
      end if;
   end Exit_Process;

   procedure Exit_Process (Process : PID; Code : Unsigned_8) is
      Exiting_Ourselves : constant Boolean :=
         Process = Arch.Local.Get_Current_Process;
   begin
      if Exiting_Ourselves then
         --  Switch to the kernel page table to make us immune to having it
         --  swept from under out feet by process cleanup.
         if not Memory.MMU.Make_Active (Memory.MMU.Kernel_Table) then
            Messages.Put_Line ("Could not switch table on thread exit");
         end if;

         Remove_Thread (Process, Arch.Local.Get_Current_Thread);
      end if;

      --  Inherit all our children to init, who will take care of them.
      Reassign_Parent_To_Init (Process);

      --  Remove all state but the return value and keep the zombie around
      --  until we are waited.
      Flush_Threads (Process);
      Flush_Files   (Process);
      Issue_Exit (Process, Code);

      if Exiting_Ourselves then
         Scheduler.Bail;
      end if;
   end Exit_Process;

   procedure Set_CWD
      (Proc : PID;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Current_Dir_FS  := FS;
      Registry (Proc).Current_Dir_Ino := Ino;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_CWD;

   procedure Get_CWD
      (Proc : PID;
       FS   : out VFS.FS_Handle;
       Ino  : out VFS.File_Inode_Number)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      FS  := Registry (Proc).Current_Dir_FS;
      Ino := Registry (Proc).Current_Dir_Ino;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         FS  := VFS.Error_Handle;
         Ino := 0;
   end Get_CWD;

   procedure Bump_Alloc_Base
      (P        : PID;
       Length   : Unsigned_64;
       Previous : out Unsigned_64)
   is
   begin
      Synchronization.Seize (Registry (P).Data_Mutex);
      Previous := Registry (P).Alloc_Base;
      Registry (P).Alloc_Base := Previous + Length;
      Synchronization.Release (Registry (P).Data_Mutex);
   exception
      when Constraint_Error =>
         Previous := 0;
   end Bump_Alloc_Base;

   procedure Get_User_Mapped_Size (P : PID; Size : out Unsigned_64) is
   begin
      Memory.MMU.Get_User_Mapped_Size (Registry (P).Common_Map, Size);
   exception
      when Constraint_Error =>
         Size := 0;
   end Get_User_Mapped_Size;

   procedure Get_Parent (Proc : PID; Parent : out PID) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Parent := Registry (Proc).Parent;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Parent := Error_PID;
   end Get_Parent;

   procedure Set_Parent (Proc : PID; Parent : PID) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Parent := Parent;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Parent;

   procedure Set_Identifier (Proc : PID; Name : String) is
      Length : Natural;
   begin
      if Name'Length > Max_Name_Length then
         Length := Max_Name_Length;
      else
         Length := Name'Length;
      end if;

      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Identifier_Len           := Length;
      Registry (Proc).Identifier (1 .. Length) :=
         Name (Name'First .. Name'First + Length - 1);
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Identifier;

   procedure Get_Identifier
      (Proc : PID;
       ID   : out String;
       Len  : out Natural)
   is
      Length : Natural;
   begin
      ID := [others => ' '];

      if ID'Length > Registry (Proc).Identifier_Len then
         Length := Registry (Proc).Identifier_Len;
      else
         Length := ID'Length;
      end if;

      Synchronization.Seize (Registry (Proc).Data_Mutex);
      ID (ID'First .. ID'First + Length - 1) :=
         Registry (Proc).Identifier (1 .. Length);
      Len := Length;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         ID  := [others => ' '];
         Len := 0;
   end Get_Identifier;

   procedure Get_UID (Proc : PID; UID : out Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      UID := Registry (Proc).User;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         UID := 0;
   end Get_UID;

   procedure Set_UID (Proc : PID; UID : Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).User := UID;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_UID;

   procedure Get_Effective_UID (Proc : PID; EUID : out Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      EUID := Registry (Proc).Effective_User;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         EUID := 0;
   end Get_Effective_UID;

   procedure Set_Effective_UID (Proc : PID; EUID : Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Effective_User := EUID;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Effective_UID;

   procedure Get_GID (Proc : PID; GID : out Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      GID := Registry (Proc).Group;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         GID := 0;
   end Get_GID;

   procedure Set_GID (Proc : PID; GID : Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Group := GID;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_GID;

   procedure Get_Effective_GID (Proc : PID; EGID : out Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      EGID := Registry (Proc).Effective_Group;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         EGID := 0;
   end Get_Effective_GID;

   procedure Set_Effective_GID (Proc : PID; EGID : Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Effective_Group := EGID;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Effective_GID;

   procedure Get_PGID (Proc : PID; PGID : out Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      PGID := Registry (Proc).Process_Group;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         PGID := 0;
   end Get_PGID;

   procedure Set_PGID (Proc : PID; PGID : Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Process_Group := PGID;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_PGID;

   procedure Get_Session_ID (Proc : PID; ID : out Unsigned_32) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      ID := Registry (Proc).Session_ID;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         ID := 0;
   end Get_Session_ID;

   procedure Create_Session (Proc : PID; Success : out Boolean) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Success := Registry (Proc).Process_Group /= Unsigned_32 (Proc);
      if Success then
         Registry (Proc).Session_ID := Unsigned_32 (Proc);
      end if;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Success := False;
   end Create_Session;

   procedure Get_Supplementary_Groups
      (Proc   : PID;
       Groups : out Supplementary_GID_Arr;
       Length : out Natural)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      if Groups'Length <= Max_Supplementary_Groups then
         Groups := Registry (Proc).SGroups (1 .. Groups'Length);
      else
         Groups (Groups'First .. Groups'First + Max_Supplementary_Groups - 1)
            := Registry (Proc).SGroups;
      end if;
      Length := Registry (Proc).SGroup_Count;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Length := 0;
   end Get_Supplementary_Groups;

   procedure Set_Supplementary_Groups
      (Proc    : PID;
       Groups  : Supplementary_GID_Arr;
       Success : out Boolean)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      if Groups'Length > Max_Supplementary_Groups then
         Success := False;
      else
         Registry (Proc).SGroups (1 .. Groups'Length) := Groups;
         Registry (Proc).SGroup_Count := Groups'Length;
         Success := True;
      end if;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Success := False;
   end Set_Supplementary_Groups;

   procedure Empty_Supplementary_Groups (Proc : PID) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).SGroup_Count := 0;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Empty_Supplementary_Groups;

   procedure Get_Umask (Proc : PID; Umask : out VFS.File_Mode) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Umask := Registry (Proc).Umask;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Umask := 0;
   end Get_Umask;

   procedure Set_Umask (Proc : PID; Umask : VFS.File_Mode) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Umask := Umask;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Umask;

   procedure Get_Masked_Signals (Proc : PID; Sig : out Signal_Bitmap) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Sig := Registry (Proc).Masked_Signals;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Sig := [others => True];
   end Get_Masked_Signals;

   procedure Set_Masked_Signals (Proc : PID; Sig : Signal_Bitmap) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);

      --  Set the signals and ensure that unmaskable signals are not masked.
      Registry (Proc).Masked_Signals               := Sig;
      Registry (Proc).Masked_Signals (Signal_Kill) := False;
      Registry (Proc).Masked_Signals (Signal_Stop) := False;

      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Masked_Signals;

   procedure Raise_Signal (Proc : PID; Sig : Signal) is
   begin
      if Sig = Signal_Kill then
         Exit_Process (Proc, Signal_Kill);
      elsif Sig = Signal_Stop then
         --  XXX: Implement this.
         null;
      else
         Synchronization.Seize (Registry (Proc).Data_Mutex);
         if not Registry (Proc).Masked_Signals (Sig) then
            Registry (Proc).Raised_Signals (Sig) := True;
         end if;
         Synchronization.Release (Registry (Proc).Data_Mutex);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Raise_Signal;

   procedure Raise_Signal
      (Sig        : Signal;
       Sender_UID : Unsigned_32;
       Bypass_UID : Boolean;
       Group      : Unsigned_32)
   is
      Tgt_Group, EUID, UID : Unsigned_32;
   begin
      for I in Registry'Range loop
         if Registry (I) /= null then
            Synchronization.Seize (Registry (I).Data_Mutex);
            Tgt_Group := Registry (I).Process_Group;
            EUID      := Registry (I).Effective_User;
            UID       := Registry (I).User;
            Synchronization.Release (Registry (I).Data_Mutex);

            if Tgt_Group = Group and
               (Bypass_UID or (EUID = Sender_UID or UID = Sender_UID))
            then
               Raise_Signal (I, Sig);
            end if;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Raise_Signal;

   procedure Raise_Signal
      (Process    : PID;
       Sig        : Signal;
       Sender_UID : Unsigned_32;
       Bypass_UID : Boolean)
   is
      EUID, UID : Unsigned_32;
   begin
      for I in Registry'Range loop
         if Registry (I) /= null and then I /= Process and then I /= 1 then
            Synchronization.Seize (Registry (I).Data_Mutex);
            EUID      := Registry (I).Effective_User;
            UID       := Registry (I).User;
            Synchronization.Release (Registry (I).Data_Mutex);

            if Bypass_UID or (EUID = Sender_UID or UID = Sender_UID) then
               Raise_Signal (I, Sig);
            end if;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Raise_Signal;

   procedure Get_Signal_Handlers
      (Proc     : PID;
       Sig      : Signal;
       Handler  : out System.Address;
       Restorer : out System.Address)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Handler  := Registry (Proc).Signal_Handlers (Sig).Handler_Addr;
      Restorer := Registry (Proc).Signal_Handlers (Sig).Restorer_Addr;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Handler  := System.Null_Address;
         Restorer := System.Null_Address;
   end Get_Signal_Handlers;

   procedure Set_Signal_Handlers
      (Proc     : PID;
       Sig      : Signal;
       Handler  : System.Address;
       Restorer : System.Address)
   is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      if Sig /= Signal_Kill or Sig /= Signal_Stop then
         Registry (Proc).Signal_Handlers (Sig).Handler_Addr  := Handler;
         Registry (Proc).Signal_Handlers (Sig).Restorer_Addr := Restorer;
      end if;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Signal_Handlers;

   procedure Get_Raised_Signal_Actions
      (Proc     : PID;
       Sig      : out Signal;
       Handler  : out System.Address;
       Restorer : out System.Address;
       No_Sig   : out Boolean;
       Ignore   : out Boolean;
       Old_Mask : out Signal_Bitmap)
   is
   begin
      Sig      := Signal_FP_Exception;
      Handler  := System.Null_Address;
      Restorer := System.Null_Address;
      No_Sig   := True;
      Ignore   := False;
      Old_Mask := [others => False];

      Synchronization.Seize (Registry (Proc).Data_Mutex);
      for I in Registry (Proc).Raised_Signals'Range loop
         if not Registry (Proc).Masked_Signals (I) and
            Registry (Proc).Raised_Signals (I)
         then
            Sig      := I;
            Handler  := Registry (Proc).Signal_Handlers (I).Handler_Addr;
            Restorer := Registry (Proc).Signal_Handlers (I).Restorer_Addr;
            No_Sig   := False;

            --  POSIX established only certain signals are safely ignored, when
            --  not ignored, POSIX instructs us to terminate the process if not
            --  handled.
            Ignore := Sig = Signal_Child or Sig = Signal_Urgent;

            Registry (Proc).Raised_Signals (I) := False;
            Old_Mask := Registry (Proc).Masked_Signals;
            Registry (Proc).Masked_Signals (I) := True;
            exit;
         end if;
      end loop;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Sig      := Signal_FP_Exception;
         Handler  := System.Null_Address;
         Restorer := System.Null_Address;
         No_Sig   := True;
         Ignore   := False;
   end Get_Raised_Signal_Actions;

   procedure Get_Default_Policy (Proc : PID; Pol : out Scheduler.Policy) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Pol := Registry (Proc).Pol;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Pol := Scheduler.Policy_Other;
   end Get_Default_Policy;

   procedure Set_Default_Policy (Proc : PID; Pol : Scheduler.Policy) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Pol := Pol;
      for Thread of Registry (Proc).Thread_List loop
         if Thread /= Error_TID then
            Scheduler.Set_Policy (Thread, Pol);
         end if;
      end loop;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Default_Policy;

   procedure Get_Priority (Proc : PID; Prio : out Scheduler.Priority) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Prio := Registry (Proc).Prio;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Prio := Scheduler.Default_Priority;
   end Get_Priority;

   procedure Set_Priority (Proc : PID; Prio : Scheduler.Priority) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).Prio := Prio;
      for Thread of Registry (Proc).Thread_List loop
         if Thread /= Error_TID then
            Scheduler.Set_Priority (Thread, Prio);
         end if;
      end loop;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_Priority;

   procedure Get_RR_Interval (Proc : PID; Sec, NS : out Unsigned_64) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Sec := Registry (Proc).RR_Sec;
      NS  := Registry (Proc).RR_NS;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         Sec := 0;
         NS  := 0;
   end Get_RR_Interval;

   procedure Set_RR_Interval (Proc : PID; Sec, NS : Unsigned_64) is
   begin
      Synchronization.Seize (Registry (Proc).Data_Mutex);
      Registry (Proc).RR_Sec := Sec;
      Registry (Proc).RR_NS  := NS;
      for Thread of Registry (Proc).Thread_List loop
         if Thread /= Error_TID then
            Scheduler.Set_RR_Interval (Thread, Sec, NS);
         end if;
      end loop;
      Synchronization.Release (Registry (Proc).Data_Mutex);
   exception
      when Constraint_Error =>
         null;
   end Set_RR_Interval;

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
   exception
      when Constraint_Error =>
         return Error_PID;
   end Convert;
   ----------------------------------------------------------------------------
   function Get_Enforcement (Proc : PID) return MAC.Enforcement is
   begin
      return MAC.Get_Enforcement (Registry (Proc).Perms);
   exception
      when Constraint_Error =>
         return MAC.Deny_And_Scream;
   end Get_Enforcement;

   procedure Set_Enforcement (Proc : PID; Act : MAC.Enforcement) is
   begin
      MAC.Set_Enforcement (Registry (Proc).Perms, Act);
   exception
      when Constraint_Error =>
         null;
   end Set_Enforcement;

   function Get_Capabilities (Proc : PID) return MAC.Capabilities is
   begin
      return MAC.Get_Capabilities (Registry (Proc).Perms);
   exception
      when Constraint_Error =>
         return (others => False);
   end Get_Capabilities;

   procedure Set_Capabilities (Proc : PID; Caps : MAC.Capabilities) is
   begin
      MAC.Set_Capabilities (Registry (Proc).Perms, Caps);
   exception
      when Constraint_Error =>
         null;
   end Set_Capabilities;

   function Check_Permissions
      (Proc : PID;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number) return MAC.Permissions
   is
   begin
      return MAC.Check_Permissions (Registry (Proc).Perms, FS, Ino);
   exception
      when Constraint_Error =>
         return (others => False);
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
   exception
      when Constraint_Error =>
         Status := MAC.Is_Conflicting;
   end Add_Entity;

   function Get_Limit
      (Proc     : PID;
       Resource : MAC.Limit_Type) return MAC.Limit_Value
   is
   begin
      return MAC.Get_Limit (Registry (Proc).Perms, Resource);
   exception
      when Constraint_Error =>
         return (0, 0);
   end Get_Limit;

   procedure Set_Limit
      (Proc      : PID;
       Resource  : MAC.Limit_Type;
       Limit     : MAC.Limit_Value;
       Could_Set : out Boolean)
   is
   begin
      MAC.Set_Limit (Registry (Proc).Perms, Resource, Limit, Could_Set);
   exception
      when Constraint_Error =>
         Could_Set := False;
   end Set_Limit;
end Userland.Process;
