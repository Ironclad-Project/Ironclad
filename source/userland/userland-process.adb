--  userland-process.adb: Process registry, PIDs, and all the fuzz.
--  Copyright (C) 2023 streaksu
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
with Ada.Unchecked_Deallocation;
with Arch.Local;
with Cryptography.Random;
with Userland.Memory_Locations;

package body Userland.Process with SPARK_Mode => Off is
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
               List (List'First + Curr_Index) :=
                  (Identifier      => Registry (I).Identifier (1 .. 20),
                   Identifier_Len  => Registry (I).Identifier_Len,
                   Process         => I,
                   Parent          => Registry (I).Parent,
                   User            => Registry (I).User,
                   Is_Being_Traced => Registry (I).Is_Traced,
                   Has_Exited      => Registry (I).Did_Exit);
               Curr_Index := Curr_Index + 1;
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
               (Umask           => Default_Umask,
                User            => 0,
                Effective_User  => 0,
                Identifier      => (others => ' '),
                Identifier_Len  => 0,
                Parent          => 0,
                Is_Traced       => False,
                Tracer_FD       => 0,
                Current_Dir_FS  => VFS.Error_Handle,
                Current_Dir_Ino => 0,
                Thread_List     => (others => Error_TID),
                File_Table      => (others => (False, null)),
                Common_Map      => null,
                Stack_Base      => 0,
                Alloc_Base      => 0,
                Perms           => MAC.Default_Context,
                Did_Exit        => False,
                Exit_Code       => 0,
                others          => 0);

            if Parent /= Error_PID then
               Registry (I).Parent          := Parent;
               Registry (I).Stack_Base      := Registry (P).Stack_Base;
               Registry (I).Alloc_Base      := Registry (P).Alloc_Base;
               Registry (I).Current_Dir_FS  := Registry (P).Current_Dir_FS;
               Registry (I).Current_Dir_Ino := Registry (P).Current_Dir_Ino;
               Registry (I).Perms           := Registry (P).Perms;
               Registry (I).User            := Registry (P).User;
               Registry (I).Effective_User  := Registry (P).Effective_User;
               Registry (I).Umask           := Registry (P).Umask;
            else
               Reroll_ASLR (PID (I));
            end if;

            Returned := PID (I);
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
   end Create_Process;

   procedure Delete_Process (Process : PID) is
   begin
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
   end Get_Runtime_Times;

   procedure Get_Children_Runtimes
      (Proc : PID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64)
   is
   begin
      System_Seconds := Registry (Proc).Children_SSec;
      System_Nanoseconds := Registry (Proc).Children_SNSec;
      User_Seconds := Registry (Proc).Children_USec;
      User_Nanoseconds := Registry (Proc).Children_UNSec;
   end Get_Children_Runtimes;

   procedure Add_Thread
      (Proc    : PID;
       Thread  : Scheduler.TID;
       Success : out Boolean)
   is
   begin
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = Error_TID then
            Registry (Proc).Thread_List (I) := Thread;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
   end Add_Thread;

   procedure Remove_Thread (Proc : PID; Thread : Scheduler.TID) is
      Temp1, Temp2, Temp3, Temp4 : Unsigned_64;
   begin
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = Thread then
            Registry (Proc).Thread_List (I) := Error_TID;
            Scheduler.Get_Runtime_Times (Thread, Temp1, Temp2, Temp3, Temp4);
            Lib.Time.Increment
               (Registry (Registry (Proc).Parent).Children_SSec,
                Registry (Registry (Proc).Parent).Children_SNSec,
                Temp1, Temp2);
            Lib.Time.Increment
               (Registry (Registry (Proc).Parent).Children_USec,
                Registry (Registry (Proc).Parent).Children_UNSec,
                Temp3, Temp4);
            exit;
         end if;
      end loop;
   end Remove_Thread;

   procedure Flush_Threads (Proc : PID) is
      Current_Thread : constant TID := Arch.Local.Get_Current_Thread;
      Temp1, Temp2, Temp3, Temp4 : Unsigned_64;
   begin
      for Thread of Registry (Proc).Thread_List loop
         if Thread /= Error_TID then
            if Registry (Proc).Parent /= Error_PID and then
               Registry (Registry (Proc).Parent) /= null
            then
               Scheduler.Get_Runtime_Times (Thread, Temp1, Temp2, Temp3, Temp4);

               Lib.Time.Increment
                  (Registry (Registry (Proc).Parent).Children_SSec,
                   Registry (Registry (Proc).Parent).Children_SNSec,
                   Temp1, Temp2);
               Lib.Time.Increment
                  (Registry (Registry (Proc).Parent).Children_USec,
                   Registry (Registry (Proc).Parent).Children_UNSec,
                   Temp3, Temp4);
            end if;

            if Thread /= Current_Thread then
               Scheduler.Delete_Thread (Thread);
            end if;
         end if;
         Thread := Error_TID;
      end loop;
   end Flush_Threads;

   procedure Reroll_ASLR (Process : PID) is
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

      Registry (Process).Alloc_Base := Rand_Addr;
      Registry (Process).Stack_Base := Rand_Addr + Rand_Jump;
   end Reroll_ASLR;

   function Is_Valid_File (Process : PID; FD : Unsigned_64) return Boolean is
   begin
      if FD > Unsigned_64 (File_Arr'Last) then
         return False;
      end if;
      return Registry (Process).File_Table (Natural (FD)).Description /= null;
   end Is_Valid_File;

   procedure Add_File
      (Process : PID;
       File    : File_Description_Acc;
       FD      : out Natural;
       Success : out Boolean;
       Start   : Natural := 0)
   is
   begin
      for I in Start .. Registry (Process).File_Table'Last loop
         if Registry (Process).File_Table (I).Description = null then
            Registry (Process).File_Table (I).Description := File;
            FD      := I;
            Success := True;
            return;
         end if;
      end loop;
      FD      := 0;
      Success := False;
   end Add_File;

   function Get_File_Count (Process : PID) return Natural is
      Count_Of_FDs : Natural := 0;
   begin
      for Ent of Registry (Process).File_Table loop
         if Ent.Description = null then
            Count_Of_FDs := Count_Of_FDs + 1;
         end if;
      end loop;
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

   procedure Duplicate_FD_Table (Process, Target : PID) is
   begin
      for I in Registry (Process).File_Table'Range loop
         Registry (Target).File_Table (I).Close_On_Exec :=
            Registry (Process).File_Table (I).Close_On_Exec;
         if Registry (Process).File_Table (I).Description /= null then
            Duplicate
               (Registry (Process).File_Table (I).Description,
                Registry (Target).File_Table (I).Description);
         else
            Registry (Target).File_Table (I).Description := null;
         end if;
      end loop;
   end Duplicate_FD_Table;

   procedure Duplicate_Standard_FDs (Process, Target : PID) is
   begin
      for I in 0 .. 2 loop
         Registry (Target).File_Table (I).Close_On_Exec :=
            Registry (Process).File_Table (I).Close_On_Exec;
         if Registry (Process).File_Table (I).Description /= null then
            Duplicate
               (Registry (Process).File_Table (I).Description,
                Registry (Target).File_Table (I).Description);
         else
            Registry (Target).File_Table (I).Description := null;
         end if;
      end loop;
   end Duplicate_Standard_FDs;

   procedure Close (F : in out File_Description_Acc) is
      procedure Free is new Ada.Unchecked_Deallocation
         (File_Description, File_Description_Acc);
   begin
      if F.Children_Count = 0 then
         case F.Description is
            when Description_Reader_FIFO => Close_Reader (F.Inner_Reader_FIFO);
            when Description_Writer_FIFO => Close_Writer (F.Inner_Writer_FIFO);
            when Description_Primary_PTY => Close (F.Inner_Primary_PTY);
            when Description_Secondary_PTY => Close (F.Inner_Secondary_PTY);
            when Description_Device => null;
            when Description_Inode => VFS.Close (F.Inner_Ino_FS, F.Inner_Ino);
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
   begin
      if FD <= Unsigned_64 (File_Arr'Last) then
         return Registry (Process).File_Table (Natural (FD)).Description;
      else
         return null;
      end if;
   end Get_File;

   function Get_Close_On_Exec
      (Process  : PID;
       FD       : Unsigned_64) return Boolean
   is
   begin
      if FD <= Unsigned_64 (File_Arr'Last) then
         return Registry (Process).File_Table (Natural (FD)).Close_On_Exec;
      else
         return False;
      end if;
   end Get_Close_On_Exec;

   procedure Set_Close_On_Exec
      (Process  : PID;
       FD       : Unsigned_64;
       Is_Close : Boolean)
   is
   begin
      if FD <= Unsigned_64 (File_Arr'Last) then
         Registry (Process).File_Table (Natural (FD)).Close_On_Exec :=
            Is_Close;
      end if;
   end Set_Close_On_Exec;

   procedure Remove_File (Process : PID; FD : Natural) is
   begin
      if FD <= File_Arr'Last then
         Close (Registry (Process).File_Table (FD).Description);
         Registry (Process).File_Table (FD).Close_On_Exec := False;
      end if;
   end Remove_File;

   procedure Flush_Files (Process : PID) is
   begin
      for F of Registry (Process).File_Table loop
         if F.Description /= null then
            Close (F.Description);
         end if;
         F.Close_On_Exec := False;
      end loop;
   end Flush_Files;

   procedure Flush_Exec_Files (Process : PID) is
   begin
      for F of Registry (Process).File_Table loop
         if F.Description /= null and then F.Close_On_Exec then
            Close (F.Description);
            F.Close_On_Exec := False;
         end if;
      end loop;
   end Flush_Exec_Files;

   procedure Set_Common_Map (Proc : PID; Map : Arch.MMU.Page_Table_Acc) is
   begin
      Registry (Proc).Common_Map := Map;
   end Set_Common_Map;

   function Get_Common_Map (Proc : PID) return Arch.MMU.Page_Table_Acc is
   begin
      return Registry (Proc).Common_Map;
   end Get_Common_Map;

   function Get_Stack_Base (Process : PID) return Unsigned_64 is
   begin
      return Registry (Process).Stack_Base;
   end Get_Stack_Base;

   procedure Set_Stack_Base (Process : PID; Base : Unsigned_64) is
   begin
      Registry (Process).Stack_Base := Base;
   end Set_Stack_Base;

   procedure Get_Traced_Info
      (Process   : PID;
       Is_Traced : out Boolean;
       FD        : out Natural)
   is
   begin
      Is_Traced := Registry (Process).Is_Traced;
      FD        := Registry (Process).Tracer_FD;
   end Get_Traced_Info;

   procedure Set_Traced_Info
      (Process   : PID;
       Is_Traced : Boolean;
       FD        : Natural)
   is
   begin
      Registry (Process).Is_Traced := Is_Traced;
      Registry (Process).Tracer_FD := FD;
   end Set_Traced_Info;

   procedure Issue_Exit (Process : PID; Code : Unsigned_8) is
   begin
      Registry (Process).Did_Exit  := True;
      Registry (Process).Exit_Code := Code;
   end Issue_Exit;

   procedure Check_Exit
      (Process  : PID;
       Did_Exit : out Boolean;
       Code     : out Unsigned_8)
   is
   begin
      Did_Exit := Registry (Process).Did_Exit;
      Code     := Registry (Process).Exit_Code;
   end Check_Exit;

   procedure Set_CWD
      (Proc : PID;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number)
   is
   begin
      Registry (Proc).Current_Dir_FS  := FS;
      Registry (Proc).Current_Dir_Ino := Ino;
   end Set_CWD;

   procedure Get_CWD
      (Proc : PID;
       FS   : out VFS.FS_Handle;
       Ino  : out VFS.File_Inode_Number)
   is
   begin
      FS  := Registry (Proc).Current_Dir_FS;
      Ino := Registry (Proc).Current_Dir_Ino;
   end Get_CWD;

   function Get_Alloc_Base (Process : PID) return Unsigned_64 is
   begin
      return Registry (Process).Alloc_Base;
   end Get_Alloc_Base;

   procedure Set_Alloc_Base (Process : PID; Base : Unsigned_64) is
   begin
      Registry (Process).Alloc_Base := Base;
   end Set_Alloc_Base;

   function Get_Parent (Proc : PID) return PID is
   begin
      return Registry (Proc).Parent;
   end Get_Parent;

   procedure Set_Identifier (Proc : PID; Name : String) is
      Length : Natural;
   begin
      if Name'Length > Max_Name_Length then
         Length := Max_Name_Length;
      else
         Length := Name'Length;
      end if;

      Registry (Proc).Identifier_Len           := Length;
      Registry (Proc).Identifier (1 .. Length) :=
         Name (Name'First .. Name'First + Length - 1);
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

      ID (ID'First .. ID'First + Length - 1) :=
         Registry (Proc).Identifier (1 .. Length);
      Len := Length;
   end Get_Identifier;

   procedure Get_UID (Proc : PID; UID : out Unsigned_32) is
   begin
      UID := Registry (Proc).User;
   end Get_UID;

   procedure Set_UID (Proc : PID; UID : Unsigned_32) is
   begin
      Registry (Proc).User := UID;
   end Set_UID;

   procedure Get_Effective_UID (Proc : PID; EUID : out Unsigned_32) is
   begin
      EUID := Registry (Proc).Effective_User;
   end Get_Effective_UID;

   procedure Set_Effective_UID (Proc : PID; EUID : Unsigned_32) is
   begin
      Registry (Proc).Effective_User := EUID;
   end Set_Effective_UID;

   procedure Get_Umask (Proc : PID; Umask : out VFS.File_Mode) is
   begin
      Umask := Registry (Proc).Umask;
   end Get_Umask;

   procedure Set_Umask (Proc : PID; Umask : VFS.File_Mode) is
   begin
      Registry (Proc).Umask := Umask;
   end Set_Umask;

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

   function Check_Permissions
      (Proc : PID;
       Dev  : Devices.Device_Handle) return MAC.Permissions
   is
   begin
      return MAC.Check_Permissions (Registry (Proc).Perms, Dev);
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

   procedure Add_Entity
      (Proc   : PID;
       Dev    : Devices.Device_Handle;
       Perms  : MAC.Permissions;
       Status : out MAC.Addition_Status)
   is
   begin
      MAC.Add_Entity (Registry (Proc).Perms, Dev, Perms, Status);
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
