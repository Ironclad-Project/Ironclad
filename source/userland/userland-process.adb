--  userland-process.adb: Process registry and handler.
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

with Lib.Alignment;
with Ada.Unchecked_Deallocation;
with Arch.Local;
with Cryptography.Random;
with Userland.Memory_Locations;

package body Userland.Process with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation
      (Process_Data, Process_Data_Acc);

   procedure Init is
   begin
      Registry := new Process_Arr'(others => null);
      Lib.Synchronization.Release (Registry_Mutex);
   end Init;

   function Get_Process_Count return Natural is
      Count : Natural := 0;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry.all'Range loop
         if Registry (I) /= null then
            Count := Count + 1;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
      return Count;
   end Get_Process_Count;

   function Get_Children (Proc : PID; Buf : out Children_Arr) return Natural is
      Count, Index : Natural := 0;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry.all'Range loop
         if Registry (I) /= null and then Registry (I).Parent = Proc
         then
            Count := Count + 1;
            if Index < Buf'Length then
               Buf (Buf'First + Index) := I;
               Index                   := Index + 1;
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
      return Count;
   end Get_Children;

   procedure List_All (List : out Process_Info_Arr; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
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
                   Is_Being_Traced => Registry (I).Is_Traced,
                   Has_Exited      => Registry (I).Did_Exit);
               Curr_Index := Curr_Index + 1;
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
   end List_All;

   function Create_Process (Parent : PID := Error_PID) return PID is
      P : PID renames Parent;
      Returned : PID := Error_PID;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);

      for I in Registry.all'Range loop
         if Registry (I) = null then
            Registry (I) := new Process_Data'
               (Did_Exit    => False,
                Is_Traced   => False,
                Tracer_FD   => 0,
                Exit_Code   => 0,
                Thread_List => (others => 0),
                File_Table  => (others => (False, null)),
                Common_Map  => null,
                others      => <>);

            if Parent /= Error_PID then
               Registry (I).Parent          := Parent;
               Registry (I).Stack_Base      := Registry (P).Stack_Base;
               Registry (I).Alloc_Base      := Registry (P).Alloc_Base;
               Registry (I).Current_Dir_Len := Registry (P).Current_Dir_Len;
               Registry (I).Current_Dir     := Registry (P).Current_Dir;
               Registry (I).Perms           := Registry (P).Perms;
               Registry (I).User            := Registry (P).User;
               Registry (I).Effective_User  := Registry (P).Effective_User;
               Registry (I).Umask           := Registry (P).Umask;
            else
               Reroll_ASLR (PID (I));
               Registry (I).Parent          := 0;
               Registry (I).Current_Dir_Len := 1;
               Registry (I).Current_Dir (1) := '/';
               Registry (I).Perms           := MAC.Default_Permissions;
               Registry (I).User            := 0;
               Registry (I).Effective_User  := 0;
               Registry (I).Umask           := Default_Umask;
            end if;

            Returned := PID (I);
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (Registry_Mutex);
      return Returned;
   end Create_Process;

   procedure Delete_Process (Process : PID) is
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      Free (Registry (Process));
      Lib.Synchronization.Release (Registry_Mutex);
   end Delete_Process;

   function Add_Thread (Proc : PID; Thread : Scheduler.TID) return Boolean is
   begin
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = 0 then
            Registry (Proc).Thread_List (I) := Thread;
            return True;
         end if;
      end loop;
      return False;
   end Add_Thread;

   procedure Remove_Thread (Proc : PID; Thread : Scheduler.TID) is
   begin
      for I in Registry (Proc).Thread_List'Range loop
         if Registry (Proc).Thread_List (I) = Thread then
            Registry (Proc).Thread_List (I) := 0;
            exit;
         end if;
      end loop;
   end Remove_Thread;

   procedure Flush_Threads (Proc : PID) is
      Current_Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      for Thread of Registry (Proc).Thread_List loop
         if Thread /= Current_Thread then
            Scheduler.Delete_Thread (Thread);
         end if;
         Thread := 0;
      end loop;
   end Flush_Threads;

   procedure Reroll_ASLR (Process : PID) is
      package Aln is new Lib.Alignment (Unsigned_64);
      Rand_Addr, Rand_Jump : Unsigned_64;
   begin
      Rand_Addr := Cryptography.Random.Get_Integer
         (Memory_Locations.Mmap_Anon_Min,
          Memory_Locations.Mmap_Anon_Max);
      Rand_Jump := Cryptography.Random.Get_Integer
         (Memory_Locations.Stack_Jump_Min,
          Memory_Locations.Stack_Jump_Max);

      Rand_Addr := Aln.Align_Up (Rand_Addr, Memory.Virtual.Page_Size);
      Rand_Jump := Aln.Align_Up (Rand_Jump, Memory.Virtual.Page_Size);

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

   function Add_File
      (Process : PID;
       File    : File_Description_Acc;
       FD      : out Natural;
       Start   : Natural := 0) return Boolean
   is
   begin
      for I in Start .. Registry (Process).File_Table'Last loop
         if Registry (Process).File_Table (I).Description = null then
            Registry (Process).File_Table (I).Description := File;
            FD := I;
            return True;
         end if;
      end loop;
      FD := 0;
      return False;
   end Add_File;

   function Duplicate (F : File_Description_Acc) return File_Description_Acc is
   begin
      if F /= null then
         F.Children_Count := F.Children_Count + 1;
      end if;
      return F;
   end Duplicate;

   function Duplicate (Proc : PID; FD : Natural) return File_Description_Acc is
   begin
      return Duplicate (Registry (Proc).File_Table (FD).Description);
   end Duplicate;

   procedure Duplicate_FD_Table (Process, Target : PID) is
   begin
      for I in Registry (Process).File_Table'Range loop
         Registry (Target).File_Table (I).Close_On_Exec :=
            Registry (Process).File_Table (I).Close_On_Exec;
         Registry (Target).File_Table (I).Description :=
            Duplicate (Registry (Process).File_Table (I).Description);
      end loop;
   end Duplicate_FD_Table;

   procedure Close (F : in out File_Description_Acc) is
      procedure Free is new Ada.Unchecked_Deallocation
         (File_Description, File_Description_Acc);
   begin
      if F /= null then
         if F.Children_Count = 0 then
            case F.Description is
               when Description_Reader_FIFO =>
                  Close_Reader (F.Inner_Reader_FIFO);
               when Description_Writer_FIFO =>
                  Close_Writer (F.Inner_Writer_FIFO);
               when Description_Primary_PTY =>
                  Close (F.Inner_Primary_PTY);
               when Description_Secondary_PTY =>
                  Close (F.Inner_Secondary_PTY);
               when Description_Device =>
                  null;
               when Description_Inode =>
                  VFS.Close (F.Inner_Ino_FS, F.Inner_Ino);
            end case;
            Free (F);
         else
            F.Children_Count := F.Children_Count - 1;
         end if;
         F := null;
      end if;
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
         Close (F.Description);
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

   procedure Set_Common_Map (Proc : PID; Map : Memory.Virtual.Page_Map_Acc) is
   begin
      Registry (Proc).Common_Map := Map;
   end Set_Common_Map;

   function Get_Common_Map (Proc : PID) return Memory.Virtual.Page_Map_Acc is
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

   function Set_CWD (Proc : PID; CWD : String) return Boolean is
   begin
      Registry (Proc).Current_Dir (1 .. CWD'Length) := CWD;
      Registry (Proc).Current_Dir_Len               := CWD'Length;
      return True;
   end Set_CWD;

   procedure Get_CWD
      (Proc : PID;
       CWD  : out String;
       Len  : out Natural)
   is
      Length : Natural;
   begin
      if CWD'Length > Registry (Proc).Current_Dir_Len then
         Length := Registry (Proc).Current_Dir_Len;
      else
         Length := CWD'Length;
      end if;

      CWD (CWD'First .. CWD'First + Length - 1) :=
         Registry (Proc).Current_Dir (1 .. Length);
      Len := Length;
   end Get_CWD;

   function Get_MAC (Proc : PID) return MAC.Permissions is
   begin
      return Registry (Proc).Perms;
   end Get_MAC;

   procedure Set_MAC (Proc : PID; Perms : MAC.Permissions) is
   begin
      Registry (Proc).Perms := Perms;
   end Set_MAC;

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
end Userland.Process;
