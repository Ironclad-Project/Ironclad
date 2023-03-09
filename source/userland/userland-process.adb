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

with Lib.Synchronization;
with Lib.Alignment;
with Ada.Unchecked_Deallocation;
with Arch.Local;
with Cryptography.Random;
with Userland.Memory_Locations;

package body Userland.Process with SPARK_Mode => Off is
   procedure Free_Proc is new Ada.Unchecked_Deallocation
      (Process_Data, Process_Data_Acc);

   --  Process registry and its lock, the index happens to do as PID as well.
   type Process_Arr is array (1 .. Max_Process_Count) of Process_Data_Acc;
   type Process_Arr_Acc is access Process_Arr;

   Process_List_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Process_List       : Process_Arr_Acc;

   procedure Init is
   begin
      Process_List := new Process_Arr'(others => null);
      Lib.Synchronization.Release (Process_List_Mutex);
   end Init;

   function Get_Process_Count return Natural is
      Count : Natural := 0;
   begin
      Lib.Synchronization.Seize (Process_List_Mutex);
      for I in Process_List.all'Range loop
         if Process_List (I) /= null then
            Count := Count + 1;
         end if;
      end loop;
      Lib.Synchronization.Release (Process_List_Mutex);
      return Count;
   end Get_Process_Count;

   function Create_Process
      (Parent : Process_Data_Acc := null) return Process_Data_Acc
   is
      Returned : Process_Data_Acc := null;
   begin
      Lib.Synchronization.Seize (Process_List_Mutex);

      for I in Process_List.all'Range loop
         if Process_List (I) = null then
            Process_List (I) := new Process_Data'
               (Process_PID => I,
                Did_Exit    => False,
                Exit_Code   => 0,
                Children    => (others => 0),
                Thread_List => (others => 0),
                File_Table  => (others => null),
                Common_Map  => null,
                others      => <>);

            --  If we have a parent, set ourselves as a child and fetch data.
            if Parent /= null then
               for PID of Parent.Children loop
                  if PID = 0 then
                     PID := I;
                     exit;
                  end if;
               end loop;

               Process_List (I).Parent_PID      := Parent.Process_PID;
               Process_List (I).Stack_Base      := Parent.Stack_Base;
               Process_List (I).Alloc_Base      := Parent.Alloc_Base;
               Process_List (I).Current_Dir_Len := Parent.Current_Dir_Len;
               Process_List (I).Current_Dir     := Parent.Current_Dir;
               Process_List (I).Perms           := Parent.Perms;
            else
               Reroll_ASLR (Process_List (I));
               Process_List (I).Parent_PID      := 0;
               Process_List (I).Current_Dir_Len := 1;
               Process_List (I).Current_Dir (1) := '/';
               Process_List (I).Perms           := MAC.Default_Permissions;
            end if;

            Returned := Process_List (I);
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (Process_List_Mutex);
      return Returned;
   end Create_Process;

   procedure Delete_Process (Process : Process_Data_Acc) is
      Parent_Process : Process_Data_Acc;
   begin
      if Process /= null then
         Lib.Synchronization.Seize (Process_List_Mutex);

         if Process.Parent_PID /= 0 then
            Parent_Process := Get_By_PID (Process.Parent_PID);
            if Parent_Process /= null then
               for PID of Parent_Process.Children loop
                  if PID = Process.Process_PID then
                     PID := 0;
                     exit;
                  end if;
               end loop;
            end if;
         end if;
         Free_Proc (Process_List (Process.Process_PID));
         Process_List (Process.Process_PID) := null;

         Lib.Synchronization.Release (Process_List_Mutex);
      end if;
   end Delete_Process;

   function Get_By_PID (Process : Positive) return Process_Data_Acc is
   begin
      return Process_List (Process);
   end Get_By_PID;

   function Get_By_Thread (Thread : Scheduler.TID) return Process_Data_Acc is
   begin
      for I in Process_List'Range loop
         if Process_List (I) /= null then
            for T of Process_List (I).Thread_List loop
               if T = Thread then
                  return Process_List (I);
               end if;
            end loop;
         end if;
      end loop;

      return null;
   end Get_By_Thread;

   function Add_Thread
      (Process : Process_Data_Acc;
       Thread  : Scheduler.TID) return Boolean is
   begin
      if Process /= null then
         for I in Process.Thread_List'Range loop
            if Process.Thread_List (I) = 0 then
               Process.Thread_List (I) := Thread;
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Add_Thread;

   procedure Remove_Thread
      (Process : Process_Data_Acc;
       Thread  : Scheduler.TID) is
   begin
      if Process /= null then
         for I in Process.Thread_List'Range loop
            if Process.Thread_List (I) = Thread then
               Process.Thread_List (I) := 0;
            end if;
         end loop;
      end if;
   end Remove_Thread;

   procedure Flush_Threads (Process : Process_Data_Acc) is
      Current_Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      if Process /= null then
         for Thread of Process.Thread_List loop
            if Thread /= Current_Thread then
               Scheduler.Delete_Thread (Thread);
            end if;

            Thread := 0;
         end loop;
      end if;
   end Flush_Threads;

   procedure Reroll_ASLR (Process : Process_Data_Acc) is
      package Aln is new Lib.Alignment (Unsigned_64);
      Rand_Addr, Rand_Jump : Unsigned_64;
   begin
      --  Get ASLR bases and ensure they are 4K aligned.
      Rand_Addr := Cryptography.Random.Get_Integer
         (Memory_Locations.Mmap_Anon_Min,
          Memory_Locations.Mmap_Anon_Max);
      Rand_Jump := Cryptography.Random.Get_Integer
         (Memory_Locations.Stack_Jump_Min,
          Memory_Locations.Stack_Jump_Max);

      --  Ensure they are page aligned.
      Rand_Addr := Aln.Align_Up (Rand_Addr, Memory.Virtual.Page_Size);
      Rand_Jump := Aln.Align_Up (Rand_Jump, Memory.Virtual.Page_Size);

      Process.Alloc_Base := Rand_Addr;
      Process.Stack_Base := Rand_Addr + Rand_Jump;
   end Reroll_ASLR;

   function Is_Valid_File
      (Process : Process_Data_Acc;
       FD      : Unsigned_64) return Boolean
   is
   begin
      if Process /= null and FD <= Unsigned_64 (Process_File_Table'Last) then
         return Process.File_Table (Natural (FD)) /= null;
      else
         return False;
      end if;
   end Is_Valid_File;

   function Add_File
      (Process : Process_Data_Acc;
       File    : File_Description_Acc;
       FD      : out Natural) return Boolean
   is
   begin
      if Process /= null then
         for I in Process.File_Table'Range loop
            if Process.File_Table (I) = null then
               Process.File_Table (I) := File;
               FD := I;
               return True;
            end if;
         end loop;
      end if;
      FD := 0;
      return False;
   end Add_File;

   function Duplicate (F : File_Description_Acc) return File_Description_Acc is
      Ret : File_Description_Acc := null;
   begin
      if F /= null then
         Ret := new File_Description'(F.all);
         case Ret.Description is
            when Description_Reader_Pipe =>
               Increase_Refcount (Ret.Inner_Reader_Pipe);
            when Description_Writer_Pipe =>
               Increase_Refcount (Ret.Inner_Writer_Pipe);
            when Description_Primary_PTY =>
               IPC.PTY.Increase_Refcount (Ret.Inner_Primary_PTY);
            when Description_Secondary_PTY =>
               IPC.PTY.Increase_Refcount (Ret.Inner_Secondary_PTY);
            when Description_File =>
               Increase_Refcount (Ret.Inner_File);
         end case;
      end if;
      return Ret;
   end Duplicate;

   procedure Close (F : in out File_Description_Acc) is
      procedure Free is new Ada.Unchecked_Deallocation
         (File_Description, File_Description_Acc);
   begin
      if F /= null then
         case F.Description is
            when Description_Reader_Pipe   => Close (F.Inner_Reader_Pipe);
            when Description_Writer_Pipe   => Close (F.Inner_Writer_Pipe);
            when Description_Primary_PTY   => Close (F.Inner_Primary_PTY);
            when Description_Secondary_PTY => Close (F.Inner_Secondary_PTY);
            when Description_File          => Close (F.Inner_File);
         end case;
         Free (F);
         F := null;
      end if;
   end Close;

   function Get_File
      (Process : Process_Data_Acc;
       FD      : Unsigned_64) return File_Description_Acc
   is
   begin
      if Process /= null and FD <= Unsigned_64 (Process_File_Table'Last) then
         return Process.File_Table (Natural (FD));
      else
         return null;
      end if;
   end Get_File;

   function Replace_File
      (Process : Process_Data_Acc;
       File    : File_Description_Acc;
       Old_FD  : Natural) return Boolean
   is
   begin
      if Process = null or Old_FD > Process_File_Table'Last or File = null then
         return False;
      end if;
      Remove_File (Process, Old_FD);
      Process.File_Table (Old_FD) := File;
      return True;
   end Replace_File;

   procedure Remove_File (Process : Process_Data_Acc; FD : Natural) is
   begin
      if Process /= null and FD <= Process_File_Table'Last then
         Close (Process.File_Table (FD));
      end if;
   end Remove_File;

   procedure Flush_Files (Process : Process_Data_Acc) is
   begin
      if Process /= null then
         for F of Process.File_Table loop
            Close (F);
         end loop;
      end if;
   end Flush_Files;

   procedure Flush_Exec_Files (Process : Process_Data_Acc) is
   begin
      if Process /= null then
         for F of Process.File_Table loop
            if F /= null and then F.Close_On_Exec then
               Close (F);
            end if;
         end loop;
      end if;
   end Flush_Exec_Files;
end Userland.Process;
