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
with Ada.Unchecked_Deallocation;
with Arch.Local;

package body Userland.Process with SPARK_Mode => Off is
   procedure Free_Proc is new Ada.Unchecked_Deallocation
      (Process_Data, Process_Data_Acc);

   --  Process registry and its lock, the index happens to do as PID as well.
   type Process_Arr is array (1 .. 256) of Process_Data_Acc;
   type Process_Arr_Acc is access Process_Arr;

   Process_List_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Process_List       : Process_Arr_Acc;

   procedure Init is
   begin
      Process_List := new Process_Arr;
      Lib.Synchronization.Release (Process_List_Mutex'Access);
   end Init;

   function Create_Process
      (Parent : Process_Data_Acc := null) return Process_Data_Acc
   is
      Returned : Process_Data_Acc := null;
   begin
      Lib.Synchronization.Seize (Process_List_Mutex'Access);

      for I in Process_List.all'Range loop
         if Process_List (I) = null then
            Process_List (I) := new Process_Data;
            Process_List (I).Process_PID := I;
            Process_List (I).Did_Exit    := False;
            Process_List (I).Exit_Code   := 0;

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
            else
               Process_List (I).Parent_PID      := 0;
               Process_List (I).Stack_Base      := 16#70000000000#;
               Process_List (I).Alloc_Base      := 16#80000000000#;
               Process_List (I).Current_Dir_Len := 1;
               Process_List (I).Current_Dir (1) := '/';
            end if;

            Returned := Process_List (I);
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (Process_List_Mutex'Access);
      return Returned;
   end Create_Process;

   procedure Delete_Process (Process : Process_Data_Acc) is
   begin
      if Process /= null then
         Lib.Synchronization.Seize (Process_List_Mutex'Access);

         if Process.Parent_PID /= 0 then
            declare
               Parent_Process : constant Process_Data_Acc :=
                  Get_By_PID (Process.Parent_PID);
            begin
               if Parent_Process /= null then
                  for PID of Parent_Process.Children loop
                     if PID = Process.Process_PID then
                        PID := 0;
                        exit;
                     end if;
                  end loop;
               end if;
            end;
         end if;

         Free_Proc (Process_List (Process.Process_PID));
         Process_List (Process.Process_PID) := null;

         Lib.Synchronization.Release (Process_List_Mutex'Access);
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

   function Is_Valid_File
      (Process : Process_Data_Acc;
       FD      : Unsigned_64) return Boolean
   is
   begin
      return Process /= null                             and then
             FD <= Unsigned_64 (Process_File_Table'Last) and then
             Process.File_Table (Natural (FD)).Inner /= null;
   end Is_Valid_File;

   function Get_File
      (Process : Process_Data_Acc;
       FD      : Unsigned_64) return VFS.File.File_Acc
   is
   begin
      if Process = null or FD > Unsigned_64 (Process_File_Table'Last) then
         return null;
      else
         return Process.File_Table (Natural (FD)).Inner;
      end if;
   end Get_File;

   function Add_File
      (Process       : Process_Data_Acc;
       File          : VFS.File.File_Acc;
       FD            : out Natural;
       Close_On_Exec : Boolean := False) return Boolean
   is
   begin
      if Process /= null then
         for I in Process.File_Table'Range loop
            if Process.File_Table (I).Inner = null then
               Process.File_Table (I) := (
                  Close_On_Exec => Close_On_Exec,
                  Inner         => File
               );
               FD := I;
               return True;
            end if;
         end loop;
      end if;
      FD := 0;
      return False;
   end Add_File;

   function Replace_File
      (Process       : Process_Data_Acc;
       File          : VFS.File.File_Acc;
       Old_FD        : Natural;
       Close_On_Exec : Boolean := False) return Boolean
   is
   begin
      if Process = null or Old_FD > Process_File_Table'Last or File = null then
         return False;
      end if;
      if Process.File_Table (Old_FD).Inner /= null then
         VFS.File.Close (Process.File_Table (Old_FD).Inner);
      end if;
      Process.File_Table (Old_FD) := (
         Close_On_Exec => Close_On_Exec,
         Inner         => File
      );
      return True;
   end Replace_File;

   procedure Remove_File (Process : Process_Data_Acc; FD : Natural) is
   begin
      if Process /= null then
         VFS.File.Close (Process.File_Table (FD).Inner);
         Process.File_Table (FD).Inner := null;
      end if;
   end Remove_File;

   procedure Flush_Files (Process : Process_Data_Acc) is
   begin
      if Process /= null then
         for F of Process.File_Table loop
            if F.Inner /= null then
               VFS.File.Close (F.Inner);
               F.Inner := null;
            end if;
         end loop;
      end if;
   end Flush_Files;

   procedure Flush_Exec_Files (Process : Process_Data_Acc) is
   begin
      if Process /= null then
         for F of Process.File_Table loop
            if F.Inner /= null and F.Close_On_Exec then
               VFS.File.Close (F.Inner);
               F.Inner := null;
            end if;
         end loop;
      end if;
   end Flush_Exec_Files;
end Userland.Process;
