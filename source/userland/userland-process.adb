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
with Arch.CPU;

package body Userland.Process is
   procedure Free_Proc is new Ada.Unchecked_Deallocation
      (Process_Data, Process_Data_Acc);
   procedure Free_File is new Ada.Unchecked_Deallocation
      (VFS.File.File, VFS.File.File_Acc);

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
            Process_List (I).Exit_Code   := 0;

            --  If we have a parent, set ourselves as a child and fetch data.
            if Parent /= null then
               for PID of Parent.Children loop
                  if PID = 0 then
                     PID := I;
                     exit;
                  end if;
               end loop;

               Process_List (I).Parent_PID := Parent.Process_PID;
               Process_List (I).Stack_Base := Parent.Stack_Base;
               Process_List (I).Alloc_Base := Parent.Alloc_Base;
            else
               Process_List (I).Parent_PID := 0;
               Process_List (I).Stack_Base := 16#70000000000#;
               Process_List (I).Alloc_Base := 16#80000000000#;
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

   function Fork (Parent : Process_Data_Acc) return Process_Data_Acc is
      Child : constant Process_Data_Acc := Create_Process (Parent);
   begin
      if Child = null then
         return null;
      end if;

      declare
         Child_PID : constant Positive := Child.Process_PID;
      begin
         --  Assign all data.
         Child.all := Parent.all;

         --  Reassign PIDs.
         Child.Process_PID := Child_PID;
         Child.Parent_PID  := Parent.Process_PID;

         --  Clear threads for the forked process.
         for T of Child.Thread_List loop
            T := 0;
         end loop;

         --  Clone the file table.
         for I in Parent.File_Table'Range loop
            if Parent.File_Table (I) /= null then
               Child.File_Table (I) := new File'(Parent.File_Table (I).all);
            end if;
         end loop;
      end;

      return Child;
   end Fork;

   function Add_Thread
      (Process : Process_Data_Acc;
       Thread  : Scheduler.TID) return Boolean is
   begin
      for I in Process.Thread_List'Range loop
         if Process.Thread_List (I) = 0 then
            Process.Thread_List (I) := Thread;
            return True;
         end if;
      end loop;
      return False;
   end Add_Thread;

   procedure Remove_Thread
      (Process : Process_Data_Acc;
       Thread  : Scheduler.TID) is
   begin
      for I in Process.Thread_List'Range loop
         if Process.Thread_List (I) = Thread then
            Process.Thread_List (I) := 0;
         end if;
      end loop;
   end Remove_Thread;

   procedure Flush_Threads (Process : Process_Data_Acc) is
      Current_Thread : constant TID := Arch.CPU.Get_Local.Current_Thread;
   begin
      for Thread of Process.Thread_List loop
         if Thread /= Current_Thread then
            Scheduler.Delete_Thread (Thread);
         end if;

         Thread := 0;
      end loop;
   end Flush_Threads;

   --  Add and remove files to the process file descriptor table.
   function Add_File
      (Process : Process_Data_Acc;
       File    : VFS.File.File_Acc;
       FD      : out Natural) return Boolean is
   begin
      for I in Process.File_Table'Range loop
         if Process.File_Table (I) = null then
            Process.File_Table (I) := File;
            FD := I;
            return True;
         end if;
      end loop;
      FD := 0;
      return False;
   end Add_File;

   procedure Remove_File (Process : Process_Data_Acc; FD : Natural) is
   begin
      VFS.File.Close (Process.File_Table (FD));
      Free_File (Process.File_Table (FD));
      Process.File_Table (FD) := null;
   end Remove_File;

   procedure Flush_Files (Process : Process_Data_Acc) is
   begin
      for F of Process.File_Table loop
         if F /= null then
            VFS.File.Close (F);
            Free_File (F);
            F := null;
         end if;
      end loop;
   end Flush_Files;
end Userland.Process;
