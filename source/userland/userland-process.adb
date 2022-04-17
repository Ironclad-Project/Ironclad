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

package body Userland.Process is
   procedure Free is new Ada.Unchecked_Deallocation
      (Memory.Virtual.Page_Map, Memory.Virtual.Page_Map_Acc);
   procedure Free_File is new Ada.Unchecked_Deallocation
      (VFS.File.File, VFS.File.File_Acc);

   --  FDs start at 0 for userspace, while PIDs start at 1, 0 is error value.
   type Process_Data_Threads is array (1 .. 20) of Scheduler.TID;
   type Process_File_Table   is array (0 .. 99) of VFS.File.File_Acc;
   type Process_Data is record
      Is_Used      : Boolean;
      Parent       : PID;
      Current_Root : VFS.Root_Name;
      Thread_List  : Process_Data_Threads;
      File_Table   : Process_File_Table;
      Common_Map   : access Memory.Virtual.Page_Map;
      Stack_Base   : Unsigned_64;
      Alloc_Base   : Unsigned_64;
   end record;

   type Process_Arr is array (1 .. 256) of Process_Data;
   type Process_Arr_Acc is access Process_Arr;

   Process_List_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Process_List       : Process_Arr_Acc;

   procedure Init is
   begin
      Process_List := new Process_Arr;
      Lib.Synchronization.Release (Process_List_Mutex'Access);
   end Init;

   function Create_Process (Parent : PID := Error_PID) return PID is
      Ret_PID : PID := Error_PID;
   begin
      Lib.Synchronization.Seize (Process_List_Mutex'Access);

      for I in Process_List.all'Range loop
         if not Process_List (I).Is_Used then
            Process_List (I).Is_Used    := True;
            Process_List (I).Parent     := Parent;
            Process_List (I).Stack_Base := 16#70000000000#;
            Process_List (I).Alloc_Base := 16#80000000000#;
            Ret_PID := PID (I);
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (Process_List_Mutex'Access);
      return Ret_PID;
   end Create_Process;

   procedure Delete_Process (Process : PID) is
   begin
      if Is_Valid_Process (Process) then
         Lib.Synchronization.Seize (Process_List_Mutex'Access);
         Process_List (Natural (Process)).Is_Used := False;
         Free (Process_List (Natural (Process)).Common_Map);
         Lib.Synchronization.Release (Process_List_Mutex'Access);
      end if;
   end Delete_Process;

   function Get_Process_By_Thread (Thread : Scheduler.TID) return PID is
   begin
      if Thread /= 0 then
         for I in Process_List.all'Range loop
            if Process_List (I).Is_Used then
               for T of Process_List (I).Thread_List loop
                  if T = Thread then
                     return PID (I);
                  end if;
               end loop;
            end if;
         end loop;
      end if;
      return Error_PID;
   end Get_Process_By_Thread;

   function Add_Thread (Process : PID; Threa : Scheduler.TID) return Boolean is
      Proc_Index : constant Natural := Natural (Process);
   begin
      if not Is_Valid_Process (Process) then
         return False;
      else
         for I in Process_List (Proc_Index).Thread_List'Range loop
            if Process_List (Proc_Index).Thread_List (I) = 0 then
               Process_List (Proc_Index).Thread_List (I) := Threa;
               return True;
            end if;
         end loop;
         return False;
      end if;
   end Add_Thread;

   procedure Remove_Thread (Process : PID; Thread : Scheduler.TID) is
      Proc_Index : constant Natural := Natural (Process);
   begin
      if Is_Valid_Process (Process) then
         for I in Process_List (Proc_Index).Thread_List'Range loop
            if Process_List (Proc_Index).Thread_List (I) = Thread then
               Process_List (Proc_Index).Thread_List (I) := 0;
            end if;
         end loop;
      end if;
   end Remove_Thread;

   procedure Flush_Threads (Process : PID) is
      Proc_Index : constant Natural := Natural (Process);
   begin
      if Is_Valid_Process (Process) then
         for Thread of Process_List (Proc_Index).Thread_List loop
            if Thread /= 0 then
               Scheduler.Delete_Thread (Thread);
               Thread := 0;
            end if;
         end loop;
      end if;
   end Flush_Threads;

   function Add_File
      (Process : PID;
       File    : VFS.File.File_Acc;
       FD      : out Natural) return Boolean
   is
      Proc_Index : constant Natural := Natural (Process);
   begin
      if Is_Valid_Process (Process) then
         for I in Process_List (Proc_Index).File_Table'Range loop
            if Process_List (Proc_Index).File_Table (I) = null then
               Process_List (Proc_Index).File_Table (I) := File;
               FD := I;
               return True;
            end if;
         end loop;
      end if;
      FD := 0;
      return False;
   end Add_File;

   function Get_File (Process : PID; FD : Natural) return VFS.File.File_Acc is
   begin
      if Is_Valid_Process (Process) then
         return Process_List (Natural (Process)).File_Table (FD);
      end if;
      return null;
   end Get_File;

   procedure Remove_File (Process : PID; FD : Natural) is
   begin
      if Is_Valid_Process (Process) then
         VFS.File.Close (Process_List (Natural (Process)).File_Table (FD));
         Free_File (Process_List (Natural (Process)).File_Table (FD));
         Process_List (Natural (Process)).File_Table (FD) := null;
      end if;
   end Remove_File;

   procedure Set_Current_Root (Process : PID; Root : VFS.Root_Name) is
   begin
      if Is_Valid_Process (Process) then
         Process_List (Natural (Process)).Current_Root := Root;
      end if;
   end Set_Current_Root;

   function Get_Current_Root (Process : PID) return VFS.Root_Name is
   begin
      if Is_Valid_Process (Process) then
         return Process_List (Natural (Process)).Current_Root;
      else
         return "errodev";
      end if;
   end Get_Current_Root;

   function Get_Parent_Process (Process : PID) return PID is
   begin
      if Is_Valid_Process (Process) then
         return Process_List (Natural (Process)).Parent;
      else
         return Error_PID;
      end if;
   end Get_Parent_Process;

   procedure Set_Memmap (Process : PID; Map : Memory.Virtual.Page_Map_Acc) is
   begin
      if Is_Valid_Process (Process) then
         Process_List (Natural (Process)).Common_Map := Map;
      end if;
   end Set_Memmap;

   function Get_Memmap (Process : PID) return Memory.Virtual.Page_Map_Acc is
   begin
      if Is_Valid_Process (Process) then
         return Process_List (Natural (Process)).Common_Map;
      else
         return null;
      end if;
   end Get_Memmap;

   function Bump_Stack (Process : PID; Val : Unsigned_64) return Unsigned_64 is
      Proc_Index : constant Natural := Natural (Process);
   begin
      if Is_Valid_Process (Process) then
         declare
            Returned : constant Unsigned_64 :=
               Process_List (Proc_Index).Stack_Base;
         begin
            Process_List (Proc_Index).Stack_Base := Returned + Val;
            return Returned;
         end;
      else
         return 0;
      end if;
   end Bump_Stack;

   function Bump_Alloc (Process : PID; Val : Unsigned_64) return Unsigned_64 is
      Proc_Index : constant Natural := Natural (Process);
   begin
      if Is_Valid_Process (Process) then
         declare
            Returned : constant Unsigned_64 :=
               Process_List (Proc_Index).Alloc_Base;
         begin
            Process_List (Proc_Index).Alloc_Base := Returned + Val;
            return Returned;
         end;
      else
         return 0;
      end if;
   end Bump_Alloc;

   function Is_Valid_Process (Process : PID) return Boolean is
   begin
      return Process /= Error_PID and Process <= Process_List'Length;
   end Is_Valid_Process;
end Userland.Process;
