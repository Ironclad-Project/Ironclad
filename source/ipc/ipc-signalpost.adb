--  ipc-signalpost.adb: Signal post creation and management.
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

with Ada.Unchecked_Deallocation;
with Arch.Local;
with Userland.Process;
with Scheduler;

package body IPC.SignalPost is
   function Create (Is_Blocking : Boolean := True) return SignalPost_Acc is
   begin
      return new SignalPost'
         (Lib.Synchronization.Unlocked_Semaphore,
          Is_Blocking);
   end Create;

   procedure Close (To_Close : in out SignalPost_Acc) is
      procedure Free is new Ada.Unchecked_Deallocation
         (SignalPost, SignalPost_Acc);
   begin
      Lib.Synchronization.Seize (To_Close.Mutex);
      Free (To_Close);
   end Close;

   function Is_Blocking (Post : SignalPost_Acc) return Boolean is
      Returned : Boolean;
   begin
      Lib.Synchronization.Seize (Post.Mutex);
      Returned := Post.Is_Blocking;
      Lib.Synchronization.Release (Post.Mutex);
      return Returned;
   end Is_Blocking;

   procedure Set_Blocking (Post : SignalPost_Acc; Is_Blocking : Boolean) is
   begin
      Lib.Synchronization.Seize (Post.Mutex);
      Post.Is_Blocking := Is_Blocking;
      Lib.Synchronization.Release (Post.Mutex);
   end Set_Blocking;

   procedure Poll
      (Post     : SignalPost_Acc;
       Can_Read : out Boolean;
       Is_Error : out Boolean)
   is
      Proc : constant Userland.Process.PID := Arch.Local.Get_Current_Process;
      Sigs : Userland.Process.Signal_Bitmap;
   begin
      Can_Read := False;
      Is_Error := False;

      Lib.Synchronization.Seize (Post.Mutex);
      Userland.Process.Get_Raised_Signals (Proc, Sigs);
      for V of Sigs loop
         if V then
            Can_Read := True;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Post.Mutex);
   end Poll;

   procedure Read
      (Post      : SignalPost_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      use Userland.Process;

      Proc : constant Userland.Process.PID := Arch.Local.Get_Current_Process;
      Sigs : Userland.Process.Signal_Bitmap;
   begin
      if Data'Length < Userland.Process.Signal_Bitmap'Length then
         Ret_Count := 0;
         Success   := True;
         return;
      end if;

      Lib.Synchronization.Seize (Post.Mutex);

      Userland.Process.Pop_Raised_Signals (Proc, Sigs);
      if Post.Is_Blocking then
         loop
            for V of Sigs loop
               if V then
                  Success := True;
                  exit;
               end if;
            end loop;
            exit when Success;
            Scheduler.Yield_If_Able;
            Userland.Process.Get_Raised_Signals (Proc, Sigs);
         end loop;
      end if;

      --  The order of signals is always determined, regardless of what process
      --  says about it.
      Data (Data'First + 0) := Boolean'Pos (Sigs (Signal_Abort));
      Data (Data'First + 1) := Boolean'Pos (Sigs (Signal_Alarm));
      Data (Data'First + 3) := Boolean'Pos (Sigs (Signal_Bad_Memory));
      Data (Data'First + 4) := Boolean'Pos (Sigs (Signal_Child));
      Data (Data'First + 5) := Boolean'Pos (Sigs (Signal_FP_Exception));
      Data (Data'First + 6) := Boolean'Pos (Sigs (Signal_Hang_Up));
      Data (Data'First + 7) := Boolean'Pos (Sigs (Signal_Illegal_Instruction));
      Data (Data'First + 8) := Boolean'Pos (Sigs (Signal_Interrupted));
      Data (Data'First + 9) := Boolean'Pos (Sigs (Signal_Broken_Pipe));
      Data (Data'First + 10) := Boolean'Pos (Sigs (Signal_Quit));
      Data (Data'First + 11) := Boolean'Pos (Sigs (Signal_Segmentation_Fault));
      Data (Data'First + 12) := Boolean'Pos (Sigs (Signal_Terminated));
      Data (Data'First + 13) := Boolean'Pos (Sigs (Signal_Terminal_In));
      Data (Data'First + 14) := Boolean'Pos (Sigs (Signal_Terminal_Out));
      Data (Data'First + 15) := Boolean'Pos (Sigs (Signal_User_1));
      Data (Data'First + 16) := Boolean'Pos (Sigs (Signal_User_2));
      Data (Data'First + 17) := Boolean'Pos (Sigs (Signal_Pollable));
      Data (Data'First + 18) := Boolean'Pos (Sigs (Signal_Bad_Syscall));
      Data (Data'First + 19) := Boolean'Pos (Sigs (Signal_Tracepoint));
      Data (Data'First + 20) := Boolean'Pos (Sigs (Signal_Urgent));
      Data (Data'First + 21) := Boolean'Pos (Sigs (Signal_Virtual_Timer));
      Data (Data'First + 22) := Boolean'Pos (Sigs (Signal_CPU_Exceeded));
      Data (Data'First + 23) := Boolean'Pos (Sigs (Signal_File_Size_Exceeded));
      Ret_Count := 24;
      Success   := True;

      Lib.Synchronization.Release (Post.Mutex);
   end Read;
end IPC.SignalPost;
