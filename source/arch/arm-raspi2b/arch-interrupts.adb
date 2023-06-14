--  arch-exceptions.adb: Interrupt utilities.
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

with Lib.Panic;
with Lib.Messages;
with Arch.Snippets;
with Userland.Syscall; use Userland.Syscall;
with Arch.Local;
with Userland.Process; use Userland.Process;
with IPC.FIFO; use IPC.FIFO;
with Devices;
with Arch.Context;
with Scheduler;

package body Arch.Interrupts with SPARK_Mode => Off is
   procedure Reset_Handler (State : in out Frame) is
   begin
      Print_Fatal (State, "Reset handler was called");
   end Reset_Handler;

   procedure UD_Handler (State : in out Frame) is
   begin
      Print_Fatal (State, "UD handler was called");
   end UD_Handler;

   procedure SWI_Handler (State : in out Frame) is
      Proc     : constant PID := Arch.Local.Get_Current_Process;
      Returned : Unsigned_64;
      Errno    : Errno_Value;
      FP_State : Context.FP_Context;
      File     : File_Description_Acc;
      State_Data : Devices.Operation_Data (1 .. State'Size / 8)
         with Import, Address => State'Address;
      Success   : IPC.FIFO.Pipe_Status;
      Ret_Count : Natural;
      Tracer_FD : Natural;
      Is_Traced : Boolean;
   begin
      Arch.Snippets.Enable_Interrupts;

      --  Check if we have to write the syscall info somewhere.
      Userland.Process.Get_Traced_Info (Proc, Is_Traced, Tracer_FD);
      if Is_Traced then
         File := Get_File (Proc, Unsigned_64 (Tracer_FD));
         if File /= null and then File.Description = Description_Writer_FIFO
         then
            Write (File.Inner_Writer_FIFO, State_Data, Ret_Count, Success);
            while not Is_Empty (File.Inner_Writer_FIFO) loop
               Scheduler.Yield;
            end loop;
         end if;
      end if;

      --  Call the inner syscall.
      --  R0: return value. R8: syscall number. R1: errno. R0 - R7: args.
      case State.R8 is
         when 0 =>
            Sys_Exit (Unsigned_64 (State.R0), Errno);
            Returned := 0;
         when 1 =>
            Returned := Arch_PRCtl
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 2 =>
            Returned := Open
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3), Errno);
         when 3 =>
            Returned := Close (Unsigned_64 (State.R0), Errno);
         when 4 =>
            Returned := Read
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 5 =>
            Returned := Write
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 6 =>
            Returned := Seek
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 7 =>
            Returned := Mmap
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5), Errno);
         when 8 =>
            Returned := Munmap
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 9 =>
            Returned := Get_PID (Errno);
         when 10 =>
            Returned := Get_Parent_PID (Errno);
         when 11 =>
            Returned := Exec
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5), Errno);
         when 12 =>
            Context.Save_FP_Context (FP_State);
            Returned := Clone
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), State, FP_State, Errno);
         when 13 =>
            Returned := Wait
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 14 =>
            Returned := Uname (Unsigned_64 (State.R0), Errno);
         when 15 =>
            Returned := Set_Hostname
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 16 =>
            Returned := Unlink
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 17 =>
            Returned := FStat
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 18 =>
            Returned := Get_CWD
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 19 =>
            Returned := Chdir
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 20 =>
            Returned := IOCTL
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 21 =>
            Returned := Sched_Yield (Errno);
         when 22 =>
            Returned := Set_Deadlines
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 23 =>
            Returned := Pipe
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 25 =>
            Returned := Rename
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Unsigned_64 (State.R6), Errno);
         when 26 =>
            Returned := Sysconf
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 27 =>
            Returned := Spawn
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5), Errno);
         when 28 =>
            Returned := Get_Thread_Sched (Errno);
         when 29 =>
            Returned := Set_Thread_Sched (Unsigned_64 (State.R0), Errno);
         when 30 =>
            Returned := Fcntl
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 31 =>
            Exit_Thread (Errno);
            Returned := 0;
         when 32 =>
            Returned := Get_Random
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 33 =>
            Returned := MProtect
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 34 =>
            Returned := Sync (Errno);
         when 35 =>
            Returned := Set_MAC_Capabilities (Unsigned_64 (State.R0), Errno);
         when 36 =>
            Returned := Get_MAC_Capabilities (Errno);
         when 37 =>
            Returned := Add_MAC_Permissions
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 38 =>
            Returned := Set_MAC_Enforcement (Unsigned_64 (State.R0), Errno);
         when 39 =>
            Returned := Mount
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5), Errno);
         when 40 =>
            Returned := Umount
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 41 =>
            Returned := Readlink
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Errno);
         when 42 =>
            Returned := GetDEnts
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 43 =>
            Returned := MakeNode
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Errno);
         when 44 =>
            Returned := Truncate
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 46 =>
            Returned := Symlink
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5), Errno);
         when 47 =>
            Returned := Integrity_Setup
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 48 =>
            Returned := Open_PTY
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when 49 =>
            Returned := FSync
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1), Errno);
         when 50 =>
            Returned := Link
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5), Errno);
         when 51 =>
            Returned := PTrace
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3), Errno);
         when 57 =>
            Returned := Poll
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Errno);
         when others =>
            Returned := Unsigned_64'Last;
            Errno    := Error_Not_Implemented;
      end case;

      --  Assign the return values.
      State.R0 := Unsigned_32 (Returned);
      State.R1 := Unsigned_32 (Errno_Value'Enum_Rep (Errno));

      --  Trace again.
      if Is_Traced then
         File := Get_File (Proc, Unsigned_64 (Tracer_FD));
         if File /= null and then File.Description = Description_Writer_FIFO
         then
            Write (File.Inner_Writer_FIFO, State_Data, Ret_Count, Success);
            while not Is_Empty (File.Inner_Writer_FIFO) loop
               Scheduler.Yield;
            end loop;
         end if;
      end if;
   end SWI_Handler;

   procedure Prefetch_Handler (State : in out Frame) is
   begin
      Print_Fatal (State, "Prefetch handler was called");
   end Prefetch_Handler;

   procedure Data_Handler (State : in out Frame) is
   begin
      Print_Fatal (State, "Data handler was called");
   end Data_Handler;

   procedure IRQ_Handler (State : in out Frame) is
   begin
      Print_Fatal (State, "IRQ handler was called");
   end IRQ_Handler;

   procedure FIQ_Handler (State : in out Frame) is
   begin
      Print_Fatal (State, "FIQ handler was called");
   end FIQ_Handler;

   procedure Print_Fatal (Fr : Frame; Message : String) is
   begin
      Lib.Messages.Put (" R0: ");
      Lib.Messages.Put (Fr.R0, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R1: ");
      Lib.Messages.Put (Fr.R1, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R2: ");
      Lib.Messages.Put (Fr.R2, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R3: ");
      Lib.Messages.Put (Fr.R3, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" R4: ");
      Lib.Messages.Put (Fr.R4, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R5: ");
      Lib.Messages.Put (Fr.R5, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R6: ");
      Lib.Messages.Put (Fr.R6, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R7: ");
      Lib.Messages.Put (Fr.R7, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" R8: ");
      Lib.Messages.Put (Fr.R8, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R9: ");
      Lib.Messages.Put (Fr.R9, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R10: ");
      Lib.Messages.Put (Fr.R10, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R11: ");
      Lib.Messages.Put (Fr.R11, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("R12: ");
      Lib.Messages.Put (Fr.R12, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R13: ");
      Lib.Messages.Put (Fr.R13, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R14: ");
      Lib.Messages.Put (Fr.R14, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R15: ");
      Lib.Messages.Put (Fr.R15, True, True);
      Lib.Messages.Put_Line (" ");

      Lib.Panic.Hard_Panic (Message);
   end Print_Fatal;
end Arch.Interrupts;
