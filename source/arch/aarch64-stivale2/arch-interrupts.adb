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
   procedure Exception_Handler (State : aliased in out Frame) is
      Syndrome       : constant Unsigned_64 := Snippets.Get_Exception_Syndrome;
      Execution_Type : constant Unsigned_64 := Shift_Right (Syndrome, 26);
   begin
      case Execution_Type is
         when 16#01# => Print_Fatal (State, "Trapped wfi/wfe");
         when 16#0E# => Print_Fatal (State, "Illegal execution");
         when 16#15# => Syscall_Handler (State);
         when 16#20# => Print_Fatal (State, "Instruction abort, lower EL");
         when 16#21# => Print_Fatal (State, "Instruction abort, same EL");
         when 16#22# => Print_Fatal (State, "Instruction alignment fault");
         when 16#24# => Print_Fatal (State, "Data abort, lower EL");
         when 16#25# => Print_Fatal (State, "Data abort, same EL");
         when 16#26# => Print_Fatal (State, "Stack alignment fault");
         when 16#2C# => Print_Fatal (State, "Floating point exception");
         when others => Print_Fatal (State, "Unknown exception");
      end case;
   end Exception_Handler;

   procedure Syscall_Handler (State : aliased in out Frame) is
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
      --  X0 is the return value.
      --  X8 is the syscall number.
      --  X9 is the returned errno.
      --  Arguments can be X0 to X7.
      case State.X8 is
         when 0 =>
            Sys_Exit (State.X0, Errno);
            Returned := 0;
         when 1 =>
            Returned := Arch_PRCtl (State.X0, State.X1, Errno);
         when 2 =>
            Returned := Open (State.X0, State.X1, State.X2, State.X3, Errno);
         when 3 =>
            Returned := Close (State.X0, Errno);
         when 4 =>
            Returned := Read (State.X0, State.X1, State.X2, Errno);
         when 5 =>
            Returned := Write (State.X0, State.X1, State.X2, Errno);
         when 6 =>
            Returned := Seek (State.X0, State.X1, State.X2, Errno);
         when 7 =>
            Returned := Mmap (State.X0, State.X1, State.X2,
                              State.X3, State.X4, State.X5, Errno);
         when 8 =>
            Returned := Munmap (State.X0, State.X1, Errno);
         when 9 =>
            Returned := Get_PID (Errno);
         when 10 =>
            Returned := Get_Parent_PID (Errno);
         when 11 =>
            Returned := Exec (State.X0, State.X1, State.X2,
                              State.X3, State.X4, State.X5, Errno);
         when 12 =>
            Context.Save_FP_Context (FP_State);
            Returned := Clone (State.X0, State.X1, State.X2, State.X3,
                               State.X4,  State,  FP_State, Errno);
         when 13 =>
            Returned := Wait (State.X0, State.X1, State.X2, Errno);
         when 14 =>
            Returned := Uname (State.X0, Errno);
         when 15 =>
            Returned := Set_Hostname (State.X0, State.X1, Errno);
         when 16 =>
            Returned := Unlink (State.X0, State.X1, State.X2, Errno);
         when 17 =>
            Returned := FStat (State.X0, State.X1, Errno);
         when 18 =>
            Returned := Get_CWD (State.X0, State.X1, Errno);
         when 19 =>
            Returned := Chdir (State.X0, State.X1, Errno);
         when 20 =>
            Returned := IOCTL (State.X0, State.X1, State.X2, Errno);
         when 21 =>
            Returned := Sched_Yield (Errno);
         when 22 =>
            Returned := Set_Deadlines (State.X0, State.X1, Errno);
         when 23 =>
            Returned := Pipe (State.X0, State.X1, Errno);
         when 25 =>
            Returned := Rename (State.X0, State.X1, State.X2, State.X3,
                                State.X4, State.X5, State.X6, Errno);
         when 26 =>
            Returned := Sysconf (State.X0, State.X1, State.X2, Errno);
         when 27 =>
            Returned := Spawn (State.X0, State.X1, State.X2,
                               State.X3, State.X4, State.X5, Errno);
         when 28 =>
            Returned := Get_Thread_Sched (Errno);
         when 29 =>
            Returned := Set_Thread_Sched (State.X0, Errno);
         when 30 =>
            Returned := Fcntl (State.X0, State.X1, State.X2, Errno);
         when 31 =>
            Exit_Thread (Errno);
            Returned := 0;
         when 32 =>
            Returned := Get_Random (State.X0, State.X1, Errno);
         when 33 =>
            Returned := MProtect (State.X0, State.X1, State.X2, Errno);
         when 34 =>
            Returned := Sync (Errno);
         when 35 =>
            Returned := Set_MAC_Capabilities (State.X0, Errno);
         when 36 =>
            Returned := Get_MAC_Capabilities (Errno);
         when 37 =>
            Returned := Add_MAC_Permissions (State.X0, State.X1, State.X2,
                                             Errno);
         when 38 =>
            Returned := Set_MAC_Enforcement (State.X0, Errno);
         when 39 =>
            Returned := Mount (State.X0, State.X1, State.X2,
                               State.X3, State.X4, State.X5, Errno);
         when 40 =>
            Returned := Umount
               (State.X0, State.X1, State.X2, Errno);
         when 41 =>
            Returned := Readlink (State.X0, State.X1, State.X2,
                                  State.X3, State.X4, Errno);
         when 42 =>
            Returned := GetDEnts (State.X0, State.X1, State.X2, Errno);
         when 43 =>
            Returned := MakeNode
               (State.X0, State.X1, State.X2, State.X3, State.X4, Errno);
         when 44 =>
            Returned := Truncate (State.X0, State.X1, Errno);
         when 46 =>
            Returned := Symlink
               (State.X0, State.X1, State.X2, State.X3, State.X4, State.X5,
                Errno);
         when 47 =>
            Returned := Integrity_Setup (State.X0, State.X1, Errno);
         when 48 =>
            Returned := Open_PTY (State.X0, State.X1, State.X2, Errno);
         when 49 =>
            Returned := FSync (State.X0, State.X1, Errno);
         when 50 =>
            Returned := Link (State.X0, State.X1, State.X2, State.X3,
                              State.X4, State.X5, Errno);
         when 51 =>
            Returned := PTrace (State.X0, State.X1, State.X2, State.X3, Errno);
         when 57 =>
            Returned := Poll (State.X0, State.X1, State.X2, Errno);
         when others =>
            Returned := Unsigned_64'Last;
            Errno    := Error_Not_Implemented;
      end case;

      --  Assign the return values.
      State.X0 := Returned;
      State.X9 := Unsigned_64 (Errno_Value'Enum_Rep (Errno));
   end Syscall_Handler;

   procedure Print_Fatal (Fr : Frame; Message : String) is
   begin
      Lib.Messages.Put ("  X0: ");
      Lib.Messages.Put (Fr.X0, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("  X1: ");
      Lib.Messages.Put (Fr.X1, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("  X2: ");
      Lib.Messages.Put (Fr.X2, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("  X3: ");
      Lib.Messages.Put (Fr.X3, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("  X4: ");
      Lib.Messages.Put (Fr.X4, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("  X5: ");
      Lib.Messages.Put (Fr.X5, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("  X6: ");
      Lib.Messages.Put (Fr.X6, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("  X7: ");
      Lib.Messages.Put (Fr.X7, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("  X8: ");
      Lib.Messages.Put (Fr.X8, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("  X9: ");
      Lib.Messages.Put (Fr.X9, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X10: ");
      Lib.Messages.Put (Fr.X10, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X11: ");
      Lib.Messages.Put (Fr.X11, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" X12: ");
      Lib.Messages.Put (Fr.X12, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X13: ");
      Lib.Messages.Put (Fr.X13, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X14: ");
      Lib.Messages.Put (Fr.X14, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" X15: ");
      Lib.Messages.Put (Fr.X15, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X16: ");
      Lib.Messages.Put (Fr.X16, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X17: ");
      Lib.Messages.Put (Fr.X17, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" X18: ");
      Lib.Messages.Put (Fr.X18, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X19: ");
      Lib.Messages.Put (Fr.X19, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X20: ");
      Lib.Messages.Put (Fr.X20, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" X21: ");
      Lib.Messages.Put (Fr.X21, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X22: ");
      Lib.Messages.Put (Fr.X22, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X23: ");
      Lib.Messages.Put (Fr.X23, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" X24: ");
      Lib.Messages.Put (Fr.X24, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X25: ");
      Lib.Messages.Put (Fr.X25, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X26: ");
      Lib.Messages.Put (Fr.X26, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" X27: ");
      Lib.Messages.Put (Fr.X27, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X28: ");
      Lib.Messages.Put (Fr.X28, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" X29: ");
      Lib.Messages.Put (Fr.X29, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put (" X30: ");
      Lib.Messages.Put (Fr.X30, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("  PC: ");
      Lib.Messages.Put (Fr.PC, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("SPSR: ");
      Lib.Messages.Put (Fr.SPSR, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Panic.Hard_Panic (Message);
   end Print_Fatal;
end Arch.Interrupts;
