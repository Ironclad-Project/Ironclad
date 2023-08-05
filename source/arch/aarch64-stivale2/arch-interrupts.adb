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
      Returned : Unsigned_64;
      Errno    : Errno_Value;
      FP_State : Context.FP_Context;
   begin
      Arch.Snippets.Enable_Interrupts;

      --  Pre syscall hook.
      Pre_Syscall_Hook (Context.GP_Context (State));

      --  Call the inner syscall.
      --  X0 is the return value.
      --  X8 is the syscall number.
      --  X9 is the returned errno.
      --  Arguments can be X0 to X7.
      case State.X4 is
         when 0 =>
            Sys_Exit (State.X0, Returned, Errno);
         when 1 =>
            Arch_PRCtl (State.X0, State.X1, Returned, Errno);
         when 2 =>
            Open
               (State.X0, State.X1, State.X2, State.X3, Returned, Errno);
         when 3 =>
            Close (State.X0, Returned, Errno);
         when 4 =>
            Read (State.X0, State.X1, State.X2, Returned, Errno);
         when 5 =>
            Write (State.X0, State.X1, State.X2, Returned, Errno);
         when 6 =>
            Seek (State.X0, State.X1, State.X2, Returned, Errno);
         when 7 =>
            Mmap (State.X0, State.X1, State.X2,
                  State.X3, State.X4, State.X5, Returned, Errno);
         when 8 =>
            Munmap (State.X0, State.X1, Returned, Errno);
         when 9 =>
            Get_PID (Returned, Errno);
         when 10 =>
            Get_PPID (Returned, Errno);
         when 11 =>
            Exec (State.X0, State.X1, State.X2,
                  State.X3, State.X4, State.X5, Returned, Errno);
         when 12 =>
            Context.Save_FP_Context (FP_State);
            Clone (State.X0, State.X1, State.X2, State.X3,
                   State.X4,  State.all,  FP_State, Returned, Errno);
         when 13 =>
            Wait (State.X0, State.X1, State.X2, Returned, Errno);
         when 14 =>
            Socket (State.X0, State.X1, State.X2, Returned, Errno);
         when 15 =>
            Set_Hostname (State.X0, State.X1, Returned, Errno);
         when 16 =>
            Unlink (State.X0, State.X1, State.X2, Returned, Errno);
         when 17 =>
            FStat (State.X0, State.X1, Returned, Errno);
         when 18 =>
            Get_CWD (State.X0, State.X1, Returned, Errno);
         when 19 =>
            Chdir (State.X0, State.X1, Returned, Errno);
         when 20 =>
            IOCTL (State.X0, State.X1, State.X2, Returned, Errno);
         when 21 =>
            Sched_Yield (Returned, Errno);
         when 22 =>
            Set_Deadlines (State.X0, State.X1, Returned, Errno);
         when 23 =>
            Pipe (State.X0, State.X1, Returned, Errno);
         when 24 =>
            Get_UID (Returned, Errno);
         when 25 =>
            Rename (State.X0, State.X1, State.X2, State.X3,
                    State.X4, State.X5, State.X6, Returned, Errno);
         when 26 =>
            Sysconf (State.X0, State.X1, State.X2, Returned, Errno);
         when 27 =>
            Spawn (State.X0, State.X1, State.X2, State.X3,
                   State.X4, State.X5, State.X6, Returned, Errno);
         when 28 =>
            Get_Thread_Sched (Returned, Errno);
         when 29 =>
            Set_Thread_Sched (State.X0, Returned, Errno);
         when 30 =>
            Fcntl (State.X0, State.X1, State.X2, Returned, Errno);
         when 31 =>
            Exit_Thread (Returned, Errno);
         when 32 =>
            Get_Random (State.X0, State.X1, Returned, Errno);
         when 33 =>
            MProtect (State.X0, State.X1, State.X2, Returned, Errno);
         when 34 =>
            Sync (Returned, Errno);
         when 35 =>
            Set_MAC_Capabilities (State.X0, Returned, Errno);
         when 36 =>
            Get_MAC_Capabilities (Returned, Errno);
         when 37 =>
            Add_MAC_Permissions (State.X0, State.X1, State.X2, Returned,
                                 Errno);
         when 38 =>
            Set_MAC_Enforcement (State.X0, Returned, Errno);
         when 39 =>
            Mount (State.X0, State.X1, State.X2, State.X3, State.X4,
                   State.X5, Returned, Errno);
         when 40 =>
            Umount (State.X0, State.X1, State.X2, Returned, Errno);
         when 41 =>
            Readlink (State.X0, State.X1, State.X2,
                      State.X3, State.X4, Returned, Errno);
         when 42 =>
            GetDEnts (State.X0, State.X1, State.X2, Returned, Errno);
         when 43 =>
            MakeNode (State.X0, State.X1, State.X2, State.X3, State.X4,
                      Returned, Errno);
         when 44 =>
            Truncate (State.X0, State.X1, Returned, Errno);
         when 45 =>
            Bind (State.X0, State.X1, State.X2, Returned, Errno);
         when 46 =>
            Symlink
               (State.X0, State.X1, State.X2, State.X3, State.X4, State.X5,
                Returned, Errno);
         when 47 =>
            Connect (State.X0, State.X1, State.X2, Returned, Errno);
         when 48 =>
            Open_PTY (State.X0, State.X1, State.X2, Returned, Errno);
         when 49 =>
            FSync (State.X0, State.X1, Returned, Errno);
         when 50 =>
            Link (State.X0, State.X1, State.X2, State.X3,
                  State.X4, State.X5, Returned, Errno);
         when 51 =>
            PTrace (State.X0, State.X1, State.X2, State.X3, Returned,
                                Errno);
         when 52 =>
            Listen (State.X0, State.X1, Returned, Errno);
         when 53 =>
            Sys_Accept (State.X0, State.X1, State.X2, State.X3, Returned,
                                    Errno);
         when 54 =>
            Get_RLimit (State.X0, Returned, Errno);
         when 55 =>
            Set_RLimit (State.X0, State.X1, Returned, Errno);
         when 57 =>
            Poll (State.X0, State.X1, State.X2, Returned, Errno);
         when 58 =>
            Get_EUID (Returned, Errno);
         when 59 =>
            Set_UIDs (State.X0, State.X1, Returned, Errno);
         when 60 =>
            Fchmod (State.X0, State.X1, Returned, Errno);
         when 61 =>
            Umask (State.X0, Returned, Errno);
         when 62 =>
            Reboot (State.X0, State.X1, Returned, Errno);
         when 63 =>
            Fchown (State.X0, State.X1, State.X2, Returned, Errno);
         when 64 =>
            PRead (State.X0, State.X1, State.X2, State.X3, Returned,
                   Errno);
         when 65 =>
            PWrite (State.X0, State.X1, State.X2, State.X3, Returned,
                    Errno);
         when others =>
            Returned := Unsigned_64'Last;
            Errno    := Error_Not_Implemented;
      end case;

      --  Assign the return values.
      State.X0 := Returned;
      State.X9 := Unsigned_64 (Errno_Value'Enum_Rep (Errno));

      --  Post syscall hook.
      Post_Syscall_Hook (Context.GP_Context (State));
   end Syscall_Handler;

   procedure Print_Fatal (Fr : Frame; Message : String) is
   begin
      Lib.Panic.Hard_Panic (Message);
   end Print_Fatal;
end Arch.Interrupts;
