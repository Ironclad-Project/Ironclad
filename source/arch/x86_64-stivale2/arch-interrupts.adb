--  arch-interrupts.adb: Setup and management of interrupts.
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

with Arch.APIC;
with Arch.GDT;
with Arch.Context;
with Lib.Panic;
with Lib.Messages;
with Scheduler;
with Userland.Syscall; use Userland.Syscall;
with Arch.Wrappers;

package body Arch.Interrupts with SPARK_Mode => Off is
   procedure Exception_Handler (Num : Integer; State : not null ISR_GPRs_Acc)
   is
      Exception_Text : constant array (0 .. 30) of String (1 .. 3) := (
         0  => "#DE", 1  => "#DB", 2  => "???", 3  => "#BP",
         4  => "#OF", 5  => "#BR", 6  => "#UD", 7  => "#NM",
         8  => "#DF", 9  => "???", 10 => "#TS", 11 => "#NP",
         12 => "#SS", 13 => "#GP", 14 => "#PF", 15 => "???",
         16 => "#MF", 17 => "#AC", 18 => "#MC", 19 => "#XM",
         20 => "#VE", 21 => "#CP", 22 .. 27 => "???",
         28 => "#HV", 29 => "#VC", 30 => "#SX"
      );
   begin
      if State.Error_Code /= 0 then
         Lib.Messages.Put ("Error code: ");
         Lib.Messages.Put (State.Error_Code, False, True);
         Lib.Messages.Put_Line ("");
      end if;

      Lib.Messages.Put ("RAX: ");
      Lib.Messages.Put (State.RAX, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("RBX: ");
      Lib.Messages.Put (State.RBX, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("RCX: ");
      Lib.Messages.Put (State.RCX, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("RDX: ");
      Lib.Messages.Put (State.RDX, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("RSI: ");
      Lib.Messages.Put (State.RSI, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("RDI: ");
      Lib.Messages.Put (State.RDI, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("RBP: ");
      Lib.Messages.Put (State.RBP, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R8: ");
      Lib.Messages.Put (State.R8, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put (" R9: ");
      Lib.Messages.Put (State.R9, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("R10: ");
      Lib.Messages.Put (State.R10, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R11: ");
      Lib.Messages.Put (State.R11, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R12: ");
      Lib.Messages.Put (State.R12, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("R13: ");
      Lib.Messages.Put (State.R13, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R14: ");
      Lib.Messages.Put (State.R14, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("R15: ");
      Lib.Messages.Put (State.R15, True, True);
      Lib.Messages.Put_Line ("");

      Lib.Messages.Put ("RIP: ");
      Lib.Messages.Put (State.RIP, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("RSP: ");
      Lib.Messages.Put (State.RSP, True, True);
      Lib.Messages.Put (" ");
      Lib.Messages.Put ("CR2: ");
      Lib.Messages.Put (Wrappers.Read_CR2, True, True);
      Lib.Messages.Put_Line ("");

      --  Check whether we have to panic or just exit the thread.
      --  We can check we are in userland by checking whether the passed CS
      --  is our user code segment or'ed by 3.
      --  TODO: Send a SIGSEGV instead of just stopping execution for user.
      if State.CS = (GDT.User_Code64_Segment or 3) then
         Lib.Messages.Put_Line ("Userland " & Exception_Text (Num));
         Scheduler.Bail;
      else
         Lib.Panic.Hard_Panic ("Kernel " & Exception_Text (Num));
      end if;
   end Exception_Handler;

   procedure Syscall_Handler (Num : Integer; State : not null ISR_GPRs_Acc) is
      Returned : Unsigned_64 := Unsigned_64'Last;
      Errno    : Errno_Value := Error_No_Error;
      pragma Unreferenced (Num);
   begin
      --  Call the inner syscall.
      --  RAX is the return value, as well as the syscall number.
      --  RDX is the returned errno.
      --  Arguments can be RDI, RSI, RDX, RCX, R8, and R9, in that order.
      --
      --  TODO: Instead of this case, use the table commented out at
      --  userland-syscall.ads
      case State.RAX is
         when 0 =>
            Syscall_Exit (State.RDI);
         when 1 =>
            Returned := Syscall_Arch_PRCtl (State.RDI, State.RSI, Errno);
         when 2 =>
            Returned := Syscall_Open (State.RDI, State.RSI, State.RDX, Errno);
         when 3 =>
            Returned := Syscall_Close (State.RDI, Errno);
         when 4 =>
            Returned := Syscall_Read (State.RDI, State.RSI, State.RDX, Errno);
         when 5 =>
            Returned := Syscall_Write (State.RDI, State.RSI, State.RDX, Errno);
         when 6 =>
            Returned := Syscall_Seek (State.RDI, State.RSI, State.RDX, Errno);
         when 7 =>
            Returned := Syscall_Mmap (State.RDI, State.RSI, State.RDX,
                                      State.RCX, State.R8, State.R9, Errno);
         when 8 =>
            Returned := Syscall_Munmap (State.RDI, State.RSI, Errno);
         when 9 =>
            Returned := Syscall_Get_PID;
         when 10 =>
            Returned := Syscall_Get_Parent_PID;
         when 11 =>
            Returned := Syscall_Exec (State.RDI, State.RSI, State.RDX, Errno);
         when 12 =>
            Returned := Syscall_Fork (Context.GP_Context_Acc (State), Errno);
         when 13 =>
            Returned := Syscall_Wait (State.RDI, State.RSI, State.RDX, Errno);
         when 14 =>
            Returned := Syscall_Uname (State.RDI, Errno);
         when 15 =>
            Returned := Syscall_Set_Hostname (State.RDI, State.RSI, Errno);
         when 16 =>
            Returned := Syscall_FStat (State.RDI, State.RSI, Errno);
         when 17 =>
            Returned := Syscall_LStat (State.RDI, State.RSI, Errno);
         when 18 =>
            Returned := Syscall_Get_CWD (State.RDI, State.RSI, Errno);
         when 19 =>
            Returned := Syscall_Chdir (State.RDI, Errno);
         when 20 =>
            Returned := Syscall_IOCTL (State.RDI, State.RSI, State.RDX, Errno);
         when 21 =>
            Syscall_Sched_Yield;
            Returned := 0;
         when 22 =>
            Returned := Syscall_Get_Priority (State.RDI, State.RSI, Errno);
         when 23 =>
            Returned := Syscall_Set_Priority (State.RDI, State.RSI, State.RDX,
                                              Errno);
         when 24 =>
            Returned := Syscall_Dup (State.RDI, Errno);
         when 25 =>
            Returned := Syscall_Dup2 (State.RDI, State.RSI, Errno);
         when 26 =>
            Returned := Syscall_Dup3 (State.RDI, State.RSI, State.RDX, Errno);
         when 27 =>
            Returned := Syscall_Access (State.RDI, State.RSI, Errno);
         when 28 =>
            Returned := Syscall_Get_Thread_Sched (Errno);
         when 29 =>
            Returned := Syscall_Set_Thread_Sched (State.RDI, Errno);
         when 30 =>
            Returned := Syscall_Fcntl (State.RDI, State.RSI, State.RDX, Errno);
         when 31 =>
            Returned := Syscall_Spawn (State.RDI, State.RSI, State.RDX, Errno);
         when others =>
            Errno := Error_Not_Implemented;
      end case;

      --  Assign the return values and swap back to user GS.
      State.RAX := Returned;
      State.RDX := Unsigned_64 (Errno_Value'Enum_Rep (Errno));
   end Syscall_Handler;

   procedure Scheduler_Handler (Num : Integer; State : not null ISR_GPRs_Acc)
   is
      pragma Unreferenced (Num);
   begin
      Arch.APIC.LAPIC_EOI;
      Scheduler.Scheduler_ISR (Context.GP_Context_Acc (State));
   end Scheduler_Handler;

   procedure Default_ISR_Handler is
   begin
      Lib.Panic.Soft_Panic ("Default ISR triggered");
      Arch.APIC.LAPIC_EOI;
   end Default_ISR_Handler;

   procedure Spurious_Handler is
   begin
      Lib.Panic.Hard_Panic ("LAPIC Spurious interrupt occured");
   end Spurious_Handler;
end Arch.Interrupts;
