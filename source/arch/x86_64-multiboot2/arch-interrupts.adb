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
with Arch.Snippets;
with Arch.Local;
with Userland.Process; use Userland.Process;
with Userland.Corefile;
with IPC.FIFO; use IPC.FIFO;
with Devices;

package body Arch.Interrupts with SPARK_Mode => Off is
   procedure Exception_Handler (Num : Integer; State : not null ISR_GPRs_Acc)
   is
      N              : Integer renames Num;
      Discard        : Natural;
      Val_Buffer     : Lib.Messages.Translated_String;
      Exception_Text : constant array (0 .. 30) of String (1 .. 3) :=
         (0  => "#DE", 1  => "#DB", 2  => "???", 3  => "#BP",
          4  => "#OF", 5  => "#BR", 6  => "#UD", 7  => "#NM",
          8  => "#DF", 9  => "???", 10 => "#TS", 11 => "#NP",
          12 => "#SS", 13 => "#GP", 14 => "#PF", 15 => "???",
          16 => "#MF", 17 => "#AC", 18 => "#MC", 19 => "#XM",
          20 => "#VE", 21 => "#CP", 22 .. 27 => "???",
          28 => "#HV", 29 => "#VC", 30 => "#SX");
   begin
      --  Check whether we have to panic or just exit the thread.
      --  We can check we are in userland by checking whether the passed CS
      --  is our user code segment or'ed by 3.
      --  TODO: Send a SIGSEGV instead of just exiting.
      if State.CS = (GDT.User_Code64_Segment or 3) then
         Lib.Messages.Put_Line ("Userland " & Exception_Text (Num));
         Userland.Corefile.Generate_Corefile (Context.GP_Context (State.all));
         Do_Exit (Local.Get_Current_Process, Unsigned_8'Last - Unsigned_8 (N));
      else
         if State.Error_Code /= 0 then
            Lib.Messages.Image (State.Error_Code, Val_Buffer, Discard, True);
            Lib.Messages.Put_Line ("Error code : " & Val_Buffer);
         end if;

         Lib.Messages.Image (State.RAX, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RAX : " & Val_Buffer);
         Lib.Messages.Image (State.RBX, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RBX : " & Val_Buffer);
         Lib.Messages.Image (State.RCX, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RCX : " & Val_Buffer);
         Lib.Messages.Image (State.RDX, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RDX : " & Val_Buffer);
         Lib.Messages.Image (State.RSI, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RSI : " & Val_Buffer);
         Lib.Messages.Image (State.RDI, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RDI : " & Val_Buffer);
         Lib.Messages.Image (State.RBP, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RBP : " & Val_Buffer);
         Lib.Messages.Image (State.R8, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R8  : " & Val_Buffer);
         Lib.Messages.Image (State.R9, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R9  : " & Val_Buffer);
         Lib.Messages.Image (State.R10, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R10 : " & Val_Buffer);
         Lib.Messages.Image (State.R11, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R11 : " & Val_Buffer);
         Lib.Messages.Image (State.R12, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R12 : " & Val_Buffer);
         Lib.Messages.Image (State.R13, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R13 : " & Val_Buffer);
         Lib.Messages.Image (State.R14, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R14 : " & Val_Buffer);
         Lib.Messages.Image (State.R15, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("R15 : " & Val_Buffer);
         Lib.Messages.Image (State.RIP, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RIP : " & Val_Buffer);
         Lib.Messages.Image (State.RSP, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("RSP : " & Val_Buffer);
         Lib.Messages.Image (Snippets.Read_CR2, Val_Buffer, Discard, True);
         Lib.Messages.Put_Line ("CR2 : " & Val_Buffer);
         Lib.Panic.Hard_Panic ("Kernel " & Exception_Text (Num));
      end if;
   end Exception_Handler;

   procedure Syscall_Handler (Num : Integer; State : not null ISR_GPRs_Acc) is
      Proc     : constant PID := Arch.Local.Get_Current_Process;
      Returned : Unsigned_64;
      Errno    : Errno_Value;
      FP_State : Context.FP_Context;
      File     : File_Description_Acc;
      State_Data : Devices.Operation_Data (1 .. State.all'Size / 8)
         with Import, Address => State.all'Address;
      Success   : IPC.FIFO.Pipe_Status;
      Ret_Count : Natural;
      Tracer_FD : Natural;
      Is_Traced : Boolean;
      pragma Unreferenced (Num);
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
      --  RAX is the return value, as well as the syscall number.
      --  RDX is the returned errno.
      --  Arguments can be RDI, RSI, RDX, RCX, R8, R9, and R10, in that order.
      case State.RAX is
         when 0 =>
            Sys_Exit (State.RDI, Returned, Errno);
         when 1 =>
            Arch_PRCtl (State.RDI, State.RSI, Returned, Errno);
         when 2 =>
            Open
               (State.RDI, State.RSI, State.RDX, State.RCX, Returned, Errno);
         when 3 =>
            Close (State.RDI, Returned, Errno);
         when 4 =>
            Read (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 5 =>
            Write (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 6 =>
            Seek (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 7 =>
            Mmap (State.RDI, State.RSI, State.RDX,
                  State.RCX, State.R8, State.R9, Returned, Errno);
         when 8 =>
            Munmap (State.RDI, State.RSI, Returned, Errno);
         when 9 =>
            Get_PID (Returned, Errno);
         when 10 =>
            Get_PPID (Returned, Errno);
         when 11 =>
            Exec (State.RDI, State.RSI, State.RDX,
                  State.RCX, State.R8, State.R9, Returned, Errno);
         when 12 =>
            Context.Save_FP_Context (FP_State);
            Clone (State.RDI, State.RSI, State.RDX, State.RCX,
                   State.R8,  State.all,  FP_State, Returned, Errno);
         when 13 =>
            Wait (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 14 =>
            Socket (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 15 =>
            Set_Hostname (State.RDI, State.RSI, Returned, Errno);
         when 16 =>
            Unlink (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 17 =>
            FStat (State.RDI, State.RSI, Returned, Errno);
         when 18 =>
            Get_CWD (State.RDI, State.RSI, Returned, Errno);
         when 19 =>
            Chdir (State.RDI, State.RSI, Returned, Errno);
         when 20 =>
            IOCTL (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 21 =>
            Sched_Yield (Returned, Errno);
         when 22 =>
            Set_Deadlines (State.RDI, State.RSI, Returned, Errno);
         when 23 =>
            Pipe (State.RDI, State.RSI, Returned, Errno);
         when 24 =>
            Get_UID (Returned, Errno);
         when 25 =>
            Rename (State.RDI, State.RSI, State.RDX, State.RCX,
                    State.R8, State.R9, State.R10, Returned, Errno);
         when 26 =>
            Sysconf (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 27 =>
            Spawn (State.RDI, State.RSI, State.RDX, State.RCX,
                   State.R8, State.R9, State.R10, Returned, Errno);
         when 28 =>
            Get_Thread_Sched (Returned, Errno);
         when 29 =>
            Set_Thread_Sched (State.RDI, Returned, Errno);
         when 30 =>
            Fcntl (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 31 =>
            Exit_Thread (Returned, Errno);
         when 32 =>
            Get_Random (State.RDI, State.RSI, Returned, Errno);
         when 33 =>
            MProtect (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 34 =>
            Sync (Returned, Errno);
         when 35 =>
            Set_MAC_Capabilities (State.RDI, Returned, Errno);
         when 36 =>
            Get_MAC_Capabilities (Returned, Errno);
         when 37 =>
            Add_MAC_Permissions (State.RDI, State.RSI, State.RDX, Returned,
                                 Errno);
         when 38 =>
            Set_MAC_Enforcement (State.RDI, Returned, Errno);
         when 39 =>
            Mount (State.RDI, State.RSI, State.RDX, State.RCX, State.R8,
                   State.R9, Returned, Errno);
         when 40 =>
            Umount (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 41 =>
            Readlink (State.RDI, State.RSI, State.RDX,
                      State.RCX, State.R8, Returned, Errno);
         when 42 =>
            GetDEnts (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 43 =>
            MakeNode (State.RDI, State.RSI, State.RDX, State.RCX, State.R8,
                      Returned, Errno);
         when 44 =>
            Truncate (State.RDI, State.RSI, Returned, Errno);
         when 45 =>
            Bind (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 46 =>
            Symlink
               (State.RDI, State.RSI, State.RDX, State.RCX, State.R8, State.R9,
                Returned, Errno);
         when 47 =>
            Connect (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 48 =>
            Open_PTY (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 49 =>
            FSync (State.RDI, State.RSI, Returned, Errno);
         when 50 =>
            Link (State.RDI, State.RSI, State.RDX, State.RCX,
                  State.R8, State.R9, Returned, Errno);
         when 51 =>
            PTrace (State.RDI, State.RSI, State.RDX, State.RCX, Returned,
                                Errno);
         when 52 =>
            Listen (State.RDI, State.RSI, Returned, Errno);
         when 53 =>
            Sys_Accept (State.RDI, State.RSI, State.RDX, State.RCX, Returned,
                                    Errno);
         when 54 =>
            Get_RLimit (State.RDI, Returned, Errno);
         when 55 =>
            Set_RLimit (State.RDI, State.RSI, Returned, Errno);
         when 57 =>
            Poll (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 58 =>
            Get_EUID (Returned, Errno);
         when 59 =>
            Set_UIDs (State.RDI, State.RSI, Returned, Errno);
         when 60 =>
            Fchmod (State.RDI, State.RSI, Returned, Errno);
         when 61 =>
            Umask (State.RDI, Returned, Errno);
         when 62 =>
            Reboot (State.RDI, State.RSI, Returned, Errno);
         when 63 =>
            Fchown (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 64 =>
            PRead (State.RDI, State.RSI, State.RDX, State.RCX, Returned,
                   Errno);
         when 65 =>
            PWrite (State.RDI, State.RSI, State.RDX, State.RCX, Returned,
                    Errno);
         when others =>
            Returned := Unsigned_64'Last;
            Errno    := Error_Not_Implemented;
      end case;

      --  Assign the return values.
      State.RAX := Returned;
      State.RDX := Unsigned_64 (Errno_Value'Enum_Rep (Errno));

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
   end Syscall_Handler;

   procedure Scheduler_Handler (Num : Integer; State : not null ISR_GPRs_Acc)
   is
      pragma Unreferenced (Num);
   begin
      Arch.APIC.LAPIC_EOI;
      Scheduler.Scheduler_ISR (Context.GP_Context (State.all));
   end Scheduler_Handler;

   procedure Panic_Handler is
   begin
      Snippets.HCF;
   end Panic_Handler;

   procedure Default_ISR_Handler is
   begin
      Lib.Messages.Put_Line ("Default ISR triggered");
      Arch.APIC.LAPIC_EOI;
   end Default_ISR_Handler;

   procedure Spurious_Handler is
   begin
      Lib.Messages.Put_Line ("LAPIC Spurious interrupt occured");
      Arch.APIC.LAPIC_EOI;
   end Spurious_Handler;
end Arch.Interrupts;
