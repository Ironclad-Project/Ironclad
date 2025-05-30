--  arch-interrupts.adb: Interrupt utilities.
--  Copyright (C) 2024 streaksu
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
with Arch.CPU;
with Arch.Context;
with Arch.MMU;
with Lib.Panic;
with Lib.Messages;
with Lib.Synchronization;
with Scheduler;
with Userland.Syscall; use Userland.Syscall;
with Arch.Snippets; use Arch.Snippets;
with Arch.Local;
with Userland.Corefile;
with Userland.Process;

package body Arch.Interrupts is
   procedure Exception_Handler (Num : Integer; State : not null ISR_GPRs_Acc)
   is
      Signal : Userland.Process.Signal;
      Exception_Text : constant array (0 .. 30) of String (1 .. 3) :=
         [0  => "#DE", 1  => "#DB", 2  => "???", 3  => "#BP",
          4  => "#OF", 5  => "#BR", 6  => "#UD", 7  => "#NM",
          8  => "#DF", 9  => "???", 10 => "#TS", 11 => "#NP",
          12 => "#SS", 13 => "#GP", 14 => "#PF", 15 => "???",
          16 => "#MF", 17 => "#AC", 18 => "#MC", 19 => "#XM",
          20 => "#VE", 21 => "#CP", 22 => "???", 23 => "???",
          24 => "???", 25 => "???", 26 => "???", 27 => "???",
          28 => "#HV", 29 => "#VC", 30 => "#SX"];
   begin
      --  Check whether we have to panic or just exit the thread.
      if State.CS = (GDT.User_Code64_Segment or 3) then
         Signal := (case Num is
            when       6 => Userland.Process.Signal_Illegal_Instruction,
            when 16 | 19 => Userland.Process.Signal_FP_Exception,
            when  others => Userland.Process.Signal_Segmentation_Fault);

         Lib.Messages.Put_Line
            ("Userland " & Exception_Text (Num) &
             " (" & Userland.Process.Signal'Image (Signal) & ")");
         Userland.Corefile.Generate_Corefile (Context.GP_Context (State.all));
         Do_Exit (Local.Get_Current_Process, Signal);
      else
         Lib.Panic.Hard_Panic ("Kernel " & Exception_Text (Num), State.all);
      end if;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception...  In the exception handler!");
   end Exception_Handler;

   procedure Syscall_Handler (State : not null ISR_GPRs_Acc) is
      Returned : Unsigned_64;
      Errno    : Errno_Value;
      FP_State : Context.FP_Context;
   begin
      Arch.Snippets.Enable_Interrupts;

      --  Pre syscall hook.
      Pre_Syscall_Hook (Context.GP_Context (State.all));

      --  Call the inner syscall.
      --  RAX is the return value, as well as the syscall number.
      --  RDX is the returned errno.
      --  Arguments can be RDI, RSI, RDX, R12, R8, R9, and R10, in that order.
      case State.RAX is
         when 0 =>
            Sys_Exit (State.RDI, Returned, Errno);
         when 1 =>
            Arch_PRCtl (State.RDI, State.RSI, Returned, Errno);
         when 2 =>
            Open
               (State.RDI, State.RSI, State.RDX, State.R12, Returned, Errno);
         when 3 =>
            Close (State.RDI, Returned, Errno);
         when 4 =>
            Read (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                  Returned, Errno);
         when 5 =>
            Write (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                   Returned, Errno);
         when 6 =>
            Seek (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 7 =>
            Mmap (State.RDI, State.RSI, State.RDX,
                  State.R12, State.R8, State.R9, Returned, Errno);
         when 8 =>
            Munmap (State.RDI, State.RSI, Returned, Errno);
         when 9 =>
            Get_PID (Returned, Errno);
         when 10 =>
            Get_PPID (Returned, Errno);
         when 11 =>
            Exec (State.RDI, State.RSI, State.RDX,
                  State.R12, State.R8, State.R9, Returned, Errno);
         when 12 =>
            Context.Init_FP_Context (FP_State);
            Fork (State.all, FP_State, State.RDI, State.RSI, Returned, Errno);
            if Errno /= Error_No_Error then
               Context.Destroy_FP_Context (FP_State);
            end if;
         when 13 =>
            Wait (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 14 =>
            Socket (State.RDI, State.RSI, Returned, Errno);
         when 15 =>
            Set_Hostname (State.RDI, State.RSI, Returned, Errno);
         when 16 =>
            Unlink (State.RDI, State.RSI, State.RDX, State.R12, Returned,
                    Errno);
         when 17 =>
            FStat (State.RDI, State.RSI, State.RDX, State.R12,
                   State.R8, Returned, Errno);
         when 18 =>
            Pivot_Root (State.RDI, State.RSI, State.RDX, State.R12,
                Returned, Errno);
         when 19 =>
            Chdir (State.RDI, Returned, Errno);
         when 20 =>
            IOCTL (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 21 =>
            Sched_Yield (Returned, Errno);
         when 22 =>
            Delete_Thread_Cluster (State.RDI, Returned, Errno);
         when 23 =>
            Pipe (State.RDI, State.RSI, Returned, Errno);
         when 24 =>
            Get_UID (Returned, Errno);
         when 25 =>
            Rename (State.RDI, State.RSI, State.RDX, State.R12,
                    State.R8, State.R9, State.R10, Returned, Errno);
         when 26 =>
            List_Procs (State.RDI, State.RSI, Returned, Errno);
         when 28 =>
            Get_TID (Returned, Errno);
         when 29 =>
            Manage_Thread_Cluster (State.RDI, State.RSI, State.RDX, State.R12,
                                   Returned, Errno);
         when 30 =>
            Fcntl (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 31 =>
            Exit_Thread (Returned, Errno);
         when 32 =>
            Get_Entropy (State.RDI, State.RSI, Returned, Errno);
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
            Mount (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                   State.R9, Returned, Errno);
         when 40 =>
            Umount (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 41 =>
            Readlink (State.RDI, State.RSI, State.RDX,
                      State.R12, State.R8, Returned, Errno);
         when 42 =>
            GetDEnts (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 43 =>
            MakeNode (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                      Returned, Errno);
         when 44 =>
            Truncate (State.RDI, State.RSI, Returned, Errno);
         when 45 =>
            Bind (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 46 =>
            Symlink
               (State.RDI, State.RSI, State.RDX, State.R12, State.R8, State.R9,
                Returned, Errno);
         when 47 =>
            Connect (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 48 =>
            Open_PTY (State.RDI, Returned, Errno);
         when 49 =>
            FSync (State.RDI, State.RSI, Returned, Errno);
         when 50 =>
            Link (State.RDI, State.RSI, State.RDX, State.R12,
                  State.R8, State.R9, Returned, Errno);
         when 51 =>
            PTrace (State.RDI, State.RSI, State.RDX, State.R12, Returned,
                                Errno);
         when 52 =>
            Listen (State.RDI, State.RSI, Returned, Errno);
         when 53 =>
            Sys_Accept (State.RDI, State.RSI, State.RDX, State.R12, Returned,
                                    Errno);
         when 54 =>
            Get_RLimit (State.RDI, Returned, Errno);
         when 55 =>
            Set_RLimit (State.RDI, State.RSI, Returned, Errno);
         when 56 =>
            FAccess (State.RDI, State.RSI, State.RDX, State.R12,
                    State.R8, Returned, Errno);
         when 57 =>
            PPoll (State.RDI, State.RSI, State.RDX, State.R12, Returned,
                   Errno);
         when 58 =>
            Get_EUID (Returned, Errno);
         when 59 =>
            Set_UIDs (State.RDI, State.RSI, Returned, Errno);
         when 60 =>
            Fchmod (State.RDI, State.RSI, State.RDX, State.R12,
                    State.R8, Returned, Errno);
         when 61 =>
            Umask (State.RDI, Returned, Errno);
         when 62 =>
            Reboot (State.RDI, State.RSI, Returned, Errno);
         when 63 =>
            Fchown (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                    State.R9, Returned, Errno);
         when 66 =>
            Get_Sock_Name (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 67 =>
            Get_Peer_Name (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 68 =>
            Shutdown (State.RDI, State.RSI, Returned, Errno);
         when 69 =>
            Futex (State.RDI, State.RSI, State.RDX, State.R12,
                   Returned, Errno);
         when 70 =>
            Clock (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 71 =>
            Clock_Nanosleep (State.RDI, State.RSI, State.RDX, State.R12,
                             Returned, Errno);
         when 72 =>
            Get_RUsage (State.RDI, State.RSI, Returned, Errno);
         when 73 =>
            RecvFrom (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                      State.R9, Returned, Errno);
         when 74 =>
            SendTo (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                    State.R9, Returned, Errno);
         when 75 =>
            Config_NetInterface (State.RDI, State.RSI, State.RDX, Returned,
                                 Errno);
         when 76 =>
            UTimes (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                    Returned, Errno);
         when 77 =>
            Create_TCluster (Returned, Errno);
         when 78 =>
            Switch_TCluster (State.RDI, State.RSI, Returned, Errno);
         when 79 =>
            Sigprocmask (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 80 =>
            Sigaction (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 81 =>
            Send_Signal (State.RDI, State.RSI, Returned, Errno);
         when 82 =>
            Get_Prio (State.RDI, State.RSI, Returned, Errno);
         when 83 =>
            Set_Prio (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 84 =>
            Get_GID (Returned, Errno);
         when 85 =>
            Get_EGID (Returned, Errno);
         when 86 =>
            Set_GIDs (State.RDI, State.RSI, Returned, Errno);
         when 87 =>
            Get_Groups (State.RDI, State.RSI, Returned, Errno);
         when 88 =>
            Set_Groups (State.RDI, State.RSI, Returned, Errno);
         when 89 =>
            TTY_Name (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 90 =>
            FAdvise
               (State.RDI, State.RSI, State.RDX, State.R12, Returned, Errno);
         when 91 =>
            SHMAt (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 92 =>
            SHMCtl (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 93 =>
            SHMDt (State.RDI, Returned, Errno);
         when 94 =>
            SHMGet (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 95 =>
            GetSockOpt
               (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                Returned, Errno);
         when 96 =>
            SetSockOpt
               (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                Returned, Errno);
         when 97 =>
            Get_Thread_Name (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 98 =>
            Set_Thread_Name (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 99 =>
            Failure_Policy (State.RDI, State.RSI, Returned, Errno);
         when 100 =>
            Create_Thread
               (State.RDI, State.RSI, State.RDX, State.R12, State.R8,
                Returned, Errno);
         when 101 =>
            Signal_Return (Returned, Errno);
         when 102 =>
            Sigaltstack (State.RDI, State.RSI, Returned, Errno);
         when 103 =>
            SigSuspend (State.RDI, Returned, Errno);
         when 104 =>
            List_Mounts (State.RDI, State.RSI, Returned, Errno);
         when 105 =>
            Uname (State.RDI, Returned, Errno);
         when 106 =>
            List_Threads (State.RDI, State.RSI, Returned, Errno);
         when 107 =>
            List_Clusters (State.RDI, State.RSI, Returned, Errno);
         when 108 =>
            List_NetInter (State.RDI, State.RSI, Returned, Errno);
         when 109 =>
            Dump_Logs (State.RDI, State.RSI, Returned, Errno);
         when 110 =>
            List_Filelocks (State.RDI, State.RSI, Returned, Errno);
         when 111 =>
            Loadavg (State.RDI, State.RSI, Returned, Errno);
         when 112 =>
            Meminfo (State.RDI, Returned, Errno);
         when 113 =>
            List_PCI (State.RDI, State.RSI, Returned, Errno);
         when 114 =>
            Get_CPU_Info (State.RDI, Returned, Errno);
         when 115 =>
            Socket_Pair (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 116 =>
            MAdvise (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when others =>
            Userland.Process.Raise_Signal
               (Local.Get_Current_Process,
                Userland.Process.Signal_Bad_Syscall);
            Returned := Unsigned_64'Last;
            Errno    := Error_Not_Implemented;
      end case;

      --  Assign the return values.
      State.RAX := Returned;
      State.RDX := Unsigned_64 (Errno_Value'Enum_Rep (Errno));

      --  Post syscall hook.
      Post_Syscall_Hook (Context.GP_Context (State.all));

      Arch.Snippets.Disable_Interrupts;
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

   procedure Invalidate_Handler is
      I            : Positive;
      Final, Curr  : System.Address;
      Map          : Unsigned_64;
   begin
      I     := CPU.Get_Local.Number;
      Lib.Synchronization.Seize (CPU.Core_Locals (I).Invalidate_Lock);
      Final := CPU.Core_Locals (I).Invalidate_End;
      Curr  := CPU.Core_Locals (I).Invalidate_Start;
      Map   := CPU.Core_Locals (I).Invalidate_Map;
      Lib.Synchronization.Release (CPU.Core_Locals (I).Invalidate_Lock);

      if Snippets.Read_CR3 = Map then
         while To_Integer (Curr) < To_Integer (Final) loop
            Snippets.Invalidate_Page (To_Integer (Curr));
            Curr := Curr + Arch.MMU.Page_Size;
         end loop;
      end if;

      Arch.APIC.LAPIC_EOI;
   exception
      when Constraint_Error =>
         null;
   end Invalidate_Handler;

   procedure Default_ISR_Handler is
   begin
      Lib.Messages.Put_Line ("Default ISR triggered");
      Arch.APIC.LAPIC_EOI;
   end Default_ISR_Handler;

   procedure Spurious_Handler is
   begin
      Lib.Messages.Put_Line ("LAPIC Spurious interrupt occurred");
      Arch.APIC.LAPIC_EOI;
   end Spurious_Handler;
end Arch.Interrupts;
