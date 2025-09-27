--  arch-exceptions.adb: Specification of interrupt utilities.
--  Copyright (C) 2025 streaksu
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

with System.Machine_Code;
with Panic;
with Arch.Snippets;
with Scheduler;
with Arch.Context;
with Userland.Syscall; use Userland.Syscall;
with Arch.Local;
with Userland.Process;
with Messages;
with Userland.Corefile;

package body Arch.Interrupts with SPARK_Mode => Off is
   Interrupt_Table : array (Interrupt_Index) of Interrupt_Handler
      := [others => null];

   procedure Initialize is
   begin
      Load_Trap_Vector;
   end Initialize;

   procedure Load_Trap_Vector is
   begin
      System.Machine_Code.Asm
         ("csrw stvec, %0",
          Inputs   => System.Address'Asm_Input ("r", trap_entry'Address),
          Volatile => True);
   end Load_Trap_Vector;

   procedure Setup_Interrupt
      (Handler : Interrupt_Handler;
       Index   : out Interrupt_Index;
       Success : out Boolean)
   is
   begin
      Index   := 1;
      Success := False;

      for I in Interrupt_Table'Range loop
         if Interrupt_Table (I) = null then
            Interrupt_Table (I) := Handler;
            Index := I;
            Success := True;
            return;
         end if;
      end loop;
   end Setup_Interrupt;

   procedure Unload_Interrupt (Index : Interrupt_Index) is
   begin
      Interrupt_Table (Index) := null;
   exception
      when Constraint_Error =>
         null;
   end Unload_Interrupt;
   ----------------------------------------------------------------------------
   procedure Handle_Trap (Ctx : not null Frame_Acc) is
      SCause, SStatus : Unsigned_64;
      Is_Int, Is_User : Boolean;
      Cause           : Unsigned_64;
      Signal          : Userland.Process.Signal;
   begin
      --  Read exception data to determine cause.
      System.Machine_Code.Asm
         ("csrr %0, scause",
          Outputs  => Unsigned_64'Asm_Output ("=r", SCause),
          Clobber  => "memory",
          Volatile => True);
      System.Machine_Code.Asm
         ("csrr %0, sstatus",
          Outputs  => Unsigned_64'Asm_Output ("=r", SStatus),
          Clobber  => "memory",
          Volatile => True);
      Is_Int  := (SCause and Shift_Left (1, 63)) /= 0;
      Cause   := SCause and not Shift_Left (1, 63);
      Is_User := (SStatus and Shift_Left (1, 8)) = 0;

      if Is_Int then
         if Cause = 5 then
            System.Machine_Code.Asm
               ("csrc sip, %0",
                Inputs   => Unsigned_64'Asm_Input ("r", 32),
                Clobber  => "memory",
                Volatile => True);
            Scheduler.Scheduler_ISR (Ctx.all);
         else
            Panic.Hard_Panic ("Missing interrupt with cause " & Cause'Image);
         end if;
      else
         if Cause = 8 then
            Ctx.SEPC := Ctx.SEPC + (32 / 8);
            Handle_Syscall (Ctx);
         elsif Is_User then
            Signal :=
               (case Cause is
                 when      2 => Userland.Process.Signal_Illegal_Instruction,
                 when others => Userland.Process.Signal_Segmentation_Fault);

            Messages.Put_Line ("Userland exception: " & Signal'Image);
            Userland.Corefile.Generate_Corefile (Context.GP_Context (Ctx.all));
            Userland.Process.Exit_Process (Local.Get_Current_Process, Signal);
         else
            Panic.Hard_Panic
               ((case Cause is
                 when      0 => "Instruction misalign",
                 when      1 => "Instruction access",
                 when      2 => "Illegal instruction",
                 when      3 => "Breakpoint",
                 when      4 => "Reserved",
                 when      5 => "Load access fault",
                 when      6 => "AMO access misaligned",
                 when      7 => "Store/AMO access fault",
                 when      9 => "Environment call from HS mode",
                 when     10 => "Environment call from VS mode",
                 when     11 => "Environment call from M mode",
                 when     12 => "Instruction page fault",
                 when     13 => "Load page fault",
                 when     15 => "Store/AMO page fault",
                 when     20 => "Instruction guest-page fault",
                 when     21 => "Load guest-page fault",
                 when     22 => "Virtual instruction",
                 when     23 => "Store/AMO guest-page fault",
                 when others => "Reserved trap"), Ctx.all);
         end if;
      end if;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Trap handler faced an exception");
   end Handle_Trap;

   procedure Handle_Syscall (Ctx : not null Frame_Acc) is
      Returned : Unsigned_64;
      Errno    : Errno_Value;
      FP_State : Context.FP_Context;
   begin
      Arch.Snippets.Enable_Interrupts;

      --  Pre syscall hook.
      Pre_Syscall_Hook (Context.GP_Context (Ctx.all));

      --  Call the inner syscall.
      --  X17 is the syscall number.
      --  X10 is the return value.
      --  X11 is the returned errno.
      --  Arguments can be X10, X11, X12, X13, X15, X15, X16.
      case Ctx.X17 is
         when 0 =>
            Sys_Exit (Ctx.X10, Returned, Errno);
         when 1 =>
            Arch_PRCtl (Ctx.X10, Ctx.X11, Returned, Errno);
         when 2 =>
            Open
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned, Errno);
         when 3 =>
            Close (Ctx.X10, Returned, Errno);
         when 4 =>
            Read (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                  Returned, Errno);
         when 5 =>
            Write (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                   Returned, Errno);
         when 6 =>
            Seek (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 7 =>
            Mmap (Ctx.X10, Ctx.X11, Ctx.X12,
                  Ctx.X13, Ctx.X14, Ctx.X15, Returned, Errno);
         when 8 =>
            Munmap (Ctx.X10, Ctx.X11, Returned, Errno);
         when 9 =>
            Get_PID (Returned, Errno);
         when 10 =>
            Get_PPID (Returned, Errno);
         when 11 =>
            Exec (Ctx.X10, Ctx.X11, Ctx.X12,
                  Ctx.X13, Ctx.X14, Ctx.X15, Returned, Errno);
         when 12 =>
            Context.Init_FP_Context (FP_State);
            Fork (Ctx.all, FP_State, Ctx.X10, Returned, Errno);
            if Errno /= Error_No_Error then
               Context.Destroy_FP_Context (FP_State);
            end if;
         when 13 =>
            Wait (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 14 =>
            Socket (Ctx.X10, Ctx.X11, Returned, Errno);
         when 15 =>
            Set_Hostname (Ctx.X10, Ctx.X11, Returned, Errno);
         when 16 =>
            Unlink (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned,
                    Errno);
         when 17 =>
            FStat (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                   Ctx.X14, Returned, Errno);
         when 18 =>
            Pivot_Root (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                Returned, Errno);
         when 19 =>
            Chdir (Ctx.X10, Returned, Errno);
         when 20 =>
            IOCTL (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 21 =>
            Sched_Yield (Returned, Errno);
         when 22 =>
            Get_Min_Pri (Returned, Errno);
         when 23 =>
            Pipe (Ctx.X10, Ctx.X11, Returned, Errno);
         when 24 =>
            Get_UID (Returned, Errno);
         when 25 =>
            Rename (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                    Ctx.X14, Ctx.X15, Ctx.X16, Returned, Errno);
         when 26 =>
            List_Procs (Ctx.X10, Ctx.X11, Returned, Errno);
         when 27 =>
            Get_SID (Ctx.X10, Returned, Errno);
         when 28 =>
            Get_TID (Returned, Errno);
         when 29 =>
            Get_Max_Pri (Returned, Errno);
         when 30 =>
            Fcntl (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 31 =>
            Exit_Thread (Returned, Errno);
         when 32 =>
            Get_Entropy (Ctx.X10, Ctx.X11, Returned, Errno);
         when 33 =>
            MProtect (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 34 =>
            Sync (Returned, Errno);
         when 35 =>
            Set_MAC_Capabilities (Ctx.X10, Returned, Errno);
         when 36 =>
            Get_MAC_Capabilities (Returned, Errno);
         when 37 =>
            Add_MAC_Permissions (Ctx.X10, Ctx.X11, Ctx.X12, Returned,
                                 Errno);
         when 38 =>
            Set_MAC_Enforcement (Ctx.X10, Returned, Errno);
         when 39 =>
            Mount (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                   Ctx.X15, Returned, Errno);
         when 40 =>
            Umount (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 41 =>
            Readlink (Ctx.X10, Ctx.X11, Ctx.X12,
                      Ctx.X13, Ctx.X14, Returned, Errno);
         when 42 =>
            GetDEnts (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 43 =>
            MakeNode (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                      Returned, Errno);
         when 44 =>
            Truncate (Ctx.X10, Ctx.X11, Returned, Errno);
         when 45 =>
            Bind (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 46 =>
            Symlink
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14, Ctx.X15,
                Returned, Errno);
         when 47 =>
            Connect (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 48 =>
            Open_PTY (Ctx.X10, Returned, Errno);
         when 49 =>
            FSync (Ctx.X10, Ctx.X11, Returned, Errno);
         when 50 =>
            Link (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                  Ctx.X14, Ctx.X15, Returned, Errno);
         when 51 =>
            PTrace (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned,
                                Errno);
         when 52 =>
            Listen (Ctx.X10, Ctx.X11, Returned, Errno);
         when 53 =>
            Sys_Accept (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned,
                                    Errno);
         when 54 =>
            RLimit (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 55 =>
            Sched_RR_Interval (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 56 =>
            FAccess (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                    Ctx.X14, Returned, Errno);
         when 57 =>
            PPoll (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned,
                   Errno);
         when 58 =>
            Get_EUID (Returned, Errno);
         when 59 =>
            Set_UIDs (Ctx.X10, Ctx.X11, Returned, Errno);
         when 60 =>
            Fchmod (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                    Ctx.X14, Returned, Errno);
         when 61 =>
            Umask (Ctx.X10, Returned, Errno);
         when 62 =>
            Reboot (Ctx.X10, Ctx.X11, Returned, Errno);
         when 63 =>
            Fchown (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                    Ctx.X15, Returned, Errno);
         when 64 =>
            Get_PGID (Ctx.X10, Returned, Errno);
         when 65 =>
            Set_PGID (Ctx.X10, Ctx.X11, Returned, Errno);
         when 66 =>
            Get_Sock_Name (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 67 =>
            Get_Peer_Name (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 68 =>
            Shutdown (Ctx.X10, Ctx.X11, Returned, Errno);
         when 69 =>
            Futex (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                   Returned, Errno);
         when 70 =>
            Clock (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 71 =>
            Clock_Nanosleep (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
                             Returned, Errno);
         when 72 =>
            Get_RUsage (Ctx.X10, Ctx.X11, Returned, Errno);
         when 73 =>
            RecvFrom (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                      Ctx.X15, Returned, Errno);
         when 74 =>
            SendTo (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                    Ctx.X15, Returned, Errno);
         when 75 =>
            Config_NetInterface (Ctx.X10, Ctx.X11, Ctx.X12, Returned,
                                 Errno);
         when 76 =>
            UTimes (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                    Returned, Errno);
         when 77 =>
            Sched_GetScheduler (Ctx.X10, Ctx.X11, Returned, Errno);
         when 78 =>
            Sched_SetScheduler
               (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 79 =>
            Sigprocmask (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 80 =>
            Sigaction (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 81 =>
            Send_Signal (Ctx.X10, Ctx.X11, Returned, Errno);
         when 82 =>
            Get_Prio (Ctx.X10, Ctx.X11, Returned, Errno);
         when 83 =>
            Set_Prio (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 84 =>
            Get_GID (Returned, Errno);
         when 85 =>
            Get_EGID (Returned, Errno);
         when 86 =>
            Set_GIDs (Ctx.X10, Ctx.X11, Returned, Errno);
         when 87 =>
            Get_Groups (Ctx.X10, Ctx.X11, Returned, Errno);
         when 88 =>
            Set_Groups (Ctx.X10, Ctx.X11, Returned, Errno);
         when 89 =>
            TTY_Name (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 90 =>
            FAdvise
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned, Errno);
         when 91 =>
            SHMAt (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 92 =>
            SHMCtl (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 93 =>
            SHMDt (Ctx.X10, Returned, Errno);
         when 94 =>
            SHMGet (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 95 =>
            GetSockOpt
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                Returned, Errno);
         when 96 =>
            SetSockOpt
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                Returned, Errno);
         when 97 =>
            Get_Thread_Name (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 98 =>
            Set_Thread_Name (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 99 =>
            Failure_Policy (Ctx.X10, Ctx.X11, Returned, Errno);
         when 100 =>
            Create_Thread
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned, Errno);
         when 101 =>
            Signal_Return (Returned, Errno);
         when 102 =>
            Sigaltstack (Ctx.X10, Ctx.X11, Returned, Errno);
         when 103 =>
            Recv_Sock_Ctr (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 104 =>
            List_Mounts (Ctx.X10, Ctx.X11, Returned, Errno);
         when 105 =>
            Uname (Ctx.X10, Returned, Errno);
         when 106 =>
            List_Threads (Ctx.X10, Ctx.X11, Returned, Errno);
         when 107 =>
            Send_Sock_Ctr (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 108 =>
            List_NetInter (Ctx.X10, Ctx.X11, Returned, Errno);
         when 109 =>
            Dump_Logs (Ctx.X10, Ctx.X11, Returned, Errno);
         when 110 =>
            List_Filelocks (Ctx.X10, Ctx.X11, Returned, Errno);
         when 111 =>
            Loadavg (Ctx.X10, Ctx.X11, Returned, Errno);
         when 112 =>
            Meminfo (Ctx.X10, Returned, Errno);
         when 113 =>
            List_PCI (Ctx.X10, Ctx.X11, Returned, Errno);
         when 114 =>
            Get_CPU_Info (Ctx.X10, Returned, Errno);
         when 115 =>
            Socket_Pair (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 116 =>
            MAdvise (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 117 =>
            NVMM_Capability (Ctx.X10, Returned, Errno);
         when 118 =>
            NVMM_Machine_Create (Returned, Errno);
         when 119 =>
            NVMM_Machine_Destroy (Ctx.X10, Returned, Errno);
         when 120 =>
            NVMM_Machine_Configure
               (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 121 =>
            NVMM_VCPU_Create (Ctx.X10, Returned, Errno);
         when 122 =>
            NVMM_VCPU_Destroy (Ctx.X10, Ctx.X11, Returned, Errno);
         when 123 =>
            NVMM_VCPU_Configure
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Returned, Errno);
         when 124 =>
            NVMM_VCPU_SetState
               (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 125 =>
            NVMM_VCPU_GetState
               (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 126 =>
            NVMM_VCPU_Inject (Ctx.X10, Ctx.X11, Returned, Errno);
         when 127 =>
            NVMM_VCPU_Run (Ctx.X10, Ctx.X11, Returned, Errno);
         when 128 =>
            NVMM_GPA_Map (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
               Returned, Errno);
         when 129 =>
            NVMM_GPA_Unmap (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
               Returned, Errno);
         when 130 =>
            NVMM_HVA_Map (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 131 =>
            NVMM_HVA_Unmap (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 132 =>
            NVMM_GVA_2_GPA (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
               Ctx.X14, Returned, Errno);
         when 133 =>
            NVMM_GPA_2_HVA (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13,
               Returned, Errno);
         when 134 =>
            NVMM_Assist_IO
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                Ctx.X15, Returned, Errno);
         when 135 =>
            NVMM_Assist_Mem
               (Ctx.X10, Ctx.X11, Ctx.X12, Ctx.X13, Ctx.X14,
                Ctx.X15, Returned, Errno);
         when 136 =>
            NVMM_VCPU_Dump (Ctx.X10, Ctx.X11, Ctx.X12, Returned, Errno);
         when 137 =>
            NVMM_VCPU_Stop (Ctx.X10, Ctx.X11, Returned, Errno);
         when 138 =>
            Set_SID (Returned, Errno);
         when others =>
            Userland.Process.Raise_Signal
               (Local.Get_Current_Process,
                Userland.Process.Signal_Bad_Syscall);
            Returned := Unsigned_64'Last;
            Errno    := Error_Not_Implemented;
      end case;

      --  Assign the return values.
      Ctx.X10 := Returned;
      Ctx.X11 := Unsigned_64 (Errno_Value'Enum_Rep (Errno));

      --  Post syscall hook.
      Post_Syscall_Hook (Context.GP_Context (Ctx.all));

      Arch.Snippets.Disable_Interrupts;
   end Handle_Syscall;
end Arch.Interrupts;
