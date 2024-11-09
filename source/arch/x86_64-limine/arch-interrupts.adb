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
with Scheduler;
with Userland.Syscall; use Userland.Syscall;
with Userland.Memory_Failure;
with Arch.Snippets; use Arch.Snippets;
with Arch.Local;
with Userland.Corefile;
with Userland.Process;

package body Arch.Interrupts is
   procedure Exception_Handler (Num : Integer; State : not null ISR_GPRs_Acc)
   is
      Len : Natural;
      Str : Lib.Messages.Translated_String;
      Exception_Text : constant array (0 .. 30) of String (1 .. 3) :=
         (0  => "#DE", 1  => "#DB", 2  => "???", 3  => "#BP",
          4  => "#OF", 5  => "#BR", 6  => "#UD", 7  => "#NM",
          8  => "#DF", 9  => "???", 10 => "#TS", 11 => "#NP",
          12 => "#SS", 13 => "#GP", 14 => "#PF", 15 => "???",
          16 => "#MF", 17 => "#AC", 18 => "#MC", 19 => "#XM",
          20 => "#VE", 21 => "#CP", 22 => "???", 23 => "???",
          24 => "???", 25 => "???", 26 => "???", 27 => "???",
          28 => "#HV", 29 => "#VC", 30 => "#SX");
   begin
      --  If this is a machine check, we need special logic to dump the banks
      --  and unconditionally die.
      if Num = 18 then
         case Process_Machine_Check_Banks is
            when Memory_MCE =>
               Userland.Memory_Failure.Handle_Failure;
            when Unrecognized_MCE =>
               Lib.Panic.Hard_Panic ("MCE has no fixing");
         end case;
      end if;

      --  Check whether we have to panic or just exit the thread.
      if State.CS = (GDT.User_Code64_Segment or 3) then
         Lib.Messages.Put_Line ("Userland " & Exception_Text (Num));
         Userland.Corefile.Generate_Corefile (Context.GP_Context (State.all));
         Do_Exit (Local.Get_Current_Process,
                  Userland.Process.Signal_Segmentation_Fault);
      else
         Print_Triple ("RAX", "RBX", "RCX", State.RAX, State.RBX, State.RCX);
         Print_Triple ("RDX", "RSI", "RDI", State.RDX, State.RSI, State.RDI);
         Print_Triple ("RBP", " R8", " R9", State.RBP, State.R8, State.R9);
         Print_Triple ("R10", "R11", "R12", State.R10, State.R11, State.R12);
         Print_Triple ("R13", "R14", "R15", State.R13, State.R14, State.R15);
         Print_Triple ("RIP", "RSP", "CR2", State.RIP, State.RSP, Read_CR2);

         if Num = 11 or Num = 12 or Num = 13 or Num = 14 or Num = 17 or
            Num = 21 or Num = 29 or Num = 30
         then
            Lib.Messages.Image (State.Error_Code, Str, Len, True);
            Lib.Messages.Put_Line ("Error code: " & Str);
         end if;

         Lib.Panic.Hard_Panic ("Kernel " & Exception_Text (Num));
      end if;
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
            Read (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 5 =>
            Write (State.RDI, State.RSI, State.RDX, Returned, Errno);
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
            Clone (State.RDI, State.RSI, State.RDX, State.R12,
                   State.R8, State.R9, State.all,  FP_State, Returned, Errno);
            Context.Destroy_FP_Context (FP_State);
         when 13 =>
            Wait (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 14 =>
            Socket (State.RDI, State.RSI, Returned, Errno);
         when 15 =>
            Set_Hostname (State.RDI, State.RSI, Returned, Errno);
         when 16 =>
            Unlink (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 17 =>
            FStat (State.RDI, State.RSI, State.RDX, State.R12,
                   State.R8, Returned, Errno);
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
            Sysconf (State.RDI, State.RSI, State.RDX, Returned, Errno);
         when 27 =>
            Spawn (State.RDI, State.RSI, State.RDX, State.R12,
                   State.R8, State.R9, State.R10, Returned, Errno);
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
         when 64 =>
            PRead (State.RDI, State.RSI, State.RDX, State.R12, Returned,
                   Errno);
         when 65 =>
            PWrite (State.RDI, State.RSI, State.RDX, State.R12, Returned,
                    Errno);
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
      I     : constant Positive := CPU.Get_Local.Number;
      Final : constant System.Address := CPU.Core_Locals (I).Invalidate_End;
      Curr  :          System.Address := CPU.Core_Locals (I).Invalidate_Start;
   begin
      while To_Integer (Curr) < To_Integer (Final) loop
         Snippets.Invalidate_Page (To_Integer (Curr));
         Curr := Curr + MMU.Page_Size;
      end loop;
      Arch.APIC.LAPIC_EOI;
   end Invalidate_Handler;

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
   ----------------------------------------------------------------------------
   procedure Print_Triple (N1, N2, N3 : String; V1, V2, V3 : Unsigned_64) is
      Discard    : Natural;
      B1, B2, B3 : Lib.Messages.Translated_String;
   begin
      Lib.Messages.Image (V1, B1, Discard, True);
      Lib.Messages.Image (V2, B2, Discard, True);
      Lib.Messages.Image (V3, B3, Discard, True);
      Lib.Messages.Put_Line (N1 & " " & B1 (5 .. B1'Last) & " " &
         N2 & " " & B2 (5 .. B2'Last) & " " & N3 & " " & B3 (5 .. B3'Last));
   end Print_Triple;

   function Process_Machine_Check_Banks return Machine_Check_Type is
      IA32_MCG_CAP_MSR : constant := 16#00000179#;
      IA32_MCI_CTL_MSR : constant := 16#00000404#;

      Bank_Count, Bank_Val : Unsigned_64;
      Len    : Natural;
      B1, B2 : Lib.Messages.Translated_String;
   begin
      Lib.Messages.Put_Line ("The machine encountered a machine check, this");
      Lib.Messages.Put_Line ("is usually indicative of hardware failure, and");
      Lib.Messages.Put_Line ("the operating system is required to stop.");
      Lib.Messages.Put_Line ("Please check the values of the MC banks dumped");
      Lib.Messages.Put_Line ("below against processor and hardware manuals.");

      --  Check how many banks we have and say how many, we dont know how far
      --  we will get.
      Bank_Count := Read_MSR (IA32_MCG_CAP_MSR) and 2#11111111#;
      Lib.Messages.Image (Bank_Count, B1, Len, False);
      Lib.Messages.Put_Line
         ("Found " & B1 (B1'Last - Len + 1 .. B1'Last) & " banks.");

      --  Read them and dump them, they are in intervals of four.
      for I in 1 .. Bank_Count loop
         Bank_Val := Read_MSR (IA32_MCI_CTL_MSR + ((Unsigned_32 (I) - 1) * 4));
         Lib.Messages.Image (Bank_Val, B2, Len, True);
         Lib.Messages.Image (I, B1, Len, False);
         Lib.Messages.Put_Line
            ("Bank #" & B1 (B1'Last - Len + 1 .. B1'Last) & ": " & B2);
      end loop;

      return Unrecognized_MCE;
   end Process_Machine_Check_Banks;
end Arch.Interrupts;
