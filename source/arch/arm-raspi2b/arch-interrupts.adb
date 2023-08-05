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
with Arch.Context;

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
      Returned : Unsigned_64;
      Errno    : Errno_Value;
      FP_State : Context.FP_Context;
   begin
      Arch.Snippets.Enable_Interrupts;

      --  Pre syscall hook.
      Pre_Syscall_Hook (Context.GP_Context (State));

      --  Call the inner syscall.
      --  R0: return value. R4: syscall number. R1: errno. R0 - R7: args.
      case State.R4 is
         when 0 =>
            Sys_Exit (Unsigned_64 (State.R0), Returned, Errno);
         when 1 =>
            Arch_PRCtl
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 2 =>
            Open
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Returned, Errno);
         when 3 =>
            Close (Unsigned_64 (State.R0), Returned, Errno);
         when 4 =>
            Read
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 5 =>
            Write
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 6 =>
            Seek
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 7 =>
            Mmap
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Returned, Errno);
         when 8 =>
            Munmap
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 9 =>
            Get_PID (Returned, Errno);
         when 10 =>
            Get_PPID (Returned, Errno);
         when 11 =>
            Exec
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Returned, Errno);
         when 12 =>
            Context.Save_FP_Context (FP_State);
            Clone
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), State, FP_State, Returned, Errno);
         when 13 =>
            Wait
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 14 =>
            Socket
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 15 =>
            Set_Hostname
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 16 =>
            Unlink
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 17 =>
            FStat
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 18 =>
            Get_CWD
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 19 =>
            Chdir
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 20 =>
            IOCTL
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 21 =>
            Sched_Yield (Returned, Errno);
         when 22 =>
            Set_Deadlines
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 23 =>
            Pipe
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 24 =>
            Get_UID (Returned, Errno);
         when 25 =>
            Rename
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Unsigned_64 (State.R6), Returned, Errno);
         when 26 =>
            Sysconf
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 27 =>
            Spawn
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Unsigned_64 (State.R6), Returned, Errno);
         when 28 =>
            Get_Thread_Sched (Returned, Errno);
         when 29 =>
            Set_Thread_Sched (Unsigned_64 (State.R0), Returned, Errno);
         when 30 =>
            Fcntl
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 31 =>
            Exit_Thread (Returned, Errno);
         when 32 =>
            Get_Random
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 33 =>
            MProtect
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 34 =>
            Sync (Returned, Errno);
         when 35 =>
            Set_MAC_Capabilities (Unsigned_64 (State.R0), Returned, Errno);
         when 36 =>
            Get_MAC_Capabilities (Returned, Errno);
         when 37 =>
            Add_MAC_Permissions
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 38 =>
            Set_MAC_Enforcement (Unsigned_64 (State.R0), Returned, Errno);
         when 39 =>
            Mount
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Returned, Errno);
         when 40 =>
            Umount
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 41 =>
            Readlink
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Returned, Errno);
         when 42 =>
            GetDEnts
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 43 =>
            MakeNode
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Returned, Errno);
         when 44 =>
            Truncate
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 45 =>
            Bind
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 46 =>
            Symlink
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Returned, Errno);
         when 47 =>
            Connect
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 48 =>
            Open_PTY
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 49 =>
            FSync
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 50 =>
            Link
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Unsigned_64 (State.R4), Unsigned_64 (State.R5),
                Returned, Errno);
         when 51 =>
            PTrace
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Returned, Errno);
         when 52 =>
            Listen
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 53 =>
            Sys_Accept
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Returned, Errno);
         when 54 =>
            Get_RLimit (Unsigned_64 (State.R0), Returned, Errno);
         when 55 =>
            Set_RLimit
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 57 =>
            Poll
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 58 =>
            Get_EUID (Returned, Errno);
         when 59 =>
            Set_UIDs
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 60 =>
            Fchmod
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 61 =>
            Umask (Unsigned_64 (State.R0), Returned, Errno);
         when 62 =>
            Reboot
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Returned, Errno);
         when 63 =>
            Fchown
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Returned, Errno);
         when 64 =>
            PRead
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Returned, Errno);
         when 65 =>
            PWrite
               (Unsigned_64 (State.R0), Unsigned_64 (State.R1),
                Unsigned_64 (State.R2), Unsigned_64 (State.R3),
                Returned, Errno);
         when others =>
            Returned := Unsigned_64'Last;
            Errno    := Error_Not_Implemented;
      end case;

      --  Assign the return values.
      State.R0 := Unsigned_32 (Returned and 16#FFFFFFFF#);
      State.R1 := Unsigned_32 (Errno_Value'Enum_Rep (Errno));

      --  Post syscall hook.
      Post_Syscall_Hook (Context.GP_Context (State));
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
      Discard    : Natural;
      Val_Buffer : Lib.Messages.Translated_String;
   begin
      Lib.Messages.Image (Fr.R0, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R0 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R1, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R1 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R2, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R2 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R3, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R3 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R4, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R4 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R5, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R5 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R6, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R6 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R7, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R7 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R4, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R4 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R5, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R5 : " & Val_Buffer);
      Lib.Messages.Image (Fr.R6, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R6: " & Val_Buffer);
      Lib.Messages.Image (Fr.R11, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R11: " & Val_Buffer);
      Lib.Messages.Image (Fr.R12, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R12: " & Val_Buffer);
      Lib.Messages.Image (Fr.R13, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R13: " & Val_Buffer);
      Lib.Messages.Image (Fr.R14, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R14: " & Val_Buffer);
      Lib.Messages.Image (Fr.R15, Val_Buffer, Discard, True);
      Lib.Messages.Put_Line ("R15: " & Val_Buffer);
      Lib.Panic.Hard_Panic (Message);
   end Print_Fatal;
end Arch.Interrupts;
