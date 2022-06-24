--  arch-syscall.adb: Syscall table and implementation.
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

with Interfaces; use Interfaces;
with Arch.Wrappers;
with Userland.Syscall; use Userland.Syscall;

package body Arch.Syscall is
   procedure Syscall_Handler (Number : Integer; State : access ISR_GPRs) is
      Returned : Unsigned_64 := Unsigned_64'Last;
      Errno    : Errno_Value := Error_No_Error;
      pragma Unreferenced (Number);
   begin
      Wrappers.Swap_GS;

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
            Returned := Syscall_Fork (State, Errno);
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
         when others =>
            Errno := Error_Not_Implemented;
      end case;

      --  Assign the return values and swap back to user GS.
      State.RAX := Returned;
      State.RDX := Unsigned_64 (Errno_Value'Enum_Rep (Errno));
      Wrappers.Swap_GS;
   end Syscall_Handler;
end Arch.Syscall;
