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

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.Wrappers;
with Lib.Messages;
with Lib;
with Scheduler;

package body Arch.Syscall is
   --  Errno values, they are ABI and arbitrary.
   Error_No_Error        : constant := 0;
   Error_Not_Implemented : constant := 1051; -- ENOSYS.

   procedure Syscall_Log (Address : Unsigned_64; Errno : out Unsigned_64) is
      Addr : constant System.Address := To_Address (Integer_Address (Address));
      Message_Length : constant Natural := Lib.C_String_Length (Addr);
      Message_String : String (1 .. Message_Length) with Address => Addr;
   begin
      Lib.Messages.Put_Line (Message_String);
      Errno := Error_No_Error;
   end Syscall_Log;

   procedure Syscall_Exit (Error_Code : Integer; Errno : out Unsigned_64) is
      pragma Unreferenced (Error_Code);
   begin
      Errno := Error_No_Error;
      Scheduler.Bail;
   end Syscall_Exit;

   procedure Syscall_Set_TCB (Addr : Unsigned_64; Errno : out Unsigned_64) is
   begin
      Wrappers.Write_FS (Addr);
      Errno := Error_No_Error;
   end Syscall_Set_TCB;

   procedure Syscall_Handler (Number : Integer; State : access ISR_GPRs) is
      Ret_Value : Unsigned_64 := 0;
      Errno     : Unsigned_64 := Error_No_Error;
      pragma Unreferenced (Number);
   begin
      --  Swap to kernel GS.
      Wrappers.Swap_GS;

      --  Call the inner syscall.
      --  RAX is the return value, RDX is the returned errno.
      --  Arguments can be RDI, RSI, RDX, RCX, and R8, in that order.
      case State.RAX is
         when 0 => Syscall_Log (State.RDI, Errno);
         when 1 => Syscall_Exit (Integer (State.RDI), Errno);
         when 2 => Syscall_Set_TCB (State.RDI, Errno);
         when others => Errno := Error_Not_Implemented;
      end case;

      --  Assign the return values and swap back to user GS.
      State.RAX := Ret_Value;
      State.RDX := Errno;
      Wrappers.Swap_GS;
   end Syscall_Handler;
end Arch.Syscall;
