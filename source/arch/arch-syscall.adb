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
with Userland.Process;
with FS.File; use FS.File;
with Scheduler;
with Memory.Virtual;
with Memory.Physical;
with Memory; use Memory;

package body Arch.Syscall is
   --  Errno values, they are ABI and arbitrary.
   Error_No_Error        : constant := 0;
   Error_Invalid_Value   : constant := 1026; -- EINVAL.
   Error_Not_Implemented : constant := 1051; -- ENOSYS.
   Error_Bad_File        : constant := 1081; -- EBADFD.

   procedure Syscall_Handler (Number : Integer; State : access ISR_GPRs) is
      Returned : Unsigned_64 := Unsigned_64'Last;
      Errno    : Unsigned_64 := Error_No_Error;
      pragma Unreferenced (Number);
   begin
      --  Swap to kernel GS.
      Wrappers.Swap_GS;

      --  Call the inner syscall.
      --  RAX is the return value, as well as the syscall number.
      --  RDX is the returned errno.
      --  Arguments can be RDI, RSI, RDX, RCX, R8, and R9, in that order.
      case State.RAX is
         when 0 =>
            Returned := Syscall_Log (State.RDI, Errno);
         when 1 =>
            Syscall_Exit (Integer (State.RDI));
         when 2 =>
            Returned := Syscall_Set_TCB (State.RDI, Errno);
         when 3 =>
            Returned := Syscall_Open (State.RDI, State.RSI, Errno);
         when 4 =>
            Returned := Syscall_Close (State.RDI, Errno);
         when 5 =>
            Returned := Syscall_Read (State.RDI, State.RSI, State.RDX, Errno);
         when 6 =>
            Returned := Syscall_Write (State.RDI, State.RSI, State.RDX, Errno);
         when 7 =>
            Returned := Syscall_Seek (State.RDI, State.RSI, State.RDX, Errno);
         when 8 =>
            Returned := Syscall_Alloc (State.RDI, Errno);
         when 9 =>
            Syscall_Free (State.RDI);
         when others =>
            Errno := Error_Not_Implemented;
      end case;

      --  Assign the return values and swap back to user GS.
      State.RAX := Returned;
      State.RDX := Errno;
      Wrappers.Swap_GS;
   end Syscall_Handler;

   function Syscall_Log
      (Address : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64
   is
      Addr : constant System.Address := To_Address (Integer_Address (Address));
   begin
      if Address = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         declare
            Message_Length : constant Natural := Lib.C_String_Length (Addr);
            Message_String : String (1 .. Message_Length) with Address => Addr;
         begin
            Lib.Messages.Put_Line (Message_String);
            Errno := Error_No_Error;
            return 0;
         end;
      end if;
   end Syscall_Log;

   procedure Syscall_Exit (Error_Code : Integer) is
      pragma Unreferenced (Error_Code);
   begin
      Scheduler.Bail;
   end Syscall_Exit;

   function Syscall_Set_TCB
      (Address : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64 is
   begin
      if Address = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Wrappers.Write_FS (Address);
         Errno := Error_No_Error;
         return 0;
      end if;
   end Syscall_Set_TCB;

   function Syscall_Open
      (Address : Unsigned_64;
       Flags   : Unsigned_64;
       Errno   : out Unsigned_64) return Unsigned_64
   is
      Addr : constant System.Address := To_Address (Integer_Address (Address));
   begin
      if Address = 0 then
         goto Error_Return;
      end if;
      declare
         Path_Length  : constant Natural := Lib.C_String_Length (Addr);
         Path_String  : String (1 .. Path_Length) with Address => Addr;
         Current_Thre : constant Scheduler.TID := Scheduler.Get_Current_Thread;
         Current_Proc : constant Userland.Process.PID :=
            Userland.Process.Get_Process_By_Thread (Current_Thre);
         Open_Mode    : FS.File.Access_Mode;
         Returned_FD  : FS.File.FD;
      begin
         --  Parse the mode.
         if (Flags and O_RDWR) /= 0 then
            Open_Mode := FS.File.Access_RW;
         elsif (Flags and O_RDONLY) /= 0 then
            Open_Mode := FS.File.Access_R;
         elsif (Flags and O_WRONLY) /= 0 then
            Open_Mode := FS.File.Access_W;
         else
            goto Error_Return;
         end if;

         --  Actually open the file.
         Returned_FD := FS.File.Open (Path_String, Open_Mode);
         if Returned_FD = FS.File.Error_FD then
            goto Error_Return;
         else
            if not Userland.Process.Add_File (Current_Proc, Returned_FD) then
               goto Error_Return;
            else
               Errno := Error_No_Error;
               return Unsigned_64 (Returned_FD);
            end if;
         end if;
      end;
   <<Error_Return>>
      Errno := Error_Invalid_Value;
      return Unsigned_64'Last;
   end Syscall_Open;

   function Syscall_Close
      (File_D : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64
   is
      Current_Thread  : constant Scheduler.TID := Scheduler.Get_Current_Thread;
      Current_Process : constant Userland.Process.PID :=
         Userland.Process.Get_Process_By_Thread (Current_Thread);
      File            : constant FS.File.FD := FD (File_D);
   begin
      if File = FS.File.Error_FD or
         not Userland.Process.Contains_File (Current_Process, File)
      then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Remove_File (Current_Process, File);
      FS.File.Close (File);
      Errno := Error_No_Error;
      return 0;
   end Syscall_Close;

   function Syscall_Read
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64
   is
      Buffer_Addr     : constant System.Address :=
         To_Address (Integer_Address (Buffer));
      Current_Thread  : constant Scheduler.TID := Scheduler.Get_Current_Thread;
      Current_Process : constant Userland.Process.PID :=
         Userland.Process.Get_Process_By_Thread (Current_Thread);
      File            : constant FS.File.FD := FD (File_D);
   begin
      if File = FS.File.Error_FD or
         not Userland.Process.Contains_File (Current_Process, File)
      then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;
      if Buffer = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      Errno := Error_No_Error;
      return Unsigned_64 (FS.File.Read (File, Integer (Count), Buffer_Addr));
   end Syscall_Read;

   function Syscall_Write
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64
   is
      Buffer_Addr     : constant System.Address :=
         To_Address (Integer_Address (Buffer));
      Current_Thread  : constant Scheduler.TID := Scheduler.Get_Current_Thread;
      Current_Process : constant Userland.Process.PID :=
         Userland.Process.Get_Process_By_Thread (Current_Thread);
      File            : constant FS.File.FD := FD (File_D);
   begin
      if File = FS.File.Error_FD or
         not Userland.Process.Contains_File (Current_Process, File)
      then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;
      if Buffer = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      Errno := Error_No_Error;
      return Unsigned_64 (FS.File.Write (File, Integer (Count), Buffer_Addr));
   end Syscall_Write;

   function Syscall_Seek
      (File_D : Unsigned_64;
       Offset : Unsigned_64;
       Whence : Unsigned_64;
       Errno  : out Unsigned_64) return Unsigned_64
   is
      Current_Thread  : constant Scheduler.TID := Scheduler.Get_Current_Thread;
      Current_Process : constant Userland.Process.PID :=
         Userland.Process.Get_Process_By_Thread (Current_Thread);
      File            : constant FS.File.FD := FD (File_D);
      Passed_Offset   : constant Natural    := Natural (Offset);
   begin
      if File = FS.File.Error_FD or
         not Userland.Process.Contains_File (Current_Process, File)
      then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      case Whence is
         when SEEK_SET =>
            FS.File.Set_Index (File, Passed_Offset);
         when SEEK_CURRENT =>
            FS.File.Set_Index (File, FS.File.Get_Index (File) + Passed_Offset);
         when SEEK_END =>
            FS.File.Set_Index (File, FS.File.Get_Size (File) + Passed_Offset);
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      Errno := Error_No_Error;
      return Unsigned_64 (FS.File.Get_Index (File));
   end Syscall_Seek;

   function Syscall_Alloc
      (Count : Unsigned_64;
       Errno : out Unsigned_64) return Unsigned_64
   is
      Current_Thread  : constant Scheduler.TID := Scheduler.Get_Current_Thread;
      Current_Process : constant Userland.Process.PID :=
         Userland.Process.Get_Process_By_Thread (Current_Thread);
      Map : constant Memory.Virtual.Page_Map_Acc :=
         Userland.Process.Get_Memmap (Current_Process);
      Allocated : constant Virtual_Address :=
         Memory.Physical.Alloc (Size (Count));
      Base : constant Unsigned_64 :=
         Userland.Process.Bump_Alloc (Current_Process, Count);
   begin
      --  Map the memory.
      Memory.Virtual.Map_Range
         (Map,
          Virtual_Address (Base),
          Allocated,
          Count,
         (Present         => True,
          Read_Write      => True,
          User_Supervisor => True,
          Write_Through   => False,
          Cache_Disable   => False,
          Accessed        => False,
          Dirty           => False,
          PAT             => False,
          Global          => False),
         False);
      Errno := Error_No_Error;
      return Base;
   end Syscall_Alloc;

   --  Free a block.
   procedure Syscall_Free (Address : Unsigned_64) is
      Current_Thread  : constant Scheduler.TID := Scheduler.Get_Current_Thread;
      Current_Process : constant Userland.Process.PID :=
         Userland.Process.Get_Process_By_Thread (Current_Thread);
      Map : constant Memory.Virtual.Page_Map_Acc :=
         Userland.Process.Get_Memmap (Current_Process);
      Addr : constant Physical_Address :=
         Memory.Virtual.Virtual_To_Physical (Map, Virtual_Address (Address));
   begin
      Memory.Physical.Free (Addr);
   end Syscall_Free;
end Arch.Syscall;
