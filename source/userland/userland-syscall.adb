--  userland-syscall.adb: Syscall implementation.
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

with System.Storage_Elements; use System.Storage_Elements;
with Ada.Characters.Latin_1;
with Config;
with System; use System;
with Lib.Messages;
with Lib;
with Lib.Time;
with Lib.Panic;
with Lib.Alignment;
with Userland.Loader;
with Arch.MMU; use Arch.MMU;
with Arch.Clocks;
with Arch.Local;
with Memory.Physical;
with Memory; use Memory;
with Ada.Unchecked_Deallocation;
with Arch.Hooks;
with Cryptography.Random;
with IPC.PTY;  use IPC.PTY;
with IPC.Futex;
with Devices.TermIOs;
with Arch.Power;
with Devices; use Devices;

package body Userland.Syscall with SPARK_Mode => Off is
   procedure Sys_Exit
      (Code     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
   begin
      Returned := 0;
      Errno    := Error_No_Error;
      Do_Exit (Arch.Local.Get_Current_Process, Unsigned_8 (Code and 16#FF#));
   end Sys_Exit;

   procedure Arch_PRCtl
      (Code     : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Map   : constant  Page_Table_Acc := Get_Common_Map (Proc);
      I_Arg : constant Integer_Address := Integer_Address (Argument);
      S_Arg : constant  System.Address := To_Address (I_Arg);
   begin
      if not Check_Userland_Access (Map, I_Arg, 8) then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
      elsif Code > Unsigned_64 (Natural'Last) or else
            not Arch.Hooks.PRCTL_Hook (Natural (Code), S_Arg)
      then
         Returned := Unsigned_64'Last;
         Errno    := Error_Invalid_Value;
      else
         Returned := 0;
         Errno    := Error_No_Error;
      end if;
   end Arch_PRCtl;

   procedure Open
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Path_IAddr  : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr  : constant  System.Address := To_Address (Path_IAddr);
      Curr_Proc   : constant             PID := Arch.Local.Get_Current_Process;
      Map         : constant  Page_Table_Acc := Get_Common_Map (Curr_Proc);
      Do_Cloexec  : constant         Boolean := (Flags and O_CLOEXEC)  /= 0;
      Do_Read     : constant         Boolean := (Flags and O_RDONLY)   /= 0;
      Do_Write    : constant         Boolean := (Flags and O_WRONLY)   /= 0;
      Dont_Follow : constant         Boolean := (Flags and O_NOFOLLOW) /= 0;
      Do_Append   : constant         Boolean := (Flags and O_APPEND)   /= 0;
      Discard     : Boolean;
      Success     : VFS.FS_Status;
      CWD_FS      : VFS.FS_Handle;
      CWD_Ino     : VFS.File_Inode_Number;
      Opened_Ino  : VFS.File_Inode_Number;
      Opened_Dev  : Devices.Device_Handle;
      Opened_Stat : VFS.File_Stat;
      New_Descr   : File_Description_Acc;
      File_Perms  : MAC.Permissions;
      Returned_FD : Natural;
      User        : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Curr_Proc, User);

      declare
         Path : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         if Path'Length > 5 and then Path (1 .. 5) = "/dev/" then
            Opened_Dev := Devices.Fetch (Path (6 .. Path'Last));
            if Opened_Dev = Devices.Error_Handle then
               Returned := Unsigned_64'Last;
               Errno    := Error_No_Entity;
               return;
            end if;

            File_Perms := Check_Permissions (Curr_Proc, Opened_Dev);
            New_Descr  := new File_Description'
               (Children_Count  => 0,
                Description     => Description_Device,
                Inner_Dev_Read  => Do_Read,
                Inner_Dev_Write => Do_Write,
                Inner_Dev_Pos   => 0,
                Inner_Dev       => Opened_Dev);
         else
            Resolve_AT_Directive (Curr_Proc, Dir_FD, CWD_FS, CWD_Ino);
            if CWD_FS = VFS.Error_Handle then
               Returned := Unsigned_64'Last;
               Errno    := Error_Bad_File;
               return;
            end if;

            VFS.Open (CWD_FS, CWD_Ino, Path, Opened_Ino, Success, User,
                      not Dont_Follow);
            if Success /= VFS.FS_Success then
               Returned := Unsigned_64'Last;
               Errno    := Error_No_Entity;
               return;
            end if;

            if Do_Append then
               VFS.Stat (CWD_FS, Opened_Ino, Opened_Stat, Success, User);
            else
               Opened_Stat.Byte_Size := 0;
            end if;

            File_Perms := Check_Permissions (Curr_Proc, CWD_FS, Opened_Ino);
            New_Descr  := new File_Description'
               (Children_Count  => 0,
                Description     => Description_Inode,
                Inner_Ino_Read  => Do_Read,
                Inner_Ino_Write => Do_Write,
                Inner_Ino_FS    => CWD_FS,
                Inner_Ino_Pos   => Opened_Stat.Byte_Size,
                Inner_Ino       => Opened_Ino);
         end if;
      end;

      if (not Do_Read   and not Do_Write)             or
         (Do_Read       and not File_Perms.Can_Read)  or
         (Do_Write      and not File_Perms.Can_Write) or
         (not Do_Append and File_Perms.Can_Append_Only)
      then
         Close (New_Descr);
         Execute_MAC_Failure ("open", Curr_Proc);
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_Access;
         return;
      end if;

      Check_Add_File (Curr_Proc, New_Descr, Discard, Returned_FD);
      if Discard then
         Process.Set_Close_On_Exec (Curr_Proc, Unsigned_64 (Returned_FD),
                                    Do_Cloexec);
         Errno    := Error_No_Error;
         Returned := Unsigned_64 (Returned_FD);
      else
         Close (New_Descr);
         Returned := Unsigned_64'Last;
         Errno    := Error_Too_Many_Files;
      end if;
   end Open;

   procedure Close
      (File_D   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Curr : constant PID := Arch.Local.Get_Current_Process;
   begin
      if Userland.Process.Is_Valid_File (Curr, File_D) then
         Userland.Process.Remove_File (Curr, Natural (File_D));
         Returned := 0;
         Errno    := Error_No_Error;
      else
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
      end if;
   end Close;

   procedure Read
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Map       : constant    Page_Table_Acc := Get_Common_Map (Proc);
      File      : constant File_Description_Acc := Get_File (Proc, File_D);
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success2  : IPC.FIFO.Pipe_Status;
      Success3  : Boolean;
      Success4  : IPC.Socket.Socket_Status;
      User      : Unsigned_32;
      Final_Cnt : Natural;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      elsif File = null then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      declare
         Data : Devices.Operation_Data (1 .. Final_Cnt)
            with Import, Address => Buf_SAddr;
      begin
         case File.Description is
            when Description_Device =>
               if not File.Inner_Dev_Read then
                  Returned := Unsigned_64'Last;
                  Errno    := Error_Invalid_Value;
                  return;
               end if;
               Devices.Read
                  (File.Inner_Dev, File.Inner_Dev_Pos, Data, Ret_Count,
                   Success3);
               if Success3 then
                  File.Inner_Dev_Pos := File.Inner_Dev_Pos +
                                        Unsigned_64 (Ret_Count);
                  Returned := Unsigned_64 (Ret_Count);
                  Errno    := Error_No_Error;
                  return;
               else
                  Returned := Unsigned_64'Last;
                  Errno    := Error_IO;
                  return;
               end if;
            when Description_Inode =>
               if not File.Inner_Ino_Read then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
               end if;
               VFS.Read (File.Inner_Ino_FS, File.Inner_Ino, File.Inner_Ino_Pos,
                         Data, Ret_Count, Success1, User);
               File.Inner_Ino_Pos := File.Inner_Ino_Pos +
                                     Unsigned_64 (Ret_Count);
               Translate_Status
                  (Success1, Unsigned_64 (Ret_Count), Returned, Errno);
               return;
            when Description_Reader_FIFO =>
               Read (File.Inner_Reader_FIFO, Data, Ret_Count, Success2);
               Translate_Status (Success2, Unsigned_64 (Ret_Count), Returned,
                                 Errno);
               return;
            when Description_Primary_PTY =>
               IPC.PTY.Read_Primary (File.Inner_Primary_PTY, Data, Ret_Count);
               Returned := Unsigned_64 (Ret_Count);
               Errno    := Error_No_Error;
               return;
            when Description_Secondary_PTY =>
               IPC.PTY.Read_Secondary
                  (File.Inner_Secondary_PTY, Data, Ret_Count);
               Returned := Unsigned_64 (Ret_Count);
               Errno    := Error_No_Error;
            when Description_Writer_FIFO =>
               Errno    := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
               return;
            when Description_Socket =>
               IPC.Socket.Read (File.Inner_Socket, Data, Ret_Count, Success4);
               Translate_Status
                  (Success4, Unsigned_64 (Ret_Count), Returned, Errno);
         end case;
      end;
   end Read;

   procedure Write
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Map       : constant    Page_Table_Acc := Get_Common_Map (Proc);
      File      : constant File_Description_Acc := Get_File (Proc, File_D);
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success2  : IPC.FIFO.Pipe_Status;
      Success3  : Boolean;
      Success4  : IPC.Socket.Socket_Status;
      User      : Unsigned_32;
      Final_Cnt : Natural;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      Process.Get_Effective_UID (Proc, User);

      declare
         Data : Devices.Operation_Data (1 .. Final_Cnt)
            with Import, Address => Buf_SAddr;
      begin
         case File.Description is
            when Description_Device =>
               if not File.Inner_Dev_Write then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
               end if;

               Devices.Write
                  (File.Inner_Dev, File.Inner_Dev_Pos, Data, Ret_Count,
                   Success3);
               if Success3 then
                  File.Inner_Dev_Pos := File.Inner_Dev_Pos +
                                        Unsigned_64 (Ret_Count);
                  Errno := Error_No_Error;
                  Returned := Unsigned_64 (Ret_Count);
               else
                  Errno := Error_IO;
                  Returned := Unsigned_64'Last;
               end if;
            when Description_Inode =>
               if not File.Inner_Ino_Write then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
               elsif File.Inner_Ino_Pos + Unsigned_64 (Final_Cnt) >
                  Unsigned_64 (Get_Limit (Proc, MAC.File_Size_Limit))
               then
                  Errno := Error_File_Too_Big;
                  Returned := Unsigned_64'Last;
                  return;
               end if;

               VFS.Write (File.Inner_Ino_FS, File.Inner_Ino,
                          File.Inner_Ino_Pos, Data, Ret_Count, Success1, User);
               File.Inner_Ino_Pos := File.Inner_Ino_Pos +
                                     Unsigned_64 (Ret_Count);
               Translate_Status (Success1, Unsigned_64 (Ret_Count), Returned,
                                        Errno);
            when Description_Writer_FIFO =>
               Write (File.Inner_Writer_FIFO, Data, Ret_Count, Success2);
               Translate_Status (Success2, Unsigned_64 (Ret_Count), Returned,
                                        Errno);
            when Description_Primary_PTY =>
               IPC.PTY.Write_Primary (File.Inner_Primary_PTY, Data, Ret_Count);
               Errno := Error_No_Error;
               Returned := Unsigned_64 (Ret_Count);
            when Description_Secondary_PTY =>
               IPC.PTY.Write_Secondary
                  (File.Inner_Secondary_PTY, Data, Ret_Count);
               Errno := Error_No_Error;
               Returned := Unsigned_64 (Ret_Count);
            when Description_Reader_FIFO =>
               Errno := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
            when Description_Socket =>
               IPC.Socket.Write (File.Inner_Socket, Data, Ret_Count, Success4);
               Translate_Status
                  (Success4, Unsigned_64 (Ret_Count), Returned, Errno);
         end case;
      end;
   end Write;

   procedure Seek
      (File_D   : Unsigned_64;
       Offset   : Unsigned_64;
       Whence   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc     : constant PID := Arch.Local.Get_Current_Process;
      File     : File_Description_Acc;
      Stat_Val : VFS.File_Stat;
      Success  : VFS.FS_Status;
      User     : Unsigned_32;
      Result   : Unsigned_64;
   begin
      File := Get_File (Proc, File_D);
      if File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      case File.Description is
         when Description_Inode =>
            VFS.Stat (File.Inner_Ino_FS, File.Inner_Ino, Stat_Val, Success,
                      User);
            if Success /= VFS.FS_Success then
               goto Invalid_Seek_Error;
            end if;

            case Whence is
               when SEEK_SET =>
                  File.Inner_Ino_Pos := Offset;
               when SEEK_CURRENT =>
                  File.Inner_Ino_Pos := File.Inner_Ino_Pos + Offset;
               when SEEK_END =>
                  File.Inner_Ino_Pos := Stat_Val.Byte_Size + Offset;
               when others =>
                  goto Invalid_Value_Error;
            end case;

            Result := File.Inner_Ino_Pos;
         when Description_Device =>
            if not Devices.Is_Block_Device (File.Inner_Dev) then
               goto Invalid_Seek_Error;
            end if;

            case Whence is
               when SEEK_SET =>
                  File.Inner_Dev_Pos := Offset;
               when SEEK_CURRENT =>
                  File.Inner_Dev_Pos := File.Inner_Dev_Pos + Offset;
               when SEEK_END =>
                  File.Inner_Dev_Pos :=
                     (Unsigned_64 (Get_Block_Size (File.Inner_Dev)) *
                     Get_Block_Count (File.Inner_Dev)) + Offset;
               when others =>
                  goto Invalid_Value_Error;
            end case;

            Result := File.Inner_Dev_Pos;
         when others =>
            goto Invalid_Seek_Error;
      end case;

      Errno := Error_No_Error;
      Returned := Result;
      return;

   <<Invalid_Seek_Error>>
      Errno := Error_Invalid_Seek;
      Returned := Unsigned_64'Last;
      return;

   <<Invalid_Value_Error>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
      return;
   end Seek;

   procedure Mmap
      (Hint       : Unsigned_64;
       Length     : Unsigned_64;
       Protection : Unsigned_64;
       Flags      : Unsigned_64;
       File_D     : Unsigned_64;
       Offset     : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      pragma Unreferenced (Offset);
      Perms : constant Arch.MMU.Page_Permissions :=
         Get_Mmap_Prot (Protection, Flags);
      Proc       : constant            PID := Arch.Local.Get_Current_Process;
      Map        : constant Page_Table_Acc := Get_Common_Map (Proc);
      Final_Hint : Unsigned_64 := Hint;
      Ignored    : System.Address;
      File       : File_Description_Acc;
      User       : Unsigned_32;
      Success    : Boolean;
   begin
      if not Get_Capabilities (Proc).Can_Modify_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mmap", Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif (Perms.Can_Write and Perms.Can_Execute) or
            (Hint   mod Page_Size /= 0)             or
            (Length mod Page_Size /= 0)
      then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      --  Check that we got a length.
      if Length = 0 then
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      --  Check for our own hint if none was provided.
      if Hint = 0 then
         if (Flags and MAP_FIXED) /= 0 then
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
         else
            Final_Hint := Get_Alloc_Base (Proc);
            Set_Alloc_Base (Proc, Final_Hint + Length);
         end if;
      end if;

      --  Check the address is good.
      if not Check_Userland_Mappability
         (Map, Virtual_Address (Final_Hint), Length)
      then
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      --  Do mmap anon or pass it to the VFS.
      if (Flags and MAP_ANON) /= 0 then
         Map_Allocated_Range
            (Map => Map,
             Virtual_Start  => To_Address (Virtual_Address (Final_Hint)),
             Length         => Storage_Count (Length),
             Permissions    => Perms,
             Physical_Start => Ignored,
             Success        => Success);
         if Success then
            Errno := Error_No_Error;
            Returned := Final_Hint;
            return;
         else
            Errno := Error_No_Memory;
            Returned := Unsigned_64'Last;
            return;
         end if;
      else
         File := Get_File (Proc, File_D);
         Process.Get_Effective_UID (Proc, User);
         if User /= 0 then
            Errno := Error_Bad_Access;
            Returned := Unsigned_64'Last;
            return;
         end if;

         if File.Description = Description_Device then
            if Devices.Mmap
               (Handle      => File.Inner_Dev,
                Address     => Virtual_Address (Final_Hint),
                Length      => Length,
                Flags       => Perms)
            then
               Errno := Error_No_Error;
               Returned := Final_Hint;
               return;
            end if;
         end if;

         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;
   end Mmap;

   procedure Munmap
      (Address    : Unsigned_64;
       Length     : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      Proc : constant            PID := Arch.Local.Get_Current_Process;
      Map  : constant Page_Table_Acc := Get_Common_Map (Proc);
      Addr : constant System.Address := To_Address (Virtual_Address (Address));
   begin
      if not Get_Capabilities (Proc).Can_Modify_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("munmap", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      if Unmap_Range (Map, Addr, Storage_Count (Length)) then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;
   end Munmap;

   procedure Get_PID (Returned : out Unsigned_64; Errno : out Errno_Value) is
   begin
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Convert (Arch.Local.Get_Current_Process));
   end Get_PID;

   procedure Get_PPID (Returned : out Unsigned_64; Errno : out Errno_Value) is
      Parent : constant PID := Get_Parent (Arch.Local.Get_Current_Process);
   begin
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Convert (Parent));
   end Get_PPID;

   procedure Exec
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Th      : constant TID := Arch.Local.Get_Current_Thread;
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      Tmp_Map : Arch.MMU.Page_Table_Acc;
      Success : Boolean;
   begin
      --  Flush our threads and keep the previous map just in case.
      Userland.Process.Flush_Threads (Proc);
      Tmp_Map := Get_Common_Map (Proc);

      Exec_Into_Process
         (Path_Addr => Path_Addr,
          Path_Len  => Path_Len,
          Argv_Addr => Argv_Addr,
          Argv_Len  => Argv_Len,
          Envp_Addr => Envp_Addr,
          Envp_Len  => Envp_Len,
          Proc      => Proc,
          Success   => Success,
          Errno     => Errno);
      if Success then
         --  Free critical state now that we know wont be running.
         Success := Arch.MMU.Make_Active (Get_Common_Map (Proc));
         Userland.Process.Remove_Thread (Proc, Th);
         Arch.MMU.Destroy_Table (Tmp_Map);
         Scheduler.Bail;
      else
         Set_Common_Map (Proc, Tmp_Map);
         Returned := Unsigned_64'Last;
      end if;
   end Exec;

   procedure Clone
      (Callback : Unsigned_64;
       Call_Arg : Unsigned_64;
       Stack    : Unsigned_64;
       Flags    : Unsigned_64;
       TLS_Addr : Unsigned_64;
       Cluster  : Unsigned_64;
       GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (Call_Arg);

      Parent  : PID := Arch.Local.Get_Current_Process;
      Child   : PID;
      New_TID : Scheduler.TID;
      Ret     : Unsigned_64;
      Id      : String (1 .. Process.Max_Name_Length);
      Id_Len  : Natural;
      Success : Boolean;

      Use_Parent : constant Boolean := (Flags and CLONE_PARENT) /= 0;
      Do_Thread  : constant Boolean := (Flags and CLONE_THREAD) /= 0;
   begin
      if not Get_Capabilities (Parent).Can_Spawn_Others then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("clone", Parent);
         Returned := Unsigned_64'Last;
         return;
      elsif Cluster > Unsigned_64 (Natural'Last) then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      if Use_Parent then
         Parent := Get_Parent (Parent);
         if Parent = Error_PID then
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      if Do_Thread then
         Child   := Parent;
         New_TID := Create_User_Thread
            (Address    => Integer_Address (Callback),
             Map        => Get_Common_Map (Child),
             Stack_Addr => Stack,
             TLS_Addr   => TLS_Addr,
             Cluster    => Scheduler.Convert (Natural (Cluster)),
             PID        => Convert (Child));
         Ret := Unsigned_64 (Scheduler.Convert (New_TID));
      else
         Create_Process (Parent, Child);
         if Child = Error_PID then
            goto Block_Error;
         end if;

         Get_Identifier (Parent, Id, Id_Len);
         Set_Identifier (Child, Id (1 .. Id_Len));

         Set_Common_Map (Child, Fork_Table (Get_Common_Map (Parent)));
         if Get_Common_Map (Child) = null then
            goto Block_Error;
         end if;

         Duplicate_FD_Table (Parent, Child);
         New_TID := Scheduler.Create_User_Thread
            (GP_State => GP_State,
             FP_State => FP_State,
             Map      => Get_Common_Map (Child),
             Cluster  => Scheduler.Convert (Natural (Cluster)),
             PID      => Convert (Child),
             TCB      => Arch.Local.Fetch_TCB);
         Ret := Unsigned_64 (Convert (Child));
      end if;

      if New_TID = Error_TID then
         goto Block_Error;
      end if;
      Add_Thread (Child, New_TID, Success);
      if not Success then
         goto Block_Error;
      end if;

      Errno := Error_No_Error;
      Returned := Ret;
      return;

   <<Block_Error>>
      Errno    := Error_Would_Block;
      Returned := Unsigned_64'Last;
   end Clone;

   procedure Wait
      (Waited_PID, Exit_Addr, Options : Unsigned_64;
       Returned                       : out Unsigned_64;
       Errno                          : out Errno_Value)
   is
      Addr       : constant Integer_Address := Integer_Address (Exit_Addr);
      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      Dont_Hang  : constant         Boolean := (Options and WNOHANG) /= 0;
      Map        :           Page_Table_Acc := Get_Common_Map (Proc);
      Waited     : PID;
      Children   : Process.Children_Arr (1 .. 25);
      Count      : Natural;
      Did_Exit   : Boolean;
      Error_Code : Unsigned_8;
      Exit_Value : Unsigned_32 with Address => To_Address (Addr), Import;
   begin
      --  If -1, we have to wait for any of the children, else, wait for the
      --  passed PID.
      if Waited_PID = Unsigned_64 (Unsigned_32'Last) then
         Process.Get_Children (Proc, Children, Count);
         if Count = 0 then
            Errno    := Error_Child;
            Returned := Unsigned_64'Last;
            return;
         end if;

         loop
            for PID_Item of Children (1 .. Count) loop
               Waited := PID_Item;
               if Waited /= Error_PID then
                  Check_Exit (Waited, Did_Exit, Error_Code);
                  if Did_Exit then
                     goto Waited_Exited;
                  end if;
               end if;
            end loop;

            exit when Dont_Hang;
            Scheduler.Yield_If_Able;
         end loop;
      else
         Waited := Userland.Process.Convert (Natural (Waited_PID));
         if Get_Parent (Waited) /= Proc then
            Errno    := Error_Child;
            Returned := Unsigned_64'Last;
            return;
         end if;

         if Waited /= Error_PID then
            loop
               Check_Exit (Waited, Did_Exit, Error_Code);
               if Did_Exit then
                  goto Waited_Exited;
               end if;
               exit when Dont_Hang;
               Scheduler.Yield_If_Able;
            end loop;
         end if;
      end if;

      --  If we get here, it means we are not blocking, and that the
      --  process has not exited, so lets return what we have to.
      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Waited_Exited>>
      --  Set the return value if we are to.
      if Exit_Value'Address /= System.Null_Address then
         if not Check_Userland_Access (Map, Addr, 4) then
            Errno    := Error_Would_Fault;
            Returned := Unsigned_64'Last;
            return;
         end if;
         Exit_Value := Wait_EXITED or Unsigned_32 (Error_Code);
      end if;

      --  Now that we got the exit code, finally allow the process to die.
      Map := Get_Common_Map (Waited);
      Arch.MMU.Destroy_Table          (Map);
      Userland.Process.Delete_Process (Waited);

      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Convert (Waited));
   end Wait;

   procedure Socket
      (Domain   : Unsigned_64;
       DataType : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Success    : Boolean;
      Desc       : File_Description_Acc;
      New_Sock   : IPC.Socket.Socket_Acc;
      Dom        : IPC.Socket.Domain;
      Data       : IPC.Socket.DataType;
   begin
      case Domain is
         when AF_INET  => Dom := IPC.Socket.IPv4;
         when AF_INET6 => Dom := IPC.Socket.IPv6;
         when AF_UNIX  => Dom := IPC.Socket.UNIX;
         when others   => goto Invalid_Value_Return;
      end case;

      case DataType and 16#FFF# is
         when SOCK_STREAM => Data := IPC.Socket.Stream;
         when SOCK_DGRAM  => Data := IPC.Socket.Datagram;
         when SOCK_RAW    => Data := IPC.Socket.Raw;
         when others      => goto Invalid_Value_Return;
      end case;

      New_Sock := Create (Dom, Data, (DataType and SOCK_NONBLOCK) = 0);
      if New_Sock = null then
         goto Invalid_Value_Return;
      end if;

      Desc := new File_Description'(Description_Socket, 0, New_Sock);
      Check_Add_File (Proc, Desc, Success, Natural (Returned));
      if Success then
         Set_Close_On_Exec (Proc, Returned, (DataType and SOCK_CLOEXEC) /= 0);
         Errno := Error_No_Error;
      else
         Close (New_Sock);
         Close (Desc);
         Errno    := Error_Too_Many_Files;
         Returned := Unsigned_64'Last;
      end if;

      return;

   <<Invalid_Value_Return>>
      Errno    := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   end Socket;

   procedure Set_Hostname
      (Address  : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      Map     : constant     Page_Table_Acc := Get_Common_Map (Proc);
      Len     : constant          Natural := Natural (Length);
      IAddr   : constant  Integer_Address := Integer_Address (Address);
      SAddr   : constant   System.Address := To_Address (IAddr);
      Name    : String (1 .. Len) with Import, Address => SAddr;
      Success : Boolean;
   begin
      if not Get_Capabilities (Proc).Can_Manage_Networking then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_hostname", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      if not Check_Userland_Access (Map, IAddr, Length) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Networking.Set_Hostname (Name, Success);

      if not Success then
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         Returned := 0;
      end if;
   end Set_Hostname;

   procedure FStat
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Stat_Addr : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Table_Acc := Get_Common_Map (Proc);
      Stat_IAddr : constant  Integer_Address := Integer_Address (Stat_Addr);
      Stat_SAddr : constant   System.Address := To_Address (Stat_IAddr);
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant   System.Address := To_Address (Path_IAddr);
      File_Desc  : File_Description_Acc;
      Stat_Val   : VFS.File_Stat;
      ID         : Natural;
      FS         : VFS.FS_Handle;
      D_Ino, Ino : VFS.File_Inode_Number;
      Success    : VFS.FS_Status;
      Stat_Buf   : Stat with Import, Address => Stat_SAddr;
      User       : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Stat_IAddr, Stat'Size / 8) or else
         not Check_Userland_Access (Map, Path_IAddr, Path_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      if (Flags and AT_EMPTY_PATH) /= 0 then
         File_Desc := Get_File (Proc, Dir_FD);
         if File_Desc = null then
            Errno := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;

         case File_Desc.Description is
            when Description_Inode =>
               FS  := File_Desc.Inner_Ino_FS;
               Ino := File_Desc.Inner_Ino;
            when Description_Device =>
               ID := Devices.Get_Unique_ID (File_Desc.Inner_Dev);
               Stat_Buf :=
                  (Device_Number => Unsigned_64 (ID),
                   Inode_Number  => Unsigned_64 (ID),
                   Mode          => 8#644#,
                   Number_Links  => 1,
                   UID           => 0,
                   GID           => 0,
                   Inner_Device  => Unsigned_64 (ID),
                   File_Size     => 512,
                   Access_Time   => (0, 0),
                   Modify_Time   => (0, 0),
                   Create_Time   => (0, 0),
                   Block_Size    =>
                     Unsigned_64 (Get_Block_Size (File_Desc.Inner_Dev)),
                   Block_Count   => Get_Block_Count (File_Desc.Inner_Dev));

               --  Set the access part of mode.
               if Devices.Is_Block_Device (File_Desc.Inner_Dev) then
                  Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFBLK;
               else
                  Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFCHR;
               end if;
               goto Success_Return;
            when Description_Reader_FIFO | Description_Writer_FIFO |
                 Description_Primary_PTY | Description_Secondary_PTY =>
               Stat_Buf :=
                  (Device_Number => 0,
                   Inode_Number  => 1,
                   Mode          => Stat_IFIFO,
                   Number_Links  => 1,
                   UID           => 0,
                   GID           => 0,
                   Inner_Device  => 1,
                   File_Size     => 512,
                   Access_Time   => (Seconds => 0, Nanoseconds => 0),
                   Modify_Time   => (Seconds => 0, Nanoseconds => 0),
                   Create_Time   => (Seconds => 0, Nanoseconds => 0),
                   Block_Size    => 512,
                   Block_Count   => 1);
               goto Success_Return;
            when Description_Socket =>
               Stat_Buf :=
                  (Device_Number => 0,
                   Inode_Number  => 1,
                   Mode          => Stat_ISOCK,
                   Number_Links  => 1,
                   UID           => 0,
                   GID           => 0,
                   Inner_Device  => 1,
                   File_Size     => 512,
                   Access_Time   => (Seconds => 0, Nanoseconds => 0),
                   Modify_Time   => (Seconds => 0, Nanoseconds => 0),
                   Create_Time   => (Seconds => 0, Nanoseconds => 0),
                   Block_Size    => 512,
                   Block_Count   => 1);
               goto Success_Return;
         end case;
      else
         declare
            Path : String (1 .. Natural (Path_Len))
               with Import, Address => Path_SAddr;
         begin
            Resolve_AT_Directive (Proc, Dir_FD, FS, D_Ino);
            if FS = VFS.Error_Handle then
               Errno    := Error_Bad_File;
               Returned := Unsigned_64'Last;
               return;
            end if;

            VFS.Open
               (Key       => FS,
                Relative  => D_Ino,
                Path      => Path,
                Ino       => Ino,
                Success   => Success,
                User      => User,
                Do_Follow => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
            if Success /= VFS.FS_Success then
               Errno    := Error_No_Entity;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end;
      end if;

      VFS.Stat (FS, Ino, Stat_Val, Success, User);
      if Success /= VFS.FS_Success then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      ID := Get_Unique_ID (Get_Backing_Device (FS));
      Stat_Buf :=
         (Device_Number => Unsigned_64 (ID),
          Inode_Number  => Unsigned_64 (Stat_Val.Unique_Identifier),
          Mode          => Unsigned_32 (Stat_Val.Mode),
          Number_Links  => Unsigned_32 (Stat_Val.Hard_Link_Count),
          UID           => Stat_Val.UID,
          GID           => Stat_Val.GID,
          Inner_Device  => Unsigned_64 (ID),
          File_Size     => Stat_Val.Byte_Size,
          Access_Time   =>
             (Stat_Val.Access_Time.Seconds_Since_Epoch,
              Stat_Val.Access_Time.Additional_Nanoseconds),
          Modify_Time   =>
             (Stat_Val.Modification_Time.Seconds_Since_Epoch,
              Stat_Val.Modification_Time.Additional_Nanoseconds),
          Create_Time   =>
             (Stat_Val.Creation_Time.Seconds_Since_Epoch,
              Stat_Val.Creation_Time.Additional_Nanoseconds),
          Block_Size    => Unsigned_64 (Stat_Val.IO_Block_Size),
          Block_Count   => Stat_Val.IO_Block_Count);

      --  Set the access part of mode.
      case Stat_Val.Type_Of_File is
         when VFS.File_Regular =>
            Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFREG;
         when VFS.File_Directory =>
            Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFDIR;
         when VFS.File_Symbolic_Link =>
            Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFLNK;
         when VFS.File_Character_Device =>
            Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFCHR;
         when VFS.File_Block_Device =>
            Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFBLK;
      end case;

   <<Success_Return>>
      Errno    := Error_No_Error;
      Returned := 0;
   end FStat;

   procedure Chdir
      (FD       : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant                  PID := Arch.Local.Get_Current_Process;
      Desc : constant File_Description_Acc := Get_File (Proc, FD);
      St   : VFS.File_Stat;
      Succ : FS_Status;
      User : Unsigned_32;
   begin
      if Desc = null or else Desc.Description /= Description_Inode then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      VFS.Stat (Desc.Inner_Ino_FS, Desc.Inner_Ino, St, Succ, User);
      if Succ /= VFS.FS_Success or else St.Type_Of_File /= File_Directory then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Set_CWD (Proc, Desc.Inner_Ino_FS, Desc.Inner_Ino);
      Errno    := Error_No_Error;
      Returned := 0;
   end Chdir;

   procedure IOCTL
      (FD       : Unsigned_64;
       Request  : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      I_Arg : constant      Integer_Address := Integer_Address (Argument);
      S_Arg : constant       System.Address := To_Address (I_Arg);
      Proc  : constant     PID := Arch.Local.Get_Current_Process;
      Map   : constant         Page_Table_Acc := Get_Common_Map (Proc);
      File  : constant File_Description_Acc := Get_File (Proc, FD);
      Succ  : Boolean;
      FSSuc : VFS.FS_Status;
      User  : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, I_Arg, 8) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif File = null then
         Errno := Error_Not_A_TTY;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      case File.Description is
         when Description_Inode =>
            if File.Inner_Ino_Read and File.Inner_Ino_Write then
               VFS.IO_Control (File.Inner_Ino_FS, File.Inner_Ino,
                               Request, S_Arg, User, FSSuc);
               Succ := FSSuc = VFS.FS_Success;
            else
               Succ := False;
            end if;
         when Description_Device =>
            if File.Inner_Dev_Read and File.Inner_Dev_Write then
               Succ := IO_Control (File.Inner_Dev, Request, S_Arg);
            else
               Succ := False;
            end if;
         when Description_Primary_PTY =>
            PTY_IOCTL (File.Inner_Primary_PTY, Request, S_Arg, Succ);
         when Description_Secondary_PTY =>
            PTY_IOCTL (File.Inner_Secondary_PTY, Request, S_Arg, Succ);
         when others =>
            Succ := False;
      end case;

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_Not_A_TTY;
         Returned := Unsigned_64'Last;
      end if;
   end IOCTL;

   procedure Sched_Yield (Returned : out Unsigned_64; Errno : out Errno_Value)
   is
   begin
      Scheduler.Yield;
      Returned := 0;
      Errno    := Error_No_Error;
   end Sched_Yield;

   procedure Delete_Thread_Cluster
      (Cluster  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
   begin
      if not Get_Capabilities (Proc).Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("delete_thread_cluster", Proc);
         Returned := Unsigned_64'Last;
      elsif Cluster > Unsigned_64 (Natural'Last) or else
         not Scheduler.Delete_Cluster (Convert (Natural (Cluster)))
      then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
      else
         Errno    := Error_No_Error;
         Returned := 0;
      end if;
   end Delete_Thread_Cluster;

   procedure Pipe
      (Result_Addr : Unsigned_64;
       Flags       : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      Ad   : constant Integer_Address  := Integer_Address (Result_Addr);
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Map  : constant Page_Table_Acc     := Get_Common_Map (Proc);
      Res  : array (1 .. 2) of Integer with Import, Address => To_Address (Ad);
      Returned2 : IPC.FIFO.Inner_Acc;
      Succ1, Succ2 : Boolean;
      Reader_Desc, Writer_Desc : File_Description_Acc;
   begin
      if not Check_Userland_Access (Map, Ad, Res'Size / 8) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Returned2 := IPC.FIFO.Create ((Flags and O_NONBLOCK) = 0);
      Reader_Desc := new File_Description'(
         Children_Count    => 0,
         Description       => Description_Reader_FIFO,
         Inner_Reader_FIFO => Returned2
      );
      Writer_Desc := new File_Description'(
         Children_Count    => 0,
         Description       => Description_Writer_FIFO,
         Inner_Writer_FIFO => Returned2
      );
      Check_Add_File (Proc, Reader_Desc, Succ1, Res (1));
      Check_Add_File (Proc, Writer_Desc, Succ2, Res (2));
      if not Succ1 or not Succ2 then
         Close (Returned2);
         Close (Reader_Desc);
         Close (Writer_Desc);
         Errno := Error_Too_Many_Files;
         Returned := Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         Returned := 0;
      end if;
   end Pipe;

   procedure Get_UID (Returned : out Unsigned_64; Errno : out Errno_Value) is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Ret  : Unsigned_32;
   begin
      Userland.Process.Get_UID (Proc, Ret);
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Ret);
   end Get_UID;

   procedure Rename
      (Source_FD   : Unsigned_64;
       Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Target_FD   : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       Flags       : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      Proc      : constant              PID := Arch.Local.Get_Current_Process;
      Map       : constant   Page_Table_Acc := Get_Common_Map (Proc);
      Src_IAddr : constant  Integer_Address := Integer_Address (Source_Addr);
      Src_SAddr : constant   System.Address := To_Address (Src_IAddr);
      Tgt_IAddr : constant  Integer_Address := Integer_Address (Target_Addr);
      Tgt_SAddr : constant   System.Address := To_Address (Tgt_IAddr);
      Do_Keep   : constant Boolean := (Flags and RENAME_NOREPLACE) /= 0;
      Src_FS, Tgt_FS   : VFS.FS_Handle;
      Src_Ino, Tgt_Ino : VFS.File_Inode_Number;
      Success          : VFS.FS_Status;
      User             : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Src_IAddr, Source_Len) or
         not Check_Userland_Access (Map, Tgt_IAddr, Target_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Source_Len > Unsigned_64 (Natural'Last) or
            Target_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Src : String (1 .. Natural (Source_Len))
            with Import, Address => Src_SAddr;
         Tgt : String (1 .. Natural (Target_Len))
            with Import, Address => Tgt_SAddr;
      begin
         Userland.Process.Get_Effective_UID (Proc, User);
         Resolve_AT_Directive (Proc, Source_FD, Src_FS, Src_Ino);
         Resolve_AT_Directive (Proc, Target_FD, Tgt_FS, Tgt_Ino);
         if Src_FS = VFS.Error_Handle or else
            Tgt_FS = VFS.Error_Handle or else
            Src_FS /= Tgt_FS
         then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;

         Rename (Src_FS, Src_Ino, Src, Tgt_Ino, Tgt, Do_Keep, User, Success);
         Translate_Status (Success, 0, Returned, Errno);
      end;
   end Rename;

   procedure Sysconf
      (Request  : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc   : constant             PID := Arch.Local.Get_Current_Process;
      Map    : constant  Page_Table_Acc := Get_Common_Map (Proc);
      IAddr  : constant Integer_Address := Integer_Address (Addr);
      SAddr  : constant  System.Address := To_Address (IAddr);
      Stats  : Memory.Physical.Statistics;
      Result : Unsigned_64;
   begin
      --  Simple request that do not use the memory argument.
      case Request is
         when SC_PAGESIZE =>
            Result := Page_Size;
         when SC_OPEN_MAX =>
            Result := Unsigned_64 (Process.Max_File_Count);
         when SC_HOST_NAME_MAX =>
            Result := Unsigned_64 (Networking.Hostname_Max_Len);
         when SC_AVPHYS_PAGES =>
            Memory.Physical.Get_Statistics (Stats);
            Result := Unsigned_64 (Stats.Free) / Page_Size;
         when SC_PHYS_PAGES =>
            Memory.Physical.Get_Statistics (Stats);
            Result := Unsigned_64 (Stats.Available) / Page_Size;
         when SC_NPROC_ONLN =>
            Result := Unsigned_64 (Arch.Hooks.Get_Active_Core_Count);
         when SC_TOTAL_PAGES =>
            Memory.Physical.Get_Statistics (Stats);
            Result := Unsigned_64 (Stats.Total) / Page_Size;
         when SC_CHILD_MAX =>
            Result := Unsigned_64 (Process.Max_Process_Count);
         when others =>
            goto Not_Matched;
      end case;

      goto Success_Return;

   <<Not_Matched>>
      --  Requests that use the memory argument, for common checking.
      if not Check_Userland_Access (Map, IAddr, Length) then
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Request is
         when SC_LIST_PROCS =>
            declare
               Len   : constant Natural :=
                  Natural (Length / (Process_Info'Size / 8));
               Procs : Proc_Info_Arr (1 .. Len) with Import, Address => SAddr;
               KProc : Process_Info_Arr (1 .. Len);
               Ret   : Natural;
            begin
               List_All (KProc, Ret);
               for I in 1 .. Ret loop
                  Procs (I) :=
                     (Identifier  => KProc (I).Identifier,
                      Id_Len      => Unsigned_16 (KProc (I).Identifier_Len),
                      Parent_PID  => Unsigned_16 (Convert (KProc (I).Parent)),
                      Process_PID => Unsigned_16 (Convert (KProc (I).Process)),
                      UID         => KProc (I).User,
                      Flags       => 0);
                  if KProc (I).Is_Being_Traced then
                     Procs (I).Flags := Procs (I).Flags or PROC_IS_TRACED;
                  end if;
                  if KProc (I).Has_Exited then
                     Procs (I).Flags := Procs (I).Flags or PROC_EXITED;
                  end if;
               end loop;

               Result := Unsigned_64 (Ret);
            end;
         when SC_LIST_MOUNTS =>
            declare
               Len   : constant Natural :=
                  Natural (Length / (Mount_Info'Size / 8));
               Mnts  : Mount_Info_Arr (1 .. Len) with Import, Address => SAddr;
               KMnts : Mountpoint_Info_Arr (1 .. Len);
               Ret   : Natural;
            begin
               List_All (KMnts, Ret);
               for I in 1 .. Ret loop
                  Mnts (I) :=
                     (FS_Type      => 0,
                      Flags        => 0,
                      Source       => KMnts (I).Source,
                      Source_Len   => Unsigned_32 (KMnts (I).Source_Len),
                      Location     => KMnts (I).Location,
                      Location_Len => Unsigned_32 (KMnts (I).Location_Len));
                  case KMnts (I).Inner_Type is
                     when FS_EXT => Mnts (I).FS_Type := MNT_EXT;
                     when FS_FAT => Mnts (I).FS_Type := MNT_FAT;
                     when FS_QNX => Mnts (I).FS_Type := MNT_QNX;
                  end case;
               end loop;

               Result := Unsigned_64 (Ret);
            end;
         when SC_UNAME =>
            declare
               UTS      : UTS_Name with Import, Address => SAddr;
               Host_Len : Natural;
            begin
               Networking.Get_Hostname (UTS.Node_Name, Host_Len);
               UTS.Node_Name (Host_Len + 1) := Ada.Characters.Latin_1.NUL;

               UTS.System_Name (1 .. Config.Name'Length + 1) :=
                  Config.Name & Ada.Characters.Latin_1.NUL;
               UTS.Release (1 .. Config.Version'Length + 1) :=
                  Config.Version & Ada.Characters.Latin_1.NUL;
               UTS.Version (1 .. Config.Version'Length + 1) :=
                  Config.Version & Ada.Characters.Latin_1.NUL;
               UTS.Machine (1 .. Config.Arch_Name'Length + 1) :=
                  Config.Arch_Name & Ada.Characters.Latin_1.NUL;

               Result := 0;
            end;
         when SC_LIST_THREADS =>
            declare
               Len   : constant Natural :=
                  Natural (Length / (Thread_Info'Size / 8));
               Ths  : Thread_Info_Arr (1 .. Len) with Import, Address => SAddr;
               KThs : Scheduler.Thread_Listing_Arr (1 .. Len);
               Ret  : Natural;
            begin
               List_All (KThs, Ret);
               for I in 1 .. Ret loop
                  Ths (I) :=
                     (Thread_Id   => Unsigned_16 (Convert (KThs (I).Thread)),
                      Cluster_Id  => Unsigned_16 (Convert (KThs (I).Cluster)),
                      Process_PID => Unsigned_16 (KThs (I).Proc));
               end loop;

               Result := Unsigned_64 (Ret);
            end;
         when SC_LIST_CLUSTERS =>
            declare
               Len   : constant Natural :=
                  Natural (Length / (Thread_Info'Size / 8));
               Ths : Cluster_Info_Arr (1 .. Len) with Import, Address => SAddr;
               KThs : Scheduler.Cluster_Listing_Arr (1 .. Len);
               Ret  : Natural;
            begin
               List_All (KThs, Ret);
               for I in 1 .. Ret loop
                  Ths (I) :=
                     (Cluster_Id => Unsigned_16 (Convert (KThs (I).Cluster)),
                      Cluster_Fl => 0,
                      Cluster_Q  => Unsigned_16 (KThs (I).Cluster_Quan));
                  case KThs (I).Cluster_Algo is
                     when Cluster_RR =>
                        Ths (I).Cluster_Fl := SCHED_RR;
                     when Cluster_Cooperative =>
                        Ths (I).Cluster_Fl := SCHED_COOP;
                  end case;
                  if KThs (I).Cluster_Int then
                     Ths (I).Cluster_Fl := Ths (I).Cluster_Fl or SCHED_INTR;
                  end if;
               end loop;

               Result := Unsigned_64 (Ret);
            end;
         when SC_LIST_NETINTER =>
            declare
               Len   : constant Natural :=
                  Natural (Length / (Interface_Info'Size / 8));
               Ths  : Interface_Arr (1 .. Len) with Import, Address => SAddr;
               KThs : Networking.Interface_Arr (1 .. Len);
               Ret  : Natural;
               NLen : Natural;
            begin
               Networking.List_Interfaces (KThs, Ret);
               for I in 1 .. Ret loop
                  Fetch_Name (KThs (I).Handle, Ths (I).Name (1 .. 64), NLen);
                  Ths (I).Name (NLen + 1) := Ada.Characters.Latin_1.NUL;
                  if KThs (I).Is_Blocked then
                     Ths (I).Flags := NETINTR_BLOCKED;
                  else
                     Ths (I).Flags := 0;
                  end if;
                  Ths (I).MAC := KThs (I).MAC;
                  Ths (I).IPv4 := KThs (I).IPv4;
                  Ths (I).IPv4_Subnet := KThs (I).IPv4_Subnet;
                  Ths (I).IPv6 := KThs (I).IPv6;
                  Ths (I).IPv6_Subnet := KThs (I).IPv6_Subnet;
               end loop;

               Result := Unsigned_64 (Ret);
            end;
         when others =>
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

   <<Success_Return>>
      Errno    := Error_No_Error;
      Returned := Result;
   end Sysconf;
   procedure Spawn
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Caps_Addr : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      Success : Boolean;
      Child   : PID;
   begin
      Create_Process (Proc, Child);
      if Child = Error_PID then
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Duplicate_Standard_FDs (Proc, Child);

      Exec_Into_Process
         (Path_Addr => Path_Addr,
          Path_Len  => Path_Len,
          Argv_Addr => Argv_Addr,
          Argv_Len  => Argv_Len,
          Envp_Addr => Envp_Addr,
          Envp_Len  => Envp_Len,
          Proc      => Child,
          Success   => Success,
          Errno     => Errno);
      if not Success then
         Errno    := Error_Bad_Access;
         Returned := Unsigned_64'Last;
         return;
      end if;

      if Caps_Addr /= 0 then
         declare
            Map    : constant  Page_Table_Acc := Get_Common_Map (Proc);
            CIAddr : constant Integer_Address := Integer_Address (Caps_Addr);
            Caps   : Unsigned_64 with Import, Address => To_Address (CIAddr);
         begin
            if Check_Userland_Access (Map, CIAddr, Unsigned_64'Size / 8) then
               Set_MAC_Capabilities (Child, Caps);
            else
               Errno    := Error_Would_Fault;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end;
      end if;

      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Convert (Child));
   end Spawn;

   procedure Get_TID (Returned : out Unsigned_64; Errno : out Errno_Value) is
   begin
      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Convert (Arch.Local.Get_Current_Thread));
   end Get_TID;

   procedure Manage_Thread_Cluster
      (Cluster    : Unsigned_64;
       Flags      : Unsigned_64;
       Quantum    : Unsigned_64;
       Percentage : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Clsr : Scheduler.TCID;
      Algo : Scheduler.Cluster_Algorithm;
   begin
      if not Get_Capabilities (Proc).Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("manage_thread_cluster", Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif Cluster > Unsigned_64 (Natural'Last) then
         goto Invalid_Value_Error;
      end if;

      if (Flags and SCHED_RR) /= 0 then
         Algo := Scheduler.Cluster_RR;
      elsif (Flags and SCHED_COOP) /= 0 then
         Algo := Scheduler.Cluster_Cooperative;
      else
         goto Invalid_Value_Error;
      end if;

      if Cluster = 0 then
         Clsr := Scheduler.Create_Cluster;
      else
         Clsr := Scheduler.Convert (Natural (Cluster));
      end if;

      if Clsr = Error_TCID then
         goto Invalid_Value_Error;
      end if;

      if not Set_Scheduling_Algorithm
         (Clsr, Algo, Natural (Quantum), (Flags and SCHED_INTR) /= 0)
      then
         goto Invalid_Value_Error;
      end if;
      if not Set_Time_Slice (Clsr, Natural (Percentage)) then
         goto Invalid_Value_Error;
      end if;

      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Scheduler.Convert (Clsr));
      return;

   <<Invalid_Value_Error>>
      Errno    := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   end Manage_Thread_Cluster;

   procedure Fcntl
      (FD       : Unsigned_64;
       Command  : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      File      : constant File_Description_Acc := Get_File (Proc, FD);
      Temp      : Boolean;
      New_File  : File_Description_Acc;
      Result_FD : Natural;
   begin
      if File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Returned := 0;

      case Command is
         when F_DUPFD | F_DUPFD_CLOEXEC =>
            Duplicate (File, New_File);
            Check_Add_File
               (Proc, New_File, Temp, Result_FD, Natural (Argument));
            if Temp then
               Returned               := Unsigned_64 (Result_FD);
               Process.Set_Close_On_Exec (Proc, Unsigned_64 (Result_FD),
                                          Command = F_DUPFD_CLOEXEC);
            else
               Errno := Error_Too_Many_Files;
               Returned := Unsigned_64'Last;
               return;
            end if;
         when F_GETFD =>
            if Get_Close_On_Exec (Proc, FD) then
               Returned := FD_CLOEXEC;
            end if;
         when F_SETFD =>
            Process.Set_Close_On_Exec (Proc, FD,
               (Argument and FD_CLOEXEC) /= 0);
         when F_GETFL =>
            case File.Description is
               when Description_Reader_FIFO =>
                  if Is_Read_Blocking (File.Inner_Reader_FIFO) then
                     Returned := O_NONBLOCK;
                  end if;
               when Description_Writer_FIFO =>
                  if Is_Write_Blocking (File.Inner_Writer_FIFO) then
                     Returned := O_NONBLOCK;
                  end if;
               when Description_Socket =>
                  if Is_Blocking (File.Inner_Socket) then
                     Returned := O_NONBLOCK;
                  end if;
               when Description_Primary_PTY | Description_Secondary_PTY |
                    Description_Device      | Description_Inode =>
                  null;
            end case;
         when F_SETFL =>
            case File.Description is
               when Description_Reader_FIFO =>
                  Set_Read_Blocking
                     (File.Inner_Reader_FIFO, (Argument and O_NONBLOCK) /= 0);
               when Description_Writer_FIFO =>
                  Set_Write_Blocking
                     (File.Inner_Reader_FIFO, (Argument and O_NONBLOCK) /= 0);
               when Description_Socket =>
                  Set_Blocking
                     (File.Inner_Socket, (Argument and O_NONBLOCK) /= 0);
               when Description_Primary_PTY | Description_Secondary_PTY |
                    Description_Device      | Description_Inode =>
                  null;
            end case;
         when F_GETPIPE_SZ =>
            case File.Description is
               when Description_Reader_FIFO =>
                  Get_Size (File.Inner_Reader_FIFO, Natural (Returned));
               when Description_Writer_FIFO =>
                  Get_Size (File.Inner_Writer_FIFO, Natural (Returned));
               when others =>
                  goto Invalid_Return;
            end case;
         when F_SETPIPE_SZ =>
            case File.Description is
               when Description_Reader_FIFO =>
                  Set_Size (File.Inner_Reader_FIFO, Natural (Argument), Temp);
                  if not Temp then
                     Errno := Error_Would_Block;
                     Returned := Unsigned_64'Last;
                     return;
                  end if;
               when Description_Writer_FIFO =>
                  Set_Size (File.Inner_Writer_FIFO, Natural (Argument), Temp);
                  if not Temp then
                     Errno := Error_Would_Block;
                     Returned := Unsigned_64'Last;
                     return;
                  end if;
               when others =>
                  goto Invalid_Return;
            end case;
         when others =>
            goto Invalid_Return;
      end case;

      Errno := Error_No_Error;
      return;

   <<Invalid_Return>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   end Fcntl;

   procedure Exit_Thread (Returned : out Unsigned_64; Errno : out Errno_Value)
   is
   begin
      Errno := Error_No_Error;
      Returned := 0;
      Userland.Process.Remove_Thread
         (Arch.Local.Get_Current_Process, Arch.Local.Get_Current_Thread);
      Scheduler.Bail;
   end Exit_Thread;

   procedure Get_Random
     (Address  : Unsigned_64;
      Length   : Unsigned_64;
      Returned : out Unsigned_64;
      Errno    : out Errno_Value)
   is
      Proc   : constant              PID := Arch.Local.Get_Current_Process;
      Map    : constant Page_Table_Acc     := Get_Common_Map (Proc);
      IAddr  : constant  Integer_Address := Integer_Address (Address);
      SAddr  : constant   System.Address := To_Address (IAddr);
      Result : Cryptography.Random.Crypto_Data (1 .. Natural (Length))
         with Import, Address => SAddr;
   begin
      if not Get_Capabilities (Proc).Can_Access_Entropy then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("getrandom", Proc);
         Returned := Unsigned_64'Last;
      elsif not Check_Userland_Access (Map, IAddr, Length) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      else
         Cryptography.Random.Fill_Data (Result);
         Errno := Error_No_Error;
         Returned := Result'Length * 4;
      end if;
   end Get_Random;

   procedure MProtect
     (Address    : Unsigned_64;
      Length     : Unsigned_64;
      Protection : Unsigned_64;
      Returned   : out Unsigned_64;
      Errno      : out Errno_Value)
   is
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map   : constant Page_Table_Acc     := Get_Common_Map (Proc);
      Flags : constant Arch.MMU.Page_Permissions :=
         Get_Mmap_Prot (Protection, 0);
      Addr : constant System.Address := To_Address (Integer_Address (Address));
   begin
      if not Get_Capabilities (Proc).Can_Modify_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mprotect", Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif (Flags.Can_Write and Flags.Can_Execute) or
            (Address mod Page_Size /= 0)            or
            (Length  mod Page_Size /= 0)
      then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      if not Remap_Range (Map, Addr, Storage_Count (Length), Flags) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      else
         Errno := Error_No_Error;
         Returned := 0;
      end if;
   end MProtect;

   procedure Set_MAC_Capabilities
      (Bits     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
   begin
      Set_MAC_Capabilities (Arch.Local.Get_Current_Process, Bits);
      Errno := Error_No_Error;
      Returned := 0;
   end Set_MAC_Capabilities;

   procedure Get_MAC_Capabilities
      (Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Pro  : constant              PID := Arch.Local.Get_Current_Process;
      Caps : constant MAC.Capabilities := Get_Capabilities (Pro);
      Res  :               Unsigned_64 := 0;
   begin
      if Caps.Can_Change_Scheduling then Res := Res or MAC_CAP_SCHED;   end if;
      if Caps.Can_Spawn_Others      then Res := Res or MAC_CAP_SPAWN;   end if;
      if Caps.Can_Access_Entropy    then Res := Res or MAC_CAP_ENTROPY; end if;
      if Caps.Can_Modify_Memory     then Res := Res or MAC_CAP_SYS_MEM; end if;
      if Caps.Can_Use_Networking    then Res := Res or MAC_CAP_USE_NET; end if;
      if Caps.Can_Manage_Networking then Res := Res or MAC_CAP_SYS_NET; end if;
      if Caps.Can_Manage_Mounts     then Res := Res or MAC_CAP_SYS_MNT; end if;
      if Caps.Can_Manage_Power      then Res := Res or MAC_CAP_SYS_PWR; end if;
      if Caps.Can_Trace_Children    then Res := Res or MAC_CAP_PTRACE;  end if;
      if Caps.Can_Change_UIDs       then Res := Res or MAC_CAP_SETUID;  end if;
      if Caps.Can_Manage_MAC        then Res := Res or MAC_CAP_SYS_MAC; end if;

      Errno := Error_No_Error;
      Returned := Res;
   end Get_MAC_Capabilities;

   procedure Add_MAC_Permissions
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc   : constant             PID := Arch.Local.Get_Current_Process;
      Map    : constant    Page_Table_Acc := Get_Common_Map (Proc);
      Addr   : constant Integer_Address := Integer_Address (Path_Addr);
      Perms  : MAC.Permissions;
      Status : MAC.Addition_Status;
      FS_Status : VFS.FS_Status;
      FS     : VFS.FS_Handle;
      Ino    : VFS.File_Inode_Number;
      Dev    : Devices.Device_Handle;
      Path   : String (1 .. Natural (Path_Len))
         with Import, Address => To_Address (Addr);
   begin
      if not Get_Capabilities (Proc).Can_Manage_MAC then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("add_mac_perms", Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif not Check_Userland_Access (Map, Addr, Path_Len) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Perms :=
         (Includes_Contents => (Flags and MAC_PERM_CONTENTS) /= 0,
          Can_Read          => (Flags and MAC_PERM_READ)     /= 0,
          Can_Write         => (Flags and MAC_PERM_WRITE)    /= 0,
          Can_Execute       => (Flags and MAC_PERM_EXEC)     /= 0,
          Can_Append_Only   => (Flags and MAC_PERM_APPEND)   /= 0,
          Can_Lock_Files    => (Flags and MAC_PERM_FLOCK)    /= 0);

      if (Flags and MAC_PERM_DEV) /= 0 then
         Dev := Devices.Fetch (Path);
         if Dev = Devices.Error_Handle then
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
         end if;
         Add_Entity
            (Proc   => Proc,
             Dev    => Devices.Fetch (Path),
             Perms  => Perms,
             Status => Status);
      else
         VFS.Open (Path, FS, Ino, FS_Status, 0);
         if FS_Status /= VFS.FS_Success then
            Translate_Status (FS_Status, 0, Returned, Errno);
            return;
         end if;
         Add_Entity
            (Proc   => Proc,
             FS     => FS,
             Ino    => Ino,
             Perms  => Perms,
             Status => Status);
         VFS.Close (FS, Ino);
      end if;

      case Status is
         when MAC.Success        =>
            Errno := Error_No_Error;
            Returned := 0;
            return;
         when MAC.No_Space       => Errno := Error_No_Memory;
         when MAC.Is_Conflicting => Errno := Error_Invalid_Value;
      end case;

      Returned := Unsigned_64'Last;
   end Add_MAC_Permissions;

   procedure Set_MAC_Enforcement
      (Action   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Act   : MAC.Enforcement;
   begin
      if not Get_Capabilities (Proc).Can_Manage_MAC then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_mac_enforcement", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Action is
         when MAC_DENY            => Act := MAC.Deny;
         when MAC_DENY_AND_SCREAM => Act := MAC.Deny_And_Scream;
         when MAC_KILL            => Act := MAC.Kill;
         when others              =>
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

      Set_Enforcement (Proc, Act);
      Errno := Error_No_Error;
      Returned := 0;
   end Set_MAC_Enforcement;

   procedure Mount
      (Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       FSType      : Unsigned_64;
       Flags       : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Table_Acc := Get_Common_Map (Proc);
      Src_IAddr  : constant  Integer_Address := Integer_Address (Source_Addr);
      Tgt_IAddr  : constant  Integer_Address := Integer_Address (Target_Addr);
      Src_Addr   : constant   System.Address := To_Address (Src_IAddr);
      Tgt_Addr   : constant   System.Address := To_Address (Tgt_IAddr);
      Do_RO      : constant          Boolean := (Flags and MS_RDONLY) /= 0;
      Parsed_Typ : VFS.FS_Type;
   begin
      if not Check_Userland_Access (Map, Src_IAddr, Source_Len) or
         not Check_Userland_Access (Map, Tgt_IAddr, Target_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Source_Len > Unsigned_64 (Natural'Last) or
            Target_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      elsif not Get_Capabilities (Proc).Can_Manage_Mounts then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mount", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      case FSType is
         when MNT_EXT => Parsed_Typ := VFS.FS_EXT;
         when MNT_FAT => Parsed_Typ := VFS.FS_FAT;
         when MNT_QNX => Parsed_Typ := VFS.FS_QNX;
         when others  =>
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

      declare
         Success : Boolean;
         Source  : String (1 .. Natural (Source_Len))
            with Import, Address => Src_Addr;
         Target : String (1 .. Natural (Target_Len))
            with Import, Address => Tgt_Addr;
      begin
         VFS.Mount (Source, Target, Parsed_Typ, Do_RO, Success);
         if Success then
            Errno := Error_No_Error;
            Returned := 0;
         else
            Errno := Error_IO;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end;
   end Mount;

   procedure Umount
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Curr_Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Table_Acc := Get_Common_Map (Curr_Proc);
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address  := To_Address (Path_IAddr);
      Flag_Force : constant Boolean := (Flags and MNT_FORCE) /= 0;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      elsif not Get_Capabilities (Curr_Proc).Can_Manage_Mounts then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("umount", Curr_Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Success : Boolean;
         Path    : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         VFS.Unmount (Path, Flag_Force, Success);
         if Success then
            Errno := Error_No_Error;
            Returned := 0;
         else
            Errno := Error_Busy;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end;
   end Umount;

   procedure Readlink
      (Dir_FD      : Unsigned_64;
       Path_Addr   : Unsigned_64;
       Path_Len    : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      Proc         : constant            PID := Arch.Local.Get_Current_Process;
      Map          : constant  Page_Table_Acc := Get_Common_Map (Proc);
      Path_IAddr   : constant Integer_Address := Integer_Address (Path_Addr);
      Buffer_IAddr : constant Integer_Address := Integer_Address (Buffer_Addr);
      Path_Add     : constant  System.Address := To_Address (Path_IAddr);
      Buffer_Add   : constant  System.Address := To_Address (Buffer_IAddr);
      CWD_FS       : VFS.FS_Handle;
      CWD_Ino      : VFS.File_Inode_Number;
      Opened_Ino   : File_Inode_Number;
      Ret_Count    : Natural;
      User         : Unsigned_32;
      Status       : VFS.FS_Status;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) or
         not Check_Userland_Access (Map, Buffer_IAddr, Buffer_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len   > Unsigned_64 (Natural'Last) or
            Buffer_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         File_Perms : MAC.Permissions;
         Path : String (1 ..   Natural (Path_Len))
            with Import, Address => Path_Add;
         Data : String (1 .. Natural (Buffer_Len))
            with Import, Address => Buffer_Add;
      begin
         Process.Get_Effective_UID (Proc, User);
         Resolve_AT_Directive (Proc, Dir_FD, CWD_FS, CWD_Ino);
         if CWD_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         VFS.Open (CWD_FS, CWD_Ino, Path, Opened_Ino, Status, User, False);
         if Status /= VFS.FS_Success then
            Errno := Error_No_Entity;
            Returned := Unsigned_64'Last;
            return;
         end if;

         File_Perms := Check_Permissions (Proc, CWD_FS, Opened_Ino);
         if not File_Perms.Can_Read then
            Errno := Error_Bad_Access;
            Execute_MAC_Failure ("readlink", Proc);
            Returned := Unsigned_64'Last;
            return;
         end if;

         VFS.Read_Symbolic_Link (CWD_FS, Opened_Ino, Data, Ret_Count,
                                 Status, User);
         Close (CWD_FS, Opened_Ino);
         Translate_Status (Status, Unsigned_64 (Ret_Count), Returned, Errno);
      end;
   end Readlink;

   procedure GetDEnts
      (FD          : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Directory_Entities, Directory_Entities_Acc);

      Buff_IAddr : constant Integer_Address := Integer_Address (Buffer_Addr);
      Buff_Addr  : constant System.Address  := To_Address (Buff_IAddr);
      Buff_Len   : constant Unsigned_64     := Buffer_Len / (Dirent'Size / 8);
      Proc       : constant     PID := Arch.Local.Get_Current_Process;
      Map        : constant         Page_Table_Acc := Get_Common_Map (Proc);
      File       : constant File_Description_Acc := Get_File (Proc, FD);
      Tmp_Buffer : VFS.Directory_Entities_Acc;
      Buffer     : Dirents (1 .. Buff_Len) with Import, Address => Buff_Addr;
      Read_Len   : Natural;
      Success    : VFS.FS_Status;
      User       : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Buff_IAddr, Buffer_Len) then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
      elsif File = null or else File.Description /= Description_Inode then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
      else
         Userland.Process.Get_Effective_UID (Proc, User);
         Tmp_Buffer := new VFS.Directory_Entities (1 .. Natural (Buff_Len));
         VFS.Read_Entries
            (Key       => File.Inner_Ino_FS,
             Ino       => File.Inner_Ino,
             Offset    => Natural (File.Inner_Ino_Pos),
             Entities  => Tmp_Buffer.all,
             Ret_Count => Read_Len,
             Success   => Success,
             User      => User);

         if Success = VFS.FS_Success then
            File.Inner_Ino_Pos := File.Inner_Ino_Pos + Unsigned_64 (Read_Len);

            for I in 1 .. Read_Len loop
               Buffer (Unsigned_64 (I)) :=
                  (D_Ino    => Tmp_Buffer (I).Inode_Number,
                   D_Off    => (Dirent'Size / 8) * Unsigned_64 (I),
                   D_Reclen => Dirent'Size / 8,
                   D_Type   => 0,
                   D_Name   => (others => Ada.Characters.Latin_1.NUL));
               Buffer (Unsigned_64 (I)).D_Name (1 .. Tmp_Buffer (I).Name_Len)
                  := Tmp_Buffer (I).Name_Buffer (1 .. Tmp_Buffer (I).Name_Len);
               Buffer (Unsigned_64 (I)).D_Type :=
                  (case Tmp_Buffer (I).Type_Of_File is
                     when File_Regular          => DT_REG,
                     when File_Directory        => DT_DIR,
                     when File_Symbolic_Link    => DT_LNK,
                     when File_Character_Device => DT_CHR,
                     when File_Block_Device     => DT_BLK);
            end loop;

            Returned := Unsigned_64 (Read_Len * (Dirent'Size / 8));
            Errno    := Error_No_Error;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_No_Entity;
         end if;

         Free (Tmp_Buffer);
      end if;
   end GetDEnts;

   procedure Sync (Returned : out Unsigned_64; Errno : out Errno_Value) is
      Success : Boolean;
   begin
      VFS.Synchronize (Success);
      if Success then
         Errno    := Error_No_Error;
         Returned := 0;
      else
         Errno    := Error_IO;
         Returned := Unsigned_64'Last;
      end if;
   end Sync;

   procedure MakeNode
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Dev       : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      pragma Unreferenced (Dev);

      Proc       : constant              PID := Arch.Local.Get_Current_Process;
      Map        : constant   Page_Table_Acc := Get_Common_Map (Proc);
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant   System.Address := To_Address (Path_IAddr);
      CWD_FS     : VFS.FS_Handle;
      CWD_Ino    : VFS.File_Inode_Number;
      Node_Type  : File_Type;
      Tmp_Mode   : constant File_Mode := File_Mode (Mode and 8#777#);
      Status     : VFS.FS_Status;
      Umask      : VFS.File_Mode;
      User       : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Path : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         Process.Get_Effective_UID (Proc, User);
         Resolve_AT_Directive (Proc, Dir_FD, CWD_FS, CWD_Ino);
         if CWD_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         if (Mode and Stat_IFDIR) /= 0 then
            Node_Type := File_Directory;
         else
            Node_Type := File_Regular;
         end if;

         Userland.Process.Get_Umask         (Proc, Umask);
         Userland.Process.Get_Effective_UID (Proc, User);
         Create_Node
            (Key      => CWD_FS,
             Relative => CWD_Ino,
             Path     => Path,
             Typ      => Node_Type,
             Mode     => VFS.Apply_Umask (Tmp_Mode, Umask),
             User     => User,
             Status   => Status);
         Translate_Status (Status, 0, Returned, Errno);
      end;
   end MakeNode;

   procedure Unlink
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Curr_Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Map        : constant  Page_Table_Acc := Get_Common_Map (Curr_Proc);
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant System.Address  := To_Address (Path_IAddr);
      CWD_FS     : VFS.FS_Handle;
      CWD_Ino    : VFS.File_Inode_Number;
      Success    : VFS.FS_Status;
      User       : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Path : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         Process.Get_Effective_UID (Curr_Proc, User);
         Resolve_AT_Directive (Curr_Proc, Dir_FD, CWD_FS, CWD_Ino);
         if CWD_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         VFS.Unlink (CWD_FS, CWD_Ino, Path, User, Success);
         Translate_Status (Success, 0, Returned, Errno);
      end;
   end Unlink;

   procedure Truncate
      (FD       : Unsigned_64;
       New_Size : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc    : constant     PID := Arch.Local.Get_Current_Process;
      File    : constant File_Description_Acc := Get_File (Proc, FD);
      Success : VFS.FS_Status;
      User    : Unsigned_32;
   begin
      if File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      case File.Description is
         when Description_Inode =>
            VFS.Truncate
               (File.Inner_Ino_FS, File.Inner_Ino, New_Size, User, Success);
            Translate_Status (Success, 0, Returned, Errno);
         when others =>
            Errno := Error_Bad_File;
            Returned := Unsigned_64'Last;
      end case;
   end Truncate;

   procedure Bind
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc  : constant                  PID := Arch.Local.Get_Current_Process;
      File  : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      IAddr : constant      Integer_Address := Integer_Address (Addr_Addr);
      SAddr : constant       System.Address := To_Address (IAddr);
      Succ  : Boolean;
   begin
      if File = null or else File.Description /= Description_Socket then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif not Check_Userland_Access (Get_Common_Map (Proc), IAddr, Addr_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               Addr : SockAddr_In with Import, Address => SAddr;
            begin
               Succ := Bind
                  (Sock => File.Inner_Socket,
                   Addr => Addr.Sin_Addr,
                   Port => Networking.IPv4_Port (Addr.Sin_Port));
            end;
         when IPC.Socket.IPv6 =>
            declare
               Addr : SockAddr_In6 with Import, Address => SAddr;
            begin
               Succ := Bind
                  (Sock => File.Inner_Socket,
                   Addr => Addr.Sin6_Addr,
                   Port => Networking.IPv6_Port (Addr.Sin6_Port));
            end;
         when IPC.Socket.UNIX =>
            declare
               A_SAddr2 : constant System.Address := SAddr + 4;
               CLen : constant Natural := Lib.C_String_Length (A_SAddr2);
               Addr : String (1 .. CLen) with Import, Address => A_SAddr2;
            begin
               Succ := Bind (File.Inner_Socket, Addr);
            end;
      end case;

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_IO;
         Returned := Unsigned_64'Last;
      end if;
   end Bind;

   procedure Symlink
      (Dir_FD      : Unsigned_64;
       Path_Addr   : Unsigned_64;
       Path_Len    : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       Mode        : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      Proc       : constant              PID := Arch.Local.Get_Current_Process;
      Map        : constant   Page_Table_Acc := Get_Common_Map (Proc);
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant   System.Address := To_Address (Path_IAddr);
      Targ_IAddr : constant  Integer_Address := Integer_Address (Target_Addr);
      Targ_SAddr : constant   System.Address := To_Address (Targ_IAddr);
      CWD_FS     : VFS.FS_Handle;
      CWD_Ino    : VFS.File_Inode_Number;
      Success    : VFS.FS_Status;
      User       : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) or
         not Check_Userland_Access (Map, Targ_IAddr, Target_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len   > Unsigned_64 (Natural'Last) or
            Target_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Path : String (1 ..   Natural (Path_Len))
            with Import, Address => Path_SAddr;
         Targ : String (1 .. Natural (Target_Len))
            with Import, Address => Targ_SAddr;
      begin
         Process.Get_Effective_UID (Proc, User);
         Resolve_AT_Directive (Proc, Dir_FD, CWD_FS, CWD_Ino);
         if CWD_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         Success := VFS.Create_Symbolic_Link
            (CWD_FS, CWD_Ino, Path, Targ, Unsigned_32 (Mode), User);
         Translate_Status (Success, 0, Returned, Errno);
      end;
   end Symlink;

   procedure Connect
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc  : constant                  PID := Arch.Local.Get_Current_Process;
      File  : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      IAddr : constant      Integer_Address := Integer_Address (Addr_Addr);
      SAddr : constant       System.Address := To_Address (IAddr);
      Succ  : Boolean;
   begin
      if File = null or else File.Description /= Description_Socket then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif not Check_Userland_Access (Get_Common_Map (Proc), IAddr, Addr_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               Addr : SockAddr_In with Import, Address => SAddr;
            begin
               Succ := Connect
                  (Sock => File.Inner_Socket,
                   Addr => Addr.Sin_Addr,
                   Port => Networking.IPv4_Port (Addr.Sin_Port));
            end;
         when IPC.Socket.IPv6 =>
            declare
               Addr : SockAddr_In6 with Import, Address => SAddr;
            begin
               Succ := Connect
                  (Sock => File.Inner_Socket,
                   Addr => Addr.Sin6_Addr,
                   Port => Networking.IPv6_Port (Addr.Sin6_Port));
            end;
         when IPC.Socket.UNIX =>
            declare
               A_SAddr2 : constant System.Address := SAddr + 4;
               CLen : constant Natural := Lib.C_String_Length (A_SAddr2);
               Addr : String (1 .. CLen) with Import, Address => A_SAddr2;
            begin
               Succ := Connect (File.Inner_Socket, Addr);
            end;
      end case;

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_IO;
         Returned := Unsigned_64'Last;
      end if;
   end Connect;

   procedure Open_PTY
      (Result_Addr  : Unsigned_64;
       Termios_Addr : Unsigned_64;
       Window_Addr  : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      Res_IAddr : constant  Integer_Address := Integer_Address (Result_Addr);
      Res_SAddr : constant   System.Address := To_Address (Res_IAddr);
      TIO_IAddr : constant  Integer_Address := Integer_Address (Termios_Addr);
      TIO_SAddr : constant   System.Address := To_Address (TIO_IAddr);
      Win_IAddr : constant  Integer_Address := Integer_Address (Window_Addr);
      Win_SAddr : constant   System.Address := To_Address (Win_IAddr);
      Proc      : constant              PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Table_Acc := Get_Common_Map (Proc);

      Result_PTY     : IPC.PTY.Inner_Acc;
      Primary_Desc   : File_Description_Acc;
      Secondary_Desc : File_Description_Acc;
      Succ1, Succ2   : Boolean;

      Result  : array (1 .. 2) of Integer with Import, Address => Res_SAddr;
      Termios : Devices.TermIOs.Main_Data with Import, Address => TIO_SAddr;
      Win_Siz : Devices.TermIOs.Win_Size  with Import, Address => Win_SAddr;
      Res_Size : constant Unsigned_64 := Result'Size  / 8;
      TIO_Size : constant Unsigned_64 := Termios'Size / 8;
      Win_Size : constant Unsigned_64 := Win_Siz'Size / 8;
   begin
      if not Check_Userland_Access (Map, Res_IAddr, Res_Size) or
         not Check_Userland_Access (Map, TIO_IAddr, TIO_Size) or
         not Check_Userland_Access (Map, Win_IAddr, Win_Size)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Result_PTY := Create (Termios, Win_Siz);
      Primary_Desc := new File_Description'(
         Children_Count    => 0,
         Description       => Description_Primary_PTY,
         Inner_Primary_PTY => Result_PTY
      );
      Secondary_Desc := new File_Description'(
         Children_Count      => 0,
         Description         => Description_Secondary_PTY,
         Inner_Secondary_PTY => Result_PTY
      );
      Check_Add_File (Proc, Primary_Desc,   Succ1, Result (1));
      Check_Add_File (Proc, Secondary_Desc, Succ2, Result (2));
      if not Succ1 or not Succ2 then
         Close (Result_PTY);
         Close (Result_PTY);
         Close (Primary_Desc);
         Close (Secondary_Desc);
         Errno := Error_Too_Many_Files;
         Returned := Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         Returned := 0;
      end if;
   end Open_PTY;

   procedure FSync
      (FD       : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant     PID := Arch.Local.Get_Current_Process;
      File : constant File_Description_Acc := Get_File (Proc, FD);
      Data : constant              Boolean := Flags /= 0;
      Succ : VFS.FS_Status;
   begin
      if File = null then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case File.Description is
         when Description_Inode =>
            Succ := VFS.Synchronize (File.Inner_Ino_FS, File.Inner_Ino, Data);
            Translate_Status (Succ, 0, Returned, Errno);
         when Description_Device =>
            if Devices.Synchronize (File.Inner_Dev) then
               Errno    := Error_No_Error;
               Returned := 0;
            else
               Errno    := Error_IO;
               Returned := Unsigned_64'Last;
            end if;
         when others =>
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
      end case;
   end FSync;

   procedure Link
      (Source_Dir  : Unsigned_64;
       Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Desto_Dir   : Unsigned_64;
       Desto_Addr  : Unsigned_64;
       Desto_Len   : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Table_Acc := Get_Common_Map (Proc);
      Src_IAddr : constant  Integer_Address := Integer_Address (Source_Addr);
      Src_SAddr : constant   System.Address := To_Address (Src_IAddr);
      Dst_IAddr : constant  Integer_Address := Integer_Address (Desto_Addr);
      Dst_SAddr : constant   System.Address := To_Address (Dst_IAddr);
      Src_FS, Dst_FS   : VFS.FS_Handle;
      Src_Ino, Dst_Ino : VFS.File_Inode_Number;
      Success          : VFS.FS_Status;
      User             : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Src_IAddr, Source_Len) or
         not Check_Userland_Access (Map, Dst_IAddr, Desto_Len)
      then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Source_Len > Unsigned_64 (Natural'Last) or
            Desto_Len  > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Src : String (1 .. Natural (Source_Len))
            with Import, Address => Src_SAddr;
         Dst : String (1 ..  Natural (Desto_Len))
            with Import, Address => Dst_SAddr;
      begin
         Process.Get_Effective_UID (Proc, User);
         Resolve_AT_Directive (Proc, Source_Dir, Src_FS, Src_Ino);
         Resolve_AT_Directive (Proc, Desto_Dir,  Dst_FS, Dst_Ino);
         if Src_FS = VFS.Error_Handle or else
            Dst_FS = VFS.Error_Handle or else
            Src_FS /= Dst_FS
         then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         Create_Hard_Link (Src_FS, Src_Ino, Src, Dst_Ino, Dst, User, Success);
         Translate_Status (Success, 0, Returned, Errno);
      end;
   end Link;

   procedure PTrace
      (Request     : Unsigned_64;
       Traced_PID  : Unsigned_64;
       Traced_Addr : Unsigned_64;
       Result_Addr : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      pragma Unreferenced (Traced_Addr);

      Proc  : constant PID := Arch.Local.Get_Current_Process;
      TProc : constant PID := Convert (Positive (Traced_PID));
   begin
      if TProc = Error_PID or else Proc /= Get_Parent (TProc) then
         Errno := Error_Bad_Permissions;
         Returned := Unsigned_64'Last;
         return;
      elsif not Get_Capabilities (Proc).Can_Trace_Children then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("ptrace", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Request is
         when PTRACE_SYSCALL_PIPE =>
            Set_Traced_Info (TProc, True, Natural (Result_Addr));
         when others =>
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

      Errno := Error_No_Error;
      Returned := 0;
   end PTrace;

   procedure Listen
      (Sock_FD  : Unsigned_64;
       Backlog  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant                  PID := Arch.Local.Get_Current_Process;
      File : constant File_Description_Acc := Get_File (Proc, Sock_FD);
   begin
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
      elsif Get_Type (File.Inner_Socket) /= IPC.Socket.Stream then
         Errno    := Error_Not_Supported;
         Returned := Unsigned_64'Last;
      else
         if IPC.Socket.Listen (File.Inner_Socket, Natural (Backlog)) then
            Errno := Error_No_Error;
            Returned := 0;
         else
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
         end if;
      end if;
   end Listen;

   procedure Sys_Accept
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc  : constant                PID := Arch.Local.Get_Current_Process;
      Map   : constant     Page_Table_Acc := Get_Common_Map (Proc);
      File  : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      CExec : constant              Boolean := (Flags and SOCK_CLOEXEC)  /= 0;
      Block : constant              Boolean := (Flags and SOCK_NONBLOCK) /= 0;
      A_IAddr  : constant  Integer_Address := Integer_Address (Addr_Addr);
      A_SAddr  : constant   System.Address := To_Address (A_IAddr);
      AL_IAddr : constant  Integer_Address := Integer_Address (Addr_Len);
      Desc  : File_Description_Acc;
      Sock  : Socket_Acc;
      Ret   : Natural;
      Succ  : Boolean;
   begin
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif Get_Type (File.Inner_Socket) /= IPC.Socket.Stream then
         Errno    := Error_Not_Supported;
         Returned := Unsigned_64'Last;
         return;
      end if;

      if A_IAddr = 0 or AL_IAddr = 0 then
         Accept_Connection (File.Inner_Socket, not Block, Sock);
      else
         if not Check_Userland_Access (Map, A_IAddr,  Natural'Size / 8) or
            not Check_Userland_Access (Map, AL_IAddr, Natural'Size / 8)
         then
            goto Would_Fault_Error;
         end if;

         case Get_Domain (File.Inner_Socket) is
            when IPC.Socket.IPv4 =>
               declare
                  Addr : SockAddr_In with Import, Address => A_SAddr;
               begin
                  Accept_Connection
                     (Sock         => File.Inner_Socket,
                      Is_Blocking  => not Block,
                      Peer_Address => Addr.Sin_Addr,
                      Peer_Port    => Networking.IPv4_Port (Addr.Sin_Port),
                      Result       => Sock);
               end;
            when IPC.Socket.IPv6 =>
               declare
                  Addr : SockAddr_In6 with Import, Address => A_SAddr;
               begin
                  Accept_Connection
                     (Sock         => File.Inner_Socket,
                      Is_Blocking  => not Block,
                      Peer_Address => Addr.Sin6_Addr,
                      Peer_Port    => Networking.IPv6_Port (Addr.Sin6_Port),
                      Result       => Sock);
               end;
            when IPC.Socket.UNIX =>
               declare
                  A_SAddr2 : constant System.Address := A_SAddr + 4;
                  CLen : Natural := Lib.C_String_Length (A_SAddr2);
                  Addr : String (1 .. CLen) with Import, Address => A_SAddr2;
               begin
                  Accept_Connection
                     (Sock                => File.Inner_Socket,
                      Is_Blocking         => not Block,
                      Peer_Address        => Addr,
                      Peer_Address_Length => CLen,
                      Result              => Sock);
               end;
         end case;
      end if;

      if Sock /= null then
         Desc := new File_Description'(Description_Socket, 0, Sock);
         Check_Add_File (Proc, Desc, Succ, Ret);
         if Succ then
            Set_Close_On_Exec (Proc, Unsigned_64 (Ret), CExec);
            Errno    := Error_No_Error;
            Returned := Unsigned_64 (Ret);
         else
            Close (Sock);
            Close (Desc);
            Errno    := Error_Too_Many_Files;
            Returned := Unsigned_64'Last;
         end if;
      else
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
      end if;

      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   end Sys_Accept;

   procedure Get_RLimit
      (Limit    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc     : constant PID := Arch.Local.Get_Current_Process;
      Success  : Boolean;
      Resource : MAC.Limit_Type;
   begin
      MAC_Syscall_To_Kernel (Limit, Success, Resource);
      if Success then
         Errno := Error_No_Error;
         Returned := Unsigned_64 (Get_Limit (Proc, Resource));
         return;
      end if;

      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   end Get_RLimit;

   procedure Set_RLimit
      (Limit    : Unsigned_64;
       Data     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Success  : Boolean;
      Resource : MAC.Limit_Type;
   begin
      MAC_Syscall_To_Kernel (Limit, Success, Resource);
      if Success then
         Process.Set_Limit
            (Proc      => Arch.Local.Get_Current_Process,
             Resource  => Resource,
             Limit     => MAC.Limit_Value (Data),
             Could_Set => Success);
         if Success then
            Errno := Error_No_Error;
            Returned := 0;
            return;
         end if;
      end if;

      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   end Set_RLimit;

   procedure FAccess
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc        : constant             PID := Arch.Local.Get_Current_Process;
      Map         : constant  Page_Table_Acc := Get_Common_Map (Proc);
      Path_IAddr  : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr  : constant  System.Address := To_Address (Path_IAddr);
      FS          : VFS.FS_Handle;
      D_Ino, Ino  : VFS.File_Inode_Number;
      Succ        : VFS.FS_Status;
      User        : Unsigned_32;
      File_Perms  : MAC.Permissions;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno    := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      if (Flags and AT_EACCESS) /= 0 then
         Process.Get_Effective_UID (Proc, User);
      else
         Process.Get_UID (Proc, User);
      end if;

      declare
         Path : String (1 .. Natural (Path_Len))
               with Import, Address => Path_SAddr;
      begin
         Resolve_AT_Directive (Proc, Dir_FD, FS, D_Ino);
         if FS = VFS.Error_Handle then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;

         VFS.Open
            (Key       => FS,
             Relative  => D_Ino,
             Path      => Path,
             Ino       => Ino,
             Success   => Succ,
             User      => User,
             Do_Follow => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
         if Succ /= VFS.FS_Success then
            Errno    := Error_No_Entity;
            Returned := Unsigned_64'Last;
            return;
         end if;

         File_Perms := Check_Permissions (Proc, FS, Ino);
         if not File_Perms.Can_Write then
            Errno    := Error_Bad_Access;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end;

      VFS.Check_Access
         (Key         => FS,
          Ino         => Ino,
          Exists_Only => (Mode and F_OK) /= 0,
          Can_Read    => (Mode and R_OK) /= 0,
          Can_Write   => (Mode and W_OK) /= 0,
          Can_Exec    => (Mode and X_OK) /= 0,
          User        => User,
          Status      => Succ);
      Translate_Status (Succ, 0, Returned, Errno);
   end FAccess;

   procedure Poll
      (FDs_Addr  : Unsigned_64;
       FDs_Count : Unsigned_64;
       Timeout   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      Map        : constant  Page_Table_Acc := Get_Common_Map (Proc);
      FIAddr     : constant Integer_Address := Integer_Address (FDs_Addr);
      FSAddr     : constant  System.Address := To_Address (FIAddr);
      TIAddr     : constant Integer_Address := Integer_Address (Timeout);
      TSAddr     : constant  System.Address := To_Address (TIAddr);
      Size_FD    : constant     Unsigned_64 := FDs_Count * (Poll_FD'Size / 8);
      Count      :                  Natural := 0;
      Discard    : Boolean;
      File       : File_Description_Acc;
      Curr_Sec   : Unsigned_64;
      Curr_NSec  : Unsigned_64;
      Final_Sec  : Unsigned_64;
      Final_NSec : Unsigned_64;
      Can_Read   : Boolean;
      Can_Write  : Boolean;
      Is_Error   : Boolean;
      Is_Broken  : Boolean;
   begin
      if not Check_Userland_Access (Map, FIAddr, Size_FD) or else
         not Check_Userland_Access (Map, TIAddr, Time_Spec'Size / 8)
      then
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif FDs_Count = 0 then
         goto Normal_Empty_Exit;
      end if;

      Arch.Clocks.Get_Monotonic_Time (Final_Sec, Final_NSec);

      declare
         FDs  : Poll_FDs (1 .. FDs_Count) with Import, Address => FSAddr;
         Time : Time_Spec                 with Import, Address => TSAddr;
      begin
         Lib.Time.Increment (Final_Sec, Final_NSec, Time.Seconds,
                             Time.Nanoseconds);

         loop
            for Polled of FDs loop
               Polled.Out_Events := 0;

               --  We are to ignore the FD if its negative.
               if (Polled.FD and Shift_Left (1, 31)) /= 0 then
                  goto End_Iter;
               end if;

               --  Check the FD actually points to anything valuable.
               File := Get_File (Proc, Unsigned_64 (Polled.FD));
               if File = null then
                  Polled.Out_Events := POLLNVAL;
                  goto End_Iter;
               end if;

               --  Fill out events depending on the file type.
               case File.Description is
                  when Description_Device =>
                     Devices.Poll (File.Inner_Dev, Can_Read, Can_Write,
                                   Is_Error);
                     Is_Broken := False;
                  when Description_Reader_FIFO =>
                     IPC.FIFO.Poll_Reader
                        (File.Inner_Reader_FIFO,
                         Can_Read, Can_Write, Is_Error, Is_Broken);
                  when Description_Writer_FIFO =>
                     IPC.FIFO.Poll_Writer
                        (File.Inner_Writer_FIFO,
                         Can_Read, Can_Write, Is_Error, Is_Broken);
                  when Description_Primary_PTY =>
                     IPC.PTY.Poll_Primary
                        (File.Inner_Primary_PTY, Can_Read, Can_Write);
                     Is_Error  := False;
                     Is_Broken := False;
                  when Description_Secondary_PTY =>
                     IPC.PTY.Poll_Secondary
                        (File.Inner_Secondary_PTY, Can_Read, Can_Write);
                     Is_Error  := False;
                     Is_Broken := False;
                  when Description_Socket =>
                     IPC.Socket.Poll
                        (File.Inner_Socket, Can_Read, Can_Write, Is_Broken,
                         Is_Error);
                  when Description_Inode =>
                     Can_Read  := True;
                     Can_Write := True;
                     Is_Error  := False;
                     Is_Broken := False;
               end case;

               if Can_Read and (Polled.Events and POLLIN) /= 0 then
                  Polled.Out_Events := Polled.Out_Events or POLLIN;
               end if;
               if Can_Write and (Polled.Events and POLLOUT) /= 0 then
                  Polled.Out_Events := Polled.Out_Events or POLLOUT;
               end if;
               if Is_Error then
                  Polled.Out_Events := Polled.Out_Events or POLLERR;
               end if;
               if Is_Broken then
                  Polled.Out_Events := Polled.Out_Events or POLLHUP;
               end if;

            <<End_Iter>>
               if Polled.Out_Events /= 0 then
                  Count := Count + 1;
               end if;
            end loop;

            Arch.Clocks.Get_Monotonic_Time (Curr_Sec, Curr_NSec);
            exit when Count /= 0 or Lib.Time.Is_Greater_Equal (Curr_Sec,
               Curr_NSec, Final_Sec, Final_NSec);

            Scheduler.Yield_If_Able;
         end loop;
      end;

   <<Normal_Empty_Exit>>
      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Count);
   end Poll;

   procedure Get_EUID (Returned : out Unsigned_64; Errno : out Errno_Value) is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Ret  : Unsigned_32;
   begin
      Userland.Process.Get_Effective_UID (Proc, Ret);
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Ret);
   end Get_EUID;

   procedure Set_UIDs
      (UID      : Unsigned_64;
       EUID     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
   begin
      if not Get_Capabilities (Proc).Can_Change_UIDs then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("setuids", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      if UID <= Unsigned_64 (Unsigned_32'Last) then
         Userland.Process.Set_UID (Proc, Unsigned_32 (UID));
      end if;
      if EUID <= Unsigned_64 (Unsigned_32'Last) then
         Userland.Process.Set_Effective_UID (Proc, Unsigned_32 (EUID));
      end if;

      Errno := Error_No_Error;
      Returned := 0;
   end Set_UIDs;

   procedure Fchmod
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc        : constant             PID := Arch.Local.Get_Current_Process;
      Map         : constant  Page_Table_Acc := Get_Common_Map (Proc);
      Path_IAddr  : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr  : constant  System.Address := To_Address (Path_IAddr);
      FS          : VFS.FS_Handle;
      D_Ino, Ino  : VFS.File_Inode_Number;
      Succ        : VFS.FS_Status;
      User        : Unsigned_32;
      File_Desc   : File_Description_Acc;
      File_Perms  : MAC.Permissions;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno    := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Process.Get_Effective_UID (Proc, User);

      if (Flags and AT_EMPTY_PATH) /= 0 then
         File_Desc := Get_File (Proc, Dir_FD);
         if File_Desc = null or else File_Desc.Description /= Description_Inode
         then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;
         FS  := File_Desc.Inner_Ino_FS;
         Ino := File_Desc.Inner_Ino;
      else
         declare
            Path : String (1 .. Natural (Path_Len))
               with Import, Address => Path_SAddr;
         begin
            Resolve_AT_Directive (Proc, Dir_FD, FS, D_Ino);
            if FS = VFS.Error_Handle then
               Errno    := Error_Bad_File;
               Returned := Unsigned_64'Last;
               return;
            end if;

            VFS.Open
               (Key       => FS,
                Relative  => D_Ino,
                Path      => Path,
                Ino       => Ino,
                Success   => Succ,
                User      => User,
                Do_Follow => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
            if Succ /= VFS.FS_Success then
               Errno    := Error_No_Entity;
               Returned := Unsigned_64'Last;
               return;
            end if;

            File_Perms := Check_Permissions (Proc, FS, Ino);
            if not File_Perms.Can_Write then
               Errno    := Error_Bad_Access;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end;
      end if;


      VFS.Change_Mode
         (Key    => FS,
          Ino    => Ino,
          Mode   => File_Mode (Mode and Unsigned_64 (File_Mode'Last)),
          User   => User,
          Status => Succ);
      Translate_Status (Succ, 0, Returned, Errno);
   end Fchmod;

   procedure Umask
      (Mode     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Old  : File_Mode;
   begin
      Process.Get_Umask (Proc, Old);
      Process.Set_Umask (Proc, File_Mode (Mode and 8#777#));
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Old);
   end Umask;

   procedure Reboot
      (Command  : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc    : constant     PID := Arch.Local.Get_Current_Process;
      Do_Ret  : constant Boolean := (Flags and RB_ERROR_RET) /= 0;
      Success : Arch.Power.Power_Status;
   begin
      if not Get_Capabilities (Proc).Can_Manage_Power then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("reboot", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Command is
         when RB_HALT     => Success := Arch.Power.Halt;
         when RB_POWEROFF => Success := Arch.Power.Poweroff;
         when RB_RESTART  => Success := Arch.Power.Reboot;
         when others =>
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

      --  If we are here, its because the functions failed.
      if Do_Ret then
         case Success is
            when Arch.Power.Not_Supported => Errno := Error_Not_Implemented;
            when Arch.Power.Failure       => Errno := Error_IO;
         end case;
         Returned := Unsigned_64'Last;
         return;
      else
         Lib.Panic.Hard_Panic ("reboot() operation failed");
      end if;
   end Reboot;

   procedure Fchown
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       User      : Unsigned_64;
       Group     : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc        : constant             PID := Arch.Local.Get_Current_Process;
      Map         : constant  Page_Table_Acc := Get_Common_Map (Proc);
      Path_IAddr  : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr  : constant  System.Address := To_Address (Path_IAddr);
      FS          : VFS.FS_Handle;
      D_Ino, Ino  : VFS.File_Inode_Number;
      Succ        : VFS.FS_Status;
      Usr         : Unsigned_32;
      File_Desc   : File_Description_Acc;
      File_Perms  : MAC.Permissions;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno    := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Process.Get_Effective_UID (Proc, Usr);

      if (Flags and AT_EMPTY_PATH) /= 0 then
         File_Desc := Get_File (Proc, Dir_FD);
         if File_Desc = null or else File_Desc.Description /= Description_Inode
         then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;
         FS  := File_Desc.Inner_Ino_FS;
         Ino := File_Desc.Inner_Ino;
      else
         declare
            Path : String (1 .. Natural (Path_Len))
               with Import, Address => Path_SAddr;
         begin
            Resolve_AT_Directive (Proc, Dir_FD, FS, D_Ino);
            if FS = VFS.Error_Handle then
               Errno    := Error_Bad_File;
               Returned := Unsigned_64'Last;
               return;
            end if;

            VFS.Open
               (Key       => FS,
                Relative  => D_Ino,
                Path      => Path,
                Ino       => Ino,
                Success   => Succ,
                User      => Usr,
                Do_Follow => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
            if Succ /= VFS.FS_Success then
               Errno    := Error_No_Entity;
               Returned := Unsigned_64'Last;
               return;
            end if;

            File_Perms := Check_Permissions (Proc, FS, Ino);
            if not File_Perms.Can_Write then
               Errno    := Error_Bad_Access;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end;
      end if;

      VFS.Change_Owner
         (FS,
          Ino,
          Unsigned_32 (User  and 16#FFFFFFFF#),
          Unsigned_32 (Group and 16#FFFFFFFF#),
          Usr,
          Succ);
      Translate_Status (Succ, 0, Returned, Errno);
   end Fchown;

   procedure PRead
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Offset   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Map       : constant    Page_Table_Acc := Get_Common_Map (Proc);
      File      : constant File_Description_Acc := Get_File (Proc, File_D);
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success3  : Boolean;
      User      : Unsigned_32;
      Final_Cnt : Natural;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      declare
         Data : Devices.Operation_Data (1 .. Final_Cnt)
            with Import, Address => Buf_SAddr;
      begin
         case File.Description is
            when Description_Device =>
               if not File.Inner_Dev_Read then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
               end if;
               Devices.Read
                  (File.Inner_Dev, Offset, Data, Ret_Count, Success3);
               if Success3 then
                  Errno := Error_No_Error;
                  Returned := Unsigned_64 (Ret_Count);
                  return;
               else
                  Errno := Error_IO;
                  Returned := Unsigned_64'Last;
                  return;
               end if;
            when Description_Inode =>
               if not File.Inner_Ino_Read then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
               end if;
               VFS.Read (File.Inner_Ino_FS, File.Inner_Ino, Offset,
                         Data, Ret_Count, Success1, User);
               Translate_Status (Success1, Unsigned_64 (Ret_Count), Returned,
                                        Errno);
            when others =>
               Errno := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
               return;
         end case;
      end;
   end PRead;

   procedure PWrite
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Offset   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Map       : constant    Page_Table_Acc := Get_Common_Map (Proc);
      File      : constant File_Description_Acc := Get_File (Proc, File_D);
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success3  : Boolean;
      User      : Unsigned_32;
      Final_Cnt : Natural;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      elsif File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      Process.Get_Effective_UID (Proc, User);

      declare
         Data : Devices.Operation_Data (1 .. Final_Cnt)
            with Import, Address => Buf_SAddr;
      begin
         case File.Description is
            when Description_Device =>
               if not File.Inner_Dev_Write then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
               end if;

               Devices.Write
                  (File.Inner_Dev, Offset, Data, Ret_Count, Success3);
               if Success3 then
                  Errno := Error_No_Error;
                  Returned := Unsigned_64 (Ret_Count);
                  return;
               else
                  Errno := Error_IO;
                  Returned := Unsigned_64'Last;
                  return;
               end if;
            when Description_Inode =>
               if not File.Inner_Ino_Write then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
               elsif Offset + Unsigned_64 (Final_Cnt) >
                  Unsigned_64 (Get_Limit (Proc, MAC.File_Size_Limit))
               then
                  Errno := Error_File_Too_Big;
                  Returned := Unsigned_64'Last;
                  return;
               end if;

               VFS.Write (File.Inner_Ino_FS, File.Inner_Ino, Offset,
                          Data, Ret_Count, Success1, User);
               Translate_Status (Success1, Unsigned_64 (Ret_Count), Returned,
                                        Errno);
            when others =>
               Errno := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
               return;
         end case;
      end;
   end PWrite;

   procedure Get_Sock_Name
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc   : constant                  PID := Arch.Local.Get_Current_Process;
      Map    : constant       Page_Table_Acc := Get_Common_Map (Proc);
      File   : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      AIAddr : constant      Integer_Address := Integer_Address (Addr_Addr);
      ASAddr : constant       System.Address := To_Address (AIAddr);
      LIAddr : constant      Integer_Address := Integer_Address (Addr_Len);
      LSAddr : constant       System.Address := To_Address (LIAddr);
      Succ   : Boolean;
   begin
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif not Check_Userland_Access (Map, LIAddr, Natural'Size / 8) then
         goto Would_Fault_Error;
      end if;

      declare
         Length : Natural with Import, Address => LSAddr;
      begin
         if not Check_Userland_Access (Map, AIAddr, Unsigned_64 (Length)) then
            goto Would_Fault_Error;
         end if;
      end;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               Addr : SockAddr_In with Import, Address => ASAddr;
            begin
               Get_Bound (File.Inner_Socket, Addr.Sin_Addr,
                  Networking.IPv4_Port (Addr.Sin_Port), Succ);
            end;
         when IPC.Socket.IPv6 =>
            declare
               Addr : SockAddr_In6 with Import, Address => ASAddr;
            begin
               Get_Bound (File.Inner_Socket, Addr.Sin6_Addr,
                  Networking.IPv6_Port (Addr.Sin6_Port), Succ);
            end;
         when IPC.Socket.UNIX =>
            declare
               A_SAddr2 : constant System.Address := ASAddr + 4;
               CLen : Natural := Lib.C_String_Length (A_SAddr2);
               Addr : String (1 .. CLen) with Import, Address => A_SAddr2;
            begin
               Get_Bound (File.Inner_Socket, Addr, CLen, Succ);
            end;
      end case;

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
      end if;

      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   end Get_Sock_Name;

   procedure Get_Peer_Name
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc   : constant                  PID := Arch.Local.Get_Current_Process;
      Map    : constant       Page_Table_Acc := Get_Common_Map (Proc);
      File   : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      AIAddr : constant      Integer_Address := Integer_Address (Addr_Addr);
      ASAddr : constant       System.Address := To_Address (AIAddr);
      LIAddr : constant      Integer_Address := Integer_Address (Addr_Len);
      LSAddr : constant       System.Address := To_Address (LIAddr);
      Succ   : Boolean;
   begin
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif not Check_Userland_Access (Map, LIAddr, Natural'Size / 8) then
         goto Would_Fault_Error;
      end if;

      declare
         Length : Natural with Import, Address => LSAddr;
      begin
         if not Check_Userland_Access (Map, AIAddr, Unsigned_64 (Length)) then
            goto Would_Fault_Error;
         end if;
      end;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               Addr : SockAddr_In with Import, Address => ASAddr;
            begin
               Get_Peer (File.Inner_Socket, Addr.Sin_Addr,
                  Networking.IPv4_Port (Addr.Sin_Port), Succ);
            end;
         when IPC.Socket.IPv6 =>
            declare
               Addr : SockAddr_In6 with Import, Address => ASAddr;
            begin
               Get_Peer (File.Inner_Socket, Addr.Sin6_Addr,
                  Networking.IPv6_Port (Addr.Sin6_Port), Succ);
            end;
         when IPC.Socket.UNIX =>
            declare
               A_SAddr2 : constant System.Address := ASAddr + 4;
               CLen : Natural := Lib.C_String_Length (A_SAddr2);
               Addr : String (1 .. CLen) with Import, Address => A_SAddr2;
            begin
               Get_Peer (File.Inner_Socket, Addr, CLen, Succ);
            end;
      end case;

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_Not_Connected;
         Returned := Unsigned_64'Last;
      end if;

      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   end Get_Peer_Name;

   procedure Shutdown
      (Sock_FD  : Unsigned_64;
       How      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant                  PID := Arch.Local.Get_Current_Process;
      File : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      Succ : Boolean;
   begin
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case How is
         when SHUT_RD =>
            Succ := IPC.Socket.Shutdown (File.Inner_Socket, True, False);
         when SHUT_WR =>
            Succ := IPC.Socket.Shutdown (File.Inner_Socket, False, True);
         when SHUT_RDWR =>
            Succ := IPC.Socket.Shutdown (File.Inner_Socket, True, True);
         when others =>
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

      if Succ then
         Errno    := Error_No_Error;
         Returned := 0;
      else
         Errno    := Error_Not_Connected;
         Returned := Unsigned_64'Last;
      end if;
   end Shutdown;

   procedure Futex
      (Operation : Unsigned_64;
       Address   : Unsigned_64;
       Count     : Unsigned_64;
       Timeout   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      pragma Unreferenced (Timeout);

      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Map   : constant  Page_Table_Acc := Get_Common_Map (Proc);
      IAddr : constant Integer_Address := Integer_Address (Address);
      SAddr : constant  System.Address := To_Address (IAddr);
   begin
      if not Check_Userland_Access (Map, IAddr, Count * (Futex_Item'Size / 8))
      then
         goto Would_Fault_Error;
      elsif Count > Unsigned_64 (Natural'Last) then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Cnt     : constant Natural := Natural (Count);
         Items   : Futex_Item_Arr (1 .. Cnt) with Import, Address => SAddr;
         Futexes : IPC.Futex.Element_Arr (1 .. Cnt);
         IA      : Integer_Address;
      begin
         for I in Items'Range loop
            IA := Integer_Address (Items (I).Address);
            if not Check_Userland_Access (Map, IA, Unsigned_32'Size / 8) then
               goto Would_Fault_Error;
            end if;

            declare
               V : aliased Unsigned_32 with Import, Address => To_Address (IA);
            begin
               Futexes (I) := (V'Unchecked_Access, Items (I).Expected);
            end;
         end loop;

         case Operation is
            when FUTEX_WAIT =>
               if IPC.Futex.Wait (Futexes) then
                  Returned := 0;
                  Errno    := Error_No_Error;
               else
                  Returned := Unsigned_64'Last;
                  Errno    := Error_No_Memory;
               end if;
            when FUTEX_WAKE =>
               IPC.Futex.Wake (Futexes);
            when others =>
               Returned := Unsigned_64'Last;
               Errno    := Error_Invalid_Value;
         end case;
      end;

      Returned := 0;
      Errno    := Error_No_Error;
      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   end Futex;

   procedure Clock
      (Operation : Unsigned_64;
       Clock_ID  : Unsigned_64;
       Address   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Map   : constant  Page_Table_Acc := Get_Common_Map (Proc);
      IAddr : constant Integer_Address := Integer_Address (Address);
   begin
      if not Check_Userland_Access (Map, IAddr, Time_Spec'Size / 8) then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      elsif not Get_Capabilities (Proc).Can_Use_Clocks then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("clock", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Spec : Time_Spec with Import, Address => To_Address (IAddr);
      begin
         case Operation is
            when CLOCK_GETRES =>
               case Clock_ID is
                  when CLOCK_MONOTONIC =>
                     Arch.Clocks.Get_Monotonic_Resolution
                        (Spec.Seconds, Spec.Nanoseconds);
                  when CLOCK_REALTIME =>
                     Arch.Clocks.Get_Real_Time_Resolution
                        (Spec.Seconds, Spec.Nanoseconds);
                  when others =>
                     goto Invalid_Value_Error;
               end case;
            when CLOCK_GETTIME =>
               case Clock_ID is
                  when CLOCK_MONOTONIC =>
                     Arch.Clocks.Get_Monotonic_Time
                        (Spec.Seconds, Spec.Nanoseconds);
                  when CLOCK_REALTIME =>
                     Arch.Clocks.Get_Real_Time
                        (Spec.Seconds, Spec.Nanoseconds);
                  when others =>
                     goto Invalid_Value_Error;
               end case;
            when CLOCK_SETTIME =>
               case Clock_ID is
                  when CLOCK_REALTIME =>
                     Arch.Clocks.Set_Real_Time
                        (Spec.Seconds, Spec.Nanoseconds);
                  when others =>
                     goto Invalid_Value_Error;
               end case;
            when others =>
               goto Invalid_Value_Error;
         end case;
      end;

      Returned := 0;
      Errno := Error_No_Error;
      return;

   <<Invalid_Value_Error>>
      Returned := Unsigned_64'Last;
      Errno    := Error_Invalid_Value;
   end Clock;

   procedure Clock_Nanosleep
      (Clock_ID     : Unsigned_64;
       Flags        : Unsigned_64;
       Request_Addr : Unsigned_64;
       Remain_Addr  : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      Proc     : constant             PID := Arch.Local.Get_Current_Process;
      Map      : constant  Page_Table_Acc := Get_Common_Map (Proc);
      ReqIAddr : constant Integer_Address := Integer_Address (Request_Addr);
      RemIAddr : constant Integer_Address := Integer_Address (Remain_Addr);
   begin
      if not Check_Userland_Access (Map, ReqIAddr, Time_Spec'Size / 8) or
         not Check_Userland_Access (Map, RemIAddr, Time_Spec'Size / 8)
      then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      elsif not Get_Capabilities (Proc).Can_Use_Clocks then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("clock_nanosleep", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Request     : Time_Spec with Import, Address => To_Address (ReqIAddr);
         Remaining   : Time_Spec with Import, Address => To_Address (RemIAddr);
         CSec, CNSec : Unsigned_64;
         FSec, FNSec : Unsigned_64;
      begin
         if (Flags and TIMER_ABSTIME) /= 0 then
            FSec  := Request.Seconds;
            FNSec := Request.Nanoseconds;
         else
            if Clock_ID = CLOCK_MONOTONIC then
               Arch.Clocks.Get_Monotonic_Time (FSec, FNSec);
            else
               Arch.Clocks.Get_Real_Time (FSec, FNSec);
            end if;
            Lib.Time.Increment (FSec, FNSec, Request.Seconds,
                                Request.Nanoseconds);
         end if;

         loop
            if Clock_ID = CLOCK_MONOTONIC then
               Arch.Clocks.Get_Monotonic_Time (CSec, CNSec);
            else
               Arch.Clocks.Get_Real_Time (CSec, CNSec);
            end if;
            exit when Lib.Time.Is_Greater_Equal (CSec, CNSec, FSec, FNSec);
            Scheduler.Yield_If_Able;
         end loop;

         Remaining.Seconds := 0;
         Remaining.Nanoseconds := 0;

         Returned := 0;
         Errno    := Error_No_Error;
      end;
   end Clock_Nanosleep;

   procedure Get_RUsage
      (Who        : Unsigned_64;
       Usage_Addr : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Map   : constant  Page_Table_Acc := Get_Common_Map (Proc);
      IAddr : constant Integer_Address := Integer_Address (Usage_Addr);
   begin
      if not Check_Userland_Access (Map, IAddr, RUsage'Size / 8) then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      end if;

      declare
         Usage : RUsage with Import, Address => To_Address (IAddr);
      begin
         case Who is
            when RUSAGE_SELF =>
               Process.Get_Runtime_Times (Proc, Usage.System_Time.Seconds,
                  Usage.System_Time.Nanoseconds, Usage.User_Time.Seconds,
                  Usage.User_Time.Nanoseconds);
            when RUSAGE_CHILDREN =>
               Process.Get_Children_Runtimes
                  (Proc, Usage.System_Time.Seconds,
                   Usage.System_Time.Nanoseconds, Usage.User_Time.Seconds,
                   Usage.User_Time.Nanoseconds);
            when others =>
               Returned := Unsigned_64'Last;
               Errno    := Error_Invalid_Value;
               return;
         end case;
         Returned := 0;
         Errno    := Error_No_Error;
      end;
   end Get_RUsage;

   procedure RecvFrom
      (Sock_FD   : Unsigned_64;
       Buffer    : Unsigned_64;
       Count     : Unsigned_64;
       Flags     : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      pragma Unreferenced (Flags);

      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      AIAddr    : constant      Integer_Address := Integer_Address (Addr_Addr);
      ASAddr    : constant       System.Address := To_Address (AIAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Map       : constant    Page_Table_Acc := Get_Common_Map (Proc);
      File      : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      Ret_Count : Natural;
      Success   : IPC.Socket.Socket_Status;
      Final_Cnt : Natural;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) or else
         (AIAddr /= 0 and not Check_Userland_Access (Map, AIAddr, Addr_Len))
      then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      elsif File = null or else File.Description /= Description_Socket then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      declare
         Data : Devices.Operation_Data (1 .. Final_Cnt)
            with Import, Address => Buf_SAddr;
      begin
         if AIAddr /= 0 and Get_Type (File.Inner_Socket) /= IPC.Socket.Stream
         then
            case Get_Domain (File.Inner_Socket) is
               when IPC.Socket.IPv4 =>
                  declare
                     Addr : SockAddr_In with Import, Address => ASAddr;
                  begin
                     IPC.Socket.Read
                        (Sock      => File.Inner_Socket,
                         Data      => Data,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin_Addr,
                         Port      => Networking.IPv4_Port (Addr.Sin_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.IPv6 =>
                  declare
                     Addr : SockAddr_In6 with Import, Address => ASAddr;
                  begin
                     IPC.Socket.Read
                        (Sock      => File.Inner_Socket,
                         Data      => Data,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin6_Addr,
                         Port      => Networking.IPv6_Port (Addr.Sin6_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.UNIX =>
                  declare
                     A_SAddr : constant System.Address := ASAddr + 4;
                     CLen : constant Natural := Lib.C_String_Length (A_SAddr);
                     Addr : String (1 .. CLen) with Import, Address => A_SAddr;
                  begin
                     IPC.Socket.Read
                        (Sock      => File.Inner_Socket,
                         Data      => Data,
                         Ret_Count => Ret_Count,
                         Path      => Addr,
                         Success   => Success);
                  end;
            end case;
         else
            IPC.Socket.Read (File.Inner_Socket, Data, Ret_Count, Success);
         end if;
         Translate_Status (Success, Unsigned_64 (Ret_Count), Returned, Errno);
      end;
   end RecvFrom;

   procedure SendTo
      (Sock_FD   : Unsigned_64;
       Buffer    : Unsigned_64;
       Count     : Unsigned_64;
       Flags     : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      pragma Unreferenced (Flags);

      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      AIAddr    : constant      Integer_Address := Integer_Address (Addr_Addr);
      ASAddr    : constant       System.Address := To_Address (AIAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Map       : constant    Page_Table_Acc := Get_Common_Map (Proc);
      File      : constant File_Description_Acc := Get_File (Proc, Sock_FD);
      Ret_Count : Natural;
      Success   : IPC.Socket.Socket_Status;
      Final_Cnt : Natural;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) or else
         (AIAddr /= 0 and not Check_Userland_Access (Map, AIAddr, Addr_Len))
      then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      elsif File = null or else File.Description /= Description_Socket then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      declare
         Data : Devices.Operation_Data (1 .. Final_Cnt)
            with Import, Address => Buf_SAddr;
      begin
         if AIAddr /= 0 and Get_Type (File.Inner_Socket) /= IPC.Socket.Stream
         then
            case Get_Domain (File.Inner_Socket) is
               when IPC.Socket.IPv4 =>
                  declare
                     Addr : SockAddr_In with Import, Address => ASAddr;
                  begin
                     IPC.Socket.Write
                        (Sock      => File.Inner_Socket,
                         Data      => Data,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin_Addr,
                         Port      => Networking.IPv4_Port (Addr.Sin_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.IPv6 =>
                  declare
                     Addr : SockAddr_In6 with Import, Address => ASAddr;
                  begin
                     IPC.Socket.Write
                        (Sock      => File.Inner_Socket,
                         Data      => Data,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin6_Addr,
                         Port      => Networking.IPv6_Port (Addr.Sin6_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.UNIX =>
                  declare
                     A_SAddr : constant System.Address := ASAddr + 4;
                     CLen : constant Natural := Lib.C_String_Length (A_SAddr);
                     Addr : String (1 .. CLen) with Import, Address => A_SAddr;
                  begin
                     IPC.Socket.Write
                        (Sock      => File.Inner_Socket,
                         Data      => Data,
                         Ret_Count => Ret_Count,
                         Path      => Addr,
                         Success   => Success);
                  end;
            end case;
         else
            IPC.Socket.Write (File.Inner_Socket, Data, Ret_Count, Success);
         end if;
         Translate_Status (Success, Unsigned_64 (Ret_Count), Returned, Errno);
      end;
   end SendTo;
   ----------------------------------------------------------------------------
   procedure Do_Exit (Proc : PID; Code : Unsigned_8) is
   begin
      --  Remove all state but the return value and keep the zombie around
      --  until we are waited.
      Userland.Process.Flush_Threads (Proc);
      Userland.Process.Flush_Files   (Proc);
      Userland.Process.Issue_Exit    (Proc, Code);
      Userland.Process.Remove_Thread (Proc, Arch.Local.Get_Current_Thread);
      Scheduler.Bail;
   end Do_Exit;

   procedure Pre_Syscall_Hook (State : Arch.Context.GP_Context) is
      Thread  : constant TID := Arch.Local.Get_Current_Thread;
   begin
      Scheduler.Signal_Kernel_Entry (Thread);
      Common_Syscall_Hook (Thread, State);
   end Pre_Syscall_Hook;

   procedure Post_Syscall_Hook (State : Arch.Context.GP_Context) is
      Thread  : constant TID := Arch.Local.Get_Current_Thread;
   begin
      Common_Syscall_Hook (Thread, State);
      Scheduler.Signal_Kernel_Exit (Thread);
   end Post_Syscall_Hook;
   ----------------------------------------------------------------------------
   procedure Common_Syscall_Hook
      (Thread : TID;
       State  : Arch.Context.GP_Context)
   is
      type Trace_Info is record
         Thread : Unsigned_16;
         State  : Arch.Context.GP_Context;
      end record with Pack;

      Proc       : constant PID := Arch.Local.Get_Current_Process;
      File       : File_Description_Acc;
      Success    : IPC.FIFO.Pipe_Status;
      Ret_Count  : Natural;
      Tracer_FD  : Natural;
      Is_Traced  : Boolean;
      TInfo      : Trace_Info;
      TInfo_Data : Devices.Operation_Data (1 .. TInfo'Size / 8)
         with Import, Address => TInfo'Address;
   begin
      Userland.Process.Get_Traced_Info (Proc, Is_Traced, Tracer_FD);
      if Is_Traced then
         File := Get_File (Proc, Unsigned_64 (Tracer_FD));
         if File /= null and then File.Description = Description_Writer_FIFO
         then
            while not Is_Empty (File.Inner_Writer_FIFO) loop
               Scheduler.Yield_If_Able;
            end loop;
            TInfo := (Unsigned_16 (Convert (Thread)), State);
            Write (File.Inner_Writer_FIFO, TInfo_Data, Ret_Count, Success);
         end if;
      end if;
   end Common_Syscall_Hook;

   procedure Translate_Status
      (Status         : VFS.FS_Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value)
   is
   begin
      case Status is
         when VFS.FS_Success =>
            Errno    := Error_No_Error;
            Returned := Success_Return;
            return;
         when VFS.FS_Invalid_Value => Errno := Error_Invalid_Value;
         when VFS.FS_Not_Supported => Errno := Error_Not_Implemented;
         when VFS.FS_RO_Failure    => Errno := Error_Read_Only_FS;
         when VFS.FS_IO_Failure    => Errno := Error_IO;
         when VFS.FS_Not_Allowed   => Errno := Error_Bad_Permissions;
      end case;
      Returned := Unsigned_64'Last;
   end Translate_Status;

   procedure Translate_Status
      (Status         : IPC.Socket.Socket_Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value)
   is
   begin
      case Status is
         when Plain_Success =>
            Errno    := Error_No_Error;
            Returned := Success_Return;
            return;
         when Is_Bad_Type   => Errno := Error_Invalid_Value;
         when Would_Block   => Errno := Error_Would_Block;
      end case;
      Returned := Unsigned_64'Last;
   end Translate_Status;

   procedure Translate_Status
      (Status         : IPC.FIFO.Pipe_Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value)
   is
   begin
      case Status is
         when Pipe_Success   =>
            Errno    := Error_No_Error;
            Returned := Success_Return;
            return;
         when Broken_Failure => Errno := Error_Invalid_Value;
         when Would_Block_Failure => Errno := Error_Would_Block;
      end case;
      Returned := Unsigned_64'Last;
   end Translate_Status;

   procedure Exec_Into_Process
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Proc      : PID;
       Success   : out Boolean;
       Errno     : out Errno_Value)
   is
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Acc);
      type Arg_Arr is array (Natural range <>) of Unsigned_64;

      Map        : constant    Page_Table_Acc := Get_Common_Map (Proc);
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      Path       : String (1 .. Natural (Path_Len))
         with Import, Address => Path_SAddr;
      Path_FS    : FS_Handle;
      Path_Ino   : File_Inode_Number;
      Success2   : FS_Status;
      Succ       : Boolean;
      File_Perms : MAC.Permissions;
      User       : Unsigned_32;
      Argv_IAddr : constant Integer_Address := Integer_Address (Argv_Addr);
      Argv_SAddr : constant  System.Address := To_Address (Argv_IAddr);
      Envp_IAddr : constant Integer_Address := Integer_Address (Envp_Addr);
      Envp_SAddr : constant  System.Address := To_Address (Envp_IAddr);
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) or
         not Check_Userland_Access (Map, Argv_IAddr, Argv_Len) or
         not Check_Userland_Access (Map, Envp_IAddr, Envp_Len)
      then
         Errno := Error_Would_Fault;
         Success := False;
         return;
      elsif not Get_Capabilities (Proc).Can_Spawn_Others then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("spawn", Proc);
         Success := False;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      Open (Path, Path_FS, Path_Ino, Success2, User);
      if Success2 /= VFS.FS_Success then
         Errno := Error_No_Entity;
         Success := False;
         return;
      end if;

      File_Perms := Check_Permissions (Proc, Path_FS, Path_Ino);
      if not File_Perms.Can_Execute then
         VFS.Close (Path_FS, Path_Ino);
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("exec", Proc);
         Success := False;
         return;
      end if;

      declare
         Argv : Arg_Arr (1 .. Natural (Argv_Len))
            with Import, Address => Argv_SAddr;
         Envp : Arg_Arr (1 .. Natural (Envp_Len))
            with Import, Address => Envp_SAddr;
         Args : Userland.Argument_Arr    (1 .. Argv'Length);
         Env  : Userland.Environment_Arr (1 .. Envp'Length);
      begin
         for I in Argv'Range loop
            declare
               Addr : constant System.Address :=
                  To_Address (Integer_Address (Argv (I)));
               Arg_Length : constant Natural := Lib.C_String_Length (Addr);
               Arg_String : String (1 .. Arg_Length)
                  with Import, Address => Addr;
            begin
               Args (I) := new String'(Arg_String);
            end;
         end loop;
         for I in Envp'Range loop
            declare
               Addr : constant System.Address :=
                  To_Address (Integer_Address (Envp (I)));
               Arg_Length : constant Natural := Lib.C_String_Length (Addr);
               Arg_String : String (1 .. Arg_Length)
                  with Import, Address => Addr;
            begin
               Env (I) := new String'(Arg_String);
            end;
         end loop;

         --  Create a new map for the process and reroll ASLR.
         Userland.Process.Flush_Exec_Files (Proc);
         Userland.Process.Reroll_ASLR (Proc);
         Set_Common_Map
            (Proc, Arch.MMU.Fork_Table (Arch.MMU.Kernel_Table));
         Set_Identifier (Proc, Args (1).all);

         --  Start the actual program.
         Succ := Userland.Loader.Start_Program
            (Exec_Path   => Path,
             FS          => Path_FS,
             Ino         => Path_Ino,
             Arguments   => Args,
             Environment => Env,
             Proc        => Proc);

         for Arg of Args loop
            Free (Arg);
         end loop;
         for En of Env loop
            Free (En);
         end loop;

         if Succ then
            Errno := Error_No_Error;
            Success := True;
            return;
         else
            Errno := Error_Bad_Access;
            Success := False;
            return;
         end if;
      end;
   end Exec_Into_Process;

   function Get_Mmap_Prot
      (Prot  : Unsigned_64;
       Perms : Unsigned_64) return Arch.MMU.Page_Permissions is
   begin
      return
         (Is_User_Accesible => True,
          Can_Read          => (Prot and PROT_READ)  /= 0,
          Can_Write         => (Prot and PROT_WRITE) /= 0,
          Can_Execute       => (Prot and PROT_EXEC)  /= 0,
          Is_Global         => False,
          Is_Write_Combine  => (Perms and MAP_WC) /= 0);
   end Get_Mmap_Prot;

   procedure Execute_MAC_Failure (Name : String; Curr_Proc : PID) is
      Discard    : Errno_Value;
      PID_Buffer : Lib.Messages.Translated_String;
      PID_Len    : Natural;
   begin
      case Get_Enforcement (Curr_Proc) is
         when MAC.Deny =>
            null;
         when MAC.Deny_And_Scream =>
            Lib.Messages.Image
               (Unsigned_32 (Convert (Curr_Proc)), PID_Buffer, PID_Len);
            Lib.Messages.Put_Line (PID_Buffer & " MAC failure " & Name);
         when MAC.Kill =>
            --  TODO: Kill and not exit, once we have such a thing.
            --  The semantics of SIGTERM and SIGKILL matter.
         --  https://linuxhandbook.com/content/images/2020/06/dont-sigkill.jpeg
            Do_Exit (Curr_Proc, 42);
      end case;
   end Execute_MAC_Failure;

   procedure PTY_IOCTL
      (P        : IPC.PTY.Inner_Acc;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out Boolean)
   is
      Result_Info : TermIOs.Main_Data with Import, Address => Argument;
      Result_Size : TermIOs.Win_Size  with Import, Address => Argument;
   begin
      Success := True;
      case Request is
         when TermIOs.TCGETS =>
            IPC.PTY.Get_TermIOs (P, Result_Info);
         when TermIOs.TCSETS | TermIOs.TCSETSW | TermIOs.TCSETSF =>
            IPC.PTY.Set_TermIOs (P, Result_Info);
         when TermIOs.TIOCGWINSZ =>
            IPC.PTY.Get_WinSize (P, Result_Size);
         when TermIOs.TIOCSWINSZ =>
            IPC.PTY.Set_WinSize (P, Result_Size);
         when others =>
            Success := False;
      end case;
   end PTY_IOCTL;

   procedure Set_MAC_Capabilities (Proc : PID; Bits : Unsigned_64) is
      Caps : constant MAC.Capabilities := Get_Capabilities (Proc);
   begin
      Set_Capabilities
         (Proc,
          (Caps.Can_Change_Scheduling and ((Bits and MAC_CAP_SCHED)   /= 0),
           Caps.Can_Spawn_Others      and ((Bits and MAC_CAP_SPAWN)   /= 0),
           Caps.Can_Access_Entropy    and ((Bits and MAC_CAP_ENTROPY) /= 0),
           Caps.Can_Modify_Memory     and ((Bits and MAC_CAP_SYS_MEM) /= 0),
           Caps.Can_Use_Networking    and ((Bits and MAC_CAP_USE_NET) /= 0),
           Caps.Can_Manage_Networking and ((Bits and MAC_CAP_SYS_NET) /= 0),
           Caps.Can_Manage_Mounts     and ((Bits and MAC_CAP_SYS_MNT) /= 0),
           Caps.Can_Manage_Power      and ((Bits and MAC_CAP_SYS_PWR) /= 0),
           Caps.Can_Trace_Children    and ((Bits and MAC_CAP_PTRACE)  /= 0),
           Caps.Can_Change_UIDs       and ((Bits and MAC_CAP_SETUID)  /= 0),
           Caps.Can_Manage_MAC        and ((Bits and MAC_CAP_SYS_MAC) /= 0),
           Caps.Can_Use_Clocks        and ((Bits and MAC_CAP_CLOCK)   /= 0)));
   end Set_MAC_Capabilities;

   procedure MAC_Syscall_To_Kernel
      (Val     : Unsigned_64;
       Success : out Boolean;
       Limit   : out MAC.Limit_Type)
   is
   begin
      case Val is
         when RLIMIT_CORE   => Limit := MAC.Core_Size_Limit;
         when RLIMIT_CPU    => Limit := MAC.CPU_Time_Limit;
         when RLIMIT_DATA   => Limit := MAC.Data_Size_Limit;
         when RLIMIT_FSIZE  => Limit := MAC.File_Size_Limit;
         when RLIMIT_NOFILE => Limit := MAC.Opened_File_Limit;
         when RLIMIT_STACK  => Limit := MAC.Stack_Size_Limit;
         when RLIMIT_AS     => Limit := MAC.Memory_Size_Limit;
         when others => Limit := MAC.Opened_File_Limit; Success := False;
      end case;
      Success := True;
   end MAC_Syscall_To_Kernel;

   procedure Check_Add_File
      (Process : PID;
       File    : File_Description_Acc;
       Success : out Boolean;
       FD      : out Natural;
       Start   : Natural := 0)
   is
   begin
      if Unsigned_64 (Get_File_Count (Process)) <
         Unsigned_64 (Get_Limit (Process, MAC.Opened_File_Limit))
      then
         Add_File (Process, File, FD, Success, Start);
      else
         FD      := 0;
         Success := False;
      end if;
   end Check_Add_File;

   function Check_Userland_Access
      (Map        : Arch.MMU.Page_Table_Acc;
       Addr       : Memory.Virtual_Address;
       Byte_Count : Unsigned_64) return Boolean
   is
      package Al is new Lib.Alignment (Unsigned_64);
      Length : Unsigned_64;
      Result : System.Address;
      Is_Mapped, Is_Readable, Is_Writeable, Is_Executable : Boolean;
      Is_User_Accessible : Boolean;
   begin
      Length := Al.Align_Up (Byte_Count + 1, Arch.MMU.Page_Size);
      Arch.MMU.Translate_Address
         (Map                => Map,
          Virtual            => To_Address (Addr),
          Length             => Storage_Count (Length),
          Physical           => Result,
          Is_Mapped          => Is_Mapped,
          Is_User_Accessible => Is_User_Accessible,
          Is_Readable        => Is_Readable,
          Is_Writeable       => Is_Writeable,
          Is_Executable      => Is_Executable);
      return Is_User_Accessible;
   end Check_Userland_Access;

   function Check_Userland_Mappability
      (Map        : Arch.MMU.Page_Table_Acc;
       Addr       : Memory.Virtual_Address;
       Byte_Count : Unsigned_64) return Boolean
   is
      Result : System.Address;
      Is_Mapped, Is_Readable, Is_Writeable, Is_Executable : Boolean;
      Is_User_Accessible : Boolean;
   begin
      if Addr + Virtual_Address (Byte_Count) > Memory_Offset then
         return False;
      end if;

      Arch.MMU.Translate_Address
         (Map                => Map,
          Virtual            => To_Address (Addr),
          Length             => Storage_Count (Byte_Count),
          Physical           => Result,
          Is_Mapped          => Is_Mapped,
          Is_User_Accessible => Is_User_Accessible,
          Is_Readable        => Is_Readable,
          Is_Writeable       => Is_Writeable,
          Is_Executable      => Is_Executable);
      return not Is_Mapped;
   end Check_Userland_Mappability;

   procedure Resolve_AT_Directive
      (Proc   : PID;
       Dir_FD : Unsigned_64;
       FS     : out VFS.FS_Handle;
       Ino    : out VFS.File_Inode_Number)
   is
      Descr : File_Description_Acc;
   begin
      if Dir_FD = AT_FDCWD then
         Process.Get_CWD (Proc, FS, Ino);
      else
         Descr := Get_File (Proc, Dir_FD);
         if Descr = null or else Descr.Description /= Description_Inode then
            FS  := VFS.Error_Handle;
            Ino := 0;
         else
            FS  := Descr.Inner_Ino_FS;
            Ino := Descr.Inner_Ino;
         end if;
      end if;
   end Resolve_AT_Directive;
end Userland.Syscall;
