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
with Lib.Panic;
with Networking;
with Userland.Loader;
with VFS; use VFS;
with Scheduler; use Scheduler;
with Memory.Virtual; use Memory.Virtual;
with Memory.Physical;
with Memory; use Memory;
with Ada.Unchecked_Deallocation;
with Arch.Hooks;
with Arch.Local;
with Cryptography.Random;
with Userland.MAC;
with IPC.FIFO; use IPC.FIFO;
with IPC.PTY;  use IPC.PTY;
with Devices;  use Devices;
with Userland.Integrity;
with Devices.TermIOs;
with Arch.Power;

package body Userland.Syscall with SPARK_Mode => Off is
   procedure Sys_Exit (Code : Unsigned_64; Errno : out Errno_Value) is
      Proc : constant PID := Arch.Local.Get_Current_Process;
   begin
      Errno := Error_No_Error;
      Do_Exit (Proc, Unsigned_8 (Code and 16#FF#));
   end Sys_Exit;

   function Arch_PRCtl
      (Code     : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map   : constant     Page_Map_Acc := Get_Common_Map (Proc);
      I_Arg : constant  Integer_Address := Integer_Address (Argument);
      S_Arg : constant   System.Address := To_Address (I_Arg);
   begin
      if not Check_Userland_Access (Map, I_Arg, 8) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Code > Unsigned_64 (Natural'Last) then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      if Arch.Hooks.PRCTL_Hook (Natural (Code), S_Arg) then
         Errno := Error_No_Error;
         return 0;
      else
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;
   end Arch_PRCtl;

   function Open
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant   System.Address := To_Address (Path_IAddr);
      Curr_Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Map_Acc := Get_Common_Map (Curr_Proc);
      Do_Close_On_Exec  : constant Boolean := (Flags and O_CLOEXEC)  /= 0;
      Do_Read           : constant Boolean := (Flags and O_RDONLY)   /= 0;
      Do_Write          : constant Boolean := (Flags and O_WRONLY)   /= 0;
      Dont_Follow       : constant Boolean := (Flags and O_NOFOLLOW) /= 0;
      Do_Append         : constant Boolean := (Flags and O_APPEND)   /= 0;

      Discard      : Boolean;
      Success      : VFS.FS_Status;
      Final_Path   : String (1 .. 1024);
      Final_Path_L : Natural;
      CWD          : String (1 .. Process.Max_CWD_Length);
      CWD_Len      : Natural;
      Opened_FS    : VFS.FS_Handle;
      Opened_Ino   : VFS.File_Inode_Number;
      Opened_Dev   : Devices.Device_Handle;
      Opened_Stat  : VFS.File_Stat;
      New_Descr    : File_Description_Acc;
      File_Perms   : MAC.Permissions;
      Returned_FD  : Natural;
      User         : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif Dir_FD /= AT_FDCWD then
         Errno := Error_Not_Implemented;
         return Unsigned_64'Last;
      end if;

      declare
         Path : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         Process.Get_CWD (Curr_Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Path,
             Result    => Final_Path,
             Count     => Final_Path_L);
         if Final_Path_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;
      end;

      Userland.Process.Get_Effective_UID (Curr_Proc, User);

      if Final_Path_L > 5 and then Final_Path (1 .. 5) = "/dev/" then
         Opened_Dev := Devices.Fetch (Final_Path (6 .. Final_Path_L));
         if Opened_Dev = Devices.Error_Handle then
            Errno := Error_No_Entity;
            return Unsigned_64'Last;
         end if;

         File_Perms := MAC.Check_Permissions (Get_MAC (Curr_Proc), Opened_Dev);
         New_Descr  := new File_Description'
            (Children_Count  => 0,
             Description     => Description_Device,
             Inner_Dev_Read  => Do_Read,
             Inner_Dev_Write => Do_Write,
             Inner_Dev_Pos   => 0,
             Inner_Dev       => Opened_Dev);
      else
         VFS.Open (Final_Path (1 .. Final_Path_L), Opened_FS, Opened_Ino,
                   Success, User, not Dont_Follow);
         if Success /= VFS.FS_Success then
            Errno := Error_No_Entity;
            return Unsigned_64'Last;
         end if;

         if Do_Append then
            VFS.Stat (Opened_FS, Opened_Ino, Opened_Stat, Success, User);
         else
            Opened_Stat.Byte_Size := 0;
         end if;

         File_Perms := MAC.Check_Permissions (Get_MAC (Curr_Proc), Opened_FS,
                                              Opened_Ino);
         New_Descr  := new File_Description'
            (Children_Count  => 0,
             Description     => Description_Inode,
             Inner_Ino_Read  => Do_Read,
             Inner_Ino_Write => Do_Write,
             Inner_Ino_FS    => Opened_FS,
             Inner_Ino_Pos   => Opened_Stat.Byte_Size,
             Inner_Ino       => Opened_Ino);
      end if;

      if (not Do_Read   and not Do_Write)             or
         (Do_Read       and not File_Perms.Can_Read)  or
         (Do_Write      and not File_Perms.Can_Write) or
         (not Do_Append and File_Perms.Can_Append_Only)
      then
         Close (New_Descr);
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("open", Curr_Proc);
         return Unsigned_64'Last;
      elsif Userland.Process.Add_File (Curr_Proc, New_Descr, Returned_FD) then
         Process.Set_Close_On_Exec (Curr_Proc, Unsigned_64 (Returned_FD),
                                    Do_Close_On_Exec);
         Errno := Error_No_Error;
         return Unsigned_64 (Returned_FD);
      else
         Close (New_Descr);
         Errno := Error_Too_Many_Files;
         return Unsigned_64'Last;
      end if;
   end Open;

   function Close
      (File_D : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Curr : constant PID := Arch.Local.Get_Current_Process;
   begin
      if not Userland.Process.Is_Valid_File (Curr, File_D) then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Remove_File (Curr, Natural (File_D));
      Errno := Error_No_Error;
      return 0;
   end Close;

   function Read
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Buf_IAddr : constant  Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant   System.Address := To_Address (Buf_IAddr);
      Curr_Proc : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Map_Acc := Get_Common_Map (Curr_Proc);
      Final_Cnt : constant          Natural := Natural (Count);
      File      : File_Description_Acc;
      Data      : Devices.Operation_Data (1 .. Final_Cnt)
         with Import, Address => Buf_SAddr;
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success2  : IPC.FIFO.Pipe_Status;
      Success3  : Boolean;
      User      : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      File := Userland.Process.Get_File (Curr_Proc, File_D);
      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Curr_Proc, User);

      case File.Description is
         when Description_Device =>
            if not File.Inner_Dev_Read then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;
            Devices.Read (File.Inner_Dev, File.Inner_Dev_Pos, Data, Ret_Count,
                          Success3);
            if Success3 then
               File.Inner_Dev_Pos := File.Inner_Dev_Pos +
                                     Unsigned_64 (Ret_Count);
               Errno := Error_No_Error;
               return Unsigned_64 (Ret_Count);
            else
               Errno := Error_IO;
               return Unsigned_64'Last;
            end if;
         when Description_Inode =>
            if not File.Inner_Ino_Read then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;
            VFS.Read (File.Inner_Ino_FS, File.Inner_Ino, File.Inner_Ino_Pos,
                      Data, Ret_Count, Success1, User);
            File.Inner_Ino_Pos := File.Inner_Ino_Pos + Unsigned_64 (Ret_Count);
            return Translate_Status (Success1, Unsigned_64 (Ret_Count), Errno);
         when Description_Reader_FIFO =>
            IPC.FIFO.Read (File.Inner_Reader_FIFO, Data, Ret_Count, Success2);
            case Success2 is
               when IPC.FIFO.Pipe_Success =>
                  Errno := Error_No_Error;
                  return Unsigned_64 (Ret_Count);
               when IPC.FIFO.Broken_Failure =>
                  Errno := Error_Invalid_Value;
                  return Unsigned_64'Last;
               when IPC.FIFO.Would_Block_Failure =>
                  Errno := Error_Would_Block;
                  return Unsigned_64'Last;
            end case;
         when Description_Primary_PTY =>
            IPC.PTY.Read_Primary (File.Inner_Primary_PTY, Data, Ret_Count);
            Errno := Error_No_Error;
            return Unsigned_64 (Ret_Count);
         when Description_Secondary_PTY =>
            IPC.PTY.Read_Secondary (File.Inner_Secondary_PTY, Data, Ret_Count);
            Errno := Error_No_Error;
            return Unsigned_64 (Ret_Count);
         when Description_Writer_FIFO =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;
   end Read;

   function Write
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Buf_IAddr : constant  Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant   System.Address := To_Address (Buf_IAddr);
      Curr_Proc : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Map_Acc := Get_Common_Map (Curr_Proc);
      Final_Cnt : constant          Natural := Natural (Count);
      File      : File_Description_Acc;
      Data      : Devices.Operation_Data (1 .. Final_Cnt)
         with Import, Address => Buf_SAddr;
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success2  : IPC.FIFO.Pipe_Status;
      Success3  : Boolean;
      User      : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      File := Userland.Process.Get_File (Curr_Proc, File_D);
      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Process.Get_Effective_UID (Curr_Proc, User);

      case File.Description is
         when Description_Device =>
            if not File.Inner_Dev_Write then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;

            Devices.Write (File.Inner_Dev, File.Inner_Dev_Pos, Data, Ret_Count,
                           Success3);
            if Success3 then
               File.Inner_Dev_Pos := File.Inner_Dev_Pos +
                                     Unsigned_64 (Ret_Count);
               Errno := Error_No_Error;
               return Unsigned_64 (Ret_Count);
            else
               Errno := Error_IO;
               return Unsigned_64'Last;
            end if;
         when Description_Inode =>
            if not File.Inner_Ino_Write then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;
            VFS.Write (File.Inner_Ino_FS, File.Inner_Ino, File.Inner_Ino_Pos,
                       Data, Ret_Count, Success1, User);
            File.Inner_Ino_Pos := File.Inner_Ino_Pos + Unsigned_64 (Ret_Count);
            return Translate_Status (Success1, Unsigned_64 (Ret_Count), Errno);
         when Description_Writer_FIFO =>
            IPC.FIFO.Write (File.Inner_Writer_FIFO, Data, Ret_Count, Success2);
            case Success2 is
               when IPC.FIFO.Pipe_Success =>
                  Errno := Error_No_Error;
                  return Unsigned_64 (Ret_Count);
               when IPC.FIFO.Broken_Failure =>
                  Errno := Error_Invalid_Value;
                  return Unsigned_64'Last;
               when IPC.FIFO.Would_Block_Failure =>
                  Errno := Error_Would_Block;
                  return Unsigned_64'Last;
            end case;
         when Description_Primary_PTY =>
            IPC.PTY.Write_Primary (File.Inner_Primary_PTY, Data, Ret_Count);
            Errno := Error_No_Error;
            return Unsigned_64 (Ret_Count);
         when Description_Secondary_PTY =>
            IPC.PTY.Write_Secondary
               (File.Inner_Secondary_PTY, Data, Ret_Count);
            Errno := Error_No_Error;
            return Unsigned_64 (Ret_Count);
         when Description_Reader_FIFO =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;
   end Write;

   function Seek
      (File_D : Unsigned_64;
       Offset : Unsigned_64;
       Whence : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
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
         return Unsigned_64'Last;
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
      return Result;

   <<Invalid_Seek_Error>>
      Errno := Error_Invalid_Seek;
      return Unsigned_64'Last;

   <<Invalid_Value_Error>>
      Errno := Error_Invalid_Value;
      return Unsigned_64'Last;
   end Seek;

   function Mmap
      (Hint       : Unsigned_64;
       Length     : Unsigned_64;
       Protection : Unsigned_64;
       Flags      : Unsigned_64;
       File_D     : Unsigned_64;
       Offset     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Perms : constant Arch.MMU.Page_Permissions :=
         Get_Mmap_Prot (Protection, Flags);
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map   : constant Page_Map_Acc := Get_Common_Map (Proc);
      Final_Hint : Unsigned_64 := Hint;
      Ignored    : Virtual_Address;
      File       : File_Description_Acc;
      User       : Unsigned_32;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Modify_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mmap", Proc);
         return Unsigned_64'Last;
      end if;

      --  Check that we got a length.
      if Length = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      --  Check for our own hint if none was provided.
      if Hint = 0 then
         if (Flags and MAP_FIXED) /= 0 then
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
         else
            Final_Hint := Get_Alloc_Base (Proc);
            Set_Alloc_Base (Proc, Final_Hint + Length);
         end if;
      end if;

      --  Check the address is good.
      if not Check_Userland_Mappability (Virtual_Address (Final_Hint), Length)
      then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      --  Do mmap anon or pass it to the VFS.
      if (Flags and MAP_ANON) /= 0 then
         if Map_Memory_Backed_Region
            (Map,
             Virtual_Address (Final_Hint),
             Length,
             Perms,
             Ignored)
         then
            Errno := Error_No_Error;
            return Final_Hint;
         else
            Errno := Error_No_Memory;
            return Unsigned_64'Last;
         end if;
      else
         File := Get_File (Proc, File_D);
         Process.Get_Effective_UID (Proc, User);
         if User /= 0 then
            Errno := Error_Bad_Access;
            return Unsigned_64'Last;
         end if;

         if File.Description = Description_Device then
            if Devices.Mmap
               (Handle      => File.Inner_Dev,
                Address     => Virtual_Address (Final_Hint),
                Length      => Length,
                Flags       => Perms)
            then
               Errno := Error_No_Error;
               return Final_Hint;
            end if;
         end if;

         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;
   end Mmap;

   function Munmap
      (Address    : Unsigned_64;
       Length     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64
   is
      Proc : constant             PID := Arch.Local.Get_Current_Process;
      Map  : constant    Page_Map_Acc := Get_Common_Map (Proc);
      Addr : constant Virtual_Address := Virtual_Address (Address);
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Modify_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("munmap", Proc);
         return Unsigned_64'Last;
      end if;

      if Unmap_Range (Map, Addr, Length) then
         Errno := Error_No_Error;
         return 0;
      else
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;
   end Munmap;

   function Get_PID (Errno : out Errno_Value) return Unsigned_64 is
   begin
      Errno := Error_No_Error;
      return Unsigned_64 (Convert (Arch.Local.Get_Current_Process));
   end Get_PID;

   function Get_Parent_PID (Errno : out Errno_Value) return Unsigned_64 is
      Parent : constant PID := Get_Parent (Arch.Local.Get_Current_Process);
   begin
      Errno := Error_No_Error;
      return Unsigned_64 (Convert (Parent));
   end Get_Parent_PID;

   function Exec
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Acc);
      type Arg_Arr is array (Natural range <>) of Unsigned_64;

      Th      : constant    Scheduler.TID := Arch.Local.Get_Current_Thread;
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      Map     : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Tmp_Map : Memory.Virtual.Page_Map_Acc;
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      Path       : String (1 .. Natural (Path_Len))
         with Import, Address => Path_SAddr;
      Path_FS    : FS_Handle;
      Path_Ino   : File_Inode_Number;
      File_Perms : MAC.Permissions;
      Argv_IAddr : constant Integer_Address := Integer_Address (Argv_Addr);
      Argv_SAddr : constant  System.Address := To_Address (Argv_IAddr);
      Envp_IAddr : constant Integer_Address := Integer_Address (Envp_Addr);
      Envp_SAddr : constant  System.Address := To_Address (Envp_IAddr);
      User       : Unsigned_32;
      Success    : FS_Status;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) or
         not Check_Userland_Access (Map, Argv_IAddr, Argv_Len) or
         not Check_Userland_Access (Map, Envp_IAddr, Envp_Len)
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      Open (Path, Path_FS, Path_Ino, Success, User);
      if Success /= VFS.FS_Success then
         Errno := Error_No_Entity;
         return Unsigned_64'Last;
      end if;

      File_Perms := MAC.Check_Permissions (Get_MAC (Proc), Path_FS, Path_Ino);
      if not File_Perms.Can_Execute then
         VFS.Close (Path_FS, Path_Ino);
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("exec", Proc);
         return Unsigned_64'Last;
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

         --  Free state.
         Userland.Process.Flush_Threads (Proc);
         Userland.Process.Flush_Exec_Files (Proc);

         --  Create a new map for the process.
         Userland.Process.Reroll_ASLR (Proc);
         Set_Identifier (Proc, Args (1).all);

         Tmp_Map := Get_Common_Map (Proc);
         Set_Common_Map (Proc, Memory.Virtual.New_Map);

         --  Start the actual program.
         if not Userland.Loader.Start_Program (Path, Path_FS, Path_Ino, Args,
                                               Env, Proc)
         then
            Set_Common_Map (Proc, Tmp_Map);
            Errno := Error_Bad_Access;
            return Unsigned_64'Last;
         end if;

         for Arg of Args loop
            Free (Arg);
         end loop;
         for En of Env loop
            Free (En);
         end loop;

         --  Free critical state now that we know wont be running.
         Userland.Process.Remove_Thread (Proc, Th);
         Memory.Virtual.Delete_Map (Tmp_Map);
         Scheduler.Bail;
      end;
   end Exec;

   function Clone
      (Callback  : Unsigned_64;
       Call_Arg  : Unsigned_64;
       Stack     : Unsigned_64;
       Flags     : Unsigned_64;
       TLS_Addr  : Unsigned_64;
       GP_State  : Arch.Context.GP_Context;
       FP_State  : Arch.Context.FP_Context;
       Errno     : out Errno_Value) return Unsigned_64
   is
      pragma Unreferenced (Call_Arg);
      Parent  : PID := Arch.Local.Get_Current_Process;
      Child   : PID;
      New_TID : Scheduler.TID;
      Ret     : Unsigned_64;
      Id      : String (1 .. Process.Max_Name_Length);
      Id_Len  : Natural;

      Use_Parent : constant Boolean := (Flags and CLONE_PARENT) /= 0;
      Do_Thread  : constant Boolean := (Flags and CLONE_THREAD) /= 0;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Parent)).Can_Spawn_Others then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("clone", Parent);
         return Unsigned_64'Last;
      end if;

      if Use_Parent then
         Parent := Get_Parent (Parent);
         if Parent = Error_PID then
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
         end if;
      end if;

      if Do_Thread then
         Child   := Parent;
         New_TID := Create_User_Thread
            (Address    => Integer_Address (Callback),
             Map        => Get_Common_Map (Child),
             Stack_Addr => Stack,
             TLS_Addr   => TLS_Addr,
             PID        => Convert (Child));
         Ret := Unsigned_64 (New_TID);
      else
         Child := Create_Process (Parent);
         if Child = Error_PID then
            goto Block_Error;
         end if;

         Get_Identifier (Parent, Id, Id_Len);
         Set_Identifier (Child, Id (1 .. Id_Len));

         Set_Common_Map (Child, Fork_Map (Get_Common_Map (Parent)));
         if Get_Common_Map (Child) = null then
            goto Block_Error;
         end if;

         Duplicate_FD_Table (Parent, Child);
         New_TID := Scheduler.Create_User_Thread
            (GP_State => GP_State,
             FP_State => FP_State,
             Map      => Get_Common_Map (Child),
             PID      => Convert (Child),
             TCB      => Arch.Local.Fetch_TCB);
         Ret := Unsigned_64 (Convert (Child));
      end if;

      if New_TID = 0 or else not Add_Thread (Child, New_TID) then
         goto Block_Error;
      end if;

      Errno := Error_No_Error;
      return Ret;

   <<Block_Error>>
      Errno := Error_Would_Block;
      return Unsigned_64'Last;
   end Clone;

   function Wait
      (Waited_PID, Exit_Addr, Options : Unsigned_64;
       Errno                          : out Errno_Value) return Unsigned_64
   is
      --  TODO: Support things like WCONTINUE once signals work.

      Addr : constant Integer_Address  := Integer_Address (Exit_Addr);
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Map  :              Page_Map_Acc := Get_Common_Map (Proc);
      Exit_Value : Unsigned_32 with Address => To_Address (Addr), Import;
      Waited : PID;
      Final_Waited_PID : Unsigned_64 := Waited_PID;
      Dont_Hang : constant Boolean := (Options and WNOHANG) /= 0;
      Children       : Process.Children_Arr (1 .. 50);
      Children_Count : Natural;
      Did_Exit       : Boolean;
      Error_Code     : Unsigned_8;
   begin
      --  Fail on having to wait on the process group, we dont support that.
      if Waited_PID = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      --  Check whether there is anything to wait.
      Children_Count := Get_Children (Proc, Children);
      if Children_Count = 0 then
         Errno := Error_Child;
         return Unsigned_64'Last;
      end if;

      --  If -1, we have to wait for any of the children, else, wait for the
      --  passed PID.
      if Waited_PID = Unsigned_64 (Unsigned_32'Last) then
         loop
            for PID_Item of Children (1 .. Children_Count) loop
               Waited := PID_Item;
               if Waited /= Error_PID then
                  Check_Exit (Waited, Did_Exit, Error_Code);
                  if Did_Exit then
                     Final_Waited_PID := Unsigned_64 (Convert (PID_Item));
                     goto Waited_Exited;
                  end if;
               end if;
            end loop;

            if Dont_Hang then
               exit;
            end if;
            Scheduler.Yield;
         end loop;
      else
         --  Check the process is actually our child.
         if Get_Parent (Convert (Natural (Waited_PID))) /= Proc then
            Errno := Error_Child;
            return Unsigned_64'Last;
         end if;

         Waited := Userland.Process.Convert (Natural (Waited_PID));
         if Waited /= Error_PID then
            loop
               Check_Exit (Waited, Did_Exit, Error_Code);
               if Did_Exit then
                  Final_Waited_PID := Waited_PID;
                  goto Waited_Exited;
               end if;
               if Dont_Hang then
                  exit;
               end if;
               Scheduler.Yield;
            end loop;
         end if;
      end if;

      --  If we get here, it means we are not blocking, and that the
      --  process has not exited, so lets return what we have to.
      Errno := Error_No_Error;
      return 0;

   <<Waited_Exited>>
      --  Set the return value if we are to.
      if Exit_Value'Address /= System.Null_Address then
         if not Check_Userland_Access (Map, Addr, 4) then
            Errno := Error_Would_Fault;
            return Unsigned_64'Last;
         end if;
         Exit_Value := Wait_EXITED or Unsigned_32 (Error_Code);
      end if;

      --  Now that we got the exit code, finally allow the process to die.
      Map := Get_Common_Map (Waited);
      Memory.Virtual.Delete_Map       (Map);
      Userland.Process.Delete_Process (Waited);
      Errno := Error_No_Error;
      return Final_Waited_PID;
   end Wait;

   function Uname
      (Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Proc     : constant PID := Arch.Local.Get_Current_Process;
      Map      : constant     Page_Map_Acc := Get_Common_Map (Proc);
      IAddr    : constant Integer_Address  := Integer_Address (Address);
      SAddr    : constant  System.Address  := To_Address (IAddr);
      UTS      : UTS_Name with Import, Address => SAddr;
      Host_Len : Networking.Hostname_Len;
   begin
      if not Check_Userland_Access (Map, IAddr, UTS'Size / 8) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

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

      Errno := Error_No_Error;
      return 0;
   end Uname;

   function Set_Hostname
      (Address : Unsigned_64;
       Length  : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      Map     : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Len     : constant          Natural := Natural (Length);
      IAddr   : constant  Integer_Address := Integer_Address (Address);
      SAddr   : constant   System.Address := To_Address (IAddr);
      Name    : String (1 .. Len) with Import, Address => SAddr;
      Success : Boolean;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Manage_Networking then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_hostname", Proc);
         return Unsigned_64'Last;
      end if;

      if not Check_Userland_Access (Map, IAddr, Length) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      Networking.Set_Hostname (Name, Success);

      if not Success then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Set_Hostname;

   function FStat
      (FD        : Unsigned_64;
       Stat_Addr : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Stat_IAddr : constant  Integer_Address := Integer_Address (Stat_Addr);
      Stat_SAddr : constant   System.Address := To_Address (Stat_IAddr);
      File_Desc  : constant File_Description_Acc := Get_File (Proc, FD);
      Stat_Val   : VFS.File_Stat;
      ID         : Natural;
      Success    : VFS.FS_Status;
      Stat_Buf   : Stat with Import, Address => Stat_SAddr;
      User       : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Stat_IAddr, Stat'Size / 8) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif File_Desc = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      case File_Desc.Description is
         when Description_Inode =>
            VFS.Stat (File_Desc.Inner_Ino_FS, File_Desc.Inner_Ino, Stat_Val,
                      Success, User);
            if Success /= VFS.FS_Success then
               Errno := Error_Bad_File;
               return Unsigned_64'Last;
            end if;

            ID := Get_Unique_ID (Get_Backing_Device (File_Desc.Inner_Ino_FS));
            Stat_Buf := (
               Device_Number => Unsigned_64 (ID),
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
               Block_Count   => Stat_Val.IO_Block_Count
            );

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
         when Description_Device =>
            ID := Devices.Get_Unique_ID (File_Desc.Inner_Dev);
            Stat_Buf := (
               Device_Number => Unsigned_64 (ID),
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
                  Unsigned_64 (Devices.Get_Block_Size (File_Desc.Inner_Dev)),
               Block_Count   => Devices.Get_Block_Count (File_Desc.Inner_Dev)
            );

            --  Set the access part of mode.
            if Devices.Is_Block_Device (File_Desc.Inner_Dev) then
               Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFBLK;
            else
               Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFCHR;
            end if;
         when Description_Reader_FIFO | Description_Writer_FIFO |
              Description_Primary_PTY | Description_Secondary_PTY =>
            Stat_Buf := (
               Device_Number => 0,
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
               Block_Count   => 1
            );
      end case;

      Errno := Error_No_Error;
      return 0;
   end FStat;

   function Get_CWD
      (Buffer : Unsigned_64;
       Length : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map   : constant     Page_Map_Acc := Get_Common_Map (Proc);
      IAddr : constant  Integer_Address := Integer_Address (Buffer);
      SAddr : constant   System.Address := To_Address (IAddr);
      Len   :                   Natural := Natural (Length);
      Path  : String (1 .. Len) with Import, Address => SAddr;
   begin
      if not Check_Userland_Access (Map, IAddr, Length) then
         Errno := Error_Would_Fault;
         return 0;
      end if;
      if Len = 0 then
         Errno := Error_Invalid_Value;
         return 0;
      end if;

      Get_CWD (Proc, Path, Len);
      if Len <= Path'Length then
         Errno := Error_No_Error;
         return Buffer;
      else
         Errno := Error_Not_Big_Enough;
         return 0;
      end if;
   end Get_CWD;

   function Chdir
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      SAddr : constant   System.Address := To_Address (IAddr);
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map   : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Final_Path   : String (1 .. 1024);
      Final_Path_L : Natural;
      CWD          : String (1 .. Process.Max_CWD_Length);
      CWD_Len      : Natural;
      File_FS      : FS_Handle;
      File_Ino     : File_Inode_Number;
      Success      : FS_Status;
      User         : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      end if;

      declare
         Path : String (1 .. Natural (Path_Len)) with Import, Address => SAddr;
      begin
         Process.Get_CWD (Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Path,
             Result    => Final_Path,
             Count     => Final_Path_L);
         if Final_Path_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;
      end;

      Userland.Process.Get_Effective_UID (Proc, User);
      Open (Final_Path (1 .. Final_Path_L), File_FS, File_Ino, Success, User,
            False);
      if Success /= VFS.FS_Success then
         Errno := Error_No_Entity;
         return Unsigned_64'Last;
      end if;

      Close (File_FS, File_Ino);
      if Set_CWD (Proc, Final_Path (1 .. Final_Path_L)) then
         Errno := Error_No_Error;
         return 0;
      else
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      end if;
   end Chdir;

   function IOCTL
      (FD       : Unsigned_64;
       Request  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      I_Arg : constant      Integer_Address := Integer_Address (Argument);
      S_Arg : constant       System.Address := To_Address (I_Arg);
      Proc  : constant     PID := Arch.Local.Get_Current_Process;
      Map   : constant         Page_Map_Acc := Get_Common_Map (Proc);
      File  : constant File_Description_Acc := Get_File (Proc, FD);
      Succ  : Boolean;
      FSSuc : VFS.FS_Status;
      User  : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, I_Arg, 8) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif File = null then
         Errno := Error_Not_A_TTY;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      case File.Description is
         when Description_Inode =>
            if File.Inner_Ino_Read and File.Inner_Ino_Write then
               FSSuc := VFS.IO_Control (File.Inner_Ino_FS, File.Inner_Ino,
                                        Request, S_Arg, User);
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
         return 0;
      else
         Errno := Error_Not_A_TTY;
         return Unsigned_64'Last;
      end if;
   end IOCTL;

   function Sched_Yield (Errno : out Errno_Value) return Unsigned_64 is
   begin
      Scheduler.Yield;
      Errno := Error_No_Error;
      return 0;
   end Sched_Yield;

   function Set_Deadlines
      (Run_Time, Period : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Thre : constant    Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("setdeadlines", Proc);
         return Unsigned_64'Last;
      elsif not Scheduler.Set_Deadlines
         (Thre, Positive (Run_Time), Positive (Period))
      then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Set_Deadlines;

   function Pipe
      (Result_Addr : Unsigned_64;
       Flags       : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Ad   : constant Integer_Address  := Integer_Address (Result_Addr);
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Map  : constant Page_Map_Acc     := Get_Common_Map (Proc);
      Res  : array (1 .. 2) of Integer with Import, Address => To_Address (Ad);
      Returned : IPC.FIFO.Inner_Acc;
      Reader_Desc, Writer_Desc : File_Description_Acc;
   begin
      if not Check_Userland_Access (Map, Ad, Res'Size / 8) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      Returned := IPC.FIFO.Create ((Flags and O_NONBLOCK) = 0);
      Reader_Desc := new File_Description'(
         Children_Count    => 0,
         Description       => Description_Reader_FIFO,
         Inner_Reader_FIFO => Returned
      );
      Writer_Desc := new File_Description'(
         Children_Count    => 0,
         Description       => Description_Writer_FIFO,
         Inner_Writer_FIFO => Returned
      );
      if not Userland.Process.Add_File (Proc, Reader_Desc, Res (1)) or
         not Userland.Process.Add_File (Proc, Writer_Desc, Res (2))
      then
         Close (Returned);
         Close (Reader_Desc);
         Close (Writer_Desc);
         Errno := Error_Too_Many_Files;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Pipe;

   function Get_UID (Errno : out Errno_Value) return Unsigned_64 is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Ret  : Unsigned_32;
   begin
      Userland.Process.Get_UID (Proc, Ret);
      Errno := Error_No_Error;
      return Unsigned_64 (Ret);
   end Get_UID;

   function Rename
      (Source_FD   : Unsigned_64;
       Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Target_FD   : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       Flags       : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Src_IAddr : constant  Integer_Address := Integer_Address (Source_Addr);
      Src_SAddr : constant   System.Address := To_Address (Src_IAddr);
      Tgt_IAddr : constant  Integer_Address := Integer_Address (Target_Addr);
      Tgt_SAddr : constant   System.Address := To_Address (Tgt_IAddr);
      Do_Keep   : constant Boolean := (Flags and RENAME_NOREPLACE) /= 0;
      CWD           : String (1 .. Process.Max_CWD_Length);
      CWD_Len       : Natural;
      Source_Path   : String (1 .. 1024);
      Source_Path_L : Natural;
      Target_Path   : String (1 .. 1024);
      Target_Path_L : Natural;
      Success       : VFS.FS_Status;
      User          : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Src_IAddr, Source_Len) or
         not Check_Userland_Access (Map, Tgt_IAddr, Target_Len)
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Source_Len > Unsigned_64 (Natural'Last) or
            Target_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif Source_FD /= AT_FDCWD or Target_FD /= AT_FDCWD then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      declare
         Src : String (1 .. Natural (Source_Len))
            with Import, Address => Src_SAddr;
         Tgt : String (1 .. Natural (Target_Len))
            with Import, Address => Tgt_SAddr;
      begin
         Process.Get_CWD (Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Src,
             Result    => Source_Path,
             Count     => Source_Path_L);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Tgt,
             Result    => Target_Path,
             Count     => Target_Path_L);
         if Source_Path_L = 0 or Target_Path_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;

         Userland.Process.Get_Effective_UID (Proc, User);
         VFS.Rename
            (Source_Path (1 .. Source_Path_L),
             Target_Path (1 .. Target_Path_L),
             Do_Keep,
             Success,
             User);
         return Translate_Status (Success, 0, Errno);
      end;
   end Rename;

   function Sysconf
      (Request : Unsigned_64;
       Addr    : Unsigned_64;
       Length  : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Proc   : constant PID := Arch.Local.Get_Current_Process;
      Map    : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Stats  : Memory.Physical.Statistics;
      Result : Unsigned_64;
   begin
      case Request is
         when SC_PAGESIZE =>
            Result := Page_Size;
         when SC_OPEN_MAX =>
            Result := Unsigned_64 (Process.Max_File_Count);
         when SC_HOST_NAME_MAX =>
            Result := Networking.Hostname_Max_Len;
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
         when SC_LIST_PROCS =>
            declare
               IAddr : constant Integer_Address := Integer_Address (Addr);
               SAddr : constant  System.Address := To_Address (IAddr);
               Len   : constant Natural :=
                  Natural (Length / (Process_Info'Size / 8));
               Procs : Proc_Info_Arr (1 .. Len) with Import, Address => SAddr;
               KProc : Process_Info_Arr (1 .. Len);
               Ret   : Natural;
            begin
               if not Check_Userland_Access (Map, IAddr, Length) then
                  Errno := Error_Would_Fault;
                  return Unsigned_64'Last;
               end if;

               List_All (KProc, Ret);
               for I in 1 .. Ret loop
                  Procs (I) :=
                     (Identifier  => KProc (I).Identifier,
                      Id_Len      => Unsigned_16 (KProc (I).Identifier_Len),
                      Parent_PID  => Unsigned_16 (Convert (KProc (I).Parent)),
                      Process_PID => Unsigned_16 (Convert (KProc (I).Process)),
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
               IAddr : constant Integer_Address := Integer_Address (Addr);
               SAddr : constant  System.Address := To_Address (IAddr);
               Len   : constant Natural :=
                  Natural (Length / (Mount_Info'Size / 8));
               Mnts  : Mount_Info_Arr (1 .. Len) with Import, Address => SAddr;
               KMnts : Mountpoint_Info_Arr (1 .. Len);
               Ret   : Natural;
            begin
               if not Check_Userland_Access (Map, IAddr, Length) then
                  Errno := Error_Would_Fault;
                  return Unsigned_64'Last;
               end if;

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
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      Errno := Error_No_Error;
      return Result;
   end Sysconf;

   function Spawn
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Argv_Addr : Unsigned_64;
       Argv_Len  : Unsigned_64;
       Envp_Addr : Unsigned_64;
       Envp_Len  : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Acc);
      type Arg_Arr is array (Natural range <>) of Unsigned_64;

      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant Page_Map_Acc     := Get_Common_Map (Proc);
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      Path       : String (1 .. Natural (Path_Len))
         with Import, Address => Path_SAddr;
      Path_FS    : FS_Handle;
      Path_Ino   : File_Inode_Number;
      Success    : FS_Status;
      Succ       : Boolean;
      File_Perms : MAC.Permissions;
      Child      : PID;
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
         return Unsigned_64'Last;
      end if;

      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Spawn_Others then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("spawn", Proc);
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      Open (Path, Path_FS, Path_Ino, Success, User);
      if Success /= VFS.FS_Success then
         Errno := Error_No_Entity;
         return Unsigned_64'Last;
      end if;

      File_Perms := MAC.Check_Permissions (Get_MAC (Proc), Path_FS, Path_Ino);
      if not File_Perms.Can_Execute then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("spawn", Proc);
         return Unsigned_64'Last;
      end if;

      Child := Create_Process (Proc);
      if Child = Error_PID then
         Errno := Error_Would_Block;
         return Unsigned_64'Last;
      end if;

      Duplicate_FD_Table (Proc, Child);

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

         --  Create a new map for the process.
         Userland.Process.Flush_Exec_Files (Child);
         Userland.Process.Reroll_ASLR (Child);
         Set_Common_Map (Child, Memory.Virtual.New_Map);

         --  Start the actual program.
         Succ := Userland.Loader.Start_Program
            (Exec_Path   => Path,
             FS          => Path_FS,
             Ino         => Path_Ino,
             Arguments   => Args,
             Environment => Env,
             Proc        => Child);

         for Arg of Args loop
            Free (Arg);
         end loop;
         for En of Env loop
            Free (En);
         end loop;

         if Succ then
            Errno := Error_No_Error;
            return Unsigned_64 (Convert (Child));
         else
            Errno := Error_Bad_Access;
            return Unsigned_64'Last;
         end if;
      end;
   end Spawn;

   function Get_Thread_Sched
      (Errno : out Errno_Value) return Unsigned_64
   is
      Ret  : Unsigned_64            := 0;
      Curr : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      if Scheduler.Is_Mono_Thread (Curr) then
         Ret := Ret or Thread_MONO;
      end if;

      Errno := Error_No_Error;
      return Ret;
   end Get_Thread_Sched;

   function Set_Thread_Sched
      (Flags : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant           PID := Arch.Local.Get_Current_Process;
      Curr : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_thread_sched", Proc);
         return Unsigned_64'Last;
      end if;

      Scheduler.Set_Mono_Thread (Curr, (Flags and Thread_MONO) /= 0);
      Errno := Error_No_Error;
      return 0;
   end Set_Thread_Sched;

   function Fcntl
      (FD       : Unsigned_64;
       Command  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      File      : constant File_Description_Acc := Get_File (Proc, FD);
      Temp      : Boolean;
      Returned  : Unsigned_64 := 0;
      New_File  : File_Description_Acc;
      Result_FD : Natural;
   begin
      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      case Command is
         when F_DUPFD | F_DUPFD_CLOEXEC =>
            New_File := Duplicate (File);
            if Add_File (Proc, New_File, Result_FD, Natural (Argument)) then
               Returned               := Unsigned_64 (Result_FD);
               Process.Set_Close_On_Exec (Proc, Unsigned_64 (Result_FD),
                                          Command = F_DUPFD_CLOEXEC);
            else
               Errno := Error_Too_Many_Files;
               return Unsigned_64'Last;
            end if;
         when F_GETFD =>
            if Get_Close_On_Exec (Proc, FD) then
               Returned := FD_CLOEXEC;
            end if;
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
               when others =>
                  null;
            end case;
         when F_SETFD =>
            Process.Set_Close_On_Exec (Proc, FD,
               (Argument and FD_CLOEXEC) /= 0);
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
                     return Unsigned_64'Last;
                  end if;
               when Description_Writer_FIFO =>
                  Set_Size (File.Inner_Writer_FIFO, Natural (Argument), Temp);
                  if not Temp then
                     Errno := Error_Would_Block;
                     return Unsigned_64'Last;
                  end if;
               when others =>
                  goto Invalid_Return;
            end case;
         when others =>
            goto Invalid_Return;
      end case;

      Errno := Error_No_Error;
      return Returned;

   <<Invalid_Return>>
      Errno := Error_Invalid_Value;
      return Unsigned_64'Last;
   end Fcntl;

   procedure Exit_Thread (Errno : out Errno_Value) is
   begin
      Errno := Error_No_Error;
      Scheduler.Bail;
   end Exit_Thread;

   function Get_Random
     (Address : Unsigned_64;
      Length  : Unsigned_64;
      Errno   : out Errno_Value) return Unsigned_64
   is
      Proc   : constant PID := Arch.Local.Get_Current_Process;
      Map    : constant Page_Map_Acc     := Get_Common_Map (Proc);
      IAddr  : constant  Integer_Address := Integer_Address (Address);
      SAddr  : constant   System.Address := To_Address (IAddr);
      Result : Cryptography.Random.Crypto_Data (1 .. Natural (Length / 4))
         with Import, Address => SAddr;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Access_Entropy then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("getrandom", Proc);
         return Unsigned_64'Last;
      elsif not Check_Userland_Access (Map, IAddr, Length) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      else
         Cryptography.Random.Fill_Data (Result);
         Errno := Error_No_Error;
         return Result'Length * 4;
      end if;
   end Get_Random;

   function MProtect
     (Address    : Unsigned_64;
      Length     : Unsigned_64;
      Protection : Unsigned_64;
      Errno      : out Errno_Value) return Unsigned_64
   is
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map   : constant Page_Map_Acc     := Get_Common_Map (Proc);
      Flags : constant Arch.MMU.Page_Permissions :=
         Get_Mmap_Prot (Protection, 0);
      Addr  : constant Integer_Address := Integer_Address (Address);
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Modify_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mprotect", Proc);
         return Unsigned_64'Last;
      end if;

      if not Check_Userland_Access (Map, Addr, Length) or else
         not Remap_Range (Map, Addr, Length, Flags)
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end MProtect;

   function Set_MAC_Capabilities
      (Bits  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      P     :     constant PID := Arch.Local.Get_Current_Process;
      Perms :      MAC.Context := Get_MAC (P);
      Caps  : MAC.Capabilities := MAC.Get_Capabilities (Perms);
      S1  : constant Boolean := (Bits and MAC_CAP_SCHED)   /= 0;
      S2  : constant Boolean := (Bits and MAC_CAP_SPAWN)   /= 0;
      S3  : constant Boolean := (Bits and MAC_CAP_ENTROPY) /= 0;
      S4  : constant Boolean := (Bits and MAC_CAP_SYS_MEM) /= 0;
      S5  : constant Boolean := (Bits and MAC_CAP_USE_NET) /= 0;
      S6  : constant Boolean := (Bits and MAC_CAP_SYS_NET) /= 0;
      S7  : constant Boolean := (Bits and MAC_CAP_SYS_MNT) /= 0;
      S8  : constant Boolean := (Bits and MAC_CAP_SYS_PWR) /= 0;
      S9  : constant Boolean := (Bits and MAC_CAP_PTRACE)  /= 0;
      S10 : constant Boolean := (Bits and MAC_CAP_SETUID)  /= 0;
      S11 : constant Boolean := (Bits and MAC_CAP_SYS_MAC) /= 0;
   begin
      Caps :=
         (Can_Change_Scheduling => Caps.Can_Change_Scheduling and S1,
          Can_Spawn_Others      => Caps.Can_Spawn_Others      and S2,
          Can_Access_Entropy    => Caps.Can_Access_Entropy    and S3,
          Can_Modify_Memory     => Caps.Can_Modify_Memory     and S4,
          Can_Use_Networking    => Caps.Can_Use_Networking    and S5,
          Can_Manage_Networking => Caps.Can_Manage_Networking and S6,
          Can_Manage_Mounts     => Caps.Can_Manage_Mounts     and S7,
          Can_Manage_Power      => Caps.Can_Manage_Power      and S8,
          Can_Trace_Children    => Caps.Can_Trace_Children    and S9,
          Can_Change_UIDs       => Caps.Can_Change_UIDs       and S10,
          Can_Manage_MAC        => Caps.Can_Manage_MAC        and S11);

      MAC.Set_Capabilities (Perms, Caps);
      Set_MAC (P, Perms);
      Errno := Error_No_Error;
      return 0;
   end Set_MAC_Capabilities;

   function Get_MAC_Capabilities (Errno : out Errno_Value) return Unsigned_64
   is
      Pro  : constant              PID := Arch.Local.Get_Current_Process;
      Caps : constant MAC.Capabilities := MAC.Get_Capabilities (Get_MAC (Pro));
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
      return Res;
   end Get_MAC_Capabilities;

   function Add_MAC_Permissions
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      Proc   : constant             PID := Arch.Local.Get_Current_Process;
      Map    : constant    Page_Map_Acc := Get_Common_Map (Proc);
      Addr   : constant Integer_Address := Integer_Address (Path_Addr);
      Ctx    :              MAC.Context := Get_MAC (Proc);
      Perms  : MAC.Permissions;
      Status : MAC.Addition_Status;
      FS_Status : VFS.FS_Status;
      FS     : VFS.FS_Handle;
      Ino    : VFS.File_Inode_Number;
      Dev    : Devices.Device_Handle;
      Path   : String (1 .. Natural (Path_Len))
         with Import, Address => To_Address (Addr);
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Manage_MAC then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("add_mac_perms", Proc);
         return Unsigned_64'Last;
      elsif not Check_Userland_Access (Map, Addr, Path_Len) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
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
            return Unsigned_64'Last;
         end if;
         MAC.Add_Entity
            (Data   => Ctx,
             Dev    => Devices.Fetch (Path),
             Perms  => Perms,
             Status => Status);
      else
         VFS.Open (Path, FS, Ino, FS_Status, 0);
         if FS_Status /= VFS.FS_Success then
            return Translate_Status (FS_Status, 0, Errno);
         end if;
         MAC.Add_Entity
            (Data   => Ctx,
             FS     => FS,
             Ino    => Ino,
             Perms  => Perms,
             Status => Status);
         VFS.Close (FS, Ino);
      end if;

      case Status is
         when MAC.Success =>
            Set_MAC (Proc, Ctx);
            Errno := Error_No_Error;
            return 0;
         when MAC.No_Space       => Errno := Error_No_Memory;
         when MAC.Is_Conflicting => Errno := Error_Invalid_Value;
      end case;

      return Unsigned_64'Last;
   end Add_MAC_Permissions;

   function Set_MAC_Enforcement
      (Action : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Perms :  MAC.Context := Get_MAC (Proc);
      Act   : MAC.Enforcement;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Manage_MAC then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_mac_enforcement", Proc);
         return Unsigned_64'Last;
      end if;

      case Action is
         when MAC_DENY            => Act := MAC.Deny;
         when MAC_DENY_AND_SCREAM => Act := MAC.Deny_And_Scream;
         when MAC_KILL            => Act := MAC.Kill;
         when others              =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      MAC.Set_Enforcement (Perms, Act);
      Set_MAC (Proc, Perms);
      Errno := Error_No_Error;
      return 0;
   end Set_MAC_Enforcement;

   function Mount
      (Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       FSType      : Unsigned_64;
       Flags       : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Map_Acc := Get_Common_Map (Proc);
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
         return Unsigned_64'Last;
      elsif Source_Len > Unsigned_64 (Natural'Last) or
            Target_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Manage_Mounts then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mount", Proc);
         return Unsigned_64'Last;
      end if;

      case FSType is
         when MNT_EXT => Parsed_Typ := VFS.FS_EXT;
         when MNT_FAT => Parsed_Typ := VFS.FS_FAT;
         when MNT_QNX => Parsed_Typ := VFS.FS_QNX;
         when others  =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      declare
         Source : String (1 .. Natural (Source_Len))
            with Import, Address => Src_Addr;
         Target : String (1 .. Natural (Target_Len))
            with Import, Address => Tgt_Addr;
      begin
         if VFS.Mount (Source, Target, Parsed_Typ, Do_RO) then
            Errno := Error_No_Error;
            return 0;
         else
            Errno := Error_IO;
            return Unsigned_64'Last;
         end if;
      end;
   end Mount;

   function Umount
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      Curr_Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Map_Acc := Get_Common_Map (Curr_Proc);
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address  := To_Address (Path_IAddr);
      Flag_Force : constant Boolean := (Flags and MNT_FORCE) /= 0;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif not MAC.Get_Capabilities (Get_MAC (Curr_Proc)).Can_Manage_Mounts
      then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("umount", Curr_Proc);
         return Unsigned_64'Last;
      end if;

      declare
         Path : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         if VFS.Unmount (Path, Flag_Force) then
            Errno := Error_No_Error;
            return 0;
         else
            Errno := Error_Busy;
            return Unsigned_64'Last;
         end if;
      end;
   end Umount;

   function Readlink
      (Dir_FD      : Unsigned_64;
       Path_Addr   : Unsigned_64;
       Path_Len    : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Map  : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Path_IAddr   : constant Integer_Address := Integer_Address (Path_Addr);
      Buffer_IAddr : constant Integer_Address := Integer_Address (Buffer_Addr);
      Path_Add     : constant System.Address  := To_Address (Path_IAddr);
      Buffer_Add   : constant System.Address  := To_Address (Buffer_IAddr);
      Final_Path   : String (1 .. 1024);
      Final_Path_L : Natural;
      CWD          : String (1 .. Process.Max_CWD_Length);
      CWD_Len      : Natural;
      Opened_FS    : FS_Handle;
      Opened_Ino   : File_Inode_Number;
      Ret_Count    : Natural;
      User         : Unsigned_32;
      Status       : VFS.FS_Status;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) or
         not Check_Userland_Access (Map, Buffer_IAddr, Buffer_Len)
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Path_Len   > Unsigned_64 (Natural'Last) or
            Buffer_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif Dir_FD /= AT_FDCWD then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      end if;

      declare
         File_Perms : MAC.Permissions;
         Path : String (1 ..   Natural (Path_Len))
            with Import, Address => Path_Add;
         Data : String (1 .. Natural (Buffer_Len))
            with Import, Address => Buffer_Add;
      begin
         Process.Get_CWD (Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Path,
             Result    => Final_Path,
             Count     => Final_Path_L);
         if Final_Path_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;

         Userland.Process.Get_Effective_UID (Proc, User);

         Open (Final_Path (1 .. Final_Path_L), Opened_FS, Opened_Ino, Status,
               User, False);
         if Status /= VFS.FS_Success then
            Errno := Error_No_Entity;
            return Unsigned_64'Last;
         end if;

         File_Perms := MAC.Check_Permissions (Get_MAC (Proc), Opened_FS,
                                              Opened_Ino);
         if not File_Perms.Can_Read then
            Errno := Error_Bad_Access;
            Execute_MAC_Failure ("readlink", Proc);
            return Unsigned_64'Last;
         end if;

         VFS.Read_Symbolic_Link (Opened_FS, Opened_Ino, Data, Ret_Count,
                                 Status, User);
         Close (Opened_FS, Opened_Ino);
         return Translate_Status (Status, Unsigned_64 (Ret_Count), Errno);
      end;
   end Readlink;

   function GetDEnts
      (FD          : Unsigned_64;
       Buffer_Addr : Unsigned_64;
       Buffer_Len  : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Buff_IAddr : constant Integer_Address := Integer_Address (Buffer_Addr);
      Buff_Addr  : constant System.Address  := To_Address (Buff_IAddr);
      Buff_Len   : constant Unsigned_64     := Buffer_Len / (Dirent'Size / 8);
      Buffer     : Dirents (1 .. Buff_Len) with Import, Address => Buff_Addr;
      Tmp_Buffer : VFS.Directory_Entities (1 .. Natural (Buff_Len));
      Read_Len   : Natural;
      Success    : VFS.FS_Status;
      User       : Unsigned_32;
      Proc : constant     PID := Arch.Local.Get_Current_Process;
      Map  : constant         Page_Map_Acc := Get_Common_Map (Proc);
      File : constant File_Description_Acc := Get_File (Proc, FD);
   begin
      if not Check_Userland_Access (Map, Buff_IAddr, Buffer_Len) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif File = null or else File.Description /= Description_Inode then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      VFS.Read_Entries (File.Inner_Ino_FS, File.Inner_Ino, Tmp_Buffer,
                        Read_Len, Success, User);
      if Success /= VFS.FS_Success then
         Errno := Error_No_Entity;
         return Unsigned_64'Last;
      elsif Read_Len > Tmp_Buffer'Length then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      for I in 1 .. Read_Len loop
         Buffer (Unsigned_64 (I)) := (
            D_Ino    => Tmp_Buffer (I).Inode_Number,
            D_Off    => (Dirent'Size / 8) * Unsigned_64 (I),
            D_Reclen => Dirent'Size / 8,
            D_Type   => 0,
            D_Name   => (others => Ada.Characters.Latin_1.NUL)
         );
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

      Errno := Error_No_Error;
      return Unsigned_64 (Read_Len * (Dirent'Size / 8));
   end GetDEnts;

   function Sync (Errno : out Errno_Value) return Unsigned_64 is
   begin
      if not VFS.Synchronize then
         Errno := Error_IO;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Sync;

   function MakeNode
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Dev       : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      pragma Unreferenced (Dev);
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant   System.Address := To_Address (Path_IAddr);
      CWD          : String (1 .. Process.Max_CWD_Length);
      CWD_Len      : Natural;
      Final_Path   : String (1 .. 1024);
      Final_Path_L : Natural;
      Node_Type    : File_Type;
      Tmp_Mode     : constant File_Mode := File_Mode (Mode and 8#7777#);
      Status       : VFS.FS_Status;
      Umask        : VFS.File_Mode;
      User         : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif Dir_FD /= AT_FDCWD then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      declare
         Path : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         Process.Get_CWD (Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Path,
             Result    => Final_Path,
             Count     => Final_Path_L);
         if Final_Path_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;
      end;

      if (Mode and Stat_IFDIR) /= 0 then
         Node_Type := File_Directory;
      else
         Node_Type := File_Regular;
      end if;

      Userland.Process.Get_Umask         (Proc, Umask);
      Userland.Process.Get_Effective_UID (Proc, User);
      Create_Node
         (Path    => Final_Path (1 .. Final_Path_L),
          Typ     => Node_Type,
          Mode    => VFS.Apply_Umask (Tmp_Mode, Umask),
          Success => Status,
          User    => User);
      return Translate_Status (Status, 0, Errno);
   end MakeNode;

   function Unlink
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      Curr_Proc    : constant PID := Arch.Local.Get_Current_Process;
      Map          : constant    Page_Map_Acc := Get_Common_Map (Curr_Proc);
      Path_IAddr   : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr   : constant System.Address  := To_Address (Path_IAddr);
      CWD          : String (1 .. Process.Max_CWD_Length);
      CWD_Len      : Natural;
      Final_Path   : String (1 .. 1024);
      Final_Path_L : Natural;
      Success      : VFS.FS_Status;
      User         : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Path_Len > Unsigned_64 (Natural'Last) then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif Dir_FD /= AT_FDCWD then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      declare
         Path : String (1 .. Natural (Path_Len))
            with Import, Address => Path_SAddr;
      begin
         Process.Get_CWD (Curr_Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Path,
             Result    => Final_Path,
             Count     => Final_Path_L);
         if Final_Path_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;
      end;

      Userland.Process.Get_Effective_UID (Curr_Proc, User);
      VFS.Unlink (Final_Path (1 .. Final_Path_L), Success, User);
      return Translate_Status (Success, 0, Errno);
   end Unlink;

   function Truncate
      (FD       : Unsigned_64;
       New_Size : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      Proc    : constant     PID := Arch.Local.Get_Current_Process;
      File    : constant File_Description_Acc := Get_File (Proc, FD);
      Success : VFS.FS_Status;
      User    : Unsigned_32;
   begin
      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      case File.Description is
         when Description_Inode =>
            Success := VFS.Truncate (File.Inner_Ino_FS, File.Inner_Ino,
                                     New_Size, User);
            return Translate_Status (Success, 0, Errno);
         when others =>
            Errno := Error_Bad_File;
            return Unsigned_64'Last;
      end case;
   end Truncate;

   function Symlink
      (Dir_FD      : Unsigned_64;
       Path_Addr   : Unsigned_64;
       Path_Len    : Unsigned_64;
       Target_Addr : Unsigned_64;
       Target_Len  : Unsigned_64;
       Mode        : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map        : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Path_IAddr : constant  Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant   System.Address := To_Address (Path_IAddr);
      Targ_IAddr : constant  Integer_Address := Integer_Address (Target_Addr);
      Targ_SAddr : constant   System.Address := To_Address (Targ_IAddr);
      CWD          : String (1 .. Process.Max_CWD_Length);
      CWD_Len      : Natural;
      Final_Path   : String (1 .. 1024);
      Final_Path_L : Natural;
      Success      : VFS.FS_Status;
      User         : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Path_IAddr, Path_Len) or
         not Check_Userland_Access (Map, Targ_IAddr, Target_Len)
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Path_Len   > Unsigned_64 (Natural'Last) or
            Target_Len > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif Dir_FD /= AT_FDCWD then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      declare
         Path : String (1 ..   Natural (Path_Len))
            with Import, Address => Path_SAddr;
         Targ : String (1 .. Natural (Target_Len))
            with Import, Address => Targ_SAddr;
      begin
         Process.Get_CWD (Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Path,
             Result    => Final_Path,
             Count     => Final_Path_L);
         if Final_Path_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;

         Userland.Process.Get_Effective_UID (Proc, User);
         VFS.Create_Symbolic_Link
            (Final_Path (1 .. Final_Path_L),
             Targ, Unsigned_32 (Mode), Success, User);
         return Translate_Status (Success, 0, Errno);
      end;
   end Symlink;

   function Integrity_Setup
      (Command  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      P : Integrity.Policy;
   begin
      case Command is
         when INTEGRITY_SET_POLICY =>
            case Argument is
               when INTEGRITY_POLICY_WARN  => P := Integrity.Policy_Warn;
               when INTEGRITY_POLICY_PANIC => P := Integrity.Policy_Panic;
               when others                 => goto Error;
            end case;
            Integrity.Set_Policy (P);
         when INTEGRITY_ONESHOT =>
            Integrity.Run_Checks;
         when INTEGRITY_FREE_MEMORY =>
            Integrity.Set_Free_Memory (Memory.Size (Argument));
         when INTEGRITY_MAX_PROC =>
            if Argument > Unsigned_64 (Process.Max_Process_Count) then
               Integrity.Set_Max_Processes_Check (Process.Max_Process_Count);
            else
               Integrity.Set_Max_Processes_Check (Natural (Argument));
            end if;
         when others =>
            goto Error;
      end case;

      Errno := Error_No_Error;
      return 0;

   <<Error>>
      Errno := Error_Invalid_Value;
      return Unsigned_64'Last;
   end Integrity_Setup;

   function Open_PTY
      (Result_Addr  : Unsigned_64;
       Termios_Addr : Unsigned_64;
       Window_Addr  : Unsigned_64;
       Errno        : out Errno_Value) return Unsigned_64
   is
      Res_IAddr : constant  Integer_Address := Integer_Address (Result_Addr);
      Res_SAddr : constant   System.Address := To_Address (Res_IAddr);
      TIO_IAddr : constant  Integer_Address := Integer_Address (Termios_Addr);
      TIO_SAddr : constant   System.Address := To_Address (TIO_IAddr);
      Win_IAddr : constant  Integer_Address := Integer_Address (Window_Addr);
      Win_SAddr : constant   System.Address := To_Address (Win_IAddr);
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Map_Acc := Get_Common_Map (Proc);

      Result_PTY     : IPC.PTY.Inner_Acc;
      Primary_Desc   : File_Description_Acc;
      Secondary_Desc : File_Description_Acc;

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
         return Unsigned_64'Last;
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
      if not Userland.Process.Add_File (Proc, Primary_Desc,   Result (1)) or
         not Userland.Process.Add_File (Proc, Secondary_Desc, Result (2))
      then
         Close (Result_PTY);
         Close (Result_PTY);
         Close (Primary_Desc);
         Close (Secondary_Desc);
         Errno := Error_Too_Many_Files;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Open_PTY;

   function FSync
      (FD    : Unsigned_64;
       Flags : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant     PID := Arch.Local.Get_Current_Process;
      File : constant File_Description_Acc := Get_File (Proc, FD);
      Data : constant              Boolean := Flags /= 0;
      Succ : VFS.FS_Status;
   begin
      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      case File.Description is
         when Description_Inode =>
            Succ := VFS.Synchronize (File.Inner_Ino_FS, File.Inner_Ino, Data);
            return Translate_Status (Succ, 0, Errno);
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;
   end FSync;

   function Link
      (Source_Dir  : Unsigned_64;
       Source_Addr : Unsigned_64;
       Source_Len  : Unsigned_64;
       Desto_Dir   : Unsigned_64;
       Desto_Addr  : Unsigned_64;
       Desto_Len   : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Map_Acc := Get_Common_Map (Proc);
      Src_IAddr : constant  Integer_Address := Integer_Address (Source_Addr);
      Src_SAddr : constant   System.Address := To_Address (Src_IAddr);
      Dst_IAddr : constant  Integer_Address := Integer_Address (Desto_Addr);
      Dst_SAddr : constant   System.Address := To_Address (Dst_IAddr);
      CWD           : String (1 .. Process.Max_CWD_Length);
      CWD_Len       : Natural;
      Final_Path1   : String (1 .. 1024);
      Final_Path1_L : Natural;
      Final_Path2   : String (1 .. 1024);
      Final_Path2_L : Natural;
      Success       : VFS.FS_Status;
      User          : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Src_IAddr, Source_Len) or
         not Check_Userland_Access (Map, Dst_IAddr, Desto_Len)
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif Source_Len > Unsigned_64 (Natural'Last) or
            Desto_Len  > Unsigned_64 (Natural'Last)
      then
         Errno := Error_String_Too_Long;
         return Unsigned_64'Last;
      elsif Source_Dir /= AT_FDCWD or Desto_Dir /= AT_FDCWD then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      declare
         Src : String (1 .. Natural (Source_Len))
            with Import, Address => Src_SAddr;
         Dst : String (1 ..  Natural (Desto_Len))
            with Import, Address => Dst_SAddr;
      begin
         Process.Get_CWD (Proc, CWD, CWD_Len);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Src,
             Result    => Final_Path1,
             Count     => Final_Path1_L);
         Compound_Path
            (Base      => CWD (1 .. CWD_Len),
             Extension => Dst,
             Result    => Final_Path2,
             Count     => Final_Path2_L);
         if Final_Path1_L = 0 or Final_Path2_L = 0 then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;

         Userland.Process.Get_Effective_UID (Proc, User);
         VFS.Create_Hard_Link
            (Final_Path1 (1 .. Final_Path1_L),
             Final_Path2 (1 .. Final_Path2_L),
             Success,
             User);
         return Translate_Status (Success, 0, Errno);
      end;
   end Link;

   function PTrace
      (Request     : Unsigned_64;
       Traced_PID  : Unsigned_64;
       Traced_Addr : Unsigned_64;
       Result_Addr : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      pragma Unreferenced (Traced_Addr);
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      TProc : constant PID := Convert (Positive (Traced_PID));
   begin
      if TProc = Error_PID or else Proc /= Get_Parent (TProc) then
         Errno := Error_Bad_Permissions;
         return Unsigned_64'Last;
      elsif not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Trace_Children then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("ptrace", Proc);
         return Unsigned_64'Last;
      end if;

      case Request is
         when PTRACE_SYSCALL_PIPE =>
            Set_Traced_Info (TProc, True, Natural (Result_Addr));
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      Errno := Error_No_Error;
      return 0;
   end PTrace;

   function Poll
      (FDs_Addr  : Unsigned_64;
       FDs_Count : Unsigned_64;
       Timeout   : Unsigned_64;
       Errno     : out Errno_Value) return Unsigned_64
   is
      pragma Unreferenced (Timeout);

      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Map   : constant     Page_Map_Acc := Get_Common_Map (Proc);
      IAddr : constant  Integer_Address := Integer_Address (FDs_Addr);
      SAddr : constant   System.Address := To_Address (IAddr);
      Count :                   Natural := 0;
      File  : File_Description_Acc;
      FDs   : Poll_FDs (1 .. FDs_Count) with Import, Address => SAddr;
      Can_Read, Can_Write, Is_Error, Is_Broken : Boolean;
   begin
      if not Check_Userland_Access (Map, IAddr, FDs'Size / 8) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif FDs'Length = 0 then
         Errno := Error_No_Error;
         return 0;
      end if;

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
                  Devices.Poll (File.Inner_Dev, Can_Read, Can_Write, Is_Error);
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
               when others =>
                  Can_Read  := False;
                  Can_Write := False;
                  Is_Error  := True;
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

         if Count /= 0 then
            Errno := Error_No_Error;
            return Unsigned_64 (Count);
         end if;

         Scheduler.Yield;
      end loop;
   end Poll;

   function Get_EUID (Errno : out Errno_Value) return Unsigned_64 is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Ret  : Unsigned_32;
   begin
      Userland.Process.Get_Effective_UID (Proc, Ret);
      Errno := Error_No_Error;
      return Unsigned_64 (Ret);
   end Get_EUID;

   function Set_UIDs
      (UID   : Unsigned_64;
       EUID  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Change_UIDs then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("setuids", Proc);
         return Unsigned_64'Last;
      end if;

      if UID <= Unsigned_64 (Unsigned_32'Last) then
         Userland.Process.Set_UID (Proc, Unsigned_32 (UID));
      end if;
      if EUID <= Unsigned_64 (Unsigned_32'Last) then
         Userland.Process.Set_Effective_UID (Proc, Unsigned_32 (EUID));
      end if;

      Errno := Error_No_Error;
      return 0;
   end Set_UIDs;

   function Fchmod
      (FD    : Unsigned_64;
       Mode  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Desc : constant File_Description_Acc := Get_File (Proc, FD);
      Succ : VFS.FS_Status;
      User : Unsigned_32;
   begin
      Userland.Process.Get_Effective_UID (Proc, User);
      case Desc.Description is
         when Description_Inode =>
            Succ := VFS.Change_Mode
               (Desc.Inner_Ino_FS, Desc.Inner_Ino, File_Mode (Mode), User);
            return Translate_Status (Succ, 0, Errno);
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;
   end Fchmod;

   function Umask
      (Mode  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Old  : File_Mode;
   begin
      Process.Get_Umask (Proc, Old);
      Process.Set_Umask (Proc, File_Mode (Mode and 8#777#));
      Errno := Error_No_Error;
      return Unsigned_64 (Old);
   end Umask;

   function Reboot
      (Command : Unsigned_64;
       Flags   : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Proc    : constant     PID := Arch.Local.Get_Current_Process;
      Do_Ret  : constant Boolean := (Flags and RB_ERROR_RET) /= 0;
      Success : Arch.Power.Power_Status;
   begin
      if not MAC.Get_Capabilities (Get_MAC (Proc)).Can_Manage_Power then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("reboot", Proc);
         return Unsigned_64'Last;
      end if;

      case Command is
         when RB_HALT     => Success := Arch.Power.Halt;
         when RB_POWEROFF => Success := Arch.Power.Poweroff;
         when RB_RESTART  => Success := Arch.Power.Reboot;
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      --  If we are here, its because the functions failed.
      if Do_Ret then
         case Success is
            when Arch.Power.Not_Supported => Errno := Error_Not_Implemented;
            when Arch.Power.Failure       => Errno := Error_IO;
         end case;
         return Unsigned_64'Last;
      else
         Lib.Panic.Hard_Panic ("reboot() operation failed");
      end if;
   end Reboot;

   function Fchown
      (FD    : Unsigned_64;
       User  : Unsigned_64;
       Group : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Desc : constant File_Description_Acc := Get_File (Proc, FD);
      Succ : VFS.FS_Status;
      Usr  : Unsigned_32;
   begin
      Userland.Process.Get_Effective_UID (Proc, Usr);
      case Desc.Description is
         when Description_Inode =>
            Succ := VFS.Change_Owner
               (Desc.Inner_Ino_FS,
                Desc.Inner_Ino,
                Unsigned_32 (User  and 16#FFFFFFFF#),
                Unsigned_32 (Group and 16#FFFFFFFF#),
                Usr);
            return Translate_Status (Succ, 0, Errno);
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;
   end Fchown;

   function PRead
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Offset : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Buf_IAddr : constant  Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant   System.Address := To_Address (Buf_IAddr);
      Curr_Proc : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Map_Acc := Get_Common_Map (Curr_Proc);
      Final_Cnt : constant          Natural := Natural (Count);
      File      : File_Description_Acc;
      Data      : Devices.Operation_Data (1 .. Final_Cnt)
         with Import, Address => Buf_SAddr;
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success3  : Boolean;
      User      : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      File := Userland.Process.Get_File (Curr_Proc, File_D);
      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Get_Effective_UID (Curr_Proc, User);

      case File.Description is
         when Description_Device =>
            if not File.Inner_Dev_Read then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;
            Devices.Read (File.Inner_Dev, Offset, Data, Ret_Count, Success3);
            if Success3 then
               Errno := Error_No_Error;
               return Unsigned_64 (Ret_Count);
            else
               Errno := Error_IO;
               return Unsigned_64'Last;
            end if;
         when Description_Inode =>
            if not File.Inner_Ino_Read then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;
            VFS.Read (File.Inner_Ino_FS, File.Inner_Ino, Offset,
                      Data, Ret_Count, Success1, User);
            return Translate_Status (Success1, Unsigned_64 (Ret_Count), Errno);
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;
   end PRead;

   function PWrite
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Offset : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Buf_IAddr : constant  Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant   System.Address := To_Address (Buf_IAddr);
      Curr_Proc : constant PID := Arch.Local.Get_Current_Process;
      Map       : constant     Page_Map_Acc := Get_Common_Map (Curr_Proc);
      Final_Cnt : constant          Natural := Natural (Count);
      File      : File_Description_Acc;
      Data      : Devices.Operation_Data (1 .. Final_Cnt)
         with Import, Address => Buf_SAddr;
      Ret_Count : Natural;
      Success1  : VFS.FS_Status;
      Success3  : Boolean;
      User      : Unsigned_32;
   begin
      if not Check_Userland_Access (Map, Buf_IAddr, Count) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      File := Userland.Process.Get_File (Curr_Proc, File_D);
      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Process.Get_Effective_UID (Curr_Proc, User);

      case File.Description is
         when Description_Device =>
            if not File.Inner_Dev_Write then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;

            Devices.Write (File.Inner_Dev, Offset, Data, Ret_Count, Success3);
            if Success3 then
               Errno := Error_No_Error;
               return Unsigned_64 (Ret_Count);
            else
               Errno := Error_IO;
               return Unsigned_64'Last;
            end if;
         when Description_Inode =>
            if not File.Inner_Ino_Write then
               Errno := Error_Invalid_Value;
               return Unsigned_64'Last;
            end if;
            VFS.Write (File.Inner_Ino_FS, File.Inner_Ino, Offset,
                       Data, Ret_Count, Success1, User);
            return Translate_Status (Success1, Unsigned_64 (Ret_Count), Errno);
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;
   end PWrite;
   ----------------------------------------------------------------------------
   procedure Do_Exit (Proc : PID; Code : Unsigned_8) is
   begin
      --  Remove all state but the return value and keep the zombie around
      --  until we are waited.
      Userland.Process.Flush_Threads (Proc);
      Userland.Process.Flush_Files   (Proc);
      Userland.Process.Issue_Exit    (Proc, Code);
      Scheduler.Bail;
   end Do_Exit;

   function Translate_Status
      (Status         : VFS.FS_Status;
       Success_Return : Unsigned_64;
       Errno          : out Errno_Value) return Unsigned_64
   is
   begin
      case Status is
         when VFS.FS_Success => Errno := Error_No_Error; return Success_Return;
         when VFS.FS_Invalid_Value => Errno := Error_Invalid_Value;
         when VFS.FS_Not_Supported => Errno := Error_Not_Implemented;
         when VFS.FS_RO_Failure    => Errno := Error_Read_Only_FS;
         when VFS.FS_IO_Failure    => Errno := Error_IO;
         when VFS.FS_Not_Allowed   => Errno := Error_Bad_Permissions;
      end case;
      return Unsigned_64'Last;
   end Translate_Status;

   function Get_Mmap_Prot
      (Prot  : Unsigned_64;
       Perms : Unsigned_64) return Arch.MMU.Page_Permissions is
   begin
      return
         (User_Accesible => True,
          Read_Only      => (Prot and PROT_WRITE) = 0,
          Executable     => (Prot and PROT_EXEC) /= 0,
          Global         => False,
          Write_Through  => (Perms and MAP_WC) /= 0);
   end Get_Mmap_Prot;

   procedure Execute_MAC_Failure (Name : String; Curr_Proc : PID) is
      Discard    : Errno_Value;
      PID_Buffer : Lib.Messages.Translated_String;
      PID_Len    : Natural;
   begin
      case MAC.Get_Enforcement (Get_MAC (Curr_Proc)) is
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
end Userland.Syscall;
