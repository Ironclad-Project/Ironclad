--  userland-syscall.adb: Syscall implementation.
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

with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Config;
with System; use System;
with Messages;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Time; use Time;
with Panic;
with Alignment;
with Userland.Loader;
with Memory.MMU; use Memory.MMU;
with Arch.Clocks;
with Arch.Local;
with Memory.Physical;
with Memory; use Memory;
with Ada.Unchecked_Deallocation;
with Arch.Hooks;
with Cryptography.Random;
with IPC.Futex;
with IPC.FileLock;
with IPC.SHM;
with Arch.Power;
with Devices; use Devices;
with Networking.Interfaces;
with Virtualization;
with Devices.PCI;
with Arch.Snippets;
with Memory.Userland_Transfer;
with Userland.MAC;

package body Userland.Syscall is
   procedure Sys_Exit
      (Code     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
   begin
      Returned := 0;
      Errno    := Error_No_Error;
      Exit_Process
         (Arch.Local.Get_Current_Process, Unsigned_8 (Code and 16#FF#));
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Sys_Exit;

   procedure Arch_PRCtl
      (Code     : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Unsigned_64);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Argument);
      SAddr : constant  System.Address := To_Address (IAddr);
      Arg   : Unsigned_64;
      Map   : Page_Table_Acc;
      Succ  : Boolean;
      WB    : Boolean;
   begin
      Get_Common_Map (Proc, Map);
      Trans.Take_From_Userland (Map, Arg, SAddr, Succ);
      if not Succ then
         goto Would_Fault_Error;
      end if;

      Arch.Hooks.PRCTL_Hook (Natural (Code and 16#FFFFFF#), Arg, WB, Succ);
      if not Succ then
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
         return;
      end if;

      if WB then
         Trans.Paste_Into_Userland (Map, Arg, SAddr, Succ);
         if not Succ then
            goto Would_Fault_Error;
         end if;
      end if;

      Returned := 0;
      Errno    := Error_No_Error;
      return;

   <<Would_Fault_Error>>
      Returned := Unsigned_64'Last;
      Errno    := Error_Would_Fault;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Do_Cloexec  : constant         Boolean := (Flags and O_CLOEXEC)  /= 0;
      Do_CloFork  : constant         Boolean := (Flags and O_CLOFORK)  /= 0;
      Do_Read     : constant         Boolean := (Flags and O_RDONLY)   /= 0;
      Do_Write    : constant         Boolean := (Flags and O_WRONLY)   /= 0;
      Dont_Follow : constant         Boolean := (Flags and O_NOFOLLOW) /= 0;
      Do_Append   : constant         Boolean := (Flags and O_APPEND)   /= 0;
      Do_Block    : constant         Boolean := (Flags and O_NONBLOCK) = 0;
      Success2    : Boolean;
      Success     : VFS.FS_Status;
      CWD_FS      : VFS.FS_Handle;
      CWD_Ino     : VFS.File_Inode_Number;
      Opened_Ino  : VFS.File_Inode_Number;
      Opened_Stat : VFS.File_Stat;
      New_Descr   : File_Description_Acc;
      File_Perms  : MAC.Permissions;
      Returned_FD : Natural;
      User        : Unsigned_32;
      Map         : Page_Table_Acc;
   begin
      if Path_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Path_String is String (1 .. Natural (Path_Len));
         package Trans is new Memory.Userland_Transfer (Path_String);

         Path    : Path_String;
         Rela_FS : VFS.FS_Handle;
      begin
         Get_Common_Map (Curr_Proc, Map);
         Trans.Take_From_Userland (Map, Path, Path_SAddr, Success2);
         if not Success2 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Userland.Process.Get_Effective_UID (Curr_Proc, User);

         Resolve_AT_Directive (Curr_Proc, Dir_FD, Rela_FS, CWD_Ino);
         if Rela_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         VFS.Open (Rela_FS, CWD_Ino, Path, CWD_FS, Opened_Ino, Success,
                   User, Do_Read, Do_Write, not Dont_Follow);
         if Success /= VFS.FS_Success then
            Returned := Unsigned_64'Last;
            Errno    := Error_No_Entity;
            return;
         end if;

         if Do_Append then
            VFS.Stat (CWD_FS, Opened_Ino, Opened_Stat, Success);
         else
            Opened_Stat.Byte_Size := 0;
         end if;

         File_Perms := Check_Permissions (Curr_Proc, CWD_FS, Opened_Ino);
      end;

      if (Do_Read       and not File_Perms.Can_Read)  or
         (Do_Write      and not File_Perms.Can_Write) or
         (not Do_Append and File_Perms.Can_Append_Only)
      then
         Execute_MAC_Failure ("open", Curr_Proc);
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_Access;
         return;
      end if;

      New_Descr  := new File_Description'
         (Children_Count    => 0,
          Is_Blocking       => Do_Block,
          Description       => Description_Inode,
          Inner_Is_Locked   => False,
          Inner_Ino_Read    => Do_Read,
          Inner_Ino_Write   => Do_Write,
          Inner_Ino_FS      => CWD_FS,
          Inner_Ino_Pos     => Opened_Stat.Byte_Size,
          Inner_Ino         => Opened_Ino);

      Add_File (Curr_Proc, New_Descr, Returned_FD, Success2);
      if Success2 then
         Process.Set_FD_Flags
            (Curr_Proc, Unsigned_64 (Returned_FD), Do_Cloexec, Do_CloFork);
         Errno    := Error_No_Error;
         Returned := Unsigned_64 (Returned_FD);
      else
         Close (New_Descr);
         Returned := Unsigned_64'Last;
         Errno    := Error_Too_Many_Files;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Open;

   procedure Close
      (File_D   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Curr : constant PID := Arch.Local.Get_Current_Process;
      Succ : Boolean;
   begin
      Userland.Process.Is_Valid_File (Curr, File_D, Succ);
      if Succ then
         Userland.Process.Remove_File (Curr, Natural (File_D));
         Returned := 0;
         Errno    := Error_No_Error;
      else
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Close;

   procedure Read
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Offset   : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      File      : File_Description_Acc;
      Ret_Count : Natural;
      Success   : Boolean;
      Success1  : VFS.FS_Status;
      Success2  : IPC.FIFO.Pipe_Status;
      Success4  : IPC.Socket.Socket_Status;
      Success5  : IPC.PTY.Status;
      User      : Unsigned_32;
      Final_Cnt : Natural;
      Map       : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, File_D, File);
      if File = null then
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
         Final_Len : constant Natural := Final_Cnt;
         subtype Read_Data is Devices.Operation_Data (1 .. Final_Len);
         package Trans is new Memory.Userland_Transfer (Read_Data);
         Data : Read_Data with Import, Address => Buf_SAddr;
      begin
         --  Ideally this would be handled with
         --  Take_From_Userland/Paste_From_Userland, allocating an internal
         --  buffer, reading it there, and pasting it, but 2 factors make that
         --  horrible:
         --  - Userland software has the habit of doing multiple megabyte reads
         --    to load executables and similar.
         --  - Ironclad's allocator allocates for now only contiguous physical
         --    memory, which means that these massive megabyte reads fragment
         --    like crazy and lead to early OOMs.
         --  Once one of the both is fixed, we can use the other cleaner
         --  methods.
         Trans.Check_Access (Map, Buf_SAddr, True, Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno := Error_Would_Fault;
            return;
         end if;

         Arch.Snippets.Enable_Userland_Memory_Access;

         case File.Description is
            when Description_Inode =>
               if File.Inner_Ino_Read then
                  if Flags = 0 then
                     VFS.Read
                        (File.Inner_Ino_FS, File.Inner_Ino, File.Inner_Ino_Pos,
                         Data, Ret_Count, File.Is_Blocking, Success1);
                     File.Inner_Ino_Pos := File.Inner_Ino_Pos +
                                          Unsigned_64 (Ret_Count);
                  else
                     VFS.Read
                        (File.Inner_Ino_FS, File.Inner_Ino, Offset,
                         Data, Ret_Count, File.Is_Blocking, Success1);
                  end if;
                  Translate_Status
                     (Success1, Unsigned_64 (Ret_Count), Returned, Errno);
               else
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
               end if;
            when Description_Reader_FIFO =>
               Read (File.Inner_Reader_FIFO, Data, File.Is_Blocking,
                     Ret_Count, Success2);
               Translate_Status (Success2, Unsigned_64 (Ret_Count), Returned,
                                 Errno);
            when Description_Primary_PTY =>
               IPC.PTY.Read_Primary
                  (File.Inner_Primary_PTY, Data, File.Is_Blocking,
                   Ret_Count, Success5);
               Translate_Status (Success5, Unsigned_64 (Ret_Count), Returned,
                                 Errno);
            when Description_Secondary_PTY =>
               IPC.PTY.Read_Secondary
                  (File.Inner_Secondary_PTY, Data, File.Is_Blocking,
                   Ret_Count, Success5);
               Translate_Status (Success5, Unsigned_64 (Ret_Count), Returned,
                                 Errno);
            when Description_Socket =>
               IPC.Socket.Read
                  (File.Inner_Socket, Data, File.Is_Blocking,
                   Ret_Count, Success4);
               Translate_Status
                  (Success4, Unsigned_64 (Ret_Count), Returned, Errno);
            when Description_Writer_FIFO =>
               Errno    := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
         end case;

         Arch.Snippets.Disable_Userland_Memory_Access;
      end;
   exception
      when Constraint_Error =>
         Arch.Snippets.Disable_Userland_Memory_Access;
         Errno := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Read;

   procedure Write
      (File_D   : Unsigned_64;
       Buffer   : Unsigned_64;
       Count    : Unsigned_64;
       Offset   : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      File      : File_Description_Acc;
      Ret_Count : Natural;
      Success   : Boolean;
      Success1  : VFS.FS_Status;
      Success2  : IPC.FIFO.Pipe_Status;
      Success4  : IPC.Socket.Socket_Status;
      Success5  : IPC.PTY.Status;
      User      : Unsigned_32;
      Final_Cnt : Natural;
      Final_Off : Unsigned_64;
      Map       : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, File_D, File);
      if File = null then
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
         Final_Len : constant Natural := Final_Cnt;
         subtype Read_Data is Devices.Operation_Data (1 .. Final_Len);
         package Trans is new Memory.Userland_Transfer (Read_Data);
         Data : Read_Data with Import, Address => Buf_SAddr;
      begin
         --  The same block comment writeup as Read applies.
         Trans.Check_Access (Map, Buf_SAddr, False, Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno := Error_Would_Fault;
            return;
         end if;

         Arch.Snippets.Enable_Userland_Memory_Access;

         case File.Description is
            when Description_Inode =>
               if not File.Inner_Ino_Write then
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  goto Cleanup;
               end if;

               if Flags = 0 then
                  Final_Off := File.Inner_Ino_Pos;
               else
                  Final_Off := Offset;
               end if;

               if Final_Off + Unsigned_64 (Final_Cnt) >
                  Get_Limit (Proc, MAC.File_Size_Limit).Soft_Limit
               then
                  Errno := Error_File_Too_Big;
                  Returned := Unsigned_64'Last;
                  goto Cleanup;
               end if;

               VFS.Write (File.Inner_Ino_FS, File.Inner_Ino,
                          Final_Off, Data, Ret_Count,
                          File.Is_Blocking, Success1);
               if Flags = 0 then
                  File.Inner_Ino_Pos := File.Inner_Ino_Pos +
                                        Unsigned_64 (Ret_Count);
               end if;
               Translate_Status (Success1, Unsigned_64 (Ret_Count), Returned,
                                        Errno);
            when Description_Writer_FIFO =>
               Write (File.Inner_Writer_FIFO, Data, File.Is_Blocking,
                      Ret_Count, Success2);
               Translate_Status (Success2, Unsigned_64 (Ret_Count), Returned,
                                        Errno);
            when Description_Primary_PTY =>
               IPC.PTY.Write_Primary
                  (File.Inner_Primary_PTY, Data, File.Is_Blocking,
                   Ret_Count, Success5);
               Translate_Status (Success5, Unsigned_64 (Ret_Count), Returned,
                                 Errno);
            when Description_Secondary_PTY =>
               IPC.PTY.Write_Secondary
                  (File.Inner_Secondary_PTY, Data, File.Is_Blocking,
                   Ret_Count, Success5);
               Translate_Status (Success5, Unsigned_64 (Ret_Count), Returned,
                                 Errno);
            when Description_Socket =>
               IPC.Socket.Write
                  (File.Inner_Socket, Data, File.Is_Blocking, Ret_Count,
                   Success4);
               Translate_Status
                  (Success4, Unsigned_64 (Ret_Count), Returned, Errno);
            when Description_Reader_FIFO =>
               Errno := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
         end case;

      <<Cleanup>>
         Arch.Snippets.Disable_Userland_Memory_Access;
      end;
   exception
      when Constraint_Error =>
         Arch.Snippets.Disable_Userland_Memory_Access;
         Errno := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Get_File (Proc, File_D, File);
      if File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      case File.Description is
         when Description_Inode =>
            VFS.Stat (File.Inner_Ino_FS, File.Inner_Ino, Stat_Val, Success);
            if Success /= VFS.FS_Success or
               Stat_Val.Type_Of_File = File_Character_Device
            then
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Seek;

   procedure Mmap
      (Hint2      : Unsigned_64;
       Length2    : Unsigned_64;
       Protection : Unsigned_64;
       Flags      : Unsigned_64;
       File_D     : Unsigned_64;
       Offset     : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      package Al is new Alignment (Unsigned_64);

      Perms      : constant Arch.MMU.Page_Permissions
         := Get_Mmap_Prot (Protection);
      Proc       : constant              PID := Arch.Local.Get_Current_Process;
      Final_Hint : Virtual_Address;
      File       : File_Description_Acc;
      Success    : Boolean;
      Status     : FS_Status;
      Size       : Unsigned_64;
      Map        : Page_Table_Acc;
      Hint       : Unsigned_64 := Hint2;
      Length     : Unsigned_64 := Length2;
   begin
      Get_Common_Map (Proc, Map);
      if not Get_Capabilities (Proc).Can_Modify_Memory then
         goto Bad_MAC_Return;
      elsif (Perms.Can_Write and Perms.Can_Execute) or Length2 = 0 then
         goto Invalid_Value_Return;
      end if;

      Al.Align_Memory_Range (Hint, Length, Memory.MMU.Page_Size);
      Final_Hint := Memory.Virtual_Address (Hint);

      Get_User_Mapped_Size (Map, Size);
      if Size + Length >= Get_Limit (Proc, MAC.Memory_Size_Limit).Soft_Limit
      then
         goto No_Memory_Return;
      end if;

      --  FIXME: We purposely ignore userland hints for non MAP_FIXED
      --  allocations as to simply handling, we should change this on the
      --  future as to satisfy poor userland, it already deals with enough.
      if (Flags and MAP_FIXED) /= 0 then
         if Hint = 0 then
            goto Invalid_Value_Return;
         end if;
      else
         Bump_Alloc_Base (Proc, Length, Unsigned_64 (Final_Hint));
      end if;

      --  Check the address is good.
      Check_Userland_Mappability (Map, Final_Hint, Length, Success);
      if not Success then
         goto Invalid_Value_Return;
      end if;

      --  Do mmap anon or pass it to the VFS.
      if (Flags and MAP_ANON) /= 0 then
         Map_Allocated_Range
            (Map           => Map,
             Virtual_Start => To_Address (Final_Hint),
             Length        => Storage_Count (Length),
             Permissions   => Perms,
             Success       => Success);
         if Success then
            Errno := Error_No_Error;
            Returned := Unsigned_64 (Final_Hint);
         else
            goto No_Memory_Return;
         end if;
      else
         Get_File (Proc, File_D, File);
         if File = null then
            goto Invalid_Value_Return;
         end if;

         if File.Description = Description_Inode then
            VFS.Mmap
               (Key         => File.Inner_Ino_FS,
                Ino         => File.Inner_Ino,
                Map         => Map,
                Offset      => Offset,
                Address     => Final_Hint,
                Length      => Length,
                Flags       => Perms,
                Status      => Status);
            if Status /= VFS.FS_Success then
               goto No_Memory_Return;
            end if;

            Errno := Error_No_Error;
            Returned := Unsigned_64 (Final_Hint);
         else
            Errno := Error_Bad_File;
            Returned := Unsigned_64'Last;
         end if;
      end if;

      return;

   <<Invalid_Value_Return>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
      return;

   <<No_Memory_Return>>
      Errno := Error_No_Memory;
      Returned := Unsigned_64'Last;
      return;

   <<Bad_MAC_Return>>
      Errno := Error_Bad_Access;
      Execute_MAC_Failure ("mmap", Proc);
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Mmap;

   procedure Munmap
      (Address    : Unsigned_64;
       Length     : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      Proc : constant            PID := Arch.Local.Get_Current_Process;
      Addr : constant System.Address := To_Address (Virtual_Address (Address));
      Map  : Page_Table_Acc;
      Succ : Boolean;
   begin
      Get_Common_Map (Proc, Map);
      if not Get_Capabilities (Proc).Can_Modify_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("munmap", Proc);
         Returned := Unsigned_64'Last;
      end if;

      Unmap_Range (Map, Addr, Storage_Count (Length), Succ);

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Munmap;

   procedure Get_PID (Returned : out Unsigned_64; Errno : out Errno_Value) is
   begin
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Convert (Arch.Local.Get_Current_Process));
   end Get_PID;

   procedure Get_PPID (Returned : out Unsigned_64; Errno : out Errno_Value) is
      Proc   : constant PID := Arch.Local.Get_Current_Process;
      Parent : PID;
   begin
      Get_Parent (Proc, Parent);
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
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Acc);
      Th         : constant TID := Arch.Local.Get_Current_Thread;
      Proc       : constant PID := Arch.Local.Get_Current_Process;
      Map, Orig  : Memory.MMU.Page_Table_Acc;
      Success    : Boolean;
      Success3   : Boolean;
      Success4   : Boolean;
      Path_FS    : FS_Handle;
      Path_Ino   : File_Inode_Number;
      Rela_FS    : FS_Handle;
      Rela_Ino   : File_Inode_Number;
      Success2   : FS_Status;
      File_St    : File_Stat;
      File_Perms : MAC.Permissions;
      User       : Unsigned_32;
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      Argv_IAddr : constant Integer_Address := Integer_Address (Argv_Addr);
      Argv_SAddr : constant  System.Address := To_Address (Argv_IAddr);
      Envp_IAddr : constant Integer_Address := Integer_Address (Envp_Addr);
      Envp_SAddr : constant  System.Address := To_Address (Envp_IAddr);
   begin
      Get_Common_Map (Proc, Orig);
      Userland.Process.Get_Effective_UID (Proc, User);
      Userland.Process.Get_CWD (Proc, Rela_FS, Rela_Ino);

      declare
         subtype Path_Str is String (1 .. Natural (Path_Len));
         type    Argv_Arr is array (1 .. Natural (Argv_Len)) of Unsigned_64;
         type    Envp_Arr is array (1 .. Natural (Envp_Len)) of Unsigned_64;
         type    Argv_Acc is access Argv_Arr;
         type    Envp_Acc is access Envp_Arr;
         procedure Free is new Ada.Unchecked_Deallocation (Argv_Arr, Argv_Acc);
         procedure Free is new Ada.Unchecked_Deallocation (Envp_Arr, Envp_Acc);
         package Trans_1 is new Memory.Userland_Transfer (Path_Str);
         package Trans_2 is new Memory.Userland_Transfer (Argv_Arr);
         package Trans_3 is new Memory.Userland_Transfer (Envp_Arr);
         Path : Path_Str;
         Argv : Argv_Acc := new Argv_Arr'[others => 0];
         Envp : Envp_Acc := new Envp_Arr'[others => 0];
      begin
         Trans_1.Take_From_Userland (Orig, Path, Path_SAddr, Success);
         Trans_2.Take_From_Userland (Orig, Argv.all, Argv_SAddr, Success3);
         Trans_3.Take_From_Userland (Orig, Envp.all, Envp_SAddr, Success4);
         if not Success or not Success3 or not Success4 then
            Errno    := Error_Would_Fault;
            Returned := Unsigned_64'Last;
            goto Cleanup;
         end if;

         Open
            (Key        => Rela_FS,
             Relative   => Rela_Ino,
             Path       => Path,
             Final_Key  => Path_FS,
             Ino        => Path_Ino,
             Success    => Success2,
             User       => User,
             Want_Read  => True,
             Want_Write => False);
         if Success2 /= VFS.FS_Success then
            Errno    := Error_No_Entity;
            Returned := Unsigned_64'Last;
            goto Cleanup;
         end if;

         --  Open the exec file.
         File_Perms := Check_Permissions (Proc, Path_FS, Path_Ino);
         if not File_Perms.Can_Execute then
            Errno := Error_Bad_Access;
            Execute_MAC_Failure ("exec", Proc);
            Returned := Unsigned_64'Last;
            goto Cleanup;
         end if;
         VFS.Stat (Path_FS, Path_Ino, File_St, Success2);
         if Success2 /= VFS.FS_Success then
            Errno    := Error_IO;
            Returned := Unsigned_64'Last;
            goto Cleanup;
         end if;
         if not VFS.Can_Access_File
            (User       => User,
             File_Owner => File_St.UID,
             Mode       => File_St.Mode,
             Want_Read  => True,
             Want_Write => False,
             Want_Exec  => True)
         then
            Errno    := Error_Bad_Access;
            Returned := Unsigned_64'Last;
            goto Cleanup;
         end if;

         declare
            procedure Free is new Ada.Unchecked_Deallocation
               (Argument_Arr, Argument_Arr_Acc);
            procedure Free is new Ada.Unchecked_Deallocation
               (Environment_Arr, Environment_Arr_Acc);

            Args : Argument_Arr_Acc := new Argument_Arr (1 .. Argv'Length);
            Env  : Environment_Arr_Acc :=
               new Environment_Arr (1 .. Envp'Length);
         begin
            for I in Argv'Range loop
               Args (I) := To_String (To_Address (Integer_Address (Argv (I))));
            end loop;
            for I in Envp'Range loop
               Env (I) := To_String (To_Address (Integer_Address (Envp (I))));
            end loop;

            --  Create a new map for the process and reroll ASLR.
            Userland.Process.Flush_Threads (Proc);
            Userland.Process.Flush_Exec_Files (Proc);
            Userland.Process.Reassign_Process_Addresses (Proc);
            Memory.MMU.Create_Table (Map);
            Set_Common_Map (Proc, Map);
            if Args'Length > 0 then
               Set_Identifier (Proc, Args (1).all);
            end if;

            --  Start the actual program.
            Userland.Loader.Start_Program
               (Exec_Path   => Path,
                FS          => Path_FS,
                Ino         => Path_Ino,
                Arguments   => Args.all,
                Environment => Env.all,
                Proc        => Proc,
                Success     => Success);

            for Arg of Args.all loop
               Free (Arg);
            end loop;
            for En of Env.all loop
               Free (En);
            end loop;
            Free (Args);
            Free (Env);
         end;

         if Success and then Memory.MMU.Make_Active (Map) then
            --  Free critical state now that we know wont be running.
            --  Of course dont remove the map if we are vforked.
            Userland.Process.Remove_Thread (Proc, Th);
            Pop_VFork_Marker (Proc, Success);
            if not Success then
               Memory.MMU.Destroy_Table (Orig);
            end if;
            Free (Envp);
            Free (Argv);
            Scheduler.Bail;
         else
            Errno    := Error_Bad_Access;
            Returned := Unsigned_64'Last;
         end if;

      <<Cleanup>>
         Free (Argv);
         Free (Envp);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Exec;

   procedure Fork
      (GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma SPARK_Mode (Off); --  Comparing page tables is against SPARK.
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      Pol     : Scheduler.Policy;
      Child   : PID;
      New_TID : Scheduler.TID;
      Ret     : Unsigned_64;
      Id      : String (1 .. Process.Max_Name_Length);
      Id_Len  : Natural;
      Success : Boolean;
      Map, Table : Memory.MMU.Page_Table_Acc;
      Do_VFORK : constant Boolean := (Flags and FORK_VFORK) /= 0;
   begin
      if not Get_Capabilities (Proc).Can_Spawn_Others then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("fork", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      Create_Process (Proc, Child);
      if Child = Error_PID then
         goto Block_Error;
      end if;

      Get_Identifier (Proc, Id, Id_Len);
      Set_Identifier (Child, Id (1 .. Id_Len));

      Get_Common_Map (Proc, Map);
      if Do_VFORK then
         Set_VFork_Marker (Child);
         Table := Map;
      else
         Fork_Table (Map, Table);
         if Table = null then
            goto Block_Error;
         end if;
      end if;
      Set_Common_Map (Child, Table);

      Duplicate_FD_Table (Proc, Child);
      Process.Get_Default_Policy (Proc, Pol);
      Scheduler.Create_User_Thread
         (GP_State => GP_State,
          FP_State => FP_State,
          Map      => Table,
          Pol      => Pol,
          PID      => Convert (Child),
          TCB      => Arch.Local.Fetch_TCB,
          New_TID  => New_TID);
      Ret := Unsigned_64 (Convert (Child));
      if New_TID = Error_TID then
         goto Block_Error;
      end if;

      Add_Thread (Child, New_TID, Success);
      if not Success then
         goto Block_Error;
      end if;

      if Do_VFORK then
         loop
            Get_Common_Map (Child, Table);
            exit when Table /= Map;
            Scheduler.Yield_If_Able;
         end loop;
      end if;

      Errno    := Error_No_Error;
      Returned := Ret;
      return;

   <<Block_Error>>
      Errno    := Error_Would_Block;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Fork;

   procedure Wait
      (Waited_PID, Exit_Addr, Options : Unsigned_64;
       Returned                       : out Unsigned_64;
       Errno                          : out Errno_Value)
   is
      type Signed_32 is range -2 ** 31 + 1 .. +2 ** 31 - 1;
      function Conv is new Ada.Unchecked_Conversion (Unsigned_32, Signed_32);
      package Trans is new Memory.Userland_Transfer (Unsigned_32);

      Addr        : constant Integer_Address := Integer_Address (Exit_Addr);
      Proc        : constant             PID := Arch.Local.Get_Current_Process;
      Dont_Hang   : constant         Boolean := (Options and WNOHANG) /= 0;
      Signed_Wait : Signed_32;
      Waited, Tmp : PID;
      Group       : Unsigned_32;
      Did_Exit    : Boolean;
      Was_Signal  : Boolean;
      Cause       : Signal;
      Error_Code  : Unsigned_8;
      Exit_Value  : Unsigned_32;
      Map         : Page_Table_Acc;
      Succ        : Boolean;
   begin
      --  If < -1: wait on the children in abs number as process group id.
      --  If = -1: wait on all children.
      --  If =  0: wait on children in the process group of caller.
      --  If >  0: Wait on the number as a PID.
      Signed_Wait := Conv (Unsigned_32 (Waited_PID and 16#FFFFFFFF#));
      if Signed_Wait < -1 or Signed_Wait = 0 then
         if Signed_Wait = 0 then
            Get_PGID (Proc, Group);
         else
            Group := Unsigned_32 (abs Signed_Wait);
         end if;

         loop
            Check_Children_Group_Exit
               (Process     => Proc,
                Group       => Group,
                Exited_Proc => Waited,
                Did_Exit    => Did_Exit,
                Code        => Error_Code,
                Was_Signal  => Was_Signal,
                Sig         => Cause);
            if Did_Exit then
               goto Waited_Exited;
            end if;

            exit when Dont_Hang;
            Scheduler.Yield_If_Able;
         end loop;
      elsif Signed_Wait = -1 then
         loop
            Check_Children_Exit
               (Process     => Proc,
                Exited_Proc => Waited,
                Did_Exit    => Did_Exit,
                Code        => Error_Code,
                Was_Signal  => Was_Signal,
                Sig         => Cause);
            if Did_Exit then
               goto Waited_Exited;
            end if;

            exit when Dont_Hang;
            Scheduler.Yield_If_Able;
         end loop;
      else
         Waited := Userland.Process.Convert (Natural (Signed_Wait));
         if Waited = Error_PID then
            goto Child_Error;
         end if;

         Get_Parent (Waited, Tmp);
         if Tmp /= Proc then
            goto Child_Error;
         end if;

         loop
            Check_Exit (Waited, Did_Exit, Error_Code, Was_Signal, Cause);
            if Did_Exit then
               goto Waited_Exited;
            end if;
            exit when Dont_Hang;
            Scheduler.Yield_If_Able;
         end loop;
      end if;

      --  If we get here, it means we are not blocking, and that the
      --  process has not exited, so lets return what we have to.
      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Waited_Exited>>
      --  Set the return value if we are to.
      if Addr /= 0 then
         if Was_Signal then
            Exit_Value := WIFSIGNALED or Shift_Left (Cause'Enum_Rep, 24);
         else
            Exit_Value := WIFEXITED or Unsigned_32 (Error_Code);
         end if;

         Get_Common_Map (Proc, Map);
         Trans.Paste_Into_Userland (Map, Exit_Value, To_Address (Addr), Succ);
         if not Succ then
            Errno    := Error_Would_Fault;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      --  Now that we got the exit code, finally allow the process to die.
      Get_Common_Map (Waited, Map);
      Memory.MMU.Destroy_Table          (Map);
      Userland.Process.Delete_Process (Waited);

      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Convert (Waited));
      return;

   <<Child_Error>>
      Errno    := Error_Child;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Wait;

   procedure Socket
      (Domain   : Unsigned_64;
       DataType : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma SPARK_Mode (Off); --  File modifications are against SPARK.
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

      New_Sock := Create (Dom, Data);
      if New_Sock = null then
         goto Invalid_Value_Return;
      end if;

      Desc := new File_Description'
         (Description_Socket, 0, (DataType and SOCK_NONBLOCK) = 0, New_Sock);
      Add_File (Proc, Desc, Natural (Returned), Success);
      if Success then
         Set_FD_Flags
           (Proc, Returned,
            (DataType and SOCK_CLOEXEC) /= 0,
            (DataType and SOCK_CLOFORK) /= 0);
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
      Proc    : constant             PID := Arch.Local.Get_Current_Process;
      IAddr   : constant Integer_Address := Integer_Address (Address);
      SAddr   : constant  System.Address := To_Address (IAddr);
      Success : Boolean;
      Map     : Page_Table_Acc;
   begin
      if not Get_Capabilities (Proc).Can_Manage_Networking then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_hostname", Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif Length > Unsigned_64 (Networking.Hostname_Max_Len) then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Host_String is String (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Host_String);
         Name : Host_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans.Take_From_Userland (Map, Name, SAddr, Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Networking.Set_Hostname (Name, Success);
         if not Success then
            goto Invalid_Value_Error;
         end if;

         Errno := Error_No_Error;
         Returned := 0;
      end;

      return;

   <<Invalid_Value_Error>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      package Trans is new Memory.Userland_Transfer (Stat);

      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      Stat_IAddr : constant Integer_Address := Integer_Address (Stat_Addr);
      Stat_SAddr : constant  System.Address := To_Address (Stat_IAddr);
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      File_Desc  : File_Description_Acc;
      Stat_Val   : VFS.File_Stat;
      ID         : Natural;
      Rel_FS, FS : VFS.FS_Handle;
      D_Ino, Ino : VFS.File_Inode_Number;
      Success    : VFS.FS_Status;
      Success2   : Boolean;
      Stat_Buf   : Stat;
      User       : Unsigned_32;
      Map        : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Userland.Process.Get_Effective_UID (Proc, User);

      if (Flags and AT_EMPTY_PATH) /= 0 then
         Get_File (Proc, Dir_FD, File_Desc);
         if File_Desc = null then
            Errno := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;

         case File_Desc.Description is
            when Description_Inode =>
               FS  := File_Desc.Inner_Ino_FS;
               Ino := File_Desc.Inner_Ino;
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
                   Change_Time   => (Seconds => 0, Nanoseconds => 0),
                   Birth_Time    => (Seconds => 0, Nanoseconds => 0),
                   Block_Size    => 512,
                   Block_Count   => 1);
               goto Write_Success;
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
                   Change_Time   => (Seconds => 0, Nanoseconds => 0),
                   Birth_Time    => (Seconds => 0, Nanoseconds => 0),
                   Block_Size    => 512,
                   Block_Count   => 1);
               goto Write_Success;
         end case;
      else
         if Path_Len > Path_Max_Len then
            Returned := Unsigned_64'Last;
            Errno    := Error_String_Too_Long;
            return;
         end if;

         declare
            subtype Path_String is String (1 .. Natural (Path_Len));
            package Trans2 is new Memory.Userland_Transfer (Path_String);
            Path : Path_String;
         begin
            Trans2.Take_From_Userland (Map, Path, Path_SAddr, Success2);
            if not Success2 then
               goto Would_Fault_Error;
            end if;

            Resolve_AT_Directive (Proc, Dir_FD, Rel_FS, D_Ino);
            if Rel_FS = VFS.Error_Handle then
               Errno    := Error_Bad_File;
               Returned := Unsigned_64'Last;
               return;
            end if;

            VFS.Open
               (Key        => Rel_FS,
                Relative   => D_Ino,
                Path       => Path,
                Final_Key  => FS,
                Ino        => Ino,
                Success    => Success,
                User       => User,
                Want_Read  => False,
                Want_Write => False,
                Do_Follow  => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
            if Success /= VFS.FS_Success then
               Translate_Status (Success, 0, Returned, Errno);
               return;
            end if;
         end;
      end if;

      VFS.Stat (FS, Ino, Stat_Val, Success);
      if Success /= VFS.FS_Success then
         Translate_Status (Success, 0, Returned, Errno);
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
          Change_Time  =>
             (Stat_Val.Change_Time.Seconds_Since_Epoch,
              Stat_Val.Change_Time.Additional_Nanoseconds),
          Birth_Time   =>
             (Stat_Val.Birth_Time.Seconds_Since_Epoch,
              Stat_Val.Birth_Time.Additional_Nanoseconds),
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

   <<Write_Success>>
      Trans.Paste_Into_Userland (Map, Stat_Buf, Stat_SAddr, Success2);
      if not Success2 then
         goto Would_Fault_Error;
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Would_Fault_Error>>
      Returned := Unsigned_64'Last;
      Errno    := Error_Would_Fault;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end FStat;

   procedure Pivot_Root
      (New_Addr : Unsigned_64;
       New_Len  : Unsigned_64;
       Old_Addr : Unsigned_64;
       Old_Len  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      New_IAddr : constant Integer_Address := Integer_Address (New_Addr);
      New_SAddr : constant  System.Address := To_Address (New_IAddr);
      Old_IAddr : constant Integer_Address := Integer_Address (Old_Addr);
      Old_SAddr : constant  System.Address := To_Address (Old_IAddr);
      Curr_Proc : constant             PID := Arch.Local.Get_Current_Process;
      Success1  : Boolean;
      Success2  : Boolean;
      Map       : Page_Table_Acc;
   begin
      if not Get_Capabilities (Curr_Proc).Can_Manage_Mounts then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("pivot_root", Curr_Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif New_Len > Path_Max_Len or Old_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype New_String is String (1 .. Natural (New_Len));
         subtype Old_String is String (1 .. Natural (Old_Len));
         package Trans_1 is new Memory.Userland_Transfer (New_String);
         package Trans_2 is new Memory.Userland_Transfer (Old_String);

         New_Path : New_String;
         Old_Path : Old_String;
      begin
         Get_Common_Map (Curr_Proc, Map);
         Trans_1.Take_From_Userland (Map, New_Path, New_SAddr, Success1);
         Trans_2.Take_From_Userland (Map, Old_Path, Old_SAddr, Success2);
         if not Success1 or not Success2 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         VFS.Pivot_Root
            (New_Mount => New_Path,
             Old_Mount => Old_Path,
             Success   => Success1);
         if Success1 then
            Errno    := Error_No_Error;
            Returned := 0;
         else
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Pivot_Root;

   procedure Chdir
      (FD       : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Desc : File_Description_Acc;
      St   : VFS.File_Stat;
      Succ : FS_Status;
      User : Unsigned_32;
   begin
      Get_File (Proc, FD, Desc);
      if Desc = null or else Desc.Description /= Description_Inode then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      VFS.Stat (Desc.Inner_Ino_FS, Desc.Inner_Ino, St, Succ);
      if Succ /= VFS.FS_Success or else St.Type_Of_File /= File_Directory then
         Errno    := Error_Not_Directory;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Set_CWD (Proc, Desc.Inner_Ino_FS, Desc.Inner_Ino);
      Errno    := Error_No_Error;
      Returned := 0;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Chdir;

   procedure IOCTL
      (FD       : Unsigned_64;
       Request  : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      I_Arg : constant Integer_Address := Integer_Address (Argument);
      S_Arg : constant  System.Address := To_Address (I_Arg);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      File  : File_Description_Acc;
      Succ  : Boolean;
      Has_E : Boolean;
      Extra : Unsigned_64;
      FSSuc : VFS.FS_Status;
      User  : Unsigned_32;
   begin
      --  This degenerate localized SMAP disabling is required because of
      --  our current API is really poorly positioned to cleanly handle the
      --  massive variety of ioctl argument types.
      Arch.Snippets.Enable_Userland_Memory_Access;

      Get_File (Proc, FD, File);
      if File = null then
         Errno := Error_Not_A_TTY;
         Returned := Unsigned_64'Last;
         goto Cleanup;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);

      case File.Description is
         when Description_Inode =>
            VFS.IO_Control
               (Key       => File.Inner_Ino_FS,
                Ino       => File.Inner_Ino,
                Request   => Request,
                Arg       => S_Arg,
                Has_Extra => Has_E,
                Extra     => Extra,
                Status    => FSSuc);
            Succ := FSSuc = VFS.FS_Success;
         when Description_Primary_PTY =>
            IPC.PTY.IO_Control
               (File.Inner_Primary_PTY, True, Request, S_Arg, Succ);
            Has_E := False;
            Extra := 0;
         when Description_Secondary_PTY =>
            IPC.PTY.IO_Control
               (File.Inner_Secondary_PTY, False, Request, S_Arg, Succ);
            Has_E := False;
            Extra := 0;
         when others =>
            Has_E := False;
            Extra := 0;
            Succ  := False;
      end case;

      if Succ then
         if Has_E then
            Errno    := Error_No_Error;
            Returned := Extra;
         else
            Errno    := Error_No_Error;
            Returned := 0;
         end if;
      else
         Errno    := Error_Not_A_TTY;
         Returned := Unsigned_64'Last;
      end if;

   <<Cleanup>>
      Arch.Snippets.Disable_Userland_Memory_Access;
   exception
      when Constraint_Error =>
         Arch.Snippets.Disable_Userland_Memory_Access;
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end IOCTL;

   procedure Sched_Yield (Returned : out Unsigned_64; Errno : out Errno_Value)
   is
   begin
      Scheduler.Yield_If_Able;
      Returned := 0;
      Errno    := Error_No_Error;
   end Sched_Yield;

   procedure Get_Min_Pri (Returned : out Unsigned_64; Errno : out Errno_Value)
   is
   begin
      Returned := Unsigned_64 (Scheduler.Priority'First);
      Errno    := Error_No_Error;
   end Get_Min_Pri;

   procedure Pipe
      (Result_Addr : Unsigned_64;
       Flags       : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      pragma SPARK_Mode (Off); --  File modifications are against SPARK.
      type Result_Arr is array (1 .. 2) of Integer;
      package Trans is new Memory.Userland_Transfer (Result_Arr);

      Ad    : constant Integer_Address := Integer_Address (Result_Addr);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Block : constant         Boolean := (Flags and O_NONBLOCK) = 0;

      Res : Result_Arr;
      Returned2 : IPC.FIFO.Inner_Acc;
      Succ1, Succ2 : Boolean;
      Reader_Desc, Writer_Desc : File_Description_Acc;
      Map : Page_Table_Acc;
   begin
      Returned2 := IPC.FIFO.Create;
      Reader_Desc := new File_Description'
         (Children_Count    => 0,
          Is_Blocking       => Block,
          Description       => Description_Reader_FIFO,
          Inner_Reader_FIFO => Returned2);
      Writer_Desc := new File_Description'
         (Children_Count    => 0,
          Is_Blocking       => Block,
          Description       => Description_Writer_FIFO,
          Inner_Writer_FIFO => Returned2);
      Add_File (Proc, Reader_Desc, Res (1), Succ1);
      Add_File (Proc, Writer_Desc, Res (2), Succ2);
      if not Succ1 or not Succ2 then
         Close (Reader_Desc);
         Close (Writer_Desc);
         Errno := Error_Too_Many_Files;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Res, To_Address (Ad), Succ1);
      if not Succ1 then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Errno := Error_No_Error;
      Returned := 0;
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
      Src_IAddr : constant  Integer_Address := Integer_Address (Source_Addr);
      Src_SAddr : constant   System.Address := To_Address (Src_IAddr);
      Tgt_IAddr : constant  Integer_Address := Integer_Address (Target_Addr);
      Tgt_SAddr : constant   System.Address := To_Address (Tgt_IAddr);
      Do_Keep   : constant Boolean := (Flags and RENAME_NOREPLACE) /= 0;
      Src_FS, Tgt_FS   : VFS.FS_Handle;
      Src_Ino, Tgt_Ino : VFS.File_Inode_Number;
      Success          : VFS.FS_Status;
      User             : Unsigned_32;
      Map              : Page_Table_Acc;
      Success_1        : Boolean;
      Success_2        : Boolean;
   begin
      if Source_Len > Path_Max_Len or Target_Len > Path_Max_Len then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         subtype Src_String is String (1 .. Natural (Source_Len));
         subtype Tgt_String is String (1 .. Natural (Target_Len));
         package Trans_1 is new Memory.Userland_Transfer (Src_String);
         package Trans_2 is new Memory.Userland_Transfer (Tgt_String);
         Src : Src_String;
         Tgt : Tgt_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans_1.Take_From_Userland (Map, Src, Src_SAddr, Success_1);
         Trans_2.Take_From_Userland (Map, Tgt, Tgt_SAddr, Success_2);
         if not Success_1 or not Success_2 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Rename;

   procedure List_Procs
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);

      declare
         subtype KProc_List is Process_Info_Arr (1 .. Natural (Length));
         subtype Proc_List  is Proc_Info_Arr (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Proc_List);
         Ret   : Natural;
         KProc : KProc_List;
         Procs : Proc_List;
         Succs : Boolean;
         Stamp : Timestamp;
      begin
         List_All (KProc, Ret);
         for I in 1 .. Ret loop
            Procs (I) :=
               (Identifier  => KProc (I).Identifier,
                Id_Len      => Unsigned_16 (KProc (I).Identifier_Len),
                Parent_PID  => Unsigned_16 (Convert (KProc (I).Parent)),
                Process_PID => Unsigned_16 (Convert (KProc (I).Process)),
                UID         => KProc (I).User,
                Flags       => 0,
                Elapsed_Time => (0, 0));
            Get_Elapsed_Time (KProc (I).Process, Stamp);
            Procs (I).Elapsed_Time := (Stamp.Seconds, Stamp.Nanoseconds);
            if KProc (I).Is_Being_Traced then
               Procs (I).Flags := Procs (I).Flags or PROC_IS_TRACED;
            end if;
            if KProc (I).Has_Exited then
               Procs (I).Flags := Procs (I).Flags or PROC_EXITED;
            end if;
         end loop;

         Trans.Paste_Into_Userland (Map, Procs, SAddr, Succs);
         if Succs then
            Returned := Unsigned_64 (Ret);
            Errno    := Error_No_Error;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end List_Procs;

   procedure Get_SID
      (ID       : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : PID;
   begin
      if ID = 0 then
         Proc := Arch.Local.Get_Current_Process;
      else
         Proc := Userland.Process.Convert (Natural (ID and 16#FFFFFF#));
         if Proc = Error_PID then
            Errno    := Error_Bad_Search;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      Userland.Process.Get_Session_ID (Proc, Unsigned_32 (Returned));
      Errno := Error_No_Error;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_SID;

   procedure List_Mounts
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);

      declare
         subtype KMount_List is Mountpoint_Arr (1 .. Natural (Length));
         subtype Mount_List  is Mount_Info_Arr (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Mount_List);

         KMnts : KMount_List;
         Ret   : Natural;
         Mnts  : Mount_List;
         Succs : Boolean;
      begin
         List_All (KMnts, Ret);
         for I in 1 .. Ret loop
            case Get_Backing_FS (KMnts (I)) is
               when FS_DEV => Mnts (I).FS_Type := MNT_DEV;
               when FS_EXT => Mnts (I).FS_Type := MNT_EXT;
               when FS_FAT => Mnts (I).FS_Type := MNT_FAT;
            end case;
            Mnts (I).Flags := 0;
            Fetch_Name
               (Get_Backing_Device (KMnts (I)),
                Mnts (I).Source,
                Natural (Mnts (I).Source_Len));
            Get_Mount_Point
               (KMnts (I),
                Mnts (I).Location,
                Natural (Mnts (I).Location_Len));
            Get_Block_Size (KMnts (I), Mnts (I).Block_Size);
            Get_Fragment_Size (KMnts (I), Mnts (I).Fragment_Size);
            Get_Size (KMnts (I), Mnts (I).Size_In_Frags);
            Get_Inode_Count (KMnts (I), Mnts (I).Inode_Count);
            Get_Max_Length (KMnts (I), Mnts (I).Max_File_Name);
            Get_Free_Blocks
               (KMnts (I), Mnts (I).Free_Blocks, Mnts (I).Free_BlocksU);
            Get_Free_Blocks
               (KMnts (I), Mnts (I).Free_Blocks, Mnts (I).Free_BlocksU);
         end loop;

         Trans.Paste_Into_Userland (Map, Mnts, SAddr, Succs);
         if Succs then
            Returned := Unsigned_64 (Ret);
            Errno    := Error_No_Error;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end List_Mounts;

   procedure Uname
      (Addr     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (UTS_Name);
      Proc    : constant             PID := Arch.Local.Get_Current_Process;
      IAddr   : constant Integer_Address := Integer_Address (Addr);
      Map     : Page_Table_Acc;
      UTS     : UTS_Name;
      Len     : Natural;
      Success : Boolean;
   begin
      Networking.Get_Hostname (UTS.Node_Name, Len);
      UTS.Node_Name (Len + 1) := Ada.Characters.Latin_1.NUL;
      UTS.System_Name (1 .. Config.Name'Length + 1) :=
         Config.Name & Ada.Characters.Latin_1.NUL;
      UTS.Release (1 .. Config.Version'Length + 1) :=
         Config.Version & Ada.Characters.Latin_1.NUL;
      UTS.Version (1 .. Config.Arch_Name'Length + 1) :=
         Config.Arch_Name & Ada.Characters.Latin_1.NUL;
      UTS.Machine (1 .. Config.Architecture'Length + 1) :=
         Config.Architecture & Ada.Characters.Latin_1.NUL;

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, UTS, To_Address (IAddr), Success);
      if Success then
         Errno    := Error_No_Error;
         Returned := 0;
      else
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Uname;

   procedure List_Threads
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);

      declare
         subtype KTh is Scheduler.Thread_Listing_Arr  (1 .. Natural (Length));
         subtype Th_List is Thread_Info_Arr (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Th_List);

         KInfo : KTh;
         Ret  : Natural;
         N    : Niceness;
         Info : Th_List;
         Succ : Boolean;
      begin
         List_All (KInfo, Ret);
         for I in 1 .. Ret loop
            Info (I) :=
               (Thread_Id   => Unsigned_16 (Convert (KInfo (I).Thread)),
                Niceness    => 0,
                Priority    => 0,
                Process_PID => Unsigned_16 (KInfo (I).Proc));
            N := Get_Niceness (KInfo (I).Thread);
            if N >= 0 then
               Info (I).Niceness := Unsigned_16 (N);
            else
               Info (I).Niceness := Unsigned_16'Last - Unsigned_16 (abs N) + 1;
            end if;
            Info (I).Priority := Unsigned_16
               (Scheduler.Get_Priority (KInfo (I).Thread));
         end loop;

         Trans.Paste_Into_Userland (Map, Info, SAddr, Succ);
         if Succ then
            Returned := Unsigned_64 (Ret);
            Errno    := Error_No_Error;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end List_Threads;

   procedure List_NetInter
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);

      declare
         subtype KInter_Arr is
            Networking.Interfaces.Interface_Arr (1 .. Natural (Length));
         subtype Inter_Arr is Interface_Arr (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Inter_Arr);

         KInfo : KInter_Arr;
         Ret   : Natural;
         NLen  : Natural;
         Info  : Inter_Arr;
         Succ  : Boolean;
      begin
         Networking.Interfaces.List_Interfaces (KInfo, Ret);
         for I in 1 .. Ret loop
            Fetch_Name (KInfo (I).Handle, Info (I).Name (1 .. 64), NLen);
            Info (I).Name (NLen + 1) := Ada.Characters.Latin_1.NUL;
            if KInfo (I).Is_Blocked then
               Info (I).Flags := NETINTR_BLOCKED;
            else
               Info (I).Flags := 0;
            end if;
            Info (I).MAC := KInfo (I).MAC;
            Info (I).IPv4 := KInfo (I).IPv4;
            Info (I).IPv4_Subnet := KInfo (I).IPv4_Subnet;
            Info (I).IPv6 := KInfo (I).IPv6;
            Info (I).IPv6_Subnet := KInfo (I).IPv6_Subnet;
         end loop;

         Trans.Paste_Into_Userland (Map, Info, SAddr, Succ);
         if Succ then
            Returned := Unsigned_64 (Ret);
            Errno    := Error_No_Error;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end List_NetInter;

   procedure Dump_Logs
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      Map   : Page_Table_Acc;
      Succ  : Boolean;
   begin
      --  Arbitrary length just to make sure we do not allocate 30 quintillion
      --  bytes.
      if Length > 4096 * 4 then
         Errno    := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         subtype Logs_String is String (1 .. Natural (Length));
         type Logs_String_Acc is access Logs_String;
         procedure Free is new Ada.Unchecked_Deallocation
            (Logs_String, Logs_String_Acc);
         package Trans is new Memory.Userland_Transfer (Logs_String);
         Logs : Logs_String_Acc := new Logs_String'[others => ' '];
         Ret  : Natural;
      begin
         Get_Common_Map (Proc, Map);
         Messages.Dump_Logs (Logs.all, Ret);
         Trans.Paste_Into_Userland (Map, Logs.all, SAddr, Succ);
         if Succ then
            Returned := Unsigned_64 (Ret);
            Errno    := Error_No_Error;
         else
            Errno    := Error_Would_Fault;
            Returned := Unsigned_64'Last;
         end if;
         Free (Logs);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Dump_Logs;

   procedure List_Filelocks
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);

      declare
         subtype KLocks_Arr is IPC.FileLock.Lock_Arr (1 .. Natural (Length));
         subtype Lock_Arr   is Flock_Info_Arr (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Lock_Arr);

         KLks : KLocks_Arr;
         Ret  : Natural;
         Lks  : Lock_Arr;
         Succ : Boolean;
      begin
         IPC.FileLock.List_All (KLks, Ret);
         for I in 1 .. Ret loop
            Lks (I).PID    := Unsigned_32 (Convert (KLks (I).Acquirer));
            Lks (I).Mode   := (if KLks (I).Is_Writing then 1 else 0);
            Lks (I).Start  := KLks (I).Start;
            Lks (I).Length := KLks (I).Length;
            Lks (I).FS     := Unsigned_64
               (Get_Unique_ID (Get_Backing_Device (KLks (I).FS)));
            Lks (I).Ino    := Unsigned_64 (KLks (I).Ino);
         end loop;

         Trans.Paste_Into_Userland (Map, Lks, SAddr, Succ);
         if Succ then
            Returned := Unsigned_64 (Ret);
            Errno    := Error_No_Error;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end List_Filelocks;

   procedure Loadavg
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
      package Trans is new Memory.Userland_Transfer (Load_Arr);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      Map   : Page_Table_Acc;
      Lks   : Load_Arr;
      Success : Boolean;
   begin
      if Length / (Unsigned_32'Size / 8) < Lks'Length then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Scheduler.Get_Load_Averages (Lks (1), Lks (2), Lks (3));

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Lks, To_Address (IAddr), Success);
      if Success then
         Returned := 3;
         Errno    := Error_No_Error;
      else
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Loadavg;

   procedure Meminfo
      (Addr     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Mem_Info);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      Map   : Page_Table_Acc;
      Info  : Mem_Info;
      Succ  : Boolean;
      St          : Memory.MMU.Virtual_Statistics;
      Shared_Size : Unsigned_64;
      Stats       : Memory.Physical.Statistics;
   begin
      Memory.Physical.Get_Statistics (Stats);
      Memory.MMU.Get_Statistics (St);
      IPC.SHM.Get_Total_Size (Shared_Size);
      Info :=
         (Phys_Total     => Unsigned_64 (Stats.Total),
          Phys_Available => Unsigned_64 (Stats.Available),
          Phys_Free      => Unsigned_64 (Stats.Free),
          Shared_Usage   => Shared_Size,
          Kernel_Usage   => Unsigned_64 (St.Kernel_Usage),
          Table_Usage    => Unsigned_64 (St.Table_Usage),
          Poison_Usage   => 0);

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Info, To_Address (IAddr), Succ);
      if Succ then
         Returned := 0;
         Errno    := Error_No_Error;
      else
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Meminfo;

   procedure List_PCI
      (Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);

      declare
         subtype PCIs is Devices.PCI.PCI_Listing_Arr (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (PCIs);

         Ret  : Natural;
         Devs : PCIs;
         Succ : Boolean;
      begin
         Devices.PCI.List_All (Devs, Ret);
         Trans.Paste_Into_Userland (Map, Devs, SAddr, Succ);
         if Succ then
            Returned := Unsigned_64 (Ret);
            Errno    := Error_No_Error;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end List_PCI;

   procedure Get_TID (Returned : out Unsigned_64; Errno : out Errno_Value) is
   begin
      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Convert (Arch.Local.Get_Current_Thread));
   end Get_TID;

   procedure Get_Max_Pri (Returned : out Unsigned_64; Errno : out Errno_Value)
   is
   begin
      Returned := Unsigned_64 (Scheduler.Priority'Last);
      Errno    := Error_No_Error;
   end Get_Max_Pri;

   procedure Fcntl
      (FD       : Unsigned_64;
       Command  : Unsigned_64;
       Argument : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Flock_Data);

      Proc        : PID := Arch.Local.Get_Current_Process;
      File        : File_Description_Acc;
      Temp, Temp2 : Boolean;
      New_File    : File_Description_Acc;
      Result_FD   : Natural;
      Map         : Page_Table_Acc;
   begin
      Get_File (Proc, FD, File);
      if File = null then
         Errno := Error_Bad_File;
         goto Error_Return;
      end if;

      Get_Common_Map (Proc, Map);

      Returned := 0;

      case Command is
         when F_DUPFD | F_DUPFD_CLOEXEC =>
            Duplicate (File, New_File);
            Add_File (Proc, New_File, Result_FD, Temp, Natural (Argument));
            if Temp then
               Returned := Unsigned_64 (Result_FD);
               Process.Set_FD_Flags
                  (Proc, Unsigned_64 (Result_FD),
                   Command = F_DUPFD_CLOEXEC, False);
            else
               Errno := Error_Too_Many_Files;
               goto Error_Return;
            end if;
         when F_GETFD =>
            Get_FD_Flags (Proc, FD, Temp, Temp2);
            if Temp then
               Returned := Returned or FD_CLOEXEC;
            end if;
            if Temp2 then
               Returned := Returned or FD_CLOFORK;
            end if;
         when F_SETFD =>
            Set_FD_Flags
               (Proc, FD,
                (Argument and FD_CLOEXEC) /= 0,
                (Argument and FD_CLOFORK) /= 0);
         when F_GETFL =>
            Returned := (if File.Is_Blocking then 0 else O_NONBLOCK);
            case File.Description is
               when Description_Reader_FIFO =>
                  Returned := Returned or O_RDONLY;
               when Description_Writer_FIFO =>
                  Returned := Returned or O_WRONLY;
               when Description_Primary_PTY | Description_Secondary_PTY =>
                  Returned := Returned or O_RDONLY or O_WRONLY;
               when Description_Inode =>
                  if File.Inner_Ino_Read then
                     Returned := Returned or O_RDONLY;
                  end if;
                  if File.Inner_Ino_Write then
                     Returned := Returned or O_WRONLY;
                  end if;
               when Description_Socket =>
                  Returned := Returned or O_RDONLY or O_WRONLY;
            end case;
         when F_SETFL =>
            File.Is_Blocking := (Argument and O_NONBLOCK) = 0;
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
                     goto Error_Return;
                  end if;
               when Description_Writer_FIFO =>
                  Set_Size (File.Inner_Writer_FIFO, Natural (Argument), Temp);
                  if not Temp then
                     Errno := Error_Would_Block;
                     goto Error_Return;
                  end if;
               when others =>
                  goto Invalid_Return;
            end case;
         when F_GETLK | F_SETLK | F_SETLKW =>
            declare
               IAddr : constant Integer_Address := Integer_Address (Argument);
               Addr  : constant  System.Address := To_Address (IAddr);
               Lock  : Flock_Data;
               IW    : Boolean;
            begin
               if File.Description /= Description_Inode then
                  goto Invalid_Return;
               end if;

               Trans.Take_From_Userland (Map, Lock, Addr, Temp);
               if not Temp then
                  goto Would_Fault_Error;
               end if;
               IW := Lock.Lock_Type = F_WRLCK;

               if Command = F_GETLK then
                  Proc := Convert (Natural (Lock.PID));
                  IPC.FileLock.Could_Acquire_Lock
                     (Acquired_FS  => File.Inner_Ino_FS,
                      Acquired_Ino => File.Inner_Ino,
                      Start        => Lock.Start,
                      Length       => Lock.Length,
                      Acquirer     => Proc,
                      Is_Write     => IW,
                      Success      => Temp);
                  if Temp then
                     Lock.Lock_Type := F_UNLCK;
                  end if;
                  Lock.PID := Unsigned_32 (Convert (Proc));

                  Trans.Paste_Into_Userland (Map, Lock, Addr, Temp);
                  if not Temp then
                     goto Would_Fault_Error;
                  end if;
               else
                  if Lock.Lock_Type = F_UNLCK then
                     IPC.FileLock.Release_Lock
                        (Acquired_FS  => File.Inner_Ino_FS,
                         Acquired_Ino => File.Inner_Ino,
                         Start        => Lock.Start,
                         Length       => Lock.Length,
                         Acquirer     => Proc,
                         Success      => Temp);
                     File.Inner_Is_Locked := not Temp;
                  else
                     IPC.FileLock.Acquire_Lock
                        (Acquired_FS  => File.Inner_Ino_FS,
                         Acquired_Ino => File.Inner_Ino,
                         Start        => Lock.Start,
                         Length       => Lock.Length,
                         Acquirer     => Proc,
                         Is_Write     => IW,
                         Is_Blocking  => Command = F_SETLKW,
                         Success      => Temp);
                     File.Inner_Is_Locked := Temp;
                  end if;

                  if not Temp then
                     Errno := Error_Would_Block;
                     goto Error_Return;
                  end if;
               end if;
            end;
         when others =>
            goto Invalid_Return;
      end case;

      Errno := Error_No_Error;
      return;

   <<Invalid_Return>>
      Errno := Error_Invalid_Value;
      goto Error_Return;

   <<Would_Fault_Error>>
      Errno := Error_Would_Fault;
      goto Error_Return;

   <<Error_Return>>
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
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

   procedure Get_Entropy
     (Address  : Unsigned_64;
      Length   : Unsigned_64;
      Returned : out Unsigned_64;
      Errno    : out Errno_Value)
   is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
      Proc   : constant              PID := Arch.Local.Get_Current_Process;
      IAddr  : constant  Integer_Address := Integer_Address (Address);
      SAddr  : constant   System.Address := To_Address (IAddr);
      Map    : Page_Table_Acc;
      Success : Boolean;
   begin
      if not Get_Capabilities (Proc).Can_Access_Entropy then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("getentropy", Proc);
         Returned := Unsigned_64'Last;
      elsif Length > Entropy_Max_Len then
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
      else
         declare
            subtype Crypto_Data is
               Cryptography.Random.Crypto_Data (1 .. Natural (Length));
            package Trans is new Memory.Userland_Transfer (Crypto_Data);
            Data : Crypto_Data;
         begin
            Cryptography.Random.Fill_Data (Data);
            Get_Common_Map (Proc, Map);
            Trans.Paste_Into_Userland (Map, Data, SAddr, Success);
            if Success then
               Errno    := Error_No_Error;
               Returned := 0;
            else
               Errno := Error_Would_Fault;
               Returned := Unsigned_64'Last;
            end if;
         end;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_Entropy;

   procedure MProtect
     (Address    : Unsigned_64;
      Length     : Unsigned_64;
      Protection : Unsigned_64;
      Returned   : out Unsigned_64;
      Errno      : out Errno_Value)
   is
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      Flags : constant Arch.MMU.Page_Permissions :=
         Get_Mmap_Prot (Protection);
      Addr : constant System.Address := To_Address (Integer_Address (Address));
      Map  : Page_Table_Acc;
      Succ : Boolean;
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

      Get_Common_Map (Proc, Map);
      Remap_Range (Map, Addr, Storage_Count (Length), Flags, Succ);
      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      if Caps.Can_Use_Clocks        then Res := Res or MAC_CAP_CLOCK;   end if;
      if Caps.Can_Signal_All      then Res := Res or MAC_CAP_SIGNALALL; end if;
      if Caps.Can_Change_GIDs       then Res := Res or MAC_CAP_SETGID;  end if;
      if Caps.Can_Bypass_IPC_Checks then Res := Res or MAC_CAP_IPC;     end if;
      if Caps.Can_Check_System_Logs then Res := Res or MAC_CAP_SYS_LOG; end if;

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
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Addr      : constant Integer_Address := Integer_Address (Path_Addr);
      Perms     : MAC.Permissions;
      User      : Unsigned_32;
      Status    : MAC.Addition_Status;
      FS_Status : VFS.FS_Status;
      FS        : VFS.FS_Handle;
      Ino       : VFS.File_Inode_Number;
      Map       : Page_Table_Acc;
      Success   : Boolean;
   begin
      if Path_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Path_String is String (1 .. Natural (Path_Len));
         package Trans is new Memory.Userland_Transfer (Path_String);
         Path : Path_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans.Take_From_Userland (Map, Path, To_Address (Addr), Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Perms :=
            (Includes_Contents => (Flags and MAC_PERM_CONTENTS) /= 0,
             Can_Read          => (Flags and MAC_PERM_READ)     /= 0,
             Can_Write         => (Flags and MAC_PERM_WRITE)    /= 0,
             Can_Execute       => (Flags and MAC_PERM_EXEC)     /= 0,
             Can_Append_Only   => (Flags and MAC_PERM_APPEND)   /= 0,
             Can_Lock_Files    => (Flags and MAC_PERM_FLOCK)    /= 0);

         Userland.Process.Get_Effective_UID (Proc, User);

         VFS.Open (Path, FS, Ino, FS_Status, User, True, False);
         if FS_Status /= VFS.FS_Success then
            Translate_Status (FS_Status, 0, Returned, Errno);
            return;
         end if;

         Add_Entity (Proc, FS, Ino, Perms, Status);

         case Status is
            when MAC.Success =>
               Errno    := Error_No_Error;
               Returned := 0;
               return;
            when MAC.No_Space       => Errno := Error_No_Memory;
            when MAC.Is_Conflicting => Errno := Error_Invalid_Value;
         end case;

         Returned := Unsigned_64'Last;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
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
      Proc       : constant              PID := Arch.Local.Get_Current_Process;
      Src_IAddr  : constant  Integer_Address := Integer_Address (Source_Addr);
      Tgt_IAddr  : constant  Integer_Address := Integer_Address (Target_Addr);
      Src_Addr   : constant   System.Address := To_Address (Src_IAddr);
      Tgt_Addr   : constant   System.Address := To_Address (Tgt_IAddr);
      Do_RO      : constant          Boolean := (Flags and MS_RDONLY)   /= 0;
      Do_Remount : constant          Boolean := (Flags and MS_REMOUNT)  /= 0;
      Do_Relatim : constant          Boolean := (Flags and MS_RELATIME) /= 0;
      Do_Noatime : constant          Boolean := (Flags and MS_NOATIME)  /= 0;
      Parsed_Kind : VFS.FS_Type;
      Parsed_Acc : VFS.Access_Time_Policy;
      Map        : Page_Table_Acc;
      Success_1  : VFS.FS_Status;
      Success_2  : Boolean;
      Success_3  : Boolean;
      Handle     : FS_Handle;
      Matched    : Natural;
   begin
      if not Get_Capabilities (Proc).Can_Manage_Mounts then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mount", Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif Source_Len > Path_Max_Len or Target_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      Parsed_Acc :=
         (if    Do_Relatim then VFS.Relative_Update
          elsif Do_Noatime then Do_Not_Update
          else  Always_Update);

      declare
         subtype Src_String is String (1 .. Natural (Source_Len));
         subtype Tgt_String is String (1 .. Natural (Target_Len));
         package Trans_1 is new Memory.Userland_Transfer (Src_String);
         package Trans_2 is new Memory.Userland_Transfer (Tgt_String);
         Source : Src_String;
         Target : Tgt_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans_1.Take_From_Userland (Map, Source, Src_Addr, Success_2);
         Trans_2.Take_From_Userland (Map, Target, Tgt_Addr, Success_3);
         if not Success_2 or not Success_3 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         if Do_Remount then
            VFS.Get_Mount (Target, Matched, Handle);
            if Handle /= VFS.Error_Handle then
               VFS.Remount (Handle, Do_RO, Parsed_Acc, Success_2);
            else
               Success_2 := False;
            end if;

            if Success_2 then
               Errno := Error_No_Error;
               Returned := 0;
            else
               Errno := Error_IO;
               Returned := Unsigned_64'Last;
            end if;
         else
            case FSType is
               when MNT_EXT => Parsed_Kind := VFS.FS_EXT;
               when MNT_FAT => Parsed_Kind := VFS.FS_FAT;
               when others  =>
                  Errno := Error_Invalid_Value;
                  Returned := Unsigned_64'Last;
                  return;
            end case;

            VFS.Mount
               (Source, Target, Parsed_Kind, Do_RO, Parsed_Acc, Success_1);
            Translate_Status (Success_1, 0, Returned, Errno);
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Mount;

   procedure Umount
      (Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Curr_Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      Flag_Force : constant Boolean := (Flags and MNT_FORCE) /= 0;
      Map        : Page_Table_Acc;
      Success    : Boolean;
   begin
      if not Get_Capabilities (Curr_Proc).Can_Manage_Mounts then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("umount", Curr_Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif Path_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Path_String is String (1 .. Natural (Path_Len));
         package Trans is new Memory.Userland_Transfer (Path_String);
         Path : Path_String;
      begin
         Get_Common_Map (Curr_Proc, Map);
         Trans.Take_From_Userland (Map, Path, Path_SAddr, Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Path_IAddr   : constant Integer_Address := Integer_Address (Path_Addr);
      Buffer_IAddr : constant Integer_Address := Integer_Address (Buffer_Addr);
      Path_Add     : constant  System.Address := To_Address (Path_IAddr);
      Buffer_Add   : constant  System.Address := To_Address (Buffer_IAddr);
      R_FS, CWD_FS : VFS.FS_Handle;
      CWD_Ino      : VFS.File_Inode_Number;
      Opened_Ino   : File_Inode_Number;
      Ret_Count    : Natural;
      User         : Unsigned_32;
      Status       : VFS.FS_Status;
      Map          : Page_Table_Acc;
      Success      : Boolean;
   begin
      if Path_Len > Path_Max_Len or Buffer_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Path_String is String (1 .. Natural (Path_Len));
         subtype Buff_String is String (1 .. Natural (Buffer_Len));
         package Trans_1 is new Memory.Userland_Transfer (Path_String);
         package Trans_2 is new Memory.Userland_Transfer (Buff_String);
         File_Perms : MAC.Permissions;
         Path : Path_String;
         Data : Buff_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans_1.Take_From_Userland (Map, Path, Path_Add, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;

         Process.Get_Effective_UID (Proc, User);
         Resolve_AT_Directive (Proc, Dir_FD, R_FS, CWD_Ino);
         if R_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         VFS.Open (R_FS, CWD_Ino, Path, CWD_FS, Opened_Ino, Status, User, True,
            False, False);
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

         VFS.Read_Symbolic_Link (CWD_FS, Opened_Ino, Data, Ret_Count, Status);
         Trans_2.Paste_Into_Userland (Map, Data, Buffer_Add, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
         Translate_Status (Status, Unsigned_64 (Ret_Count), Returned, Errno);
         return;
      end;

   <<Would_Fault_Error>>
      Returned := Unsigned_64'Last;
      Errno    := Error_Would_Fault;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Buff_Addr  : constant  System.Address := To_Address (Buff_IAddr);
      Buff_Len   : Unsigned_64;
      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      File       : File_Description_Acc;
      Tmp_Buffer : VFS.Directory_Entities_Acc;
      Read_Len   : Natural;
      Success    : VFS.FS_Status;
      Success2   : Boolean;
      User       : Unsigned_32;
      Map        : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, FD, File);
      if File = null or else File.Description /= Description_Inode then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
      else
         Userland.Process.Get_Effective_UID (Proc, User);
         Buff_Len   := Buffer_Len / (Dirent'Size / 8);
         Tmp_Buffer := new VFS.Directory_Entities'[1 .. Natural (Buff_Len) =>
            (Inode_Number => 0,
             Name_Buffer  => [others => ' '],
             Name_Len     => 0,
             Type_Of_File => VFS.File_Regular)];
         VFS.Read_Entries
            (Key       => File.Inner_Ino_FS,
             Ino       => File.Inner_Ino,
             Offset    => Natural (File.Inner_Ino_Pos),
             Entities  => Tmp_Buffer.all,
             Ret_Count => Read_Len,
             Success   => Success);

         if Success = VFS.FS_Success then
            File.Inner_Ino_Pos := File.Inner_Ino_Pos + Unsigned_64 (Read_Len);

            declare
               Len : constant Unsigned_64 := Buff_Len;
               subtype Dirents_Arr is Dirents (1 .. Len);
               package Trans is new Memory.Userland_Transfer (Dirents_Arr);
               Buffer : Dirents_Arr;
            begin
               for I in 1 .. Read_Len loop
                  Buffer (Unsigned_64 (I)) :=
                     (D_Ino    => Tmp_Buffer (I).Inode_Number,
                      D_Off    => (Dirent'Size / 8) * Unsigned_64 (I),
                      D_Reclen => Dirent'Size / 8,
                      D_Type   => 0,
                      D_Name   => [others => Ada.Characters.Latin_1.NUL]);
                  Buffer (Unsigned_64 (I)).D_Name
                     (1 .. Tmp_Buffer (I).Name_Len) :=
                     Tmp_Buffer (I).Name_Buffer (1 .. Tmp_Buffer (I).Name_Len);
                  Buffer (Unsigned_64 (I)).D_Type :=
                     (case Tmp_Buffer (I).Type_Of_File is
                        when File_Regular          => DT_REG,
                        when File_Directory        => DT_DIR,
                        when File_Symbolic_Link    => DT_LNK,
                        when File_Character_Device => DT_CHR,
                        when File_Block_Device     => DT_BLK);
               end loop;

               Trans.Paste_Into_Userland (Map, Buffer, Buff_Addr, Success2);
               if Success2 then
                  Returned := Unsigned_64 (Read_Len * (Dirent'Size / 8));
                  Errno    := Error_No_Error;
               else
                  Returned := Unsigned_64'Last;
                  Errno    := Error_Would_Fault;
               end if;

               Free (Tmp_Buffer);
            end;
         else
            Returned := Unsigned_64'Last;
            Errno    := Error_No_Entity;
         end if;
      end if;
   exception
      when others =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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

      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      CWD_FS     : VFS.FS_Handle;
      CWD_Ino    : VFS.File_Inode_Number;
      Node_Type  : File_Type;
      Tmp_Mode   : File_Mode;
      Status     : VFS.FS_Status;
      Umask      : VFS.File_Mode;
      User       : Unsigned_32;
      Map        : Page_Table_Acc;
      Success    : Boolean;
   begin
      if Path_Len > Path_Max_Len then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         subtype Path_String is String (1 .. Natural (Path_Len));
         package Trans is new Memory.Userland_Transfer (Path_String);
         Path : Path_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans.Take_From_Userland (Map, Path, Path_SAddr, Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

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

         Tmp_Mode := File_Mode (Mode and 8#777#);

         Userland.Process.Get_Umask         (Proc, Umask);
         Userland.Process.Get_Effective_UID (Proc, User);
         Create_Node
            (Key      => CWD_FS,
             Relative => CWD_Ino,
             Path     => Path,
             Kind      => Node_Type,
             Mode     => VFS.Apply_Umask (Tmp_Mode, Umask),
             User     => User,
             Status   => Status);
         Translate_Status (Status, 0, Returned, Errno);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end MakeNode;

   procedure Unlink
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Do_Dirs    : constant         Boolean := Flags = AT_REMOVEDIR;
      Curr_Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      CWD_FS     : VFS.FS_Handle;
      CWD_Ino    : VFS.File_Inode_Number;
      Success    : VFS.FS_Status;
      Success2   : Boolean;
      User       : Unsigned_32;
      Map        : Page_Table_Acc;
   begin
      if Path_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Path_String is String (1 .. Natural (Path_Len));
         package Trans is new Memory.Userland_Transfer (Path_String);
         Path : Path_String;
      begin
         Get_Common_Map (Curr_Proc, Map);
         Trans.Take_From_Userland (Map, Path, Path_SAddr, Success2);
         if not Success2 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Process.Get_Effective_UID (Curr_Proc, User);
         Resolve_AT_Directive (Curr_Proc, Dir_FD, CWD_FS, CWD_Ino);
         if CWD_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         VFS.Unlink (CWD_FS, CWD_Ino, Path, User, Do_Dirs, Success);
         Translate_Status (Success, 0, Returned, Errno);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Unlink;

   procedure Truncate
      (FD       : Unsigned_64;
       New_Size : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      File    : File_Description_Acc;
      Success : VFS.FS_Status;
      User    : Unsigned_32;
   begin
      Get_File (Proc, FD, File);
      if File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Userland.Process.Get_Effective_UID (Proc, User);
      case File.Description is
         when Description_Inode =>
            VFS.Truncate
               (File.Inner_Ino_FS, File.Inner_Ino, New_Size, Success);
            Translate_Status (Success, 0, Returned, Errno);
         when others =>
            Errno := Error_Bad_File;
            Returned := Unsigned_64'Last;
      end case;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Truncate;

   procedure Bind
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      pragma Unreferenced (Addr_Len);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr_Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      File  : File_Description_Acc;
      Succ  : Boolean;
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In);
               Addr : SockAddr_In;
            begin
               Trans.Take_From_Userland (Map, Addr, SAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;

               Succ := Bind
                  (Sock => File.Inner_Socket,
                   Addr => Addr.Sin_Addr,
                   Port => Networking.IPv4_Port (Addr.Sin_Port));
            end;
         when IPC.Socket.IPv6 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In6);
               Addr : SockAddr_In6;
            begin
               Trans.Take_From_Userland (Map, Addr, SAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;

               Succ := Bind
                  (Sock => File.Inner_Socket,
                   Addr => Addr.Sin6_Addr,
                   Port => Networking.IPv6_Port (Addr.Sin6_Port));
            end;
         when IPC.Socket.UNIX =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_UNIX);
               Addr : SockAddr_UNIX;
               Len  : Natural := 0;
            begin
               Trans.Take_From_Userland (Map, Addr, SAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;

               for C of Addr.Sun_Path loop
                  exit when C = Ada.Characters.Latin_1.NUL;
                  Len := Len + 1;
               end loop;

               Bind (File.Inner_Socket, Addr.Sun_Path (1 .. Len), Succ);
            end;
      end case;

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_IO;
         Returned := Unsigned_64'Last;
      end if;
      return;

   <<Would_Fault_Error>>
      Errno := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      Targ_IAddr : constant Integer_Address := Integer_Address (Target_Addr);
      Targ_SAddr : constant  System.Address := To_Address (Targ_IAddr);
      CWD_FS     : VFS.FS_Handle;
      CWD_Ino    : VFS.File_Inode_Number;
      Success    : VFS.FS_Status;
      Succ1, Succ2 : Boolean;
      User       : Unsigned_32;
      Map        : Page_Table_Acc;
   begin
      if Path_Len > Path_Max_Len or Target_Len > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Path_String   is String (1 .. Natural (Path_Len));
         subtype Target_String is String (1 .. Natural (Target_Len));
         package Trans_1 is new Memory.Userland_Transfer (Path_String);
         package Trans_2 is new Memory.Userland_Transfer (Target_String);
         Path   : Path_String;
         Targ   : Target_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans_1.Take_From_Userland (Map, Path, Path_SAddr, Succ1);
         Trans_2.Take_From_Userland (Map, Targ, Targ_SAddr, Succ2);
         if not Succ1 or not Succ2 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Process.Get_Effective_UID (Proc, User);
         Resolve_AT_Directive (Proc, Dir_FD, CWD_FS, CWD_Ino);
         if CWD_FS = VFS.Error_Handle then
            Returned := Unsigned_64'Last;
            Errno    := Error_Bad_File;
            return;
         end if;

         VFS.Create_Symbolic_Link
            (Key => CWD_FS,
             Relative => CWD_Ino,
             Path     => Path,
             Target   => Targ,
             Mode     => Unsigned_32 (Mode),
             User     => User,
             Status   => Success);
         Translate_Status (Success, 0, Returned, Errno);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Symlink;

   procedure Connect
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      pragma Unreferenced (Addr_Len);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr_Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      File  : File_Description_Acc;
      Succ  : Boolean;
      Map   : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In);
               Addr : SockAddr_In;
            begin
               Trans.Take_From_Userland (Map, Addr, SAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;

               Connect
                  (Sock    => File.Inner_Socket,
                   Addr    => Addr.Sin_Addr,
                   Port    => Networking.IPv4_Port (Addr.Sin_Port),
                   Success => Succ);
            end;
         when IPC.Socket.IPv6 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In6);
               Addr : SockAddr_In6;
            begin
               Trans.Take_From_Userland (Map, Addr, SAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;

               Connect
                  (Sock    => File.Inner_Socket,
                   Addr    => Addr.Sin6_Addr,
                   Port    => Networking.IPv6_Port (Addr.Sin6_Port),
                   Success => Succ);
            end;
         when IPC.Socket.UNIX =>
            declare
               UID, GID : Unsigned_32;
               package Trans is new Memory.Userland_Transfer (SockAddr_UNIX);
               Addr : SockAddr_UNIX;
               Len  : Natural := 0;
            begin
               Trans.Take_From_Userland (Map, Addr, SAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;

               for C of Addr.Sun_Path loop
                  exit when C = Ada.Characters.Latin_1.NUL;
                  Len := Len + 1;
               end loop;

               Process.Get_UID (Proc, UID);
               Process.Get_GID (Proc, GID);
               Connect
                  (File.Inner_Socket, Addr.Sun_Path (1 .. Len),
                   Unsigned_32 (Convert (Proc)), UID, GID, Succ);
            end;
      end case;

      if Succ then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_IO;
         Returned := Unsigned_64'Last;
      end if;
      return;

   <<Would_Fault_Error>>
      Errno := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Connect;

   procedure Open_PTY
      (Result_Addr : Unsigned_64;
       Returned    : out Unsigned_64;
       Errno       : out Errno_Value)
   is
      pragma SPARK_Mode (Off); --  File modifications are against SPARK.
      type Result_Arr is array (1 .. 2) of Integer;
      package Trans is new Memory.Userland_Transfer (Result_Arr);

      Res_IAddr : constant Integer_Address := Integer_Address (Result_Addr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Res_PTY        : IPC.PTY.Inner_Acc;
      P_Desc, S_Desc : File_Description_Acc;
      Succ1, Succ2   : Boolean;
      Map            : Page_Table_Acc;
      Result         : Result_Arr;
   begin
      IPC.PTY.Create (Res_PTY);
      if Res_PTY = null then
         Errno    := Error_No_Memory;
         Returned := Unsigned_64'Last;
         return;
      end if;

      P_Desc := new File_Description'
         (Description_Primary_PTY, 0, True, Res_PTY);
      S_Desc := new File_Description'
         (Description_Secondary_PTY, 0, True, Res_PTY);
      Add_File (Proc, P_Desc, Result (1), Succ1);
      Add_File (Proc, S_Desc, Result (2), Succ2);
      if not Succ1 or not Succ2 then
         Close (Res_PTY);
         Close (Res_PTY);
         Close (P_Desc);
         Close (S_Desc);
         Errno := Error_Too_Many_Files;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Result, To_Address (Res_IAddr), Succ1);
      if Succ1 then
         Errno := Error_No_Error;
         Returned := 0;
      else
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;
   end Open_PTY;

   procedure FSync
      (FD       : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant     PID := Arch.Local.Get_Current_Process;
      Data : constant Boolean := Flags /= 0;
      File : File_Description_Acc;
      Succ : VFS.FS_Status;
   begin
      Get_File (Proc, FD, File);
      if File = null then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case File.Description is
         when Description_Inode =>
            Succ := VFS.Synchronize (File.Inner_Ino_FS, File.Inner_Ino, Data);
            Translate_Status (Succ, 0, Returned, Errno);
         when others =>
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
      end case;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      Src_IAddr : constant Integer_Address := Integer_Address (Source_Addr);
      Src_SAddr : constant  System.Address := To_Address (Src_IAddr);
      Dst_IAddr : constant Integer_Address := Integer_Address (Desto_Addr);
      Dst_SAddr : constant  System.Address := To_Address (Dst_IAddr);
      Src_FS, Dst_FS   : VFS.FS_Handle;
      Src_Ino, Dst_Ino : VFS.File_Inode_Number;
      Success          : VFS.FS_Status;
      User             : Unsigned_32;
      Map              : Page_Table_Acc;
      Succ1, Succ2     : Boolean;
   begin
      if Source_Len > Path_Max_Len or Desto_Len > Path_Max_Len then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         subtype Src_String is String (1 .. Natural (Source_Len));
         subtype Dst_String is String (1 .. Natural (Desto_Len));
         package Trans_1 is new Memory.Userland_Transfer (Src_String);
         package Trans_2 is new Memory.Userland_Transfer (Dst_String);
         Src : Src_String;
         Dst : Dst_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans_1.Take_From_Userland (Map, Src, Src_SAddr, Succ1);
         Trans_2.Take_From_Userland (Map, Dst, Dst_SAddr, Succ2);
         if not Succ1 or not Succ2 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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

      Proc : constant PID := Arch.Local.Get_Current_Process;
      TProc, TProc_Parent : PID;
   begin
      TProc := Convert (Positive (Traced_PID));
      if TProc = Error_PID then
         goto Bad_Permission_Error;
      elsif not Get_Capabilities (Proc).Can_Trace_Children then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("ptrace", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      Get_Parent (TProc, TProc_Parent);
      if TProc_Parent /= Proc then
         goto Bad_Permission_Error;
      end if;

      case Request is
         when PTRACE_SYSCALL_PIPE =>
            Set_Traced_Info (TProc, True, Natural (Result_Addr and 16#FFFF#));
         when others =>
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

      Errno := Error_No_Error;
      Returned := 0;
      return;

   <<Bad_Permission_Error>>
      Errno := Error_Bad_Permissions;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end PTrace;

   procedure Listen
      (Sock_FD  : Unsigned_64;
       Backlog  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      File : File_Description_Acc;
      Succ : Boolean;
   begin
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
      elsif Get_Type (File.Inner_Socket) /= IPC.Socket.Stream then
         Errno    := Error_Not_Supported;
         Returned := Unsigned_64'Last;
      else
         IPC.Socket.Listen (File.Inner_Socket, Natural (Backlog), Succ);
         if Succ then
            Errno := Error_No_Error;
            Returned := 0;
         else
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
         end if;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Listen;

   procedure Sys_Accept
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      pragma SPARK_Mode (Off); --  File modifications are against SPARK.
      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      CExec      : constant         Boolean := (Flags and SOCK_CLOEXEC)  /= 0;
      Block      : constant         Boolean := (Flags and SOCK_NONBLOCK) /= 0;
      CloFork    : constant         Boolean := (Flags and SOCK_CLOFORK)  /= 0;
      A_IAddr    : constant Integer_Address := Integer_Address (Addr_Addr);
      A_SAddr    : constant  System.Address := To_Address (A_IAddr);
      AL_IAddr   : constant Integer_Address := Integer_Address (Addr_Len);
      File, Desc : File_Description_Acc;
      Sock       : Socket_Acc;
      Ret        : Natural;
      Succ       : Boolean;
      Map        : Page_Table_Acc;
      PI, UID, GID : Unsigned_32;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif Get_Type (File.Inner_Socket) /= IPC.Socket.Stream then
         Errno    := Error_Not_Supported;
         Returned := Unsigned_64'Last;
         return;
      end if;

      PI := Unsigned_32 (Convert (Proc));
      Get_UID (Proc, UID);
      Get_GID (Proc, GID);

      if A_IAddr = 0 or AL_IAddr = 0 then
         Accept_Connection (File.Inner_Socket, not Block, PI, UID, GID, Sock);
      else
         case Get_Domain (File.Inner_Socket) is
            when IPC.Socket.IPv4 =>
               declare
                  package Trans is new Memory.Userland_Transfer (SockAddr_In);
                  Addr : SockAddr_In;
               begin
                  Accept_Connection
                     (Sock         => File.Inner_Socket,
                      Is_Blocking  => not Block,
                      Peer_Address => Addr.Sin_Addr,
                      Peer_Port    => Networking.IPv4_Port (Addr.Sin_Port),
                      Result       => Sock);

                  Trans.Paste_Into_Userland (Map, Addr, A_SAddr, Succ);
                  if not Succ then
                     goto Would_Fault_Error;
                  end if;
               end;
            when IPC.Socket.IPv6 =>
               declare
                  package Trans is new Memory.Userland_Transfer (SockAddr_In6);
                  Addr : SockAddr_In6;
               begin
                  Accept_Connection
                     (Sock         => File.Inner_Socket,
                      Is_Blocking  => not Block,
                      Peer_Address => Addr.Sin6_Addr,
                      Peer_Port    => Networking.IPv6_Port (Addr.Sin6_Port),
                      Result       => Sock);

                  Trans.Paste_Into_Userland (Map, Addr, A_SAddr, Succ);
                  if not Succ then
                     goto Would_Fault_Error;
                  end if;
               end;
            when IPC.Socket.UNIX =>
               declare
                  package Trans is new Memory.Userland_Transfer
                     (SockAddr_UNIX);
                  Addr : SockAddr_UNIX;
                  Len  : Natural;
               begin
                  Accept_Connection
                     (Sock                => File.Inner_Socket,
                      Is_Blocking         => not Block,
                      Peer_Address        => Addr.Sun_Path,
                      Peer_Address_Length => Len,
                      PID                 => PI,
                      GID                 => GID,
                      UID                 => UID,
                      Result              => Sock);

                  if Len < Addr.Sun_Path'Length then
                     Addr.Sun_Path (Len + 1) := Ada.Characters.Latin_1.NUL;
                  end if;

                  Trans.Paste_Into_Userland (Map, Addr, A_SAddr, Succ);
                  if not Succ then
                     goto Would_Fault_Error;
                  end if;
               end;
         end case;
      end if;

      if Sock /= null then
         Desc := new File_Description'(Description_Socket, 0, not Block, Sock);
         Add_File (Proc, Desc, Ret, Succ);
         if Succ then
            Set_FD_Flags (Proc, Unsigned_64 (Ret), CExec, CloFork);
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Sys_Accept;

   procedure RLimit
      (Limit    : Unsigned_64;
       New_Addr : Unsigned_64;
       Old_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (MAC.Limit_Value);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      New_IAddr : constant Integer_Address := Integer_Address (New_Addr);
      New_SAddr : constant  System.Address := To_Address (New_IAddr);
      Old_IAddr : constant Integer_Address := Integer_Address (Old_Addr);
      Old_SAddr : constant  System.Address := To_Address (Old_IAddr);
      Success   : Boolean;
      Resource  : MAC.Limit_Type;
      Map       : Page_Table_Acc;
      Value     : MAC.Limit_Value;
   begin
      case Limit is
         when RLIMIT_CORE   => Resource := MAC.Core_Size_Limit;
         when RLIMIT_CPU    => Resource := MAC.CPU_Time_Limit;
         when RLIMIT_FSIZE  => Resource := MAC.File_Size_Limit;
         when RLIMIT_NOFILE => Resource := MAC.Opened_File_Limit;
         when RLIMIT_STACK  => Resource := MAC.Stack_Size_Limit;
         when RLIMIT_AS     => Resource := MAC.Memory_Size_Limit;
         when others =>
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
      end case;

      Get_Common_Map (Proc, Map);

      if Old_IAddr /= 0 then
         Value := Get_Limit (Proc, Resource);
         Trans.Paste_Into_Userland (Map, Value, Old_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
      end if;

      if New_IAddr /= 0 then
         Trans.Take_From_Userland (Map, Value, New_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
         Set_Limit (Proc, Resource, Value, Success);
         if not Success then
            Errno    := Error_Bad_Permissions;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   end RLimit;

   procedure Sched_RR_Interval
      (ID       : Unsigned_64;
       New_Addr : Unsigned_64;
       Old_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Time_Spec);
      Proc      : PID;
      New_IAddr : constant Integer_Address := Integer_Address (New_Addr);
      New_SAddr : constant  System.Address := To_Address (New_IAddr);
      Old_IAddr : constant Integer_Address := Integer_Address (Old_Addr);
      Old_SAddr : constant  System.Address := To_Address (Old_IAddr);
      Success   : Boolean;
      Map       : Page_Table_Acc;
      Value     : Time_Spec;
   begin
      if ID = 0 then
         Proc := Arch.Local.Get_Current_Process;
      else
         Proc := Userland.Process.Convert (Natural (ID and 16#FFFFFF#));
         if Proc = Error_PID then
            Errno    := Error_Bad_Search;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      Get_Common_Map (Proc, Map);

      if Old_IAddr /= 0 then
         Get_RR_Interval (Proc, Value.Seconds, Value.Nanoseconds);
         Trans.Paste_Into_Userland (Map, Value, Old_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
      end if;

      if New_IAddr /= 0 then
         if not Get_Capabilities (Proc).Can_Change_Scheduling then
            Errno := Error_Bad_Access;
            Execute_MAC_Failure ("sched_rr_interval", Proc);
            Returned := Unsigned_64'Last;
            return;
         end if;

         Trans.Take_From_Userland (Map, Value, New_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
         Set_RR_Interval (Proc, Value.Seconds, Value.Nanoseconds);
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Sched_RR_Interval;

   procedure FAccess
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Mode      : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      Path_IAddr : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr : constant  System.Address := To_Address (Path_IAddr);
      Rel_FS, FS : VFS.FS_Handle;
      D_Ino, Ino : VFS.File_Inode_Number;
      Succ       : VFS.FS_Status;
      User       : Unsigned_32;
      Map        : Page_Table_Acc;
      Success    : Boolean;
   begin
      if Path_Len > Path_Max_Len then
         Errno := Error_String_Too_Long;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         subtype Path_String is String (1 .. Natural (Path_Len));
         package Trans is new Memory.Userland_Transfer (Path_String);
         Path : Path_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans.Take_From_Userland (Map, Path, Path_SAddr, Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Resolve_AT_Directive (Proc, Dir_FD, Rel_FS, D_Ino);
         if Rel_FS = VFS.Error_Handle then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;

         Process.Get_Effective_UID (Proc, User);

         VFS.Open
            (Key        => Rel_FS,
             Relative   => D_Ino,
             Path       => Path,
             Final_Key  => FS,
             Ino        => Ino,
             Success    => Succ,
             User       => User,
             Want_Read  => False,
             Want_Write => False,
             Do_Follow  => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
         if Succ /= VFS.FS_Success then
            Errno    := Error_No_Entity;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end;

      if (Flags and AT_EACCESS) = 0 then
         Process.Get_UID (Proc, User);
      end if;

      VFS.Check_Access
         (Key         => FS,
          Ino         => Ino,
          Exists_Only => (Mode and F_OK) /= 0,
          Can_Read    => (Mode and R_OK) /= 0,
          Can_Write   => (Mode and W_OK) /= 0,
          Can_Exec    => (Mode and X_OK) /= 0,
          Real_UID    => User,
          Status      => Succ);
      Translate_Status (Succ, 0, Returned, Errno);
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end FAccess;

   procedure PPoll
      (FDs_Addr  : Unsigned_64;
       FDs_Count : Unsigned_64;
       Timeout   : Unsigned_64;
       Sigmask   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      type Unsigned_30 is mod 2 ** 30;
      function C1 is new Ada.Unchecked_Conversion (Unsigned_30, Signal_Bitmap);
      package Trans_2 is new Memory.Userland_Transfer (Time_Spec);
      Proc       : constant             PID := Arch.Local.Get_Current_Process;
      FIAddr     : constant Integer_Address := Integer_Address (FDs_Addr);
      FSAddr     : constant  System.Address := To_Address (FIAddr);
      TIAddr     : constant Integer_Address := Integer_Address (Timeout);
      TSAddr     : constant  System.Address := To_Address (TIAddr);
      S_IAddr    : constant Integer_Address := Integer_Address (Sigmask);
      S_SAddr    : constant  System.Address := To_Address (S_IAddr);
      Count      :                  Natural := 0;
      File       : File_Description_Acc;
      Curr       : Time.Timestamp;
      Final      : Time.Timestamp;
      Old_Set    : Signal_Bitmap;
      Map        : Page_Table_Acc;
      Success    : Boolean;
      Handled    : Boolean;
      Tim        : Time_Spec;
      Can_Read, Can_Write, Can_PrioRead, Is_Error, Is_Broken : Boolean;
   begin
      Get_Common_Map (Proc, Map);

      if S_IAddr /= 0 then
         declare
            package Trans is new Memory.Userland_Transfer (Unsigned_30);
            Passed_Set : Unsigned_30;
         begin
            Trans.Take_From_Userland (Map, Passed_Set, S_SAddr, Success);
            if not Success then
               goto Would_Fault_Error;
            end if;
            Get_Masked_Signals (Proc, Old_Set);
            Set_Masked_Signals (Proc, C1 (Passed_Set));
         end;
      end if;

      Arch.Clocks.Get_Monotonic_Time (Final);
      if TIAddr /= 0 then
         Trans_2.Take_From_Userland (Map, Tim, TSAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
      else
         Tim := (Unsigned_64'Last, Unsigned_64'Last);
      end if;
      Final := Final + (Tim.Seconds, Tim.Nanoseconds);

      --  If we have 0 items, we just eep.
      if FDs_Count = 0 then
         loop
            Arch.Clocks.Get_Monotonic_Time (Curr);
            Clear_Process_Signals (Proc, Handled);
            exit when Handled or else Curr >= Final;
            Scheduler.Yield_If_Able;
         end loop;
         goto Success_Return;
      end if;

      declare
         subtype Polled_FDs is Poll_FDs (1 .. FDs_Count);
         package Trans_1 is new Memory.Userland_Transfer (Polled_FDs);
         FDs : Polled_FDs;
      begin
         Trans_1.Take_From_Userland (Map, FDs, FSAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;

         loop
            for Polled of FDs loop
               Polled.Out_Events := 0;

               --  We are to ignore the FD if its negative.
               if (Polled.FD and Shift_Left (1, 31)) /= 0 then
                  goto End_Iter;
               end if;

               --  Check the FD actually points to anything valuable.
               Get_File (Proc, Unsigned_64 (Polled.FD), File);
               if File = null then
                  Polled.Out_Events := POLLNVAL;
                  goto End_Iter;
               end if;

               --  Fill out events depending on the file type.
               Can_Read     := False;
               Can_Write    := False;
               Can_PrioRead := False;
               Is_Error     := False;
               Is_Broken    := False;
               case File.Description is
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
                        (File.Inner_Primary_PTY, Can_Read, Can_Write,
                         Can_PrioRead);
                  when Description_Secondary_PTY =>
                     IPC.PTY.Poll_Secondary
                        (File.Inner_Secondary_PTY, Can_Read, Can_Write);
                  when Description_Socket =>
                     IPC.Socket.Poll
                        (File.Inner_Socket, Can_Read, Can_Write, Is_Broken,
                         Is_Error);
                  when Description_Inode =>
                     VFS.Poll (File.Inner_Ino_FS, File.Inner_Ino, Can_Read,
                        Can_Write, Is_Error);
               end case;

               if Can_Read and (Polled.Events and POLLIN) /= 0 then
                  Polled.Out_Events := Polled.Out_Events or POLLIN;
               end if;
               if Can_Write and (Polled.Events and POLLOUT) /= 0 then
                  Polled.Out_Events := Polled.Out_Events or POLLOUT;
               end if;
               if Can_PrioRead and (Polled.Events and POLLPRI) /= 0 then
                  Polled.Out_Events := Polled.Out_Events or POLLPRI;
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

            Arch.Clocks.Get_Monotonic_Time (Curr);
            Clear_Process_Signals (Proc, Handled);
            exit when Handled or else Count /= 0 or else Curr >= Final;
            Scheduler.Yield_If_Able;
         end loop;

         Trans_1.Paste_Into_Userland (Map, FDs, FSAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
      end;

   <<Success_Return>>
      if S_IAddr /= 0 then
         Set_Masked_Signals (Proc, Old_Set);
      end if;

      if Handled then
         Errno    := Error_Interrupted;
         Returned := Unsigned_64'Last;
      else
         Errno    := Error_No_Error;
         Returned := Unsigned_64 (Count);
      end if;
      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   exception
      when others =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end PPoll;

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
      Curr_UID : Unsigned_32;
   begin
      if Get_Capabilities (Proc).Can_Change_UIDs then
         if EUID <= Unsigned_64 (Unsigned_32'Last) then
            Userland.Process.Set_Effective_UID (Proc, Unsigned_32 (EUID));
         end if;
         if UID <= Unsigned_64 (Unsigned_32'Last) then
            Userland.Process.Set_UID (Proc, Unsigned_32 (UID));
         end if;
      else
         Userland.Process.Get_UID (Proc, Curr_UID);
         if EUID <= Unsigned_64 (Unsigned_32'Last) then
            if EUID = Unsigned_64 (Curr_UID) then
               Userland.Process.Set_Effective_UID
                  (Proc, Unsigned_32 (EUID and 16#FFFFFFFF#));
            else
               Errno := Error_Bad_Permissions;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end if;
      end if;

      Errno := Error_No_Error;
      Returned := 0;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Path_IAddr  : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr  : constant  System.Address := To_Address (Path_IAddr);
      Rel_FS, FS  : VFS.FS_Handle;
      D_Ino, Ino  : VFS.File_Inode_Number;
      Succ        : VFS.FS_Status;
      User        : Unsigned_32;
      File_Desc   : File_Description_Acc;
      File_Perms  : MAC.Permissions;
      Map         : Page_Table_Acc;
      Success     : Boolean;
   begin
      Process.Get_Effective_UID (Proc, User);

      if (Flags and AT_EMPTY_PATH) /= 0 then
         Get_File (Proc, Dir_FD, File_Desc);
         if File_Desc = null or else File_Desc.Description /= Description_Inode
         then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;
         FS  := File_Desc.Inner_Ino_FS;
         Ino := File_Desc.Inner_Ino;
      else
         if Path_Len > Path_Max_Len then
            Errno := Error_String_Too_Long;
            Returned := Unsigned_64'Last;
            return;
         end if;

         declare
            subtype Path_String is String (1 .. Natural (Path_Len));
            package Trans is new Memory.Userland_Transfer (Path_String);
            Path : Path_String;
         begin
            Get_Common_Map (Proc, Map);
            Trans.Take_From_Userland (Map, Path, Path_SAddr, Success);
            if not Success then
               Returned := Unsigned_64'Last;
               Errno    := Error_Would_Fault;
               return;
            end if;

            Resolve_AT_Directive (Proc, Dir_FD, Rel_FS, D_Ino);
            if Rel_FS = VFS.Error_Handle then
               Errno    := Error_Bad_File;
               Returned := Unsigned_64'Last;
               return;
            end if;

            VFS.Open
               (Key        => Rel_FS,
                Relative   => D_Ino,
                Path       => Path,
                Final_Key  => FS,
                Ino        => Ino,
                Success    => Succ,
                User       => User,
                Want_Read  => True,
                Want_Write => False,
                Do_Follow  => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
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
          Status => Succ);
      Translate_Status (Succ, 0, Returned, Errno);
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
         when RB_HALT     => Arch.Power.Halt (Success);
         when RB_POWEROFF => Arch.Power.Poweroff (Success);
         when RB_RESTART  => Arch.Power.Reboot (Success);
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
         Panic.Hard_Panic ("reboot() operation failed");
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      Path_IAddr  : constant Integer_Address := Integer_Address (Path_Addr);
      Path_SAddr  : constant  System.Address := To_Address (Path_IAddr);
      Rel_FS, FS  : VFS.FS_Handle;
      D_Ino, Ino  : VFS.File_Inode_Number;
      Succ        : VFS.FS_Status;
      Usr         : Unsigned_32;
      File_Desc   : File_Description_Acc;
      File_Perms  : MAC.Permissions;
      Map         : Page_Table_Acc;
      Success     : Boolean;
   begin
      Process.Get_Effective_UID (Proc, Usr);

      if (Flags and AT_EMPTY_PATH) /= 0 then
         Get_File (Proc, Dir_FD, File_Desc);
         if File_Desc = null or else File_Desc.Description /= Description_Inode
         then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;
         FS  := File_Desc.Inner_Ino_FS;
         Ino := File_Desc.Inner_Ino;
      else
         if Path_Len > Path_Max_Len then
            Errno := Error_String_Too_Long;
            Returned := Unsigned_64'Last;
            return;
         end if;

         declare
            subtype Path_String is String (1 .. Natural (Path_Len));
            package Trans is new Memory.Userland_Transfer (Path_String);
            Path : Path_String;
         begin
            Get_Common_Map (Proc, Map);
            Trans.Take_From_Userland (Map, Path, Path_SAddr, Success);
            if not Success then
               Returned := Unsigned_64'Last;
               Errno    := Error_Would_Fault;
               return;
            end if;

            Resolve_AT_Directive (Proc, Dir_FD, Rel_FS, D_Ino);
            if Rel_FS = VFS.Error_Handle then
               Errno    := Error_Bad_File;
               Returned := Unsigned_64'Last;
               return;
            end if;

            VFS.Open
               (Key        => Rel_FS,
                Relative   => D_Ino,
                Path       => Path,
                Final_Key  => FS,
                Ino        => Ino,
                Success    => Succ,
                User       => Usr,
                Want_Read  => True,
                Want_Write => False,
                Do_Follow  => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
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
          Succ);
      Translate_Status (Succ, 0, Returned, Errno);
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Fchown;

   procedure Get_PGID
      (ID       : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : PID;
   begin
      if ID = 0 then
         Proc := Arch.Local.Get_Current_Process;
      else
         Proc := Userland.Process.Convert (Natural (ID and 16#FFFFFF#));
         if Proc = Error_PID then
            Errno    := Error_Bad_Search;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      Userland.Process.Get_PGID (Proc, Unsigned_32 (Returned));
      Errno := Error_No_Error;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_PGID;

   procedure Set_PGID
      (ID       : Unsigned_64;
       PGID     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : PID;
   begin
      if ID = 0 then
         Proc := Arch.Local.Get_Current_Process;
      else
         Proc := Userland.Process.Convert (Natural (ID and 16#FFFFFF#));
         if Proc = Error_PID then
            goto Bad_Search_Error;
         end if;
      end if;

      if PGID = 0 then
         Userland.Process.Set_PGID (Proc, Unsigned_32 (Convert (Proc)));
      else
         Userland.Process.Set_PGID (Proc, Unsigned_32 (PGID and 16#FFFFFFFF#));
      end if;

      Returned := 0;
      Errno    := Error_No_Error;
      return;

   <<Bad_Search_Error>>
      Errno    := Error_Bad_Search;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Set_PGID;

   procedure Get_Sock_Name
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      package Trans_1 is new Memory.Userland_Transfer (Natural);
      Proc   : constant             PID := Arch.Local.Get_Current_Process;
      AIAddr : constant Integer_Address := Integer_Address (Addr_Addr);
      ASAddr : constant  System.Address := To_Address (AIAddr);
      LIAddr : constant Integer_Address := Integer_Address (Addr_Len);
      LSAddr : constant  System.Address := To_Address (LIAddr);
      File   : File_Description_Acc;
      Succ   : Boolean;
      BSucc  : Boolean;
      Map    : Page_Table_Acc;
      Length : Natural;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In);
               Addr : SockAddr_In;
            begin
               Length := Addr'Size / 8;
               Addr.Sin_Family := AF_INET;
               Get_Bound (File.Inner_Socket, Addr.Sin_Addr,
                  Networking.IPv4_Port (Addr.Sin_Port), BSucc);

               Trans.Paste_Into_Userland (Map, Addr, ASAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;
            end;
         when IPC.Socket.IPv6 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In6);
               Addr : SockAddr_In6;
            begin
               Length := Addr'Size / 8;
               Addr.Sin6_Family := AF_INET6;
               Get_Bound (File.Inner_Socket, Addr.Sin6_Addr,
                  Networking.IPv6_Port (Addr.Sin6_Port), BSucc);

               Trans.Paste_Into_Userland (Map, Addr, ASAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;
            end;
         when IPC.Socket.UNIX =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_UNIX);
               Addr : SockAddr_UNIX;
               Len  : Natural;
            begin
               Addr.Sun_Family := AF_UNIX;
               Get_Bound (File.Inner_Socket, Addr.Sun_Path, Len, BSucc);

               if Len < Addr.Sun_Path'Length then
                  Addr.Sun_Path (Len + 1) := Ada.Characters.Latin_1.NUL;
               end if;
               Length := 4 + Len;

               Trans.Paste_Into_Userland (Map, Addr, ASAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;
            end;
      end case;

      Trans_1.Paste_Into_Userland (Map, Length, LSAddr, Succ);
      if not Succ then
         goto Would_Fault_Error;
      end if;

      if BSucc then
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_Sock_Name;

   procedure Get_Peer_Name
      (Sock_FD   : Unsigned_64;
       Addr_Addr : Unsigned_64;
       Addr_Len  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      package Trans_1 is new Memory.Userland_Transfer (Natural);
      Proc   : constant             PID := Arch.Local.Get_Current_Process;
      AIAddr : constant Integer_Address := Integer_Address (Addr_Addr);
      ASAddr : constant  System.Address := To_Address (AIAddr);
      LIAddr : constant Integer_Address := Integer_Address (Addr_Len);
      LSAddr : constant  System.Address := To_Address (LIAddr);
      File   : File_Description_Acc;
      Succ   : Boolean;
      BSucc  : Boolean;
      Map    : Page_Table_Acc;
      Length : Natural;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Get_Domain (File.Inner_Socket) is
         when IPC.Socket.IPv4 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In);
               Addr : SockAddr_In;
            begin
               Length := Addr'Size / 8;
               Addr.Sin_Family := AF_INET;
               Get_Peer (File.Inner_Socket, Addr.Sin_Addr,
                  Networking.IPv4_Port (Addr.Sin_Port), BSucc);

               Trans.Paste_Into_Userland (Map, Addr, ASAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;
            end;
         when IPC.Socket.IPv6 =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_In6);
               Addr : SockAddr_In6;
            begin
               Length := Addr'Size / 8;
               Addr.Sin6_Family := AF_INET6;
               Get_Peer (File.Inner_Socket, Addr.Sin6_Addr,
                  Networking.IPv6_Port (Addr.Sin6_Port), BSucc);

               Trans.Paste_Into_Userland (Map, Addr, ASAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;
            end;
         when IPC.Socket.UNIX =>
            declare
               package Trans is new Memory.Userland_Transfer (SockAddr_UNIX);
               Addr : SockAddr_UNIX;
               Len  : Natural;
            begin
               Addr.Sun_Family := AF_UNIX;
               Get_Peer (File.Inner_Socket, Addr.Sun_Path, Len, BSucc);

               if Len < Addr.Sun_Path'Length then
                  Addr.Sun_Path (Len + 1) := Ada.Characters.Latin_1.NUL;
               end if;
               Length := 4 + Len;

               Trans.Paste_Into_Userland (Map, Addr, ASAddr, Succ);
               if not Succ then
                  goto Would_Fault_Error;
               end if;
            end;
      end case;

      Trans_1.Paste_Into_Userland (Map, Length, LSAddr, Succ);
      if not Succ then
         goto Would_Fault_Error;
      end if;

      if BSucc then
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_Peer_Name;

   procedure Shutdown
      (Sock_FD  : Unsigned_64;
       How      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      File : File_Description_Acc;
      Succ : Boolean;
   begin
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Errno    := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      end if;

      case How is
         when SHUT_RD =>
            IPC.Socket.Shutdown (File.Inner_Socket, True, False, Succ);
         when SHUT_WR =>
            IPC.Socket.Shutdown (File.Inner_Socket, False, True, Succ);
         when SHUT_RDWR =>
            IPC.Socket.Shutdown (File.Inner_Socket, True, True, Succ);
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Shutdown;

   procedure Futex
      (Operation : Unsigned_64;
       Address   : Unsigned_64;
       Count     : Unsigned_64;
       Timeout   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc   : constant             PID := Arch.Local.Get_Current_Process;
      IAddr  : constant Integer_Address := Integer_Address (Address);
      SAddr  : constant  System.Address := To_Address (IAddr);
      TIAddr : constant Integer_Address := Integer_Address (Timeout);
      TSAddr : constant  System.Address := To_Address (TIAddr);
      Map   : Page_Table_Acc;
      Succ2 : IPC.Futex.Wait_Status;
      Succ  : Boolean;
   begin
      if Count > Unsigned_64 (Natural'Last) then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      declare
         Cnt     : constant Natural := Natural (Count);
         subtype User_Futex_Arr is Futex_Item_Arr (1 .. Cnt);
         subtype Kern_Futex_Arr is IPC.Futex.Element_Arr (1 .. Cnt);
         package Trans_1 is new Memory.Userland_Transfer (User_Futex_Arr);
         package Trans_2 is new Memory.Userland_Transfer (Time_Spec);
         Items   : User_Futex_Arr;
         Time    : Time_Spec;
         Futexes : Kern_Futex_Arr;
      begin
         Get_Common_Map (Proc, Map);
         Trans_1.Take_From_Userland (Map, Items, SAddr, Succ);
         if not Succ then goto Would_Fault_Error; end if;
         Trans_2.Take_From_Userland (Map, Time, TSAddr, Succ);
         if not Succ then goto Would_Fault_Error; end if;

         for I in Items'Range loop
            Futexes (I) :=
               (To_Address (Integer_Address (Items (I).Address)),
                Items (I).Expected);
         end loop;

         case Operation is
            when FUTEX_WAIT =>
               IPC.Futex.Wait
                  (Map, Futexes, Time.Seconds, Time.Nanoseconds, Succ2);
               case Succ2 is
                  when IPC.Futex.Wait_No_Space =>
                     Returned := Unsigned_64'Last;
                     Errno    := Error_No_Memory;
                  when IPC.Futex.Wait_Try_Again =>
                     Returned := Unsigned_64'Last;
                     Errno    := Error_Would_Block;
                  when IPC.Futex.Wait_Success =>
                     Returned := 0;
                     Errno    := Error_No_Error;
               end case;
            when FUTEX_WAKE =>
               IPC.Futex.Wake (Futexes, Natural (Returned));
               Errno := Error_No_Error;
            when others =>
               Returned := Unsigned_64'Last;
               Errno    := Error_Invalid_Value;
         end case;
      end;

      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Futex;

   procedure Clock
      (Operation : Unsigned_64;
       Clock_ID  : Unsigned_64;
       Address   : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Time_Spec);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Address);
      Map   : Page_Table_Acc;
      Spec  : Time_Spec;
      Succ  : Boolean;
      Stamp : Timestamp;
   begin
      if not Get_Capabilities (Proc).Can_Use_Clocks then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("clock", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      case Operation is
         when CLOCK_GETRES =>
            case Clock_ID is
               when CLOCK_MONOTONIC =>
                  Arch.Clocks.Get_Monotonic_Resolution (Stamp);
               when CLOCK_REALTIME =>
                  Arch.Clocks.Get_Real_Time_Resolution (Stamp);
               when others =>
                  goto Invalid_Value_Error;
            end case;
            Spec := (Stamp.Seconds, Stamp.Nanoseconds);
         when CLOCK_GETTIME =>
            case Clock_ID is
               when CLOCK_MONOTONIC =>
                  Arch.Clocks.Get_Monotonic_Time (Stamp);
               when CLOCK_REALTIME =>
                  Arch.Clocks.Get_Real_Time (Stamp);
               when others =>
                  goto Invalid_Value_Error;
            end case;
            Spec := (Stamp.Seconds, Stamp.Nanoseconds);
         when CLOCK_SETTIME =>
            case Clock_ID is
               when CLOCK_REALTIME =>
                  Stamp := (Spec.Seconds, Spec.Nanoseconds);
                  Arch.Clocks.Set_Real_Time (Stamp);
               when others =>
                  goto Invalid_Value_Error;
            end case;
         when others =>
            goto Invalid_Value_Error;
      end case;

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Spec, To_Address (IAddr), Succ);
      if Succ then
         Returned := 0;
         Errno    := Error_No_Error;
      else
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
      end if;

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
      package Trans is new Memory.Userland_Transfer (Time_Spec);
      Proc     : constant             PID := Arch.Local.Get_Current_Process;
      ReqIAddr : constant Integer_Address := Integer_Address (Request_Addr);
      RemIAddr : constant Integer_Address := Integer_Address (Remain_Addr);
      Map      : Page_Table_Acc;
      Req, Re  : Time_Spec;
      Success  : Boolean;
      Handled  : Boolean;
      Curr, Final : Time.Timestamp;
   begin
      if not Get_Capabilities (Proc).Can_Use_Clocks then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("clock_nanosleep", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      Get_Common_Map (Proc, Map);
      Trans.Take_From_Userland (Map, Req, To_Address (ReqIAddr), Success);
      if not Success then
         goto Would_Fault_Error;
      end if;

      if (Flags and TIMER_ABSTIME) /= 0 then
         Final := (Req.Seconds, Req.Nanoseconds);
      else
         if Clock_ID = CLOCK_MONOTONIC then
            Arch.Clocks.Get_Monotonic_Time (Final);
         else
            Arch.Clocks.Get_Real_Time (Final);
         end if;
         Final := Final + (Req.Seconds, Req.Nanoseconds);
      end if;

      loop
         if Clock_ID = CLOCK_MONOTONIC then
            Arch.Clocks.Get_Monotonic_Time (Curr);
         else
            Arch.Clocks.Get_Real_Time (Curr);
         end if;
         Clear_Process_Signals (Proc, Handled);
         exit when Handled or Curr >= Final;
         Scheduler.Yield_If_Able;
      end loop;

      Re.Seconds := 0;
      Re.Nanoseconds := 0;
      Trans.Paste_Into_Userland (Map, Re, To_Address (RemIAddr), Success);
      if not Success then
         goto Would_Fault_Error;
      end if;

      if Handled then
         Returned := Unsigned_64'Last;
         Errno    := Error_Interrupted;
      else
         Returned := 0;
         Errno    := Error_No_Error;
      end if;
      return;

   <<Would_Fault_Error>>
      Returned := Unsigned_64'Last;
      Errno    := Error_Would_Fault;
   end Clock_Nanosleep;

   procedure Get_RUsage
      (Who        : Unsigned_64;
       Usage_Addr : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (RUsage);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Usage_Addr);
      Map   : Page_Table_Acc;
      Usage : RUsage;
      Succ  : Boolean;
      T1, T2 : Time.Timestamp;
   begin
      case Who is
         when RUSAGE_SELF =>
            Process.Get_Runtime_Times (Proc, T1, T2);
            Usage.System_Time := (T1.Seconds, T1.Nanoseconds);
            Usage.User_Time := (T2.Seconds, T2.Nanoseconds);
         when RUSAGE_CHILDREN =>
            Process.Get_Children_Runtimes (Proc, T1, T2);
            Usage.System_Time := (T1.Seconds, T1.Nanoseconds);
            Usage.User_Time := (T2.Seconds, T2.Nanoseconds);
         when others =>
            Returned := Unsigned_64'Last;
            Errno    := Error_Invalid_Value;
            return;
      end case;

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Usage, To_Address (IAddr), Succ);
      if Succ then
         Returned := 0;
         Errno    := Error_No_Error;
      else
         Returned := Unsigned_64'Last;
         Errno    := Error_Would_Fault;
      end if;
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
      pragma Unreferenced (Flags, Addr_Len);
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      AIAddr    : constant Integer_Address := Integer_Address (Addr_Addr);
      ASAddr    : constant  System.Address := To_Address (AIAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      File      : File_Description_Acc;
      Ret_Count : Natural;
      Success   : IPC.Socket.Socket_Status;
      Success2  : Boolean;
      Final_Cnt : Natural;
      Map       : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      declare
         Final_Len : constant Natural := Final_Cnt;
         subtype Read_Data is Devices.Operation_Data (1 .. Final_Len);
         package Trans is new Memory.Userland_Transfer (Read_Data);
         procedure Free is new Ada.Unchecked_Deallocation
            (Operation_Data, Operation_Data_Acc);
         Data : Operation_Data_Acc := new Read_Data'[others => 0];
      begin
         if AIAddr /= 0 and Get_Type (File.Inner_Socket) /= IPC.Socket.Stream
         then
            case Get_Domain (File.Inner_Socket) is
               when IPC.Socket.IPv4 =>
                  declare
                     package T is new Memory.Userland_Transfer (SockAddr_In);
                     Addr : SockAddr_In;
                  begin
                     T.Take_From_Userland (Map, Addr, ASAddr, Success2);
                     if not Success2 then
                        Returned := Unsigned_64'Last;
                        Errno    := Error_Would_Fault;
                        goto Cleanup;
                     end if;

                     IPC.Socket.Read
                        (Sock      => File.Inner_Socket,
                         Data      => Data.all,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin_Addr,
                         Port      => Networking.IPv4_Port (Addr.Sin_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.IPv6 =>
                  declare
                     package T is new Memory.Userland_Transfer (SockAddr_In6);
                     Addr : SockAddr_In6;
                  begin
                     T.Take_From_Userland (Map, Addr, ASAddr, Success2);
                     if not Success2 then
                        Returned := Unsigned_64'Last;
                        Errno    := Error_Would_Fault;
                        goto Cleanup;
                     end if;

                     IPC.Socket.Read
                        (Sock      => File.Inner_Socket,
                         Data      => Data.all,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin6_Addr,
                         Port      => Networking.IPv6_Port (Addr.Sin6_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.UNIX =>
                  declare
                     UID, GID : Unsigned_32;
                     Len      : Natural := 0;
                     package T is new Memory.Userland_Transfer (SockAddr_UNIX);
                     Addr : SockAddr_UNIX;
                  begin
                     Get_UID (Proc, UID);
                     Get_GID (Proc, GID);

                     T.Take_From_Userland (Map, Addr, ASAddr, Success2);
                     if not Success2 then
                        Returned := Unsigned_64'Last;
                        Errno    := Error_Would_Fault;
                        goto Cleanup;
                     end if;

                     for C of Addr.Sun_Path loop
                        exit when C = Ada.Characters.Latin_1.NUL;
                        Len := Len + 1;
                     end loop;

                     IPC.Socket.Read
                        (Sock        => File.Inner_Socket,
                         Data        => Data.all,
                         Is_Blocking => File.Is_Blocking,
                         Ret_Count   => Ret_Count,
                         Path        => Addr.Sun_Path (1 .. Len),
                         PID         => Unsigned_32 (Convert (Proc)),
                         UID         => UID,
                         GID         => GID,
                         Success     => Success);
                  end;
            end case;
         else
            IPC.Socket.Read
               (File.Inner_Socket, Data.all, File.Is_Blocking, Ret_Count,
                Success);
         end if;
         Translate_Status (Success, Unsigned_64 (Ret_Count), Returned, Errno);

         if Errno = Error_No_Error then
            Trans.Paste_Into_Userland (Map, Data.all, Buf_SAddr, Success2);
            if not Success2 then
               Returned := Unsigned_64'Last;
               Errno    := Error_Would_Fault;
            end if;
         end if;

      <<Cleanup>>
         Free (Data);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
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
      pragma Unreferenced (Flags, Addr_Len);
      Buf_IAddr : constant Integer_Address := Integer_Address (Buffer);
      Buf_SAddr : constant  System.Address := To_Address (Buf_IAddr);
      AIAddr    : constant Integer_Address := Integer_Address (Addr_Addr);
      ASAddr    : constant  System.Address := To_Address (AIAddr);
      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      File      : File_Description_Acc;
      Ret_Count : Natural;
      Success   : IPC.Socket.Socket_Status;
      Success2  : Boolean;
      Final_Cnt : Natural;
      Map       : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock_FD, File);
      if File = null or else File.Description /= Description_Socket then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      elsif Count > Unsigned_64 (Natural'Last) then
         Final_Cnt := Natural'Last;
      else
         Final_Cnt := Natural (Count);
      end if;

      declare
         Final_Len : constant Natural := Final_Cnt;
         subtype Read_Data is Devices.Operation_Data (1 .. Final_Len);
         package Trans is new Memory.Userland_Transfer (Read_Data);
         procedure Free is new Ada.Unchecked_Deallocation
            (Operation_Data, Operation_Data_Acc);
         Data : Operation_Data_Acc := new Read_Data'[others => 0];
      begin
         Trans.Take_From_Userland (Map, Data.all, Buf_SAddr, Success2);
         if not Success2 then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            goto Cleanup;
         end if;

         if AIAddr /= 0 and Get_Type (File.Inner_Socket) /= IPC.Socket.Stream
         then
            case Get_Domain (File.Inner_Socket) is
               when IPC.Socket.IPv4 =>
                  declare
                     package T is new Memory.Userland_Transfer (SockAddr_In);
                     Addr : SockAddr_In;
                  begin
                     T.Take_From_Userland (Map, Addr, ASAddr, Success2);
                     if not Success2 then
                        Returned := Unsigned_64'Last;
                        Errno    := Error_Would_Fault;
                        goto Cleanup;
                     end if;

                     IPC.Socket.Write
                        (Sock      => File.Inner_Socket,
                         Data      => Data.all,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin_Addr,
                         Port      => Networking.IPv4_Port (Addr.Sin_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.IPv6 =>
                  declare
                     package T is new Memory.Userland_Transfer (SockAddr_In6);
                     Addr : SockAddr_In6;
                  begin
                     T.Take_From_Userland (Map, Addr, ASAddr, Success2);
                     if not Success2 then
                        Returned := Unsigned_64'Last;
                        Errno    := Error_Would_Fault;
                        goto Cleanup;
                     end if;

                     IPC.Socket.Write
                        (Sock      => File.Inner_Socket,
                         Data      => Data.all,
                         Ret_Count => Ret_Count,
                         Addr      => Addr.Sin6_Addr,
                         Port      => Networking.IPv6_Port (Addr.Sin6_Port),
                         Success   => Success);
                  end;
               when IPC.Socket.UNIX =>
                  declare
                     UID, GID : Unsigned_32;
                     Len      : Natural := 0;
                     package T is new Memory.Userland_Transfer (SockAddr_UNIX);
                     Addr : SockAddr_UNIX;
                  begin
                     Get_UID (Proc, UID);
                     Get_GID (Proc, GID);

                     T.Take_From_Userland (Map, Addr, ASAddr, Success2);
                     if not Success2 then
                        Returned := Unsigned_64'Last;
                        Errno    := Error_Would_Fault;
                        goto Cleanup;
                     end if;

                     for C of Addr.Sun_Path loop
                        exit when C = Ada.Characters.Latin_1.NUL;
                        Len := Len + 1;
                     end loop;

                     IPC.Socket.Write
                        (Sock        => File.Inner_Socket,
                         Data        => Data.all,
                         Is_Blocking => File.Is_Blocking,
                         Ret_Count   => Ret_Count,
                         Path        => Addr.Sun_Path,
                         PID         => Unsigned_32 (Convert (Proc)),
                         UID         => UID,
                         GID         => GID,
                         Success     => Success);
                  end;
            end case;
         else
            IPC.Socket.Write
               (File.Inner_Socket, Data.all, File.Is_Blocking, Ret_Count,
                Success);
         end if;
         Translate_Status (Success, Unsigned_64 (Ret_Count), Returned, Errno);

      <<Cleanup>>
         Free (Data);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end SendTo;

   procedure Config_NetInterface
      (InterDev  : Unsigned_64;
       Operation : Unsigned_64;
       Arg_Addr  : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      package Transfer_1 is new Memory.Userland_Transfer (Boolean);
      package Transfer_2 is new Memory.Userland_Transfer (Addr4_NetInterface);
      package Transfer_3 is new Memory.Userland_Transfer (Addr6_NetInterface);

      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Arg_Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      File  : File_Description_Acc;
      Handl : Devices.Device_Handle;
      Stat  : VFS.File_Stat;
      Suc   : Boolean;
      Suc2  : VFS.FS_Status;
      Map   : Page_Table_Acc;
      Blk :            Boolean;
      IP4 : Addr4_NetInterface;
      IP6 : Addr6_NetInterface;
   begin
      Get_File (Proc, InterDev, File);
      if not Get_Capabilities (Proc).Can_Manage_Networking then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("config_netinter", Proc);
         Returned := Unsigned_64'Last;
         return;
      elsif File = null or else File.Description /= Description_Inode then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      end if;

      VFS.Stat (File.Inner_Ino_FS, File.Inner_Ino, Stat, Suc2);
      if Suc2 /= FS_Success or Stat.Type_Of_File /= File_Character_Device then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      end if;
      Handl := Devices.From_Unique_ID (Integer (File.Inner_Ino));

      Get_Common_Map (Proc, Map);
      case Operation is
         when NETINTER_SET_BLOCK =>
            Transfer_1.Take_From_Userland (Map, Blk, SAddr, Suc);
            if not Suc then
               goto Would_Fault_Error;
            end if;
            Networking.Interfaces.Block (Handl, Blk, Suc);
         when NETINTER_SET_STATIC_IP4 =>
            Transfer_2.Take_From_Userland (Map, IP4, SAddr, Suc);
            if not Suc then
               goto Would_Fault_Error;
            end if;
            Networking.Interfaces.Modify_Addresses
               (Handl, IP4.IP, IP4.Sub, Suc);
         when NETINTER_SET_STATIC_IP6 =>
            Transfer_3.Take_From_Userland (Map, IP6, SAddr, Suc);
            if not Suc then
               goto Would_Fault_Error;
            end if;
            Networking.Interfaces.Modify_Addresses
               (Handl, IP6.IP, IP6.Sub, Suc);
         when others =>
            Suc := False;
      end case;

      if Suc then
         Returned := 0;
         Errno    := Error_No_Error;
      else
         Returned := Unsigned_64'Last;
         Errno    := Error_Invalid_Value;
      end if;

      return;

   <<Would_Fault_Error>>
      Returned := Unsigned_64'Last;
      Errno    := Error_Would_Fault;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Config_NetInterface;

   procedure UTimes
      (Dir_FD    : Unsigned_64;
       Path_Addr : Unsigned_64;
       Path_Len  : Unsigned_64;
       Time_Addr : Unsigned_64;
       Flags     : Unsigned_64;
       Returned  : out Unsigned_64;
       Errno     : out Errno_Value)
   is
      Proc        : constant             PID := Arch.Local.Get_Current_Process;
      Path_IAddr  : constant Integer_Address := Integer_Address (Path_Addr);
      Time_IAddr  : constant Integer_Address := Integer_Address (Time_Addr);
      Rel_FS, FS  : VFS.FS_Handle;
      D_Ino, Ino  : VFS.File_Inode_Number;
      Succ        : VFS.FS_Status;
      User        : Unsigned_32;
      File_Desc   : File_Description_Acc;
      File_Perms  : MAC.Permissions;
      Map         : Page_Table_Acc;
      Success     : Boolean;
   begin
      Get_Common_Map (Proc, Map);
      Process.Get_Effective_UID (Proc, User);

      if (Flags and AT_EMPTY_PATH) /= 0 then
         Get_File (Proc, Dir_FD, File_Desc);
         if File_Desc = null or else File_Desc.Description /= Description_Inode
         then
            Errno    := Error_Bad_File;
            Returned := Unsigned_64'Last;
            return;
         end if;
         FS  := File_Desc.Inner_Ino_FS;
         Ino := File_Desc.Inner_Ino;
      else
         if Path_Len > Path_Max_Len then
            Errno := Error_String_Too_Long;
            Returned := Unsigned_64'Last;
            return;
         end if;

         declare
            subtype Path_String is String (1 .. Natural (Path_Len));
            package Trans is new Memory.Userland_Transfer (Path_String);
            Path : Path_String;
         begin
            Trans.Take_From_Userland (Map, Path, To_Address (Path_IAddr),
               Success);
            if not Success then
               Returned := Unsigned_64'Last;
               Errno    := Error_Would_Fault;
               return;
            end if;

            Resolve_AT_Directive (Proc, Dir_FD, Rel_FS, D_Ino);
            if Rel_FS = VFS.Error_Handle then
               Errno    := Error_Bad_File;
               Returned := Unsigned_64'Last;
               return;
            end if;

            VFS.Open
               (Key        => Rel_FS,
                Relative   => D_Ino,
                Path       => Path,
                Final_Key  => FS,
                Ino        => Ino,
                Success    => Succ,
                User       => User,
                Want_Read  => True,
                Want_Write => False,
                Do_Follow  => (Flags and AT_SYMLINK_NOFOLLOW) = 0);
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

      declare
         type Time_Arr is array (1 .. 2) of Time_Spec;
         package Trans is new Memory.Userland_Transfer (Time_Arr);
         Times : Time_Arr;
      begin
         Trans.Take_From_Userland (Map, Times, To_Address (Time_IAddr),
            Success);
         if not Success then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         VFS.Change_Access_Times
            (Key                => FS,
             Ino                => Ino,
             Access_Seconds     => Times (1).Seconds,
             Access_Nanoseconds => Times (1).Nanoseconds,
             Modify_Seconds     => Times (2).Seconds,
             Modify_Nanoseconds => Times (2).Nanoseconds,
             Status             => Succ);
         Translate_Status (Succ, 0, Returned, Errno);
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end UTimes;

   procedure Sched_GetScheduler
      (PID        : Unsigned_64;
       Param_Addr : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Sched_Param);
      Curr    : constant Process.PID := Arch.Local.Get_Current_Process;
      P_IAddr : constant Integer_Address := Integer_Address (Param_Addr);
      Proc    : Process.PID;
      Prio    : Scheduler.Priority;
      Pol     : Scheduler.Policy;
      Param   : Sched_Param;
      Map     : Page_Table_Acc;
      Success : Boolean;
   begin
      if PID = 0 then
         Proc := Arch.Local.Get_Current_Process;
      else
         Proc := Convert (Natural (PID));
         if Proc = Error_PID then
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      if P_IAddr /= 0 then
         Get_Common_Map (Curr, Map);
         Process.Get_Priority (Proc, Prio);
         Param := (Priority => Unsigned_32 (Prio));

         Trans.Paste_Into_Userland (Map, Param, To_Address (P_IAddr), Success);
         if not Success then
            Errno    := Error_Would_Fault;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      Process.Get_Default_Policy (Proc, Pol);
      case Pol is
         when Scheduler.Policy_RR    => Returned := SCHED_RR;
         when Scheduler.Policy_Other => Returned := SCHED_OTHER;
         when Scheduler.Policy_FIFO  => Returned := SCHED_FIFO;
         when Scheduler.Policy_Idle  => Returned := SCHED_IDLE;
      end case;
      Errno := Error_No_Error;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Sched_GetScheduler;

   procedure Sched_SetScheduler
      (PID        : Unsigned_64;
       Policy     : Unsigned_64;
       Param_Addr : Unsigned_64;
       Returned   : out Unsigned_64;
       Errno      : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Sched_Param);
      Curr    : constant Process.PID := Arch.Local.Get_Current_Process;
      P_IAddr : constant Integer_Address := Integer_Address (Param_Addr);
      Proc    : Process.PID;
      Success : Boolean;
      Map     : Page_Table_Acc;
      Pol     : Scheduler.Policy;
      Param   : Sched_Param;
   begin
      if not Get_Capabilities (Curr).Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("sched_setscheduler", Curr);
         Returned := Unsigned_64'Last;
      end if;

      if PID = 0 then
         Proc := Curr;
      else
         Proc := Convert (Natural (PID));
         if Proc = Error_PID then
            goto Invalid_Value_Error;
         end if;
      end if;

      case Policy is
         when SCHED_RR    => Pol := Scheduler.Policy_RR;
         when SCHED_FIFO  => Pol := Scheduler.Policy_FIFO;
         when SCHED_OTHER => Pol := Scheduler.Policy_Other;
         when SCHED_IDLE  => Pol := Scheduler.Policy_Idle;
         when others      => goto Invalid_Value_Error;
      end case;

      Process.Set_Default_Policy (Proc, Pol);

      if P_IAddr /= 0 then
         Get_Common_Map (Curr, Map);
         Trans.Take_From_Userland (Map, Param, To_Address (P_IAddr), Success);
         if not Success then
            Errno    := Error_Would_Fault;
            Returned := Unsigned_64'Last;
            return;
         end if;

         if Natural (Param.Priority) not in Scheduler.Priority'Range then
            goto Invalid_Value_Error;
         end if;
         Process.Set_Priority (Proc, Scheduler.Priority (Param.Priority));
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Invalid_Value_Error>>
      Errno    := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Sched_SetScheduler;

   procedure Sigprocmask
      (How      : Unsigned_64;
       Set_Addr : Unsigned_64;
       Old_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      type Unsigned_30 is mod 2 ** 30;
      function C1 is new Ada.Unchecked_Conversion (Signal_Bitmap, Unsigned_30);
      function C2 is new Ada.Unchecked_Conversion (Unsigned_30, Signal_Bitmap);

      package Trans_1 is new Memory.Userland_Transfer (Signal_Bitmap);
      package Trans_2 is new Memory.Userland_Transfer (Unsigned_30);

      Proc    : constant             PID := Arch.Local.Get_Current_Process;
      S_IAddr : constant Integer_Address := Integer_Address (Set_Addr);
      O_IAddr : constant Integer_Address := Integer_Address (Old_Addr);
      S_SAddr : constant  System.Address := To_Address (S_IAddr);
      O_SAddr : constant  System.Address := To_Address (O_IAddr);
      Old_Set : Signal_Bitmap;
      New_Set : Unsigned_30;
      Success : Boolean;
      Map     : Page_Table_Acc;
   begin
      Get_Common_Map (Proc, Map);
      Get_Masked_Signals (Proc, Old_Set);

      if O_IAddr /= 0 then
         Trans_1.Paste_Into_Userland (Map, Old_Set, O_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
      end if;

      if S_IAddr /= 0 then
         Trans_2.Take_From_Userland (Map, New_Set, S_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;

         case How is
            when SIG_BLOCK =>
               Set_Masked_Signals (Proc, C2 (New_Set and C1 (Old_Set)));
            when SIG_SETMASK =>
               Set_Masked_Signals (Proc, C2 (New_Set and not C1 (Old_Set)));
            when SIG_UNBLOCK =>
               Set_Masked_Signals (Proc, C2 (New_Set));
            when others =>
               Errno    := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
               return;
         end case;
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   end Sigprocmask;

   procedure Sigaction
      (Signal   : Unsigned_64;
       Act_Addr : Unsigned_64;
       Old_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Sigaction_Info);
      Proc    : constant             PID := Arch.Local.Get_Current_Process;
      A_IAddr : constant Integer_Address := Integer_Address (Act_Addr);
      O_IAddr : constant Integer_Address := Integer_Address (Old_Addr);
      A_SAddr : constant  System.Address := To_Address (A_IAddr);
      O_SAddr : constant  System.Address := To_Address (O_IAddr);
      Actual  : Process.Signal;
      Success : Boolean;
      Mask    : Process.Signal_Bitmap;
      Old     : Sigaction_Info;
      Map     : Page_Table_Acc;
      Altstk  : Boolean;
   begin
      Translate_Signal (Signal, Actual, Success);
      if not Success then
         Errno    := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Get_Common_Map (Proc, Map);
      Get_Masked_Signals (Proc, Mask);

      if O_IAddr /= 0 then
         Old.Mask  := 0;
         Get_Signal_Handlers (Proc, Actual, Old.Handler, Old.Restorer, Altstk);
         if Mask (Actual) then
            Old.Handler := To_Address (SIG_IGN);
         elsif Old.Handler = System.Null_Address then
            Old.Handler := To_Address (SIG_DFL);
         end if;
         if Altstk then
            Old.Flags := SA_ONSTACK;
         else
            Old.Flags := 0;
         end if;
         Trans.Paste_Into_Userland (Map, Old, O_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
      end if;

      if A_IAddr /= 0 then
         Trans.Take_From_Userland (Map, Old, A_SAddr, Success);
         if not Success then
            goto Would_Fault_Error;
         end if;

         Altstk := (Old.Flags and SA_ONSTACK) /= 0;
         case To_Integer (Old.Handler) is
            when SIG_DFL =>
               Set_Signal_Handlers (Proc, Actual, System.Null_Address,
                  System.Null_Address, Altstk);
            when SIG_IGN =>
               Mask (Actual) := True;
               Set_Masked_Signals (Proc, Mask);
            when others =>
               Set_Signal_Handlers
                  (Proc, Actual, Old.Handler, Old.Restorer, Altstk);
         end case;
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Sigaction;

   procedure Send_Signal
      (Target   : Unsigned_64;
       Signal   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      type Signed_32 is range -2 ** 31 + 1 .. +2 ** 31 - 1;
      function Conv is new Ada.Unchecked_Conversion (Unsigned_32, Signed_32);

      Proc    : constant PID := Arch.Local.Get_Current_Process;
      Actually_Send   : constant Boolean := Signal /= 0;
      Override_Checks : constant Boolean :=
         Get_Capabilities (Proc).Can_Signal_All;
      Tgt     : PID;
      Success : Boolean;
      Actual  : Process.Signal;
      Signed_Target : Signed_32;
      EUID, Tgt_UID, Tgt_EUID : Unsigned_32;
   begin
      if Actually_Send then
         Translate_Signal (Signal, Actual, Success);
         if not Success then
            Errno    := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      Get_Effective_UID (Proc, EUID);

      --  If < -1: send to the processes in abs number as process group id.
      --  If = -1: send to any process we can signal in the system.
      --  If =  0: signals are sent to processes in the caller's process group.
      --  If >  0: send to the process with number as PID.
      Signed_Target := Conv (Unsigned_32 (Target and 16#FFFFFFFF#));
      if Signed_Target < -1 or Signed_Target = 0 then
         if Actually_Send then
            if Signed_Target = 0 then
               Get_PGID (Proc, Tgt_EUID);
            else
               Tgt_EUID := Unsigned_32 (abs Signed_Target);
            end if;

            Raise_Signal
               (Sig        => Actual,
                Sender_UID => EUID,
                Bypass_UID => Override_Checks,
                Group      => Tgt_EUID);
         end if;
      elsif Signed_Target = -1 then
         if Actually_Send then
            Raise_Signal
               (Process    => Proc,
                Sig        => Actual,
                Sender_UID => EUID,
                Bypass_UID => Override_Checks);
         end if;
      else
         Tgt := Convert (Natural (Signed_Target));
         if Tgt = Error_PID then
            Errno    := Error_Bad_Search;
            Returned := Unsigned_64'Last;
            return;
         end if;

         if not Override_Checks then
            Get_UID (Tgt, Tgt_UID);
            Get_Effective_UID (Tgt, Tgt_EUID);
            if EUID /= Tgt_UID and EUID /= Tgt_EUID then
               Errno    := Error_Bad_Permissions;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end if;

         if Actually_Send then
            Raise_Signal (Tgt, Actual);
         end if;
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Send_Signal;

   procedure Get_Prio
      (Which    : Unsigned_64;
       Who      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : PID := Arch.Local.Get_Current_Process;
      Th    : TID;
      FNice : Niceness;
   begin
      if not Get_Capabilities (Proc).Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("getprio", Proc);
         Returned := Unsigned_64'Last;
      end if;

      case Which is
         when PRIO_PROCESS =>
            if Who /= 0 then
               Proc := Convert (Natural (Who and 16#FFFFFFFF#));
               if Proc = Error_PID then
                  goto Invalid_Value_Return;
               end if;
            end if;
            Get_Niceness (Proc, FNice);
         when PRIO_PGRP | PRIO_USER =>
            Errno    := Error_Not_Supported;
            Returned := Unsigned_64'Last;
            return;
         when PRIO_THREAD =>
            if Who = 0 then
               Th := Arch.Local.Get_Current_Thread;
            else
               Th := Convert (Natural (Who and 16#FFFFFFFF#));
               if Th = Error_TID then
                  goto Invalid_Value_Return;
               end if;
            end if;
            FNice := Get_Niceness (Th);
         when others =>
            goto Invalid_Value_Return;
      end case;

      if FNice >= 0 then
         Returned := Unsigned_64 (FNice);
      else
         Returned := Unsigned_64'Last - Unsigned_64 (abs FNice) + 1;
      end if;

      Errno := Error_No_Error;
      return;

   <<Invalid_Value_Return>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_Prio;

   procedure Set_Prio
      (Which    : Unsigned_64;
       Who      : Unsigned_64;
       Nice     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : PID := Arch.Local.Get_Current_Process;
      Th    : TID;
      FNice : Niceness;
   begin
      if not Get_Capabilities (Proc).Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("setprio", Proc);
         Returned := Unsigned_64'Last;
         return;
      end if;

      if Nice <= Unsigned_64 (Niceness'Last) then
         FNice := Niceness (Nice);
      elsif Nice >=
         Unsigned_64 (Unsigned_32'Last - Unsigned_32 (abs Niceness'First) + 1)
      then
         FNice := -Niceness (Unsigned_64 (Unsigned_32'Last) - Nice + 1);
      else
         goto Invalid_Value_Return;
      end if;

      case Which is
         when PRIO_PROCESS =>
            if Who /= 0 then
               Proc := Convert (Natural (Who and 16#FFFFFFFF#));
               if Proc = Error_PID then
                  goto Invalid_Value_Return;
               end if;
            end if;
            Set_Niceness (Proc, FNice);
         when PRIO_PGRP | PRIO_USER =>
            Errno    := Error_Not_Supported;
            Returned := Unsigned_64'Last;
            return;
         when PRIO_THREAD =>
            if Who = 0 then
               Th := Arch.Local.Get_Current_Thread;
            else
               Th := Convert (Natural (Who and 16#FFFFFFFF#));
               if Th = Error_TID then
                  goto Invalid_Value_Return;
               end if;
            end if;
            Set_Niceness (Th, FNice);
         when others =>
            goto Invalid_Value_Return;
      end case;

      Errno := Error_No_Error;
      Returned := 0;
      return;

   <<Invalid_Value_Return>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Set_Prio;

   procedure Get_GID (Returned : out Unsigned_64; Errno : out Errno_Value) is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Ret  : Unsigned_32;
   begin
      Userland.Process.Get_GID (Proc, Ret);
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Ret);
   end Get_GID;

   procedure Get_EGID (Returned : out Unsigned_64; Errno : out Errno_Value) is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Ret  : Unsigned_32;
   begin
      Userland.Process.Get_Effective_GID (Proc, Ret);
      Errno := Error_No_Error;
      Returned := Unsigned_64 (Ret);
   end Get_EGID;

   procedure Set_GIDs
      (GID      : Unsigned_64;
       EGID     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Curr_GID : Unsigned_32;
   begin
      if Get_Capabilities (Proc).Can_Change_GIDs then
         if EGID <= Unsigned_64 (Unsigned_32'Last) then
            Userland.Process.Set_Effective_GID (Proc, Unsigned_32 (EGID));
         end if;
         if GID <= Unsigned_64 (Unsigned_32'Last) then
            Userland.Process.Set_GID (Proc, Unsigned_32 (GID));
         end if;
      else
         Userland.Process.Get_GID (Proc, Curr_GID);
         if EGID <= Unsigned_64 (Unsigned_32'Last) then
            if EGID = Unsigned_64 (Curr_GID) then
               Userland.Process.Set_Effective_GID
                  (Proc, Unsigned_32 (EGID and 16#FFFFFFFF#));
            else
               Errno := Error_Bad_Permissions;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end if;
      end if;

      Errno := Error_No_Error;
      Returned := 0;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Set_GIDs;

   procedure Get_Groups
      (Count    : Unsigned_64;
       Addr     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      Map   : Page_Table_Acc;
      Succ  : Boolean;
   begin
      if Count > Groups_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Group_Arr is Supplementary_GID_Arr (1 .. Natural (Count));
         package Trans is new Memory.Userland_Transfer (Group_Arr);
         Groups : Group_Arr;
         Length : Natural;
      begin
         Get_Supplementary_Groups (Proc, Groups, Length);
         if Length <= Groups'Length then
            Get_Common_Map (Proc, Map);
            Trans.Paste_Into_Userland (Map, Groups, To_Address (IAddr), Succ);
            if not Succ then
               Returned := Unsigned_64'Last;
               Errno    := Error_Would_Fault;
               return;
            end if;

            Errno := Error_No_Error;
            Returned := Unsigned_64 (Length);
         else
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_Groups;

   procedure Set_Groups
      (Count    : Unsigned_64;
       Addr     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      Succ  : Boolean;
      Map   : Page_Table_Acc;
   begin
      if Count = 0 and Addr = 0 then
         Empty_Supplementary_Groups (Proc);
      else
         if Count > Groups_Max_Len then
            Returned := Unsigned_64'Last;
            Errno    := Error_String_Too_Long;
            return;
         end if;

         declare
            subtype Group_Arr is Supplementary_GID_Arr (1 .. Natural (Count));
            package Trans is new Memory.Userland_Transfer (Group_Arr);
            Groups : Group_Arr;
         begin
            Get_Common_Map (Proc, Map);
            Trans.Take_From_Userland (Map, Groups, To_Address (IAddr), Succ);
            if not Succ then
               Returned := Unsigned_64'Last;
               Errno    := Error_Would_Fault;
               return;
            end if;
            Set_Supplementary_Groups (Proc, Groups, Succ);

            if not Succ then
               Errno := Error_Invalid_Value;
               Returned := Unsigned_64'Last;
               return;
            end if;
         end;
      end if;

      Errno := Error_No_Error;
      Returned := 0;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Set_Groups;

   procedure TTY_Name
      (FD       : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      SAddr : constant  System.Address := To_Address (IAddr);
      File  : File_Description_Acc;
      Data  : IPC.PTY.Inner_Acc;
      Map   : Page_Table_Acc;
      Succ  : Boolean;
   begin
      Get_File (Proc, FD, File);
      if File = null then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif Length > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      case File.Description is
         when Description_Primary_PTY   => Data := File.Inner_Primary_PTY;
         when Description_Secondary_PTY => Data := File.Inner_Secondary_PTY;
         when others => goto Invalid_Error;
      end case;

      declare
         subtype Name_String is String (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Name_String);
         Name : Name_String;
      begin
         IPC.PTY.Get_Name (Data, Name, Natural (Returned));
         if Returned >= Length then
            goto Invalid_Error;
         end if;
         Name (Natural (Returned) + 1) := Ada.Characters.Latin_1.NUL;

         Get_Common_Map (Proc, Map);
         Trans.Paste_Into_Userland (Map, Name, SAddr, Succ);
         if not Succ then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Errno := Error_No_Error;
         Returned := 0;
         return;
      end;

   <<Invalid_Error>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end TTY_Name;

   procedure FAdvise
      (FD       : Unsigned_64;
       Offset   : Unsigned_64;
       Length   : Unsigned_64;
       Advice   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (Offset);
      pragma Unreferenced (Length);
      Proc  : constant PID := Arch.Local.Get_Current_Process;
      File  : File_Description_Acc;
      Tmp   : FS_Status;
   begin
      Get_File (Proc, FD, File);
      if File = null then
         Errno := Error_Bad_File;
         goto Error_Return;
      elsif File.Description /= Description_Inode then
         Errno := Error_Invalid_Seek;
         goto Error_Return;
      end if;

      case Advice is
         when POSIX_FADV_NORMAL | POSIX_FADV_SEQUENTIAL | POSIX_FADV_NOREUSE |
              POSIX_FADV_WILLNEED | POSIX_FADV_RANDOM =>
            Errno := Error_No_Error;
            Returned := 0;
         when POSIX_FADV_DONTNEED =>
            Tmp := VFS.Synchronize (File.Inner_Ino_FS, File.Inner_Ino, False);
            Translate_Status (Tmp, 0, Returned, Errno);
         when others =>
            Errno := Error_Invalid_Value;
            goto Error_Return;
      end case;

      return;

   <<Error_Return>>
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end FAdvise;

   procedure SHMAt
      (ID       : Unsigned_64;
       Addr     : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Align is new Alignment (Unsigned_64);
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Perms : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accessible => True,
          Can_Read          => True,
          Can_Write         => (Flags and SHM_RDONLY) = 0,
          others            => False);
      Ret_Addr, Ret_Size, VAddr : Unsigned_64;
      Truncated, EUID, EGID : Unsigned_32;
      Map : Page_Table_Acc;
      Success : Boolean;
   begin
      Truncated := Unsigned_32 (ID and 16#FFFFFFFF#);

      if not Get_Capabilities (Proc).Can_Bypass_IPC_Checks then
         Get_Effective_UID (Proc, EUID);
         Get_Effective_GID (Proc, EGID);
         IPC.SHM.Check_Permissions (Truncated, EUID, EGID, Success);
         if not Success then
            Errno := Error_Bad_Access;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      IPC.SHM.Get_Address (Truncated, Ret_Addr, Ret_Size);

      if Addr /= 0 then
         if (Flags and SHM_RND) /= 0 then
            VAddr := Align.Align_Down (Addr, Memory.MMU.Page_Size);
         elsif (Addr mod Memory.MMU.Page_Size) /= 0 then
            goto Invalid_Error;
         else
            VAddr := Addr;
         end if;
      else
         Bump_Alloc_Base (Proc, Ret_Size, VAddr);
      end if;

      if Ret_Size /= 0 then
         Userland.Process.Get_Common_Map (Proc, Map);
         Memory.MMU.Map_Range
            (Map            => Map,
             Virtual_Start  => To_Address (Integer_Address (VAddr)),
             Physical_Start => To_Address (Integer_Address (Ret_Addr)),
             Length         => Storage_Count (Ret_Size),
             Permissions    => Perms,
             Success        => Success);
         if Success then
            IPC.SHM.Modify_Attachment (Truncated, True);
            Errno := Error_No_Error;
            Returned := VAddr;
         else
            Errno := Error_No_Memory;
            Returned := Unsigned_64'Last;
         end if;
         return;
      end if;

   <<Invalid_Error>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end SHMAt;

   procedure SHMCtl
      (ID       : Unsigned_64;
       CMD      : Unsigned_64;
       Buffer   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (SHMID_DS);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Buffer);
      Info  : IPC.SHM.Segment_Information;
      Found : Boolean;
      Trunc_ID, EUID, EGID : Unsigned_32;
      Map : Page_Table_Acc;
      IDs : SHMID_DS;
   begin
      Trunc_ID := Unsigned_32 (ID and 16#FFFFFFFF#);

      if not Get_Capabilities (Proc).Can_Bypass_IPC_Checks then
         Get_Effective_UID (Proc, EUID);
         Get_Effective_GID (Proc, EGID);
         IPC.SHM.Check_Permissions (Trunc_ID, EUID, EGID, Found);
         if not Found then
            Errno := Error_Bad_Access;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      case CMD is
         when IPC_RMID =>
            IPC.SHM.Mark_Refcounted (Trunc_ID);
         when IPC_SET | IPC_STAT =>
            Userland.Process.Get_Common_Map (Proc, Map);
            Trans.Take_From_Userland (Map, IDs, To_Address (IAddr), Found);
            if not Found then
               goto Would_Fault_Error;
            end if;

            if CMD = IPC_SET then
               IPC.SHM.Modify_Permissions
                  (ID   => Trunc_ID,
                   UID  => IDs.SHM_Perm.UID,
                   GID  => IDs.SHM_Perm.GID,
                   Mode => Unsigned_64 (IDs.SHM_Perm.Mode));
            else
               IPC.SHM.Fetch_Information (Trunc_ID, Info, Found);
               if not Found then
                  goto Invalid_Error;
               end if;
               IDs.SHM_Perm.IPC_Perm_Key := Info.Key;
               IDs.SHM_Perm.UID := Info.Owner_UID;
               IDs.SHM_Perm.GID := Info.Owner_GID;
               IDs.SHM_Perm.CUID := Info.Creator_UID;
               IDs.SHM_Perm.CGID := Info.Creator_GID;
               IDs.SHM_Perm.Mode := Unsigned_32 (Info.Mode);
               IDs.SHM_SegSz := Info.Size;
               IDs.SHM_Nattch := Unsigned_64 (Info.Refcount);

               Trans.Paste_Into_Userland (Map, IDs, To_Address (IAddr), Found);
               if not Found then
                  goto Would_Fault_Error;
               end if;
            end if;
         when others =>
            goto Invalid_Error;
      end case;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Invalid_Error>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
      return;

   <<Would_Fault_Error>>
      Errno := Error_Would_Fault;
      Returned := Unsigned_64'Last;
      return;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end SHMCtl;

   procedure SHMDt
      (Address  : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Map : Page_Table_Acc;
      Phys : System.Address;
      Is_Mapped, Is_Readable, Is_Writeable, Is_Executable : Boolean;
      Is_User_Accessible : Boolean;
      ID : IPC.SHM.Segment_ID;
      Size : Unsigned_64;
   begin
      Get_Common_Map (Proc, Map);
      Memory.MMU.Translate_Address
         (Map                => Map,
          Virtual            => To_Address (Integer_Address (Address)),
          Length             => Memory.MMU.Page_Size,
          Physical           => Phys,
          Is_Mapped          => Is_Mapped,
          Is_User_Accessible => Is_User_Accessible,
          Is_Readable        => Is_Readable,
          Is_Writeable       => Is_Writeable,
          Is_Executable      => Is_Executable);
      IPC.SHM.Get_Segment_And_Size (Unsigned_64 (To_Integer (Phys)), Size, ID);

      if Is_Mapped and ID /= IPC.SHM.Error_ID then
         Memory.MMU.Unmap_Range
            (Map           => Map,
             Virtual_Start => To_Address (Integer_Address (Address)),
             Length        => Storage_Count (Size),
             Success       => Is_Mapped);
         if Is_Mapped then
            IPC.SHM.Modify_Attachment (ID, False);
            Errno := Error_No_Error;
            Returned := 0;
         else
            Errno := Error_No_Memory;
            Returned := Unsigned_64'Last;
         end if;
      else
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end SHMDt;

   procedure SHMGet
      (Key      : Unsigned_64;
       Size     : Unsigned_64;
       Flags    : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Align is new Alignment (Unsigned_64);
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Mode : constant Unsigned_64 := Flags and Unsigned_64 (File_Mode'Last);
      AlSz : constant Unsigned_64 :=
         Align.Align_Up (Size, Memory.MMU.Page_Size);
      Created_Key : IPC.SHM.Segment_ID;
      EUID, EGID, Truncated : Unsigned_32;
   begin
      Get_Effective_UID (Proc, EUID);
      Get_Effective_GID (Proc, EGID);

      Truncated := Unsigned_32 (Key and 16#FFFFFFFF#);

      if Key = IPC_PRIVATE then
         IPC.SHM.Create_Unkeyed_Segment (AlSz, EUID, EGID, Mode, Created_Key);
      elsif (Flags and IPC_CREAT) /= 0 then
         IPC.SHM.Create_Segment (Truncated, AlSz, EUID, EGID, Mode,
            Created_Key);
      else
         IPC.SHM.Get_Segment (Truncated, Created_Key);
      end if;

      if Created_Key /= IPC.SHM.Error_ID then
         Errno := Error_No_Error;
         Returned := Unsigned_64 (Created_Key);
      else
         Errno := Error_Invalid_Value;
         Returned := Unsigned_64'Last;
      end if;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end SHMGet;

   procedure GetSockOpt
      (Sock     : Unsigned_64;
       Level    : Unsigned_64;
       Opt      : Unsigned_64;
       Addr     : Unsigned_64;
       Len      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (Len);

      package Trans1 is new Memory.Userland_Transfer (Unsigned_32);
      package Trans2 is new Memory.Userland_Transfer (UCred);

      Proc      : constant             PID := Arch.Local.Get_Current_Process;
      IAddr     : constant Integer_Address := Integer_Address (Addr);
      File      : File_Description_Acc;
      Map       : Page_Table_Acc;
      Is_Listen : Boolean;
      Val       : Unsigned_32;
      UCred_Val : UCred;
      Success   : Boolean;
      Success2  : IPC.Socket.Socket_Status;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, Sock, File);
      if File = null or else File.Description /= Description_Socket then
         Errno := Error_Bad_File;
         Returned := Unsigned_64'Last;
         return;
      elsif Level /= SOL_SOCKET then
         goto Invalid_Value_Error;
      end if;

      case Opt is
         when SO_ACCEPTCONN =>
            Is_Listening (File.Inner_Socket, Is_Listen);
            Val := (if Is_Listen then 1 else 0);
            Trans1.Paste_Into_Userland (Map, Val, To_Address (IAddr), Success);
            if not Success then
               goto Would_Fault_Error;
            end if;
         when SO_ERROR =>
            Val := 0;
            Trans1.Paste_Into_Userland (Map, Val, To_Address (IAddr), Success);
            if not Success then
               goto Would_Fault_Error;
            end if;
         when SO_SNDBUF =>
            Val := Unsigned_32 (IPC.Socket.Default_Socket_Size);
            Trans1.Paste_Into_Userland (Map, Val, To_Address (IAddr), Success);
            if not Success then
               goto Would_Fault_Error;
            end if;
         when SO_TYPE =>
            case Get_Type (File.Inner_Socket) is
               when Stream =>
                  Val := SOCK_STREAM;
               when Datagram =>
                  Val := SOCK_DGRAM;
               when Raw =>
                  Val := SOCK_RAW;
            end case;
            Trans1.Paste_Into_Userland (Map, Val, To_Address (IAddr), Success);
            if not Success then
               goto Would_Fault_Error;
            end if;
         when SO_PEERCRED =>
            if Get_Domain (File.Inner_Socket) /= IPC.Socket.UNIX then
               goto Invalid_Value_Error;
            end if;

            Get_Peer_Credentials
               (Sock    => File.Inner_Socket,
                PID     => UCred_Val.PID,
                UID     => UCred_Val.UID,
                GID     => UCred_Val.GID,
                Success => Success2);
            if Success2 /= IPC.Socket.Plain_Success then
               goto Invalid_Value_Error;
            end if;

            Trans2.Paste_Into_Userland
               (Map, UCred_Val, To_Address (IAddr), Success);
            if not Success then
               goto Would_Fault_Error;
            end if;
         when others =>
            goto Invalid_Value_Error;
      end case;

      Errno := Error_No_Error;
      Returned := 0;
      return;

   <<Invalid_Value_Error>>
      Errno := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
      return;

   <<Would_Fault_Error>>
      Errno := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end GetSockOpt;

   procedure SetSockOpt
      (Sock     : Unsigned_64;
       Level    : Unsigned_64;
       Opt      : Unsigned_64;
       Addr     : Unsigned_64;
       Len      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (Addr, Len);
      Proc : constant PID := Arch.Local.Get_Current_Process;
      File : File_Description_Acc;
   begin
      Get_File (Proc, Sock, File);
      if File = null or else File.Description /= Description_Socket then
         Errno := Error_Bad_File;
         goto Generic_Error;
      elsif Level /= SOL_SOCKET then
         goto Invalid_Value_Error;
      end if;

      case Opt is
         when SO_SNDBUF =>
            null;
         when others =>
            goto Invalid_Value_Error;
      end case;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Invalid_Value_Error>>
      Errno := Error_Invalid_Value;
   <<Generic_Error>>
      Returned := Unsigned_64'Last;
   end SetSockOpt;

   procedure Get_Thread_Name
      (TID      : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      Th    : Scheduler.TID;
      Map   : Page_Table_Acc;
      Succ  : Boolean;
   begin
      if Length > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      Th := Scheduler.Convert (Natural (TID));

      declare
         subtype Name_String is String (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Name_String);
         Str : Name_String;
         Ret : Natural;
      begin
         Scheduler.Get_Name (Th, Str, Ret);
         if Ret /= 0 and Ret < Str'Length then
            Str (Ret + 1) := Ada.Characters.Latin_1.NUL;

            Get_Common_Map (Proc, Map);
            Trans.Paste_Into_Userland (Map, Str, To_Address (IAddr), Succ);
            if not Succ then
               Returned := Unsigned_64'Last;
               Errno    := Error_Would_Fault;
               return;
            end if;

            Returned      := 0;
            Errno         := Error_No_Error;
         else
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Get_Thread_Name;

   procedure Set_Thread_Name
      (TID      : Unsigned_64;
       Addr     : Unsigned_64;
       Length   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      Th    : Scheduler.TID;
      Map   : Page_Table_Acc;
      Succ  : Boolean;
   begin
      if Length > Path_Max_Len then
         Returned := Unsigned_64'Last;
         Errno    := Error_String_Too_Long;
         return;
      end if;

      declare
         subtype Name_String is String (1 .. Natural (Length));
         package Trans is new Memory.Userland_Transfer (Name_String);
         Str : Name_String;
      begin
         Get_Common_Map (Proc, Map);
         Trans.Take_From_Userland (Map, Str, To_Address (IAddr), Succ);
         if not Succ then
            Returned := Unsigned_64'Last;
            Errno    := Error_Would_Fault;
            return;
         end if;

         Th := Scheduler.Convert (Natural (TID));
         Scheduler.Set_Name (Th, Str, Succ);
         if Succ then
            Returned := 0;
            Errno    := Error_No_Error;
         else
            Errno := Error_Invalid_Value;
            Returned := Unsigned_64'Last;
         end if;
      end;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Set_Thread_Name;

   procedure Failure_Policy
      (Old_Addr : Unsigned_64;
       New_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (Old_Addr, New_Addr);
   begin
      --  TODO: These dont do anything and are placeholders, make these work.
      Errno    := Error_No_Error;
      Returned := 0;
   end Failure_Policy;

   procedure Create_Thread
      (Callback : Unsigned_64;
       Call_Arg : Unsigned_64;
       Stack    : Unsigned_64;
       TLS_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc    : constant PID := Arch.Local.Get_Current_Process;
      New_TID : Scheduler.TID;
      Pol     : Scheduler.Policy;
      Success : Boolean;
      Map     : Page_Table_Acc;
   begin
      if not Get_Capabilities (Proc).Can_Spawn_Others then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("create_thread", Proc);
         Returned := Unsigned_64'Last;
      end if;

      Get_Common_Map (Proc, Map);
      Process.Get_Default_Policy (Proc, Pol);
      Create_User_Thread
         (Address    => Integer_Address (Callback),
          Map        => Map,
          Stack_Addr => Stack,
          TLS_Addr   => TLS_Addr,
          Pol        => Pol,
          Argument   => Call_Arg,
          PID        => Convert (Proc),
          New_TID    => New_TID);

      if New_TID = Error_TID then
         goto Block_Error;
      end if;

      Add_Thread (Proc, New_TID, Success);
      if not Success then
         goto Block_Error;
      end if;

      Errno    := Error_No_Error;
      Returned := Unsigned_64 (Scheduler.Convert (New_TID));
      return;

   <<Block_Error>>
      Errno    := Error_Would_Block;
      Returned := Unsigned_64'Last;
   end Create_Thread;

   procedure Signal_Return
      (Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
   begin
      Scheduler.Exit_Signal_And_Reschedule;
      Errno    := Error_Would_Block;
      Returned := Unsigned_64'Last;
   end Signal_Return;

   procedure Sigaltstack
      (New_Addr : Unsigned_64;
       Old_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (Stack);
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      Th        : constant TID := Arch.Local.Get_Current_Thread;
      New_IAddr : constant Integer_Address := Integer_Address (New_Addr);
      Old_IAddr : constant Integer_Address := Integer_Address (Old_Addr);
      Stk       : Stack;
      Map       : Page_Table_Acc;
      Is_Disb   : Boolean;
      Is_Used   : Boolean;
      Success   : Boolean;
   begin
      Get_Common_Map (Proc, Map);

      if Old_IAddr /= 0 then
         Scheduler.Get_Signal_Stack (Th, Stk.Addr, Stk.Size, Is_Disb, Is_Used);
         Stk.Flags := 0;
         if Is_Disb then Stk.Flags := Stk.Flags or SS_DISABLE; end if;
         if Is_Used then Stk.Flags := Stk.Flags or SS_ONSTACK; end if;

         Trans.Paste_Into_Userland (Map, Stk, To_Address (Old_IAddr), Success);
         if not Success then
            goto Would_Fault_Error;
         end if;
      end if;

      if New_IAddr /= 0 then
         Trans.Take_From_Userland (Map, Stk, To_Address (New_IAddr), Success);
         if not Success then
            goto Would_Fault_Error;
         end if;

         Scheduler.Set_Signal_Stack
            (Th, Stk.Addr, Stk.Size, (Stk.Flags and SS_DISABLE) /= 0, Success);
         if not Success then
            Errno    := Error_Bad_Permissions;
            Returned := Unsigned_64'Last;
            return;
         end if;
      end if;

      Errno    := Error_No_Error;
      Returned := 0;
      return;

   <<Would_Fault_Error>>
      Errno    := Error_Would_Fault;
      Returned := Unsigned_64'Last;
   end Sigaltstack;

   procedure Get_CPU_Info
      (Addr     : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (CPU_Info);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      IAddr : constant Integer_Address := Integer_Address (Addr);
      Succ  : Boolean;
      Info  : CPU_Info;
      Map   : Page_Table_Acc;
   begin
      Info :=
         (Config_Cores => Unsigned_64 (Arch.Hooks.Get_Configured_Cores),
          Online_Cores => Unsigned_64 (Arch.Hooks.Get_Active_Core_Count),
          Vendor_Name  => [others => Ada.Characters.Latin_1.NUL],
          Model_Name   => [others => Ada.Characters.Latin_1.NUL],
          Base_MHz     => 0,
          Max_MHz      => 0,
          Ref_MHz      => 0);
      Arch.Hooks.Get_CPU_Frequency
         (Info.Base_MHz, Info.Max_MHz, Info.Ref_MHz);
      Arch.Hooks.Get_CPU_Vendor (Info.Vendor_Name);
      Arch.Hooks.Get_CPU_Model (Info.Model_Name);

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Info, To_Address (IAddr), Succ);
      if Succ then
         Errno    := Error_No_Error;
         Returned := 0;
      else
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;
   end Get_CPU_Info;

   procedure Socket_Pair
      (Domain   : Unsigned_64;
       DataType : Unsigned_64;
       FDs      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      type Result_Arr is array (1 .. 2) of Integer;
      package Trans is new Memory.Userland_Transfer (Result_Arr);
      A     : constant Integer_Address := Integer_Address (FDs);
      Proc  : constant             PID := Arch.Local.Get_Current_Process;
      Block : constant         Boolean := (DataType and SOCK_NONBLOCK) = 0;
      Res   : Result_Arr;
      New_Sock1, New_Sock2 : IPC.Socket.Socket_Acc;
      Dom                  : IPC.Socket.Domain;
      Data                 : IPC.Socket.DataType;
      Succ1, Succ2         : Boolean;
      New_Desc1, New_Desc2 : File_Description_Acc;
      Map                  : Page_Table_Acc;
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

      New_Sock1 := Create (Dom, Data);
      if New_Sock1 = null then
         goto Invalid_Value_Return;
      end if;
      Pipe_Socket (New_Sock1, New_Sock2);
      if New_Sock2 = null then
         goto Invalid_Value_Return;
      end if;

      New_Desc1 := new File_Description'
         (Children_Count => 0,
          Is_Blocking    => Block,
          Description    => Description_Socket,
          Inner_Socket   => New_Sock1);
      New_Desc2 := new File_Description'
         (Children_Count => 0,
          Is_Blocking    => Block,
          Description    => Description_Socket,
          Inner_Socket   => New_Sock2);
      Add_File (Proc, New_Desc1, Res (1), Succ1);
      Add_File (Proc, New_Desc2, Res (2), Succ2);
      if not Succ1 or not Succ2 then
         Close (New_Desc1);
         Close (New_Desc2);
         Errno    := Error_Too_Many_Files;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Res, To_Address (A), Succ1);
      if Succ1 then
         Errno    := Error_No_Error;
         Returned := 0;
      else
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;

      return;

   <<Invalid_Value_Return>>
      Errno    := Error_Invalid_Value;
      Returned := Unsigned_64'Last;
   end Socket_Pair;

   procedure MAdvise
      (Addr     : Unsigned_64;
       Size     : Unsigned_64;
       Advice   : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (Addr, Size, Advice);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end MAdvise;

   procedure NVMM_Capability
      (Cap_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      package Trans is new Memory.Userland_Transfer (NVMM_Caps);
      Proc : constant             PID := Arch.Local.Get_Current_Process;
      A    : constant Integer_Address := Integer_Address (Cap_Addr);
      Caps : NVMM_Caps;
      Map  : Page_Table_Acc;
      Succ : Boolean;
   begin
      if not Virtualization.Is_Supported then
         Errno    := Error_Not_Supported;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Caps :=
         (Version      => Virtualization.NVMM_Version,
          State_Size   => Virtualization.State_Size,
          Max_Machines => Virtualization.Max_Virtual_Machines,
          Max_VCPUs    => Virtualization.Max_CPUs_Per_VM,
          Max_RAM      => Virtualization.Max_RAM_Per_VM);

      Get_Common_Map (Proc, Map);
      Trans.Paste_Into_Userland (Map, Caps, To_Address (A), Succ);
      if Succ then
         Errno    := Error_No_Error;
         Returned := 0;
      else
         Errno    := Error_Would_Fault;
         Returned := Unsigned_64'Last;
      end if;
   end NVMM_Capability;

   procedure NVMM_Machine_Create
      (Machine_Addr : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_Machine_Create;

   procedure NVMM_Machine_Destroy
      (Machine_Addr : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_Machine_Destroy;

   procedure NVMM_Machine_Configure
      (Machine_Addr  : Unsigned_64;
       Operation     : Unsigned_64;
       Argument_Addr : Unsigned_64;
       Returned      : out Unsigned_64;
       Errno         : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, Operation, Argument_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_Machine_Configure;

   procedure NVMM_VCPU_Create
      (Machine_Addr : Unsigned_64;
       CPUID        : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPUID, CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_Create;

   procedure NVMM_VCPU_Destroy
      (Machine_Addr : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_Destroy;

   procedure NVMM_VCPU_Configure
      (Machine_Addr  : Unsigned_64;
       CPU_Addr      : Unsigned_64;
       Operation     : Unsigned_64;
       Argument_Addr : Unsigned_64;
       Returned      : out Unsigned_64;
       Errno         : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr, Operation, Argument_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_Configure;

   procedure NVMM_VCPU_SetState
      (Machine_Addr  : Unsigned_64;
       CPU_Addr      : Unsigned_64;
       Operation     : Unsigned_64;
       Returned      : out Unsigned_64;
       Errno         : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr, Operation);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_SetState;

   procedure NVMM_VCPU_GetState
      (Machine_Addr  : Unsigned_64;
       CPU_Addr      : Unsigned_64;
       Operation     : Unsigned_64;
       Returned      : out Unsigned_64;
       Errno         : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr, Operation);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_GetState;

   procedure NVMM_VCPU_Inject
      (Machine_Addr : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_Inject;

   procedure NVMM_VCPU_Run
      (Machine_Addr : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_Run;

   procedure NVMM_GPA_Map
      (Machine_Addr : Unsigned_64;
       HVA          : Unsigned_64;
       GPA          : Unsigned_64;
       Size         : Unsigned_64;
       Prot         : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, HVA, GPA, Size, Prot);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_GPA_Map;

   procedure NVMM_GPA_Unmap
      (Machine_Addr : Unsigned_64;
       HVA          : Unsigned_64;
       GPA          : Unsigned_64;
       Size         : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, HVA, GPA, Size);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_GPA_Unmap;

   procedure NVMM_HVA_Map
      (Machine_Addr : Unsigned_64;
       HVA          : Unsigned_64;
       Size         : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, HVA, Size);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_HVA_Map;

   procedure NVMM_HVA_Unmap
      (Machine_Addr : Unsigned_64;
       HVA          : Unsigned_64;
       Size         : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, HVA, Size);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_HVA_Unmap;

   procedure NVMM_GVA_2_GPA
      (Machine_Addr : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       GVA          : Unsigned_64;
       GPA_Addr     : Unsigned_64;
       Prot_Addr    : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr, GVA, GPA_Addr, Prot_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_GVA_2_GPA;

   procedure NVMM_GPA_2_HVA
      (Machine_Addr : Unsigned_64;
       GPA          : Unsigned_64;
       HVA_Addr     : Unsigned_64;
       Prot_Addr    : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, GPA, HVA_Addr, Prot_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_GPA_2_HVA;

   procedure NVMM_Assist_IO
      (Machine_Addr : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_Assist_IO;

   procedure NVMM_Assist_Mem
      (Machine_Addr : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_Assist_Mem;

   procedure NVMM_VCPU_Dump
      (Machine_Addr : Unsigned_64;
       CPU_Addr     : Unsigned_64;
       Returned     : out Unsigned_64;
       Errno        : out Errno_Value)
   is
      pragma Unreferenced (Machine_Addr, CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_Dump;

   procedure NVMM_VCPU_Stop
      (CPU_Addr : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (CPU_Addr);
   begin
      Errno    := Error_No_Error;
      Returned := 0;
   end NVMM_VCPU_Stop;

   procedure Set_SID
      (Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Success : Boolean;
   begin
      Userland.Process.Create_Session (Proc, Success);
      if Success then
         Returned := 0;
         Errno    := Error_No_Error;
      else
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_Permissions;
      end if;
   end Set_SID;

   procedure Recv_Sock_Ctr
      (FD       : Unsigned_64;
       Addr     : Unsigned_64;
       Len      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (Len);
      package Trans is new Memory.Userland_Transfer (Credentials_Control_Hdr);
      Proc      : constant PID := Arch.Local.Get_Current_Process;
      Hdr_IAddr : constant Integer_Address := Integer_Address (Addr);
      File      : File_Description_Acc;
      Hdr       : Credentials_Control_Hdr;
      Map       : Page_Table_Acc;
      Success   : Boolean;
      Stat      : Socket_Status;
   begin
      Get_Common_Map (Proc, Map);
      Get_File (Proc, FD, File);
      if File = null or else File.Description /= Description_Socket then
         Returned := Unsigned_64'Last;
         Errno    := Error_Bad_File;
         return;
      end if;

      Hdr :=
         (Len => Credentials_Control_Hdr'Size / 8,
          Level => SOL_SOCKET,
          Message_Type => SCM_CREDENTIALS,
          Pad => 0,
          Creds => (0, 0, 0));

      Get_Peer_Credentials
         (Sock => File.Inner_Socket,
          PID => Hdr.Creds.PID,
          UID => Hdr.Creds.UID,
          GID => Hdr.Creds.GID,
          Success => Stat);

      Trans.Paste_Into_Userland (Map, Hdr, To_Address (Hdr_IAddr), Success);
      if not Success then
         Errno := Error_Would_Fault;
         Returned := Unsigned_64'Last;
         return;
      end if;

      Errno := Error_No_Error;
      Returned := 0;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Recv_Sock_Ctr;

   procedure Send_Sock_Ctr
      (FD       : Unsigned_64;
       Addr     : Unsigned_64;
       Len      : Unsigned_64;
       Returned : out Unsigned_64;
       Errno    : out Errno_Value)
   is
      pragma Unreferenced (FD, Addr, Len);
   begin
      Errno := Error_No_Error;
      Returned := 0;
   end Send_Sock_Ctr;
   ----------------------------------------------------------------------------
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

      Proc          : constant PID := Arch.Local.Get_Current_Process;
      File          : File_Description_Acc;
      Success       : IPC.FIFO.Pipe_Status;
      Ignore_Signal : Boolean;
      Ret_Count     : Natural;
      Tracer_FD     : Natural;
      Is_Traced     : Boolean;
      TInfo         : Trace_Info;
   begin
      --  Solve signals first.
      Clear_Process_Signals (Proc, Is_Traced);

      --  Take care of syscall tracing.
      Userland.Process.Get_Traced_Info (Proc, Is_Traced, Tracer_FD);
      if Is_Traced then
         Get_File (Proc, Unsigned_64 (Tracer_FD), File);
         if File /= null and then File.Description = Description_Writer_FIFO
         then
            while not Is_Empty (File.Inner_Writer_FIFO) loop
               Scheduler.Yield_If_Able;
            end loop;
            declare
               TInfo_Data : Devices.Operation_Data (1 .. TInfo'Size / 8)
                  with Import, Address => TInfo'Address;
            begin
               TInfo := (Unsigned_16 (Convert (Thread)), State);
               Write (File.Inner_Writer_FIFO, TInfo_Data, File.Is_Blocking,
                      Ret_Count, Success);
            end;
         end if;
      end if;
   exception
      when Constraint_Error =>
         null;
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
         when VFS.FS_Is_Directory  => Errno := Error_Is_Directory;
         when VFS.FS_Not_Directory => Errno := Error_Not_Directory;
         when VFS.FS_Invalid_Value => Errno := Error_Invalid_Value;
         when VFS.FS_Not_Supported => Errno := Error_Not_Implemented;
         when VFS.FS_RO_Failure    => Errno := Error_Read_Only_FS;
         when VFS.FS_IO_Failure    => Errno := Error_IO;
         when VFS.FS_Not_Allowed   => Errno := Error_Bad_Permissions;
         when VFS.FS_Loop          => Errno := Error_File_Loop;
         when VFS.FS_Exists        => Errno := Error_Exists;
         when VFS.FS_Full          => Errno := Error_No_Space;
         when VFS.FS_Not_Empty     => Errno := Error_Not_Empty;
         when VFS.FS_Not_Found     => Errno := Error_No_Entity;
      end case;
      Returned := Unsigned_64'Last;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
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
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Translate_Status;

   procedure Translate_Status
      (Status         : IPC.PTY.Status;
       Success_Return : Unsigned_64;
       Returned       : out Unsigned_64;
       Errno          : out Errno_Value)
   is
   begin
      case Status is
         when IPC.PTY.PTY_Success =>
            Errno    := Error_No_Error;
            Returned := Success_Return;
         when IPC.PTY.PTY_Would_Block =>
            Errno    := Error_Would_Block;
            Returned := Unsigned_64'Last;
      end case;
   exception
      when Constraint_Error =>
         Errno    := Error_Would_Block;
         Returned := Unsigned_64'Last;
   end Translate_Status;

   function To_String (Addr : System.Address) return String_Acc is
   begin
      --  This degenerate localized SMAP disabling is required because of
      --  the fact that we have no previous specified length.
      Arch.Snippets.Enable_Userland_Memory_Access;
      declare
         Arg_Length : constant Natural := Interfaces.C.Strings.Strlen (Addr);
         Arg_String : String (1 .. Arg_Length) with Import, Address => Addr;
         Arg_Result : String_Acc;
      begin
         Arg_Result := new String'(Arg_String);
         Arch.Snippets.Disable_Userland_Memory_Access;
         return Arg_Result;
      end;
   end To_String;

   function Get_Mmap_Prot (P : Unsigned_64) return Arch.MMU.Page_Permissions
   is
   begin
      if P = PROT_NONE then
         return (Is_User_Accessible => True, others => False);
      else
         return
            (Is_User_Accessible => True,
             Can_Read          => (P and PROT_READ)  /= 0,
             Can_Write         => (P and PROT_WRITE) /= 0,
             Can_Execute       => (P and PROT_EXEC)  /= 0,
             Is_Global         => False);
      end if;
   end Get_Mmap_Prot;

   procedure Execute_MAC_Failure (Name : String; Curr_Proc : PID) is
      PID : constant Natural := Convert (Curr_Proc);
   begin
      case Get_Enforcement (Curr_Proc) is
         when MAC.Deny =>
            null;
         when MAC.Deny_And_Scream =>
            Messages.Put_Line (PID'Image & " MAC failure " & Name);
         when MAC.Kill =>
            Messages.Put_Line (PID'Image & " MAC killing " & Name);
            Exit_Process (Curr_Proc, 42);
      end case;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("MAC recovery killing, no fancy printing");
         Exit_Process (Curr_Proc, 42);
   end Execute_MAC_Failure;

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
           Caps.Can_Use_Clocks        and ((Bits and MAC_CAP_CLOCK)   /= 0),
           Caps.Can_Signal_All        and ((Bits and MAC_CAP_SIGNALALL) /= 0),
           Caps.Can_Change_GIDs       and ((Bits and MAC_CAP_SETGID)   /= 0),
           Caps.Can_Bypass_IPC_Checks and ((Bits and MAC_CAP_IPC) /= 0),
           Caps.Can_Check_System_Logs and ((Bits and MAC_CAP_SYS_LOG) /= 0)));
   end Set_MAC_Capabilities;

   procedure Check_Userland_Mappability
      (Map        : Memory.MMU.Page_Table_Acc;
       Addr       : Memory.Virtual_Address;
       Byte_Count : Unsigned_64;
       Can_Map    : out Boolean)
   is
      pragma Unreferenced (Map);
   begin
      Can_Map := Addr + Virtual_Address (Byte_Count) < Memory_Offset;
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
         Get_File (Proc, Dir_FD, Descr);
         if Descr = null or else Descr.Description /= Description_Inode then
            FS  := VFS.Error_Handle;
            Ino := 0;
         else
            FS  := Descr.Inner_Ino_FS;
            Ino := Descr.Inner_Ino;
         end if;
      end if;
   exception
      when Constraint_Error =>
         FS  := VFS.Error_Handle;
         Ino := 0;
   end Resolve_AT_Directive;

   procedure Translate_Signal
      (Val     : Unsigned_64;
       Sig     : out Signal;
       Success : out Boolean)
   is
   begin
      Success :=
         Val >= Process.Signal'Enum_Rep (Process.Signal'First) and
         Val <= Process.Signal'Enum_Rep (Process.Signal'Last);
      if Success then
         Sig := Process.Signal'Enum_Val (Val);
      else
         Sig := Process.Signal_Kill;
      end if;
   exception
      when Constraint_Error =>
         Sig     := Signal_Abort;
         Success := False;
   end Translate_Signal;

   procedure Common_Death_Preparations (Proc : PID) is
   begin
      --  Inherit all our children to init, who will take care of them.
      Reassign_Parent_To_Init (Proc);

      --  Remove all state but the return value and keep the zombie around
      --  until we are waited.
      Userland.Process.Flush_Threads (Proc);
      Userland.Process.Flush_Files   (Proc);
   end Common_Death_Preparations;

   procedure Clear_Process_Signals (Proc : PID; Has_Handled : out Boolean) is
      Raised_Signal : Userland.Process.Signal;
      Signal_Addr   : System.Address;
      Restorer_Addr : System.Address;
      No_Signal     : Boolean;
      Ignore_Signal : Boolean;
      Altstack      : Boolean;
      Mask          : Process.Signal_Bitmap;
   begin
      Process.Get_Raised_Signal_Actions
         (Proc     => Proc,
          Sig      => Raised_Signal,
          Handler  => Signal_Addr,
          Restorer => Restorer_Addr,
          No_Sig   => No_Signal,
          Ignore   => Ignore_Signal,
          Altstack => Altstack,
          Old_Mask => Mask);
      if not No_Signal then
         --  Handle signals.
         if Signal_Addr /= System.Null_Address then
            Scheduler.Launch_Signal_Thread
               (Unsigned_64 (Process.Signal'Enum_Rep (Raised_Signal)),
                Signal_Addr, Restorer_Addr, Altstack, Has_Handled);
         elsif not Ignore_Signal then
            Exit_Process (Proc, Raised_Signal);
         else
            Has_Handled := True;
         end if;

         --  Restore old signals if we were successful.
         if Has_Handled then
            Process.Set_Masked_Signals (Proc, Mask);
         end if;
      else
         Has_Handled := False;
      end if;
   end Clear_Process_Signals;
end Userland.Syscall;
