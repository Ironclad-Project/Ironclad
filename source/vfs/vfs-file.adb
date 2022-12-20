--  vfs-file.adb: File creation and management.
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

with Ada.Unchecked_Deallocation;
with System; use System;
with VFS.USTAR;

package body VFS.File with SPARK_Mode => Off is
   procedure Free_Str  is new Ada.Unchecked_Deallocation (String, String_Acc);
   procedure Free_File is new Ada.Unchecked_Deallocation (File, File_Acc);

   function Resolve_File
      (Path         : String;
       Is_Device    : out Boolean;
       Fetched_Dev  : out Devices.Resource_Acc;
       Fetched_Type : out FS_Type;
       Fetched_FS   : out System.Address) return System.Address
   is
      Fetched_File : System.Address := System.Null_Address;
   begin
      --  Default values.
      Is_Device    := False;
      Fetched_Dev  := null;
      Fetched_Type := FS_USTAR;
      Fetched_FS   := System.Null_Address;

      --  TODO: Handle non-canonical paths.
      if not Is_Canonical (Path) then
         goto Done;
      end if;

      --  Handle /dev/ devices, which we emulate with the internal kernel
      --  registries. Else, do the usual procedure for files.
      if Path'Length > 4 and then Path (Path'First .. Path'First + 4) = "/dev/"
      then
         Fetched_Dev := Fetch (Path (Path'First + 5 .. Path'Last));
         Is_Device   := Fetched_Dev /= null;
      else
         Fetched_FS := Get_Mount ("/", Fetched_Type, Fetched_Dev);
         if Fetched_FS /= Null_Address then
            case Fetched_Type is
               when FS_USTAR =>
                  Fetched_File := USTAR.Open (
                     Fetched_FS, Path (Path'First + 1 .. Path'Last)
                  );
            end case;
         end if;
      end if;

   <<Done>>
      return Fetched_File;
   end Resolve_File;

   function Open (Path : String; Access_Flags : Access_Mode) return File_Acc is
      Is_Device    : Boolean;
      Fetched_Dev  : Devices.Resource_Acc;
      Fetched_Type : FS_Type;
      Fetched_FS   : System.Address;
      Fetched_File : System.Address;
   begin
      Fetched_File := Resolve_File (
         Path,
         Is_Device,
         Fetched_Dev,
         Fetched_Type,
         Fetched_FS
      );
      if Fetched_File = System.Null_Address and not Is_Device then
         return null;
      else
         return new File'(
            Refcount  => 1,
            Full_Path => new String'(Path),
            Dev_Data  => Fetched_Dev,
            FS_Type   => Fetched_Type,
            FS_Data   => Fetched_FS,
            File_Data => Fetched_File,
            Index     => 0,
            Flags     => Access_Flags
         );
      end if;
   end Open;

   function Check_Permissions
      (Path      : String;
       Exists    : Boolean;
       Can_Read  : Boolean;
       Can_Write : Boolean;
       Can_Exec  : Boolean) return Boolean
   is
      Is_Device    : Boolean;
      Fetched_Dev  : Devices.Resource_Acc;
      Fetched_Type : FS_Type;
      Fetched_FS   : System.Address;
      Discard      : System.Address;
   begin
      Discard := Resolve_File (
         Path,
         Is_Device,
         Fetched_Dev,
         Fetched_Type,
         Fetched_FS
      );
      if Is_Device and Exists and not Can_Exec then
         return True;
      elsif Fetched_FS /= System.Null_Address then
         return USTAR.Check_Permissions (
            Fetched_FS,
            Path,
            Exists,
            Can_Read,
            Can_Write,
            Can_Exec
         );
      else
         return False;
      end if;
   end Check_Permissions;

   procedure Increase_Refcount (F : File_Acc) is
   begin
      if F /= null then
         F.Refcount := F.Refcount + 1;
      end if;
   end Increase_Refcount;

   procedure Close (To_Close : in out File_Acc) is
   begin
      if To_Close /= null then
         To_Close.Refcount := To_Close.Refcount - 1;
         if To_Close.Refcount = 0 then
            if To_Close.FS_Data /= System.Null_Address then
               USTAR.Close (To_Close.FS_Data, To_Close.File_Data);
            end if;
            Free_Str (To_Close.Full_Path);
            Free_File (To_Close);
         end if;
      end if;
   end Close;

   function Read
      (To_Read     : File_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
   is
      Read_Count : Unsigned_64;
   begin
      if To_Read = null or else To_Read.Flags = Access_W then
         return 0;
      end if;

      if To_Read.FS_Data /= System.Null_Address
         and To_Read.File_Data /= System.Null_Address
      then
         Read_Count := USTAR.Read (
            To_Read.FS_Data,
            To_Read.File_Data,
            To_Read.Index,
            Count,
            Destination
         );
         To_Read.Index := To_Read.Index + Read_Count;
         return Read_Count;
      elsif To_Read.Dev_Data.Read /= null then
         Read_Count := To_Read.Dev_Data.Read (
            To_Read.Dev_Data,
            To_Read.Index,
            Count,
            Destination
         );
         To_Read.Index := To_Read.Index + Read_Count;
         return Read_Count;
      else
         return 0;
      end if;
   end Read;

   function Write
      (To_Write : File_Acc;
       Count    : Unsigned_64;
       Data     : System.Address) return Unsigned_64
   is
      Write_Count : Unsigned_64;
   begin
      if To_Write = null or else To_Write.Flags = Access_R then
         return 0;
      end if;
      if To_Write.FS_Data /= System.Null_Address and
         To_Write.File_Data /= System.Null_Address
      then
         return 0;
      else
         Write_Count := To_Write.Dev_Data.Write (
            To_Write.Dev_Data,
            To_Write.Index,
            Count,
            Data
         );
         To_Write.Index := To_Write.Index + Write_Count;
         return Write_Count;
      end if;
   end Write;

   function Stat (F : File_Acc; S : out File_Stat) return Boolean is
   begin
      if F = null then
         return False;
      end if;
      if F.FS_Data /= System.Null_Address and
         F.File_Data /= System.Null_Address
      then
         return USTAR.Stat (F.FS_Data, F.File_Data, S);
      else
         S := (
            Unique_Identifier => 1,
            Type_Of_File      => File_Block_Device,
            Mode              => 8#660#,
            Hard_Link_Count   => F.Dev_Data.Unique_Identifier,
            Byte_Size        => F.Dev_Data.Block_Size * F.Dev_Data.Block_Count,
            IO_Block_Size     => Integer (F.Dev_Data.Block_Size),
            IO_Block_Count    => F.Dev_Data.Block_Count
         );
         return True;
      end if;
   end Stat;

   function IO_Control
      (F        : File_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
   begin
      if F = null then
         return False;
      end if;
      if F.FS_Data /= System.Null_Address and
         F.File_Data /= System.Null_Address
      then
         --  Support USTAR IOCTL.
         return False;
      elsif F.Dev_Data.IO_Control /= null then
         return F.Dev_Data.IO_Control (F.Dev_Data, Request, Argument);
      else
         return False;
      end if;
   end IO_Control;

   function Mmap
      (F           : File_Acc;
       Address     : Memory.Virtual_Address;
       Length      : Unsigned_64;
       Map_Read    : Boolean;
       Map_Write   : Boolean;
       Map_Execute : Boolean) return Boolean
   is
   begin
      if F = null then
         return False;
      end if;
      if F.FS_Data = System.Null_Address and F.File_Data = System.Null_Address
         and F.Dev_Data.Mmap /= null
      then
         return F.Dev_Data.Mmap (
            F.Dev_Data,
            Address,
            Length,
            Map_Read,
            Map_Write,
            Map_Execute
         );
      else
         --  TODO: Support mmaping a non-device.
         return False;
      end if;
   end Mmap;

   function Munmap
      (F       : File_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64) return Boolean
   is
   begin
      if F = null then
         return False;
      end if;
      if F.FS_Data = System.Null_Address and F.File_Data = System.Null_Address
         and F.Dev_Data.Munmap /= null
      then
         return F.Dev_Data.Munmap (F.Dev_Data, Address, Length);
      else
         --  TODO: Support mmaping a non-device.
         return False;
      end if;
   end Munmap;
end VFS.File;
