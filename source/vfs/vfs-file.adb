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

package body VFS.File with SPARK_Mode => Off is
   pragma Suppress (All_Checks);

   procedure Free_Str  is new Ada.Unchecked_Deallocation (String, String_Acc);
   procedure Free_File is new Ada.Unchecked_Deallocation (File, File_Acc);

   procedure Resolve_File
      (Path         : String;
       Is_Device    : out Boolean;
       Fetched_Dev  : out Device_Handle;
       Fetched_FS   : out FS_Handle;
       Fetched_File : out File_Inode_Number;
       Success      : out Boolean;
       Follow_Links : Boolean)
   is
      Fetched_Stat : File_Stat;
      Match_Count  : Natural;
      Last_Slash   : Natural := 0;
      Symlink      : String (1 .. 60);
      Symlink_Len  : Natural;
      Succ         : FS_Status;
   begin
      --  Default values.
      Is_Device    := False;
      Fetched_Dev  := Devices.Error_Handle;
      Fetched_FS   := Error_Handle;
      Fetched_File := 0;

      --  TODO: Handle non-canonical paths.
      if not Is_Canonical (Path) then
         Success := False;
         return;
      end if;

      --  Handle /dev/ devices, which we emulate with the internal kernel
      --  registries.
      if Path'Length > 4 and then Path (Path'First .. Path'First + 4) = "/dev/"
      then
         Fetched_Dev := Fetch (Path (Path'First + 5 .. Path'Last));
         Is_Device   := Fetched_Dev /= Devices.Error_Handle;
         Success     := Is_Device;
         return;
      end if;

      --  Do the usual file opening routine.
      Get_Mount (Path, Match_Count, Fetched_FS);
      if Fetched_FS = Error_Handle then
         Success := False;
         return;
      end if;

      Fetched_Dev  := Get_Backing_Device (Fetched_FS);
      Open
         (Fetched_FS,
          Path (Path'First + Match_Count .. Path'Last),
          Fetched_File,
          Succ);
      if Succ = FS_Success then
         Stat (Fetched_FS, Fetched_File, Fetched_Stat, Succ);
         Success := Succ = FS_Success;
      else
         Success := False;
         return;
      end if;

      --  Redirect if we are dealing with a symlink.
      if Success and then (Follow_Links and
         Fetched_Stat.Type_Of_File = File_Symbolic_Link)
      then
         VFS.Read_Symbolic_Link
            (Key       => Fetched_FS,
             Ino       => Fetched_File,
             Path      => Symlink,
             Ret_Count => Symlink_Len);

         for I in Path'Range loop
            if Path (I) = '/' then
               Last_Slash := I;
            end if;
         end loop;

         Close (Fetched_FS, Fetched_File);

         if Symlink_Len = 0 then
            Success := False;
            return;
         elsif Symlink (1) = '/' then
            Resolve_File (
               Symlink (1 .. Symlink_Len),
               Is_Device,
               Fetched_Dev,
               Fetched_FS,
               Fetched_File,
               Success,
               Follow_Links
            );
         else
            Resolve_File (
               Path (Path'First .. Last_Slash) & Symlink (1 .. Symlink_Len),
               Is_Device,
               Fetched_Dev,
               Fetched_FS,
               Fetched_File,
               Success,
               Follow_Links
            );
         end if;
      end if;
   end Resolve_File;

   procedure Open
      (Path         : String;
       Access_Flags : Access_Mode;
       Result       : out File_Acc;
       Follow_Links : Boolean := True)
   is
      Success      : Boolean;
      Is_Device    : Boolean;
      Fetched_Dev  : Device_Handle;
      Fetched_FS   : FS_Handle;
      Fetched_File : File_Inode_Number;
   begin
      Resolve_File
         (Path         => Path,
          Is_Device    => Is_Device,
          Fetched_Dev  => Fetched_Dev,
          Fetched_FS   => Fetched_FS,
          Fetched_File => Fetched_File,
          Success      => Success,
          Follow_Links => Follow_Links);

      if Success then
         Result := new File'(
            Refcount  => 1,
            Is_Device => Is_Device,
            Full_Path => new String'(Path),
            Dev_Data  => Fetched_Dev,
            FS_Data   => Fetched_FS,
            File_Data => Fetched_File,
            Index     => 0,
            Flags     => Access_Flags
         );
      else
         Result := null;
      end if;
   end Open;

   function Get_Path (File : File_Acc) return String_Acc is
   begin
      return File.Full_Path;
   end Get_Path;

   function Get_Position (File : File_Acc) return Unsigned_64 is
   begin
      return File.Index;
   end Get_Position;

   procedure Set_Position
      (File    : File_Acc;
       Pos     : Unsigned_64;
       Success : out Boolean)
   is
   begin
      if File.Is_Device and not Is_Block_Device (File.Dev_Data) then
         Success := False;
      else
         File.Index := Pos;
         Success := True;
      end if;
   end Set_Position;

   function Get_Device_ID (File : File_Acc) return Natural is
   begin
      return Devices.Get_Unique_ID (File.Dev_Data);
   end Get_Device_ID;

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
            if To_Close.FS_Data /= Error_Handle then
               VFS.Close (To_Close.FS_Data, To_Close.File_Data);
            end if;
            Free_Str (To_Close.Full_Path);
            Free_File (To_Close);
         end if;
      end if;
   end Close;

   procedure Read_Entries
      (To_Read   : File_Acc;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
   begin
      if not To_Read.Is_Device then
         VFS.Read_Entries
            (Key       => To_Read.FS_Data,
             Ino       => To_Read.File_Data,
             Entities  => Entities,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Success   := FS_Invalid_Value;
         Ret_Count := 0;
         Entities  := (others =>
            (Inode_Number => 0,
             Name_Buffer  => (others => ' '),
             Name_Len     => 0,
             Type_Of_File => File_Regular));
      end if;
   end Read_Entries;

   procedure Read_Symbolic_Link
      (To_Read   : File_Acc;
       Path      : out String;
       Ret_Count : out Natural)
   is
   begin
      if not To_Read.Is_Device then
         VFS.Read_Symbolic_Link
            (Key       => To_Read.FS_Data,
             Ino       => To_Read.File_Data,
             Path      => Path,
             Ret_Count => Ret_Count);
      else
         Path      := (others => ' ');
         Ret_Count := 0;
      end if;
   end Read_Symbolic_Link;

   procedure Read
      (To_Read   : File_Acc;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      Succ : Boolean;
   begin
      if To_Read.Flags = Write_Only then
         Data      := (others => 0);
         Ret_Count := 0;
         Success   := FS_Invalid_Value;
         return;
      end if;

      if not To_Read.Is_Device then
         VFS.Read
            (Key       => To_Read.FS_Data,
             Ino       => To_Read.File_Data,
             Offset    => To_Read.Index,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Devices.Read
            (Handle    => To_Read.Dev_Data,
             Offset    => To_Read.Index,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
         Success := (if Succ then FS_Success else FS_IO_Failure);
      end if;

      if Success = FS_Success then
         To_Read.Index := To_Read.Index + Unsigned_64 (Ret_Count);
      end if;
   end Read;

   procedure Write
      (To_Write  : File_Acc;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      Succ : Boolean;
   begin
      if To_Write.Flags = Read_Only then
         Ret_Count := 0;
         Success   := FS_Invalid_Value;
         return;
      end if;

      if not To_Write.Is_Device then
         VFS.Write
            (Key       => To_Write.FS_Data,
             Ino       => To_Write.File_Data,
             Offset    => To_Write.Index,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Devices.Write
            (Handle    => To_Write.Dev_Data,
             Offset    => To_Write.Index,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
         Success := (if Succ then FS_Success else FS_IO_Failure);
      end if;

      if Success = FS_Success then
         To_Write.Index := To_Write.Index + Unsigned_64 (Ret_Count);
      end if;
   end Write;

   procedure Stat (F : File_Acc; St : out File_Stat; Success : out FS_Status)
   is
      Is_Block                      : Boolean;
      Device_Type                   : File_Type;
      Block_Count                   : Unsigned_64;
      Block_Size, Unique_Identifier : Natural;
   begin
      if not F.Is_Device then
         VFS.Stat (F.FS_Data, F.File_Data, St, Success);
      else
         Is_Block          := Devices.Is_Block_Device (F.Dev_Data);
         Block_Size        := Devices.Get_Block_Size  (F.Dev_Data);
         Block_Count       := Devices.Get_Block_Count (F.Dev_Data);
         Unique_Identifier := Devices.Get_Unique_ID   (F.Dev_Data);
         if Is_Block then
            Device_Type := File_Block_Device;
         else
            Device_Type := File_Character_Device;
         end if;
         St := (
            Unique_Identifier => File_Inode_Number (Unique_Identifier),
            Type_Of_File      => Device_Type,
            Mode              => 8#660#,
            Hard_Link_Count   => 1,
            Byte_Size         => Unsigned_64 (Block_Size) * Block_Count,
            IO_Block_Size     => Block_Size,
            IO_Block_Count    => Block_Count,
            Creation_Time     => (0, 0),
            Modification_Time => (0, 0),
            Access_Time       => (0, 0)
         );
         Success := FS_Success;
      end if;
   end Stat;

   procedure Truncate (F : File_Acc; Sz : Unsigned_64; Success : out FS_Status)
   is
   begin
      if not F.Is_Device and F.Flags /= Read_Only then
         Success := VFS.Truncate (F.FS_Data, F.File_Data, Sz);
      else
         Success := FS_Invalid_Value;
      end if;
   end Truncate;

   procedure IO_Control
      (F        : File_Acc;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out FS_Status)
   is
   begin
      if F.Is_Device then
         if Devices.IO_Control (F.Dev_Data, Request, Argument) then
            Success := FS_Success;
         else
            Success := FS_Invalid_Value;
         end if;
      else
         Success := VFS.IO_Control (F.FS_Data, F.File_Data, Request, Argument);
      end if;
   end IO_Control;

   function Synchronize (F : File_Acc) return FS_Status is
   begin
      if F.Is_Device then
         if Devices.Synchronize (F.Dev_Data) then
            return FS_Success;
         else
            return FS_IO_Failure;
         end if;
      else
         return VFS.Synchronize (F.FS_Data, F.File_Data);
      end if;
   end Synchronize;

   function Mmap
      (F           : File_Acc;
       Address     : Memory.Virtual_Address;
       Length      : Unsigned_64;
       Map_Read    : Boolean;
       Map_Write   : Boolean;
       Map_Execute : Boolean) return Boolean
   is
   begin
      if F.Is_Device then
         return Devices.Mmap (
            F.Dev_Data,
            Address,
            Length,
            Map_Read,
            Map_Write,
            Map_Execute
         );
      else
         return False;
      end if;
   end Mmap;

   function Munmap
      (F       : File_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64) return Boolean
   is
   begin
      if F.Is_Device then
         return Devices.Munmap (F.Dev_Data, Address, Length);
      else
         return False;
      end if;
   end Munmap;

   procedure Create_Node
      (Path    : String;
       Typ     : File_Type;
       Mode    : File_Mode;
       Success : out FS_Status)
   is
      Match_Count : Natural;
      Handle      : FS_Handle;
   begin
      Get_Mount (Path, Match_Count, Handle);
      Success := Create_Node
         (Handle, Path (Path'First + Match_Count .. Path'Last), Typ, Mode);
   end Create_Node;

   procedure Create_Symbolic_Link
      (Path, Target : String;
       Mode         : Unsigned_32;
       Success      : out FS_Status)
   is
      Match_Count : Natural;
      Handle      : FS_Handle;
   begin
      Get_Mount (Path, Match_Count, Handle);
      Success := Create_Symbolic_Link
         (Handle, Path (Path'First + Match_Count .. Path'Last), Target, Mode);
   end Create_Symbolic_Link;

   procedure Create_Hard_Link
      (Path, Target : String;
       Success      : out FS_Status)
   is
      Match_Count : Natural;
      Handle      : FS_Handle;
   begin
      Get_Mount (Path, Match_Count, Handle);
      Success := Create_Hard_Link
         (Handle,
          Path   (Path'First   + Match_Count ..   Path'Last),
          Target (Target'First + Match_Count .. Target'Last));
   end Create_Hard_Link;

   procedure Rename
      (Source, Target : String;
       Keep           : Boolean;
       Success        : out FS_Status)
   is
      Match_Count : Natural;
      Handle      : FS_Handle;
   begin
      Get_Mount (Source, Match_Count, Handle);
      Success := Rename
         (Handle,
          Source (Source'First + Match_Count .. Source'Last),
          Target (Target'First + Match_Count .. Target'Last),
          Keep);
   end Rename;

   procedure Unlink (Path : String; Success : out FS_Status) is
      Match_Count : Natural;
      Handle      : FS_Handle;
   begin
      Get_Mount (Path, Match_Count, Handle);
      Success := Unlink (Handle, Path (Path'First + Match_Count .. Path'Last));
   end Unlink;
end VFS.File;
