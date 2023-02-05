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
   procedure Free_Str  is new Ada.Unchecked_Deallocation (String, String_Acc);
   procedure Free_File is new Ada.Unchecked_Deallocation (File, File_Acc);

   function Resolve_File
      (Path         : String;
       Is_Device    : out Boolean;
       Fetched_Dev  : out Device_Handle;
       Fetched_FS   : out FS_Handle;
       Follow_Links : Boolean) return System.Address
   is
      Fetched_File : System.Address := System.Null_Address;
      Fetched_Stat : File_Stat;
      Fetched_Succ : Boolean := False;
      Last_Slash   : Natural := 0;
      Symlink      : String (1 .. 60);
      Symlink_Len  : Natural;
      Discard      : Boolean;
   begin
      --  Default values.
      Is_Device   := False;
      Fetched_Dev := Devices.Error_Handle;
      Fetched_FS  := Error_Handle;

      --  TODO: Handle non-canonical paths.
      if not Is_Canonical (Path) then
         goto Done;
      end if;

      --  Handle /dev/ devices, which we emulate with the internal kernel
      --  registries.
      if Path'Length > 4 and then Path (Path'First .. Path'First + 4) = "/dev/"
      then
         Fetched_Dev := Fetch (Path (Path'First + 5 .. Path'Last));
         Is_Device   := Fetched_Dev /= Devices.Error_Handle;
         goto Done;
      end if;

      --  Do the usual file opening routine.
      Fetched_FS := Get_Mount ("/");
      if Fetched_FS = Error_Handle then
         goto Done;
      end if;

      Fetched_Dev  := Get_Backing_Device (Fetched_FS);
      Fetched_File := Open (Fetched_FS, Path (Path'First + 1 .. Path'Last));
      if Fetched_File /= Null_Address then
         Fetched_Succ := Stat (Fetched_FS, Fetched_File, Fetched_Stat);
      else
         Fetched_Succ := False;
      end if;

      --  Redirect if we are dealing with a symlink.
      if Follow_Links and Fetched_Succ and
         Fetched_Stat.Type_Of_File = File_Symbolic_Link
      then
         VFS.Read_Symbolic_Link
            (Key       => Fetched_FS,
             Obj       => Fetched_File,
             Path      => Symlink,
             Ret_Count => Symlink_Len);

         for I in Path'Range loop
            if Path (I) = '/' then
               Last_Slash := I;
            end if;
         end loop;

         if Symlink_Len = 0 then
            return Null_Address;
         elsif Symlink (1) = '/' then
            return Resolve_File (
               Symlink (1 .. Symlink_Len),
               Is_Device,
               Fetched_Dev,
               Fetched_FS,
               Follow_Links
            );
         else
            return Resolve_File (
               Path (Path'First .. Last_Slash) & Symlink (1 .. Symlink_Len),
               Is_Device,
               Fetched_Dev,
               Fetched_FS,
               Follow_Links
            );
         end if;
      end if;

   <<Done>>
      return Fetched_File;
   end Resolve_File;

   function Open
      (Path         : String;
       Access_Flags : Access_Mode;
       Follow_Links : Boolean := True) return File_Acc
   is
      Is_Device    : Boolean;
      Fetched_Dev  : Device_Handle;
      Fetched_FS   : FS_Handle;
      Fetched_File : System.Address;
   begin
      Fetched_File := Resolve_File (
         Path,
         Is_Device,
         Fetched_Dev,
         Fetched_FS,
         Follow_Links
      );
      if Fetched_File = System.Null_Address and not Is_Device then
         return null;
      else
         return new File'(
            Refcount  => 1,
            Full_Path => new String'(Path),
            Dev_Data  => Fetched_Dev,
            FS_Data   => Fetched_FS,
            File_Data => Fetched_File,
            Index     => 0,
            Flags     => Access_Flags
         );
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

   procedure Set_Position (File : File_Acc; Pos : Unsigned_64) is
   begin
      File.Index := Pos;
   end Set_Position;

   function Get_Access (File : File_Acc) return Access_Mode is
   begin
      return File.Flags;
   end Get_Access;

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
       Success   : out Boolean)
   is
   begin
      if To_Read.FS_Data   /= Error_Handle and
         To_Read.File_Data /= System.Null_Address
      then
         VFS.Read_Entries
            (Key       => To_Read.FS_Data,
             Obj       => To_Read.File_Data,
             Entities  => Entities,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Success   := False;
         Ret_Count := 0;
      end if;
   end Read_Entries;

   procedure Read_Symbolic_Link
      (To_Read   : File_Acc;
       Path      : out String;
       Ret_Count : out Natural)
   is
   begin
      if To_Read.FS_Data   /= Error_Handle and
         To_Read.File_Data /= System.Null_Address
      then
         VFS.Read_Symbolic_Link
            (Key       => To_Read.FS_Data,
             Obj       => To_Read.File_Data,
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
       Success   : out Boolean)
   is
   begin
      if To_Read.Flags = Write_Only then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      if To_Read.FS_Data   /= Error_Handle and
         To_Read.File_Data /= System.Null_Address
      then
         VFS.Read
            (Key       => To_Read.FS_Data,
             Obj       => To_Read.File_Data,
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
             Success   => Success);
      end if;

      if Success then
         To_Read.Index := To_Read.Index + Unsigned_64 (Ret_Count);
      end if;
   end Read;

   procedure Write
      (To_Write  : File_Acc;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
   begin
      if To_Write.Flags = Read_Only then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      if To_Write.FS_Data   /= Error_Handle and
         To_Write.File_Data /= System.Null_Address
      then
         VFS.Write
            (Key       => To_Write.FS_Data,
             Obj       => To_Write.File_Data,
             Offset    => To_Write.Index,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Devices.Write (
            Handle    => To_Write.Dev_Data,
            Offset    => To_Write.Index,
            Data      => Data,
            Ret_Count => Ret_Count,
            Success   => Success
         );
      end if;

      if Success then
         To_Write.Index := To_Write.Index + Unsigned_64 (Ret_Count);
      end if;
   end Write;

   function Stat (F : File_Acc; S : out File_Stat) return Boolean is
      Is_Block                      : Boolean;
      Device_Type                   : File_Type;
      Block_Count                   : Unsigned_64;
      Block_Size, Unique_Identifier : Natural;
   begin
      if F.FS_Data   /= Error_Handle and
         F.File_Data /= System.Null_Address
      then
         return VFS.Stat (F.FS_Data, F.File_Data, S);
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
         S := (
            Unique_Identifier => Unsigned_64 (Unique_Identifier),
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
         return True;
      end if;
   end Stat;

   function IO_Control
      (F        : File_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
   begin
      if F.FS_Data = Error_Handle and F.File_Data = System.Null_Address then
         return Devices.IO_Control (F.Dev_Data, Request, Argument);
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
      if F.FS_Data = Error_Handle and F.File_Data = System.Null_Address then
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
      if F.FS_Data = Error_Handle and F.File_Data = System.Null_Address then
         return Devices.Munmap (F.Dev_Data, Address, Length);
      else
         return False;
      end if;
   end Munmap;
end VFS.File;
