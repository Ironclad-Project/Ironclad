--  vfs-ext.adb: Linux Extended FS driver.
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

package body VFS.Dev is
   Device_Permissions : constant := 8#660#;

   procedure Probe
      (Handle       : Device_Handle;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Data_Addr    : out System.Address)
   is
      pragma Unreferenced (Handle);
      pragma Unreferenced (Do_Read_Only);
      pragma Unreferenced (Do_Relatime);
   begin
      Data_Addr := System'To_Address (1);
   end Probe;

   procedure Remount
      (FS           : System.Address;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
   is
      pragma Unreferenced (FS);
      pragma Unreferenced (Do_Read_Only);
      pragma Unreferenced (Do_Relatime);
   begin
      Success := True;
   end Remount;

   procedure Unmount (FS : in out System.Address) is
   begin
      FS := System.Null_Address;
   end Unmount;
   ----------------------------------------------------------------------------
   procedure Get_Block_Size (FS : System.Address; Size : out Unsigned_64) is
      pragma Unreferenced (FS);
   begin
      Size := 0;
   end Get_Block_Size;

   procedure Get_Fragment_Size (FS : System.Address; Size : out Unsigned_64) is
      pragma Unreferenced (FS);
   begin
      Size := 0;
   end Get_Fragment_Size;

   procedure Get_Size (FS : System.Address; Size : out Unsigned_64) is
      pragma Unreferenced (FS);
   begin
      Size := 0;
   end Get_Size;

   procedure Get_Inode_Count (FS : System.Address; Count : out Unsigned_64) is
      pragma Unreferenced (FS);
   begin
      Count := 0;
   end Get_Inode_Count;

   procedure Get_Free_Blocks
      (FS                 : System.Address;
       Free_Blocks        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
   is
      pragma Unreferenced (FS);
   begin
      Free_Blocks := 0;
      Free_Unpriviledged := 0;
   end Get_Free_Blocks;

   procedure Get_Free_Inodes
      (FS                 : System.Address;
       Free_Inodes        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
   is
      pragma Unreferenced (FS);
   begin
      Free_Inodes := 0;
      Free_Unpriviledged := 0;
   end Get_Free_Inodes;

   function Get_Max_Length (FS : System.Address) return Unsigned_64 is
      pragma Unreferenced (FS);
   begin
      return Unsigned_64 (Devices.Max_Name_Length);
   end Get_Max_Length;
   ----------------------------------------------------------------------------
   procedure Open
      (FS         : System.Address;
       Relative   : File_Inode_Number;
       Path       : String;
       Ino        : out File_Inode_Number;
       Success    : out FS_Status;
       User       : Unsigned_32;
       Want_Read  : Boolean;
       Want_Write : Boolean;
       Do_Follow  : Boolean)
   is
      pragma Unreferenced (FS);
      pragma Unreferenced (Relative);
      pragma Unreferenced (Path);
      pragma Unreferenced (User);
      pragma Unreferenced (Want_Read);
      pragma Unreferenced (Want_Write);
      pragma Unreferenced (Do_Follow);
   begin
      Ino     := 0;
      Success := FS_Invalid_Value;
   end Open;

   procedure Close (FS : System.Address; Ino : File_Inode_Number) is
      pragma Unreferenced (FS);
      pragma Unreferenced (Ino);
   begin
      null;
   end Close;

   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      pragma Unreferenced (FS_Data);
      pragma Unreferenced (Ino);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Entities);
   begin
      Ret_Count := 0;
      Success   := FS_Invalid_Value;
   end Read_Entries;

   procedure Read
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      pragma Unreferenced (FS_Data);
      Dev  : constant Device_Handle := From_Unique_ID (Natural (Ino));
      Succ : Boolean;
   begin
      Read
         (Handle      => Dev,
          Offset      => Offset,
          Data        => Data,
          Ret_Count   => Ret_Count,
          Success     => Succ,
          Is_Blocking => True);
      Success := (if Succ then FS_Success else FS_IO_Failure);
   end Read;

   procedure Write
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      pragma Unreferenced (FS_Data);
      Dev  : constant Device_Handle := From_Unique_ID (Natural (Ino));
      Succ : Boolean;
   begin
      Write
         (Handle      => Dev,
          Offset      => Offset,
          Data        => Data,
          Ret_Count   => Ret_Count,
          Success     => Succ,
          Is_Blocking => True);
      Success := (if Succ then FS_Success else FS_IO_Failure);
   end Write;

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status)
   is
      pragma Unreferenced (Data);
      Dev : constant Device_Handle := From_Unique_ID (Natural (Ino));
   begin

      S :=
         (Unique_Identifier => Ino,
          Type_Of_File      => File_Character_Device,
          Mode              => Device_Permissions,
          UID               => 0,
          GID               => 0,
          Hard_Link_Count   => 1,
          Byte_Size         => 0,
          IO_Block_Size     => Devices.Get_Block_Size (Dev),
          IO_Block_Count    => Devices.Get_Block_Count (Dev),
          Creation_Time     => (0, 0),
          Modification_Time => (0, 0),
          Access_Time       => (0, 0));

      S.Byte_Size := Unsigned_64 (S.IO_Block_Size) * S.IO_Block_Count;

      if Devices.Is_Block_Device (Dev) then
         S.Type_Of_File := File_Block_Device;
      end if;

      Success := FS_Success;
   end Stat;

   procedure IO_Control
      (Data   : System.Address;
       Ino    : File_Inode_Number;
       Req    : Unsigned_64;
       Arg    : System.Address;
       Status : out FS_Status)
   is
      pragma Unreferenced (Data);
      Dev : constant Device_Handle := From_Unique_ID (Natural (Ino));
   begin
      if IO_Control (Dev, Req, Arg) then
         Status := FS_Success;
      else
         Status := FS_IO_Failure;
      end if;
   end IO_Control;

   procedure Check_Access
      (Data        : System.Address;
       Ino         : File_Inode_Number;
       Exists_Only : Boolean;
       Can_Read    : Boolean;
       Can_Write   : Boolean;
       Can_Exec    : Boolean;
       Real_UID    : Unsigned_32;
       Status      : out FS_Status)
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Ino);
   begin
      if Exists_Only then
         Status := FS_Success;
         return;
      end if;

      if ((Can_Read or Can_Write) and Real_UID /= 0) or
         Can_Exec
      then
         Status := FS_Not_Allowed;
      else
         Status := FS_Success;
      end if;
   end Check_Access;

   function Synchronize (Data : System.Address) return FS_Status is
      pragma Unreferenced (Data);
   begin
      return FS_Success;
   end Synchronize;

   function Synchronize
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Data_Only);
      Dev : constant Device_Handle := From_Unique_ID (Natural (Ino));
   begin
      if Devices.Synchronize (Dev) then
         return FS_Success;
      else
         return FS_IO_Failure;
      end if;
   end Synchronize;
end VFS.Dev;
