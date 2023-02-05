--  vfs-ext.ads: Linux Extended FS driver.
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

with System;

package VFS.EXT with SPARK_Mode => Off is
   --  Probe for an ext* FS in the passed device.
   --  Return opaque FS data on success, or Null_Address on failure.
   function Probe (Handle : Device_Handle) return System.Address;

   --  Basic file operations wrapped in vfs.adb.
   function Open (FS : System.Address; Path : String) return System.Address;
   function Create
      (FS   : System.Address;
       Path : String;
       Mode : Unsigned_32) return System.Address;
   function Create_Symbolic_Link
      (FS           : System.Address;
       Path, Target : String;
       Mode         : Unsigned_32) return System.Address;
   function Create_Directory
      (FS   : System.Address;
       Path : String;
       Mode : Unsigned_32) return System.Address;
   procedure Close (FS : System.Address; Obj : out System.Address);
   procedure Read_Entries
      (FS_Data   : System.Address;
       Obj       : System.Address;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out Boolean);
   procedure Read
      (FS_Data   : System.Address;
       Obj       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);
   procedure Write
      (FS_Data   : System.Address;
       Obj       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);
   function Stat
      (Data : System.Address;
       Obj  : System.Address;
       S    : out File_Stat) return Boolean;

private

   State_Clean       : constant := 1;
   Policy_Ignore     : constant := 1;
   Policy_Remount_RO : constant := 2;
   Policy_Panic      : constant := 3;
   Superblock_Offset : constant := 512 * 2;
   type Superblock is record
      Inode_Count             : Unsigned_32;
      Block_Count             : Unsigned_32;
      Reserved_Count          : Unsigned_32;
      Unallocated_Block_Count : Unsigned_32;
      Unallocated_Inode_Count : Unsigned_32;
      Block_Containing_Super  : Unsigned_32;
      Block_Size_Log          : Unsigned_32;
      Fragment_Size_Log       : Unsigned_32;
      Blocks_Per_Group        : Unsigned_32;
      Fragments_Per_Group     : Unsigned_32;
      Inodes_Per_Group        : Unsigned_32;
      Last_Mount_Epoch        : Unsigned_32;
      Last_Write_Epoch        : Unsigned_32;
      Mounts_Since_Check      : Unsigned_16;
      Max_Mounts_Since_Check  : Unsigned_16;
      Signature               : Unsigned_16;
      Filesystem_State        : Unsigned_16;
      Error_Policy            : Unsigned_16;
      Minor_Version           : Unsigned_16;
      Last_Check_Epoch        : Unsigned_32;
      Interval_Between_Checks : Unsigned_32;
      Operating_System_ID     : Unsigned_32;
      Major_Version           : Unsigned_32;
      Reserved_Blocks_User_ID : Unsigned_16;
      Reserved_Blocks_Group   : Unsigned_16;
   end record;

   type Inode_Block_Arr is array (Natural range <>) of Unsigned_32;
   type Inode is record
      Permissions         : Unsigned_16;
      UID                 : Unsigned_16;
      Size_Low            : Unsigned_32;
      Access_Time_Epoch   : Unsigned_32;
      Creation_Time_Epoch : Unsigned_32;
      Modified_Time_Epoch : Unsigned_32;
      Deleted_Time_Epoch  : Unsigned_32;
      GID                 : Unsigned_16;
      Hard_Link_Count     : Unsigned_16;
      Sectors             : Unsigned_32;
      Flags               : Unsigned_32;
      OS_Specific_Value_1 : Unsigned_32;
      Blocks              : Inode_Block_Arr (1 .. 15);
      Generated_Number    : Unsigned_32;
      EAB                 : Unsigned_32;
      Size_High           : Unsigned_32;
      Fragment_Address    : Unsigned_32;
      OS_Specific_Value_2 : Inode_Block_Arr (1 .. 3);
   end record;

   type Directory_Entry is record
      Inode_Index : Unsigned_32;
      Entry_Count : Unsigned_16;
      Name_Length : Unsigned_8;
      Dir_Type    : Unsigned_8;
   end record;

   type EXT_Data is record
      Handle        : Device_Handle;
      Super         : Superblock;
      Is_Read_Only  : Boolean;
      Block_Size    : Unsigned_32;
      Fragment_Size : Unsigned_32;
      Error_Policy  : Unsigned_16;
   end record;
   type EXT_Data_Acc is access all EXT_Data;

   function Read_Superblock (Data : EXT_Data_Acc) return Boolean;
   function Write_Superblock (Data : EXT_Data_Acc) return Boolean;
   procedure Act_On_Policy (Data : EXT_Data_Acc; Message : String);
end VFS.EXT;
