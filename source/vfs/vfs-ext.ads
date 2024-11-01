--  vfs-ext.ads: Linux Extended FS driver.
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

with System;
with Lib.Synchronization;

package VFS.EXT is
   procedure Probe
      (Handle       : Device_Handle;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Data_Addr    : out System.Address);

   procedure Remount
      (FS           : System.Address;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean);

   procedure Unmount (FS : in out System.Address);
   ----------------------------------------------------------------------------
   procedure Get_Block_Size (FS : System.Address; Size : out Unsigned_64);

   procedure Get_Fragment_Size (FS : System.Address; Size : out Unsigned_64);

   procedure Get_Size (FS : System.Address; Size : out Unsigned_64);

   procedure Get_Inode_Count (FS : System.Address; Count : out Unsigned_64);

   procedure Get_Free_Blocks
      (FS                 : System.Address;
       Free_Blocks        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64);

   procedure Get_Free_Inodes
      (FS                 : System.Address;
       Free_Inodes        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64);

   function Get_Max_Length (FS : System.Address) return Unsigned_64;
   ----------------------------------------------------------------------------
   procedure Create_Node
      (FS       : System.Address;
       Relative : File_Inode_Number;
       Path     : String;
       Typ      : File_Type;
       Mode     : File_Mode;
       User     : Unsigned_32;
       Status   : out FS_Status);

   procedure Create_Symbolic_Link
      (FS       : System.Address;
       Relative : File_Inode_Number;
       Path     : String;
       Target   : String;
       Mode     : Unsigned_32;
       User     : Unsigned_32;
       Status   : out FS_Status);

   procedure Create_Hard_Link
      (FS              : System.Address;
       Relative_Path   : File_Inode_Number;
       Path            : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       User            : Unsigned_32;
       Status          : out FS_Status);

   procedure Rename
      (FS              : System.Address;
       Relative_Source : File_Inode_Number;
       Source          : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       Keep            : Boolean;
       User            : Unsigned_32;
       Status          : out FS_Status);

   procedure Unlink
      (FS       : System.Address;
       Relative : File_Inode_Number;
       Path     : String;
       User     : Unsigned_32;
       Status   : out FS_Status);

   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status);

   procedure Read_Symbolic_Link
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Path      : out String;
       Ret_Count : out Natural;
       Success   : out FS_Status);

   procedure Read
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status);

   procedure Write
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status);

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status);

   procedure Truncate
      (Data     : System.Address;
       Ino      : File_Inode_Number;
       New_Size : Unsigned_64;
       Status   : out FS_Status);

   procedure IO_Control
      (Data   : System.Address;
       Ino    : File_Inode_Number;
       Req    : Unsigned_64;
       Arg    : System.Address;
       Status : out FS_Status);

   procedure Change_Mode
      (Data   : System.Address;
       Ino    : File_Inode_Number;
       Mode   : File_Mode;
       Status : out FS_Status);

   procedure Change_Owner
      (Data   : System.Address;
       Ino    : File_Inode_Number;
       Owner  : Unsigned_32;
       Group  : Unsigned_32;
       Status : out FS_Status);

   procedure Change_Access_Times
      (Data               : System.Address;
       Ino                : File_Inode_Number;
       Access_Seconds     : Unsigned_64;
       Access_Nanoseconds : Unsigned_64;
       Modify_Seconds     : Unsigned_64;
       Modify_Nanoseconds : Unsigned_64;
       Status             : out FS_Status);

   function Synchronize (Data : System.Address) return FS_Status;

   function Synchronize
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status;

private

   State_Clean       : constant := 1;
   Policy_Ignore     : constant := 1;
   Policy_Remount_RO : constant := 2;
   Policy_Panic      : constant := 3;
   EXT_Signature     : constant := 16#EF53#;
   Main_Superblock_Offset : constant := 512 * 2;
   Required_Compression     : constant := 1;
   Required_Directory_Types : constant := 2;
   Required_Journal_Replay  : constant := 4;
   Required_Journal_Device  : constant := 8;
   RO_Sparse_Superblocks    : constant := 1;
   RO_64bit_Filesize        : constant := 2;
   RO_Binary_Trees          : constant := 4;
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
      First_Non_Reserved      : Unsigned_32;
      Inode_Size              : Unsigned_16;
      Block_Group_Of_Super    : Unsigned_16;
      Optional_Features       : Unsigned_32;
      Required_Features       : Unsigned_32;
      RO_If_Not_Features      : Unsigned_32;
      FS_ID_High              : Unsigned_64;
      FS_ID_Low               : Unsigned_64;
      Volume_Name             : String (1 .. 16);
      Last_Mountpoint         : String (1 .. 64);
      Compression_Algorithm   : Unsigned_32;
      Preallocate_Files       : Unsigned_8;
      Preallocate_Dirs        : Unsigned_8;
      Unused                  : Unsigned_16;
      Journal_ID_High         : Unsigned_64;
      Journal_ID_Low          : Unsigned_64;
      Journal_Inode           : Unsigned_32;
      Journal_Device          : Unsigned_32;
      Head_Of_Orphan_Inodes   : Unsigned_32;
   end record with Size => 1888;
   for Superblock use record
      Inode_Count             at 0 range    0 ..   31;
      Block_Count             at 0 range   32 ..   63;
      Reserved_Count          at 0 range   64 ..   95;
      Unallocated_Block_Count at 0 range   96 ..  127;
      Unallocated_Inode_Count at 0 range  128 ..  159;
      Block_Containing_Super  at 0 range  160 ..  191;
      Block_Size_Log          at 0 range  192 ..  223;
      Fragment_Size_Log       at 0 range  224 ..  255;
      Blocks_Per_Group        at 0 range  256 ..  287;
      Fragments_Per_Group     at 0 range  288 ..  319;
      Inodes_Per_Group        at 0 range  320 ..  351;
      Last_Mount_Epoch        at 0 range  352 ..  383;
      Last_Write_Epoch        at 0 range  384 ..  415;
      Mounts_Since_Check      at 0 range  416 ..  431;
      Max_Mounts_Since_Check  at 0 range  432 ..  447;
      Signature               at 0 range  448 ..  463;
      Filesystem_State        at 0 range  464 ..  479;
      Error_Policy            at 0 range  480 ..  495;
      Minor_Version           at 0 range  496 ..  511;
      Last_Check_Epoch        at 0 range  512 ..  543;
      Interval_Between_Checks at 0 range  544 ..  575;
      Operating_System_ID     at 0 range  576 ..  607;
      Major_Version           at 0 range  608 ..  639;
      Reserved_Blocks_User_ID at 0 range  640 ..  655;
      Reserved_Blocks_Group   at 0 range  656 ..  671;
      First_Non_Reserved      at 0 range  672 ..  703;
      Inode_Size              at 0 range  704 ..  719;
      Block_Group_Of_Super    at 0 range  720 ..  735;
      Optional_Features       at 0 range  736 ..  767;
      Required_Features       at 0 range  768 ..  799;
      RO_If_Not_Features      at 0 range  800 ..  831;
      FS_ID_High              at 0 range  832 ..  895;
      FS_ID_Low               at 0 range  896 ..  959;
      Volume_Name             at 0 range  960 .. 1087;
      Last_Mountpoint         at 0 range 1088 .. 1599;
      Compression_Algorithm   at 0 range 1600 .. 1631;
      Preallocate_Files       at 0 range 1632 .. 1639;
      Preallocate_Dirs        at 0 range 1640 .. 1647;
      Unused                  at 0 range 1648 .. 1663;
      Journal_ID_High         at 0 range 1664 .. 1727;
      Journal_ID_Low          at 0 range 1728 .. 1791;
      Journal_Inode           at 0 range 1792 .. 1823;
      Journal_Device          at 0 range 1824 .. 1855;
      Head_Of_Orphan_Inodes   at 0 range 1856 .. 1887;
   end record;

   type Descriptor_Padding is array (Natural range <>) of Unsigned_16;
   type Block_Group_Descriptor is record
      Block_Usage_Bitmap_Block : Unsigned_32;
      Inode_Usage_Bitmap_Block : Unsigned_32;
      Inode_Table_Block        : Unsigned_32;
      Unallocated_Blocks       : Unsigned_16;
      Unallocated_Inodes       : Unsigned_16;
      Directory_Count          : Unsigned_16;
      Padding                  : Descriptor_Padding (1 .. 7);
   end record with Size => 262;
   for Block_Group_Descriptor use record
      Block_Usage_Bitmap_Block at 0 range   0 .. 31;
      Inode_Usage_Bitmap_Block at 0 range  32 .. 63;
      Inode_Table_Block        at 0 range  64 .. 95;
      Unallocated_Blocks       at 0 range  96 .. 117;
      Unallocated_Inodes       at 0 range 118 .. 133;
      Directory_Count          at 0 range 134 .. 149;
      Padding                  at 0 range 150 .. 261;
   end record;

   Flags_Immutable   : constant := 16#10#;
   Flags_Append_Only : constant := 16#20#;
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
      Blocks              : Inode_Block_Arr (0 .. 14);
      Generation_Number   : Unsigned_32;
      EAB                 : Unsigned_32;
      Size_High           : Unsigned_32;
      Fragment_Address    : Unsigned_32;
      OS_Specific_Value_2 : Inode_Block_Arr (1 .. 3);
   end record with Size => 1024;
   for Inode use record
      Permissions         at 0 range   0 ..  15;
      UID                 at 0 range  16 ..  31;
      Size_Low            at 0 range  32 ..  63;
      Access_Time_Epoch   at 0 range  64 ..  95;
      Creation_Time_Epoch at 0 range  96 .. 127;
      Modified_Time_Epoch at 0 range 128 .. 159;
      Deleted_Time_Epoch  at 0 range 160 .. 191;
      GID                 at 0 range 192 .. 207;
      Hard_Link_Count     at 0 range 208 .. 223;
      Sectors             at 0 range 224 .. 255;
      Flags               at 0 range 256 .. 287;
      OS_Specific_Value_1 at 0 range 288 .. 319;
      Blocks              at 0 range 320 .. 799;
      Generation_Number   at 0 range 800 .. 831;
      EAB                 at 0 range 832 .. 863;
      Size_High           at 0 range 864 .. 895;
      Fragment_Address    at 0 range 896 .. 927;
      OS_Specific_Value_2 at 0 range 928 .. 1023;
   end record;
   type Inode_Acc is access Inode;

   type Directory_Entry is record
      Inode_Index : Unsigned_32;
      Entry_Count : Unsigned_16;
      Name_Length : Unsigned_8;
      Dir_Type    : Unsigned_8;
   end record with Size => 64;
   for Directory_Entry use record
      Inode_Index at 0 range  0 .. 31;
      Entry_Count at 0 range 32 .. 47;
      Name_Length at 0 range 48 .. 55;
      Dir_Type    at 0 range 56 .. 63;
   end record;

   type EXT_Data is record
      Mutex                 : aliased Lib.Synchronization.Binary_Semaphore;
      Handle                : Device_Handle;
      Super                 : Superblock;
      Is_Read_Only          : Boolean;
      Do_Relatime           : Boolean;
      Block_Size            : Unsigned_32;
      Fragment_Size         : Unsigned_32;
      Root                  : Inode;
      Has_Sparse_Superblock : Boolean;
      Has_64bit_Filesizes   : Boolean;
   end record;
   type EXT_Data_Acc is access all EXT_Data;

   Root_Inode         : constant := 2;
   Max_File_Name_Size : constant := 255;

   type String_Acc is access String;
   procedure Inner_Open_Inode
      (Data           : EXT_Data_Acc;
       Relative       : Unsigned_32;
       Path           : String;
       Last_Component : out String_Acc;
       Target_Index   : out Unsigned_32;
       Target_Inode   : out Inode;
       Parent_Index   : out Unsigned_32;
       Parent_Inode   : out Inode;
       Success        : out Boolean);

   procedure Inner_Read_Symbolic_Link
      (Ino       : Inode;
       File_Size : Unsigned_64;
       Path      : out String;
       Ret_Count : out Natural);

   procedure Inner_Read_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Sz    : Unsigned_64;
       File_Ino    : Inode;
       Inode_Index : Unsigned_64;
       Entity      : out Directory_Entity;
       Next_Index  : out Unsigned_64;
       Success     : out Boolean);

   procedure RW_Superblock
      (Handle          : Device_Handle;
       Offset          : Unsigned_64;
       Super           : in out Superblock;
       Write_Operation : Boolean;
       Success         : out Boolean);

   procedure RW_Block_Group_Descriptor
      (Data             : EXT_Data_Acc;
       Descriptor_Index : Unsigned_32;
       Result           : in out Block_Group_Descriptor;
       Write_Operation  : Boolean;
       Success          : out Boolean);

   procedure RW_Inode
      (Data            : EXT_Data_Acc;
       Inode_Index     : Unsigned_32;
       Result          : in out Inode;
       Write_Operation : Boolean;
       Success         : out Boolean);

   function Get_Block_Index
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : Inode;
       Searched    : Unsigned_32) return Unsigned_32;

   procedure Read_From_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : Inode;
       Inode_Size  : Unsigned_64;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean);

   procedure Write_To_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Inode_Size  : Unsigned_64;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean);

   procedure Grow_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start       : Unsigned_64;
       Count       : Unsigned_64;
       Success     : out Boolean);

   procedure Assign_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start_Blk   : Unsigned_32;
       Block_Count : Unsigned_32;
       Success     : out Boolean);

   procedure Wire_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Block_Index : Unsigned_32;
       Wired_Block : Unsigned_32;
       Success     : out Boolean);

   procedure Allocate_Block_For_Inode
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : in out Inode;
       Inode_Num  : Unsigned_32;
       Ret_Block  : out Unsigned_32;
       Success    : out Boolean);

   procedure Allocate_Inode
      (FS_Data   : EXT_Data_Acc;
       Inode_Num : out Unsigned_32;
       Success   : out Boolean);

   procedure Add_Directory_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Size  : Unsigned_64;
       Inode_Index : Unsigned_32;
       Added_Index : Unsigned_32;
       Dir_Type    : Unsigned_8;
       Name        : String;
       Success     : out Boolean);

   procedure Delete_Directory_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Size  : Unsigned_64;
       Inode_Index : Unsigned_32;
       Added_Index : Unsigned_32;
       Success     : out Boolean);

   function Get_Dir_Type (Dir_Type : Unsigned_8) return File_Type;

   function Get_Dir_Type (T : File_Type) return Unsigned_8;

   function Get_Permissions (T : File_Type) return Unsigned_16;

   function Get_Inode_Type (Perms : Unsigned_16) return File_Type;

   function Get_Inode_Type
      (T    : File_Type;
       Mode : Unsigned_32) return Unsigned_16;

   function Get_Size (Ino : Inode; Is_64_Bits : Boolean) return Unsigned_64;

   procedure Set_Size
      (Ino        : in out Inode;
       New_Size   : Unsigned_64;
       Is_64_Bits : Boolean;
       Success    : out Boolean);

   function Is_Immutable (Ino : Inode) return Boolean is
      ((Ino.Flags and Flags_Immutable) /= 0);

   function Is_Append_Only (Ino : Inode) return Boolean is
      ((Ino.Flags and Flags_Append_Only) /= 0);

   procedure Act_On_Policy (Data : EXT_Data_Acc; Message : String);

   function Check_User_Access
      (User        : Unsigned_32;
       Inod        : Inode;
       Check_Read  : Boolean;
       Check_Write : Boolean;
       Check_Exec  : Boolean) return Boolean
      with Pre => Check_Read or Check_Write;
end VFS.EXT;
