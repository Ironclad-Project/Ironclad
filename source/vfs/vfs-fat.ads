--  vfs-fat.adb: FAT FS driver.
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

package VFS.FAT is
   procedure Probe
      (Handle       : Device_Handle;
       Do_Read_Only : Boolean;
       Data_Addr    : out System.Address);


   procedure Remount
      (FS           : System.Address;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean);

   procedure Unmount (FS : in out System.Address);
   ----------------------------------------------------------------------------
   function Get_Block_Size (FS : System.Address) return Unsigned_64;

   function Get_Fragment_Size (FS : System.Address) return Unsigned_64;

   function Get_Size (FS : System.Address) return Unsigned_64;

   function Get_Inode_Count (FS : System.Address) return Unsigned_64;

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
   procedure Open
      (FS      : System.Address;
       Path    : String;
       Ino     : out File_Inode_Number;
       Success : out FS_Status);

   procedure Close (FS : System.Address; Ino : File_Inode_Number);

   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status);

   procedure Read
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status);

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status);

private

   Sector_Size    : constant := 512;
   Boot_Signature : constant := 16#AA55#;

   type FAT32_Bytes is array (Natural range <>) of Unsigned_8;
   type BIOS_Parameter_Block is record
      Infinite_Loop         : FAT32_Bytes (1 .. 3);
      OEM_Identifier        : Unsigned_64;
      Bytes_Per_Sector      : Unsigned_16;
      Sectors_Per_Cluster   : Unsigned_8;
      Reserved_Sectors      : Unsigned_16;
      FAT_Count             : Unsigned_8;
      Root_Entry_Count      : Unsigned_16;
      Sector_Count          : Unsigned_16; --  If 0, read Large.
      Media_Descriptor_Type : Unsigned_8;
      Sectors_Per_FAT_Old   : Unsigned_16; --  Only for FAT12-FAT16.
      Sectors_Per_Track     : Unsigned_16;
      Head_Count            : Unsigned_16;
      Hidden_Sector_Count   : Unsigned_32;
      Large_Sector_Count    : Unsigned_32;
      Sectors_Per_FAT       : Unsigned_32;
      Flags                 : Unsigned_16;
      FAT_Version_Number    : Unsigned_16;
      Root_Entry_Cluster    : Unsigned_32;
      FSInfo_Sector         : Unsigned_16;
      Backup_MBR_Sector     : Unsigned_16;
      Reserved              : FAT32_Bytes (1 .. 12);
      Drive_Number          : Unsigned_8;
      NT_FLags              : Unsigned_8;
      Signature             : Unsigned_8;
      Volume_ID_Serial      : Unsigned_32;
      Volume_Label          : String (1 .. 11);
      System_Identifier     : String (1 .. 8);
      Boot_Code             : FAT32_Bytes (1 .. 420);
      Boot_Signature        : Unsigned_16;
   end record with Size => 4096;
   for BIOS_Parameter_Block use record
      Infinite_Loop         at 0 range    0 ..   23;
      OEM_Identifier        at 0 range   24 ..   87;
      Bytes_Per_Sector      at 0 range   88 ..  103;
      Sectors_Per_Cluster   at 0 range  104 ..  111;
      Reserved_Sectors      at 0 range  112 ..  127;
      FAT_Count             at 0 range  128 ..  135;
      Root_Entry_Count      at 0 range  136 ..  151;
      Sector_Count          at 0 range  152 ..  167;
      Media_Descriptor_Type at 0 range  168 ..  175;
      Sectors_Per_FAT_Old   at 0 range  176 ..  191;
      Sectors_Per_Track     at 0 range  192 ..  207;
      Head_Count            at 0 range  208 ..  223;
      Hidden_Sector_Count   at 0 range  224 ..  255;
      Large_Sector_Count    at 0 range  256 ..  287;
      Sectors_Per_FAT       at 0 range  288 ..  319;
      Flags                 at 0 range  320 ..  335;
      FAT_Version_Number    at 0 range  336 ..  351;
      Root_Entry_Cluster    at 0 range  352 ..  383;
      FSInfo_Sector         at 0 range  384 ..  399;
      Backup_MBR_Sector     at 0 range  400 ..  415;
      Reserved              at 0 range  416 ..  511;
      Drive_Number          at 0 range  512 ..  519;
      NT_FLags              at 0 range  520 ..  527;
      Signature             at 0 range  528 ..  535;
      Volume_ID_Serial      at 0 range  536 ..  567;
      Volume_Label          at 0 range  568 ..  655;
      System_Identifier     at 0 range  656 ..  719;
      Boot_Code             at 0 range  720 .. 4079;
      Boot_Signature        at 0 range 4080 .. 4095;
   end record;

   Directory_Read_Only : constant := 2#000001#;
   Directory_Hidden    : constant := 2#000010#;
   Directory_System    : constant := 2#000100#;
   Directory_Volume_ID : constant := 2#001000#;
   Directory_Directory : constant := 2#010000#;
   Directory_Archive   : constant := 2#100000#;
   Directory_LFN       : constant := 2#001111#;
   type Directory_Entry is record
      File_Name          : String (1 .. 8);
      File_Extension     : String (1 .. 3);
      Attributes         : Unsigned_8;
      Reserved_NT        : Unsigned_8;
      Creation_Time_1    : Unsigned_8;
      Creation_Time_2    : Unsigned_16;
      Creation_Time_3    : Unsigned_16;
      Access_Time        : Unsigned_16;
      First_Cluster_High : Unsigned_16;
      Modification_Time  : Unsigned_16;
      Modification_Date  : Unsigned_16;
      First_Cluster_Low  : Unsigned_16;
      Size               : Unsigned_32;
   end record with Size => 256;
   for Directory_Entry use record
      File_Name          at 0 range   0 ..  63;
      File_Extension     at 0 range  64 ..  87;
      Attributes         at 0 range  88 ..  95;
      Reserved_NT        at 0 range  96 .. 103;
      Creation_Time_1    at 0 range 104 .. 111;
      Creation_Time_2    at 0 range 112 .. 127;
      Creation_Time_3    at 0 range 128 .. 143;
      Access_Time        at 0 range 144 .. 159;
      First_Cluster_High at 0 range 160 .. 175;
      Modification_Time  at 0 range 176 .. 191;
      Modification_Date  at 0 range 192 .. 207;
      First_Cluster_Low  at 0 range 208 .. 223;
      Size               at 0 range 224 .. 255;
   end record;

   type FAT_Data is record
      Handle         : Device_Handle;
      Is_Read_Only   : Boolean;
      BPB            : BIOS_Parameter_Block;
      Sector_Count   : Unsigned_32;
      FAT_Offset     : Unsigned_32;
      Cluster_Offset : Unsigned_32;
   end record;
   type FAT_Data_Acc is access all FAT_Data;

   type FAT_File is record
      Begin_Cluster : Unsigned_32;
      Inner_Type    : File_Type;
      FS_Entry      : Directory_Entry;
   end record;
   type FAT_File_Acc is access all FAT_File;

   procedure Read_Directory_Entry
      (Data    : FAT_Data_Acc;
       Cluster : Unsigned_32;
       Index   : Unsigned_64;
       Result  : out Directory_Entry;
       Success : out Boolean);

   procedure Get_Next_Cluster
      (Data          : FAT_Data_Acc;
       Cluster_Index : Unsigned_32;
       Returned      : out Unsigned_32;
       Success       : out Boolean);

   function Are_Paths_Equal
      (Base : String;
       Ent  : Directory_Entry) return Boolean;

   procedure Compose_Path
      (Ent    : Directory_Entry;
       Result : out String;
       Length : out Natural);

   function Sector_To_Disk_Offset (Sector : Unsigned_32) return Unsigned_64
      is (Unsigned_64 (Sector) * Sector_Size);

   function Cluster_To_Disk_Offset
      (Cluster             : Unsigned_32;
       Cluster_Begin       : Unsigned_32;
       Sectors_Per_Cluster : Unsigned_32) return Unsigned_64
   is (Sector_To_Disk_Offset (Cluster_Begin + (Cluster - 2) *
                              Sectors_Per_Cluster));

   function Get_Type (Attributes : Unsigned_8) return File_Type is
      (if (Attributes and Directory_Directory) /= 0 then File_Directory
       else File_Regular);
end VFS.FAT;
