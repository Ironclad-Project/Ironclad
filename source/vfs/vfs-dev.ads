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
with Devices;

package VFS.Dev is
   procedure Probe
      (Handle        : Device_Handle;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Data_Addr     : out System.Address;
       Root_Ino      : out File_Inode_Number)
      with Pre => Devices.Is_Initialized;

   procedure Remount
      (FS            : System.Address;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Success       : out Boolean)
      with Pre => Devices.Is_Initialized;

   pragma Warnings (GNATprove, Off, "unused initial value");
   procedure Unmount (FS : in out System.Address)
      with Pre => Devices.Is_Initialized;
   ----------------------------------------------------------------------------
   procedure Get_Block_Size (FS : System.Address; Size : out Unsigned_64);

   procedure Get_Fragment_Size (FS : System.Address; Size : out Unsigned_64);

   procedure Get_Size (FS : System.Address; Size : out Unsigned_64);

   procedure Get_Inode_Count (FS : System.Address; Count : out Unsigned_64);

   procedure Get_Free_Blocks
      (FS                 : System.Address;
       Free_Blocks        : out Unsigned_64;
       Free_Unprivileged : out Unsigned_64);

   procedure Get_Free_Inodes
      (FS                 : System.Address;
       Free_Inodes        : out Unsigned_64;
       Free_Unprivileged : out Unsigned_64);

   function Get_Max_Length (FS : System.Address) return Unsigned_64;
   ----------------------------------------------------------------------------
   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
      with Pre  => Devices.Is_Initialized,
           Post => Unsigned_64 (Ret_Count) <= Unsigned_64 (Entities'Length);

   procedure Read
      (FS_Data     : System.Address;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
      with Pre => Devices.Is_Initialized;

   procedure Write
      (FS_Data     : System.Address;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
      with Pre => Devices.Is_Initialized;

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status)
      with Pre => Devices.Is_Initialized;

   procedure IO_Control
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Req       : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Status    : out FS_Status)
      with Pre => Devices.Is_Initialized;

   procedure Mmap
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       Map     : Memory.MMU.Page_Table_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions;
       Status  : out FS_Status)
      with Pre => Devices.Is_Initialized;

   procedure Poll
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
      with Pre => Devices.Is_Initialized;

   function Synchronize (Data : System.Address) return FS_Status
      with Pre => Devices.Is_Initialized;

   function Synchronize
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status
      with Pre => Devices.Is_Initialized;

   function Dev_To_FS_Status (S : Devices.Dev_Status) return FS_Status;
end VFS.Dev;
