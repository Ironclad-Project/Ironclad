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

with Arch.Clocks;

package body VFS.Dev is
   --  All devices share the same permissions.
   --  The only folder of the filesystem is the root, which has inode 0.
   Root_Inode         : constant := 0;
   Device_Permissions : constant := 8#666#;

   --  We can save ourselves a bunch of code by just using globals instead of
   --  mount specific data knowing there will only be 1 /dev mount point ever.
   Birth_Seconds     : Unsigned_64;
   Birth_Nanoseconds : Unsigned_64;

   procedure Probe
      (Handle        : Device_Handle;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Data_Addr     : out System.Address;
       Root_Ino      : out File_Inode_Number)
   is
      pragma Unreferenced (Handle);
      pragma Unreferenced (Do_Read_Only);
      pragma Unreferenced (Access_Policy);
   begin
      Arch.Clocks.Get_Real_Time (Birth_Seconds, Birth_Nanoseconds);
      Data_Addr := System'To_Address (1);
      Root_Ino  := Root_Inode;
   end Probe;

   procedure Remount
      (FS            : System.Address;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Success       : out Boolean)
   is
      pragma Unreferenced (FS);
      pragma Unreferenced (Do_Read_Only);
      pragma Unreferenced (Access_Policy);
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
   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      pragma Unreferenced (FS_Data);

      Buffer     : Devices.Device_List (1 .. 30);
      Buffer_Len : Natural;
   begin
      Ret_Count := 0;
      Entities  :=
         [others =>
            (Inode_Number => 0,
             Name_Buffer  => [others => ' '],
             Name_Len     => 0,
             Type_Of_File => File_Regular)];

      if Ino /= Root_Inode then
         Success := FS_Invalid_Value;
         return;
      end if;

      Devices.List (Buffer, Buffer_Len);
      for I in 1 .. Buffer_Len loop
         pragma Loop_Invariant
            (Unsigned_64 (Ret_Count) <= Unsigned_64 (Entities'Length));

         if I - 1 >= Offset then
            if not (I in Buffer'Range)                      or else
               Entities'Length > Unsigned_64 (Natural'Last) or else
               Ret_Count >= Entities'Length
            then
               exit;
            end if;

            Entities (Entities'First + Ret_Count) :=
               (Inode_Number =>
                  Unsigned_64 (Devices.Get_Unique_ID (Buffer (I))),
                Name_Buffer  => (others => ' '),
                Name_Len     => 0,
                Type_Of_File => File_Character_Device);
            if Devices.Is_Block_Device (Buffer (I)) then
               Entities (Entities'First + Ret_Count).Type_Of_File :=
                  File_Block_Device;
            end if;
            Devices.Fetch_Name
               (Buffer (I),
                Entities (Entities'First + Ret_Count).Name_Buffer,
                Entities (Entities'First + Ret_Count).Name_Len);
            Ret_Count := Ret_Count + 1;
         end if;
      end loop;

      Success := FS_Success;
   end Read_Entries;

   procedure Read
      (FS_Data     : System.Address;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
   is
      pragma Unreferenced (FS_Data);

      Handle : Device_Handle;
      Succ   : Boolean;
   begin
      if Ino = Root_Inode or else
         not (Ino in 0 .. File_Inode_Number (Natural'Last))
      then
         Data      := [others => 0];
         Ret_Count := 0;
         Success   := FS_Invalid_Value;
      else
         Handle := From_Unique_ID (Natural (Ino));
         if Handle = Devices.Error_Handle then
            Data      := [others => 0];
            Ret_Count := 0;
            Success   := FS_Invalid_Value;
            return;
         end if;

         Read
            (Handle      => Handle,
             Offset      => Offset,
             Data        => Data,
             Ret_Count   => Ret_Count,
             Success     => Succ,
             Is_Blocking => Is_Blocking);
         Success := (if Succ then FS_Success else FS_IO_Failure);
      end if;
   end Read;

   procedure Write
      (FS_Data     : System.Address;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
   is
      pragma Unreferenced (FS_Data);

      Handle : Device_Handle;
      Succ   : Boolean;
   begin
      if Ino = Root_Inode or else
         not (Ino in 0 .. File_Inode_Number (Natural'Last))
      then
         Ret_Count := 0;
         Success   := FS_Invalid_Value;
      else
         Handle := From_Unique_ID (Natural (Ino));
         if Handle = Devices.Error_Handle then
            Ret_Count := 0;
            Success   := FS_Invalid_Value;
            return;
         end if;

         Write
            (Handle      => Handle,
             Offset      => Offset,
             Data        => Data,
             Ret_Count   => Ret_Count,
             Success     => Succ,
             Is_Blocking => Is_Blocking);
         Success := (if Succ then FS_Success else FS_IO_Failure);
      end if;
   end Write;

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status)
   is
      pragma Unreferenced (Data);

      Error_Stat : constant File_Stat :=
         (Unique_Identifier => 0,
          Type_Of_File      => File_Directory,
          Mode              => 0,
          UID               => 0,
          GID               => 0,
          Hard_Link_Count   => 1,
          Byte_Size         => 0,
          IO_Block_Size     => 0,
          IO_Block_Count    => 0,
          Change_Time       => (0, 0),
          Modification_Time => (0, 0),
          Access_Time       => (0, 0),
          Birth_Time        => (0, 0));
      Dev : Device_Handle;
   begin
      if Ino = Root_Inode then
         S :=
            (Unique_Identifier => Root_Inode,
             Type_Of_File      => File_Directory,
             Mode              => Device_Permissions,
             UID               => 0,
             GID               => 0,
             Hard_Link_Count   => 1,
             Byte_Size         => 0,
             IO_Block_Size     => 0,
             IO_Block_Count    => 0,
             Change_Time       => (Birth_Seconds, Birth_Nanoseconds),
             Modification_Time => (Birth_Seconds, Birth_Nanoseconds),
             Access_Time       => (Birth_Seconds, Birth_Nanoseconds),
             Birth_Time        => (Birth_Seconds, Birth_Nanoseconds));
         Success := FS_Success;
      elsif not (Ino in 0 .. File_Inode_Number (Natural'Last)) then
         S       := Error_Stat;
         Success := FS_Invalid_Value;
      else
         Dev := From_Unique_ID (Natural (Ino));
         if Dev = Devices.Error_Handle then
            S       := Error_Stat;
            Success := FS_Invalid_Value;
            return;
         end if;

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
             Change_Time       => (Birth_Seconds, Birth_Nanoseconds),
             Modification_Time => (Birth_Seconds, Birth_Nanoseconds),
             Access_Time       => (Birth_Seconds, Birth_Nanoseconds),
             Birth_Time        => (Birth_Seconds, Birth_Nanoseconds));
         S.Byte_Size := Unsigned_64 (S.IO_Block_Size) * S.IO_Block_Count;
         if Devices.Is_Block_Device (Dev) then
            S.Type_Of_File := File_Block_Device;
         end if;
         Success := FS_Success;
      end if;
   end Stat;

   procedure IO_Control
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Req       : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Status    : out FS_Status)
   is
      pragma Unreferenced (Data);

      DEV_UUID : constant := 16#9821#;
      Success  : Boolean;
   begin
      Has_Extra := False;
      Extra     := 0;

      if Ino = Root_Inode or else
         not (Ino in 0 .. File_Inode_Number (Natural'Last))
      then
         Status := FS_Invalid_Value;
      else
         declare
            Arg_UUID : Devices.UUID with Import, Address => Arg;
            Handle : constant Device_Handle := From_Unique_ID (Natural (Ino));
         begin
            if Handle = Devices.Error_Handle then
               Status := FS_Invalid_Value;
               return;
            end if;

            if Req = DEV_UUID then
               Arg_UUID := Fetch (Handle);
               Status   := FS_Success;
            else
               IO_Control (Handle, Req, Arg, Has_Extra, Extra, Success);
               if Success then
                  Status := FS_Success;
               else
                  Status := FS_IO_Failure;
               end if;
            end if;
         end;
      end if;
   end IO_Control;

   procedure Mmap
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       Map     : Arch.MMU.Page_Table_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions;
       Status  : out FS_Status)
   is
      pragma Unreferenced (Data);

      Handle : Device_Handle;
   begin
      if Ino = Root_Inode or else
         not (Ino in 0 .. File_Inode_Number (Natural'Last))
      then
         Status := FS_Invalid_Value;
         return;
      end if;

      Handle := From_Unique_ID (Natural (Ino));
      if Handle /= Devices.Error_Handle then
         if Devices.Mmap (Handle, Map, Address, Length, Flags) then
            Status := FS_Success;
         else
            Status := FS_IO_Failure;
         end if;
      else
         Status := FS_Invalid_Value;
      end if;
   end Mmap;

   procedure Poll
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Data);

      Handle : Device_Handle;
   begin
      if Ino = Root_Inode then
         Can_Read  := True;
         Can_Write := True;
         Is_Error  := False;
         return;
      elsif not (Ino in 0 .. File_Inode_Number (Natural'Last)) then
         goto Error_Return;
      end if;

      Handle := From_Unique_ID (Natural (Ino));
      if Handle /= Devices.Error_Handle then
         Devices.Poll (Handle, Can_Read, Can_Write, Is_Error);
         return;
      end if;

   <<Error_Return>>
      Can_Read  := False;
      Can_Write := False;
      Is_Error  := True;
   end Poll;

   function Synchronize (Data : System.Address) return FS_Status is
      pragma Unreferenced (Data);

      Buffer     : Devices.Device_List (1 .. 30);
      Buffer_Len : Natural;
   begin
      Devices.List (Buffer, Buffer_Len);
      for I in 1 .. Buffer_Len loop
         exit when I > Buffer'Last;
         if not Devices.Synchronize (Buffer (I)) then
            return FS_IO_Failure;
         end if;
      end loop;
      return FS_Success;
   end Synchronize;

   function Synchronize
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Data_Only);

      Handle : Device_Handle;
   begin
      if Ino = Root_Inode then
         return FS_Success;
      elsif not (Ino in 0 .. File_Inode_Number (Natural'Last)) then
         return FS_Invalid_Value;
      end if;

      Handle := From_Unique_ID (Natural (Ino));
      if Handle /= Devices.Error_Handle then
         if Devices.Synchronize (Handle) then
            return FS_Success;
         else
            return FS_IO_Failure;
         end if;
      else
         return FS_Invalid_Value;
      end if;
   end Synchronize;
end VFS.Dev;
