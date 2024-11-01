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

with Lib.Messages;
with Lib.Panic;
with Lib.Alignment;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;

package body VFS.EXT is
   package   Conv is new System.Address_To_Access_Conversions (EXT_Data);
   procedure Free is new Ada.Unchecked_Deallocation (String,   String_Acc);
   procedure Free is new Ada.Unchecked_Deallocation (EXT_Data, EXT_Data_Acc);
   procedure Free is new Ada.Unchecked_Deallocation (Inode,    Inode_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
      (Operation_Data, Operation_Data_Acc);

   procedure Probe
      (Handle       : Device_Handle;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Data_Addr    : out System.Address)
   is
      Sup     : Superblock;
      Data    : EXT_Data_Acc;
      Success : Boolean;
      Is_RO   : Boolean;
   begin
      RW_Superblock
         (Handle          => Handle,
          Offset          => Main_Superblock_Offset,
          Super           => Sup,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Data_Addr := Null_Address;
         return;
      end if;

      --  Check we support everything ext needs us to.
      if Sup.Signature /= EXT_Signature                           or else
         Sup.Block_Size_Log < Sup.Fragment_Size_Log               or else
         Sup.Major_Version > 1                                    or else
         (Sup.Required_Features and Required_Compression)    /= 0 or else
         (Sup.Required_Features and Required_Journal_Replay) /= 0 or else
         (Sup.Required_Features and Required_Journal_Device) /= 0
      then
         Data_Addr := Null_Address;
         return;
      end if;

      --  Check under which conditions we can only do read-only.
      Is_RO :=
         Do_Read_Only                                        or
         Devices.Is_Read_Only (Handle)                       or
         Sup.Filesystem_State /= State_Clean                 or
         Sup.Mounts_Since_Check > Sup.Max_Mounts_Since_Check or
         (Sup.RO_If_Not_Features and RO_Binary_Trees) /= 0;
      if Is_RO then
         Lib.Messages.Put_Line ("ext will be mounted RO, consider an fsck");
      end if;

      --  Commit to mounting.
      Data := new EXT_Data'
         (Mutex         => Lib.Synchronization.Unlocked_Semaphore,
          Handle        => Handle,
          Super         => Sup,
          Is_Read_Only  => Is_RO,
          Do_Relatime   => Do_Relatime,
          Block_Size    => Shift_Left (1024, Natural (Sup.Block_Size_Log)),
          Fragment_Size => Shift_Left (1024, Natural (Sup.Fragment_Size_Log)),
          Root          => <>,
          Has_Sparse_Superblock =>
            (Sup.RO_If_Not_Features and RO_Sparse_Superblocks) /= 0,
          Has_64bit_Filesizes =>
            (Sup.RO_If_Not_Features and RO_64bit_Filesize) /= 0);

      --  Read the root inode.
      RW_Inode
         (Data            => Data,
          Inode_Index     => 2,
          Result          => Data.Root,
          Write_Operation => False,
          Success         => Success);
      Data_Addr := Conv.To_Address (Conv.Object_Pointer (Data));
   end Probe;

   procedure Remount
      (FS           : System.Address;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Data.Is_Read_Only := Do_Read_Only;
      Data.Do_Relatime  := Do_Relatime;
      Lib.Synchronization.Release (Data.Mutex);
      Success := True;
   end Remount;

   procedure Unmount (FS : in out System.Address) is
      Data    : EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Success : Boolean;
   begin
      Lib.Synchronization.Seize (Data.Mutex);

      if not Data.Is_Read_Only and
         Data.Super.Mounts_Since_Check /= Unsigned_16'Last
      then
         Data.Super.Mounts_Since_Check := Data.Super.Mounts_Since_Check + 1;
         RW_Superblock
            (Handle          => Data.Handle,
             Offset          => Main_Superblock_Offset,
             Super           => Data.Super,
             Write_Operation => True,
             Success         => Success);
         if not Success then
            Act_On_Policy (Data, "superblock write error");
         end if;
      end if;

      Free (Data);
      FS := System.Null_Address;
   end Unmount;
   ----------------------------------------------------------------------------
   procedure Get_Block_Size (FS : System.Address; Size : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Size := Unsigned_64 (Data.Block_Size);
      Lib.Synchronization.Release (Data.Mutex);
   end Get_Block_Size;

   procedure Get_Fragment_Size (FS : System.Address; Size : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Size := Unsigned_64 (Data.Fragment_Size);
      Lib.Synchronization.Release (Data.Mutex);
   end Get_Fragment_Size;

   procedure Get_Size (FS : System.Address; Size : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Size := Unsigned_64 (Data.Super.Block_Count *
         Data.Block_Size / Data.Fragment_Size);
      Lib.Synchronization.Release (Data.Mutex);
   end Get_Size;

   procedure Get_Inode_Count (FS : System.Address; Count : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Count := Unsigned_64 (Data.Super.Inode_Count);
      Lib.Synchronization.Release (Data.Mutex);
   end Get_Inode_Count;

   procedure Get_Free_Blocks
      (FS                 : System.Address;
       Free_Blocks        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Free_Blocks := Unsigned_64 (Data.Super.Unallocated_Block_Count);
      Free_Unpriviledged := Unsigned_64 (Data.Super.Unallocated_Block_Count);
      Lib.Synchronization.Release (Data.Mutex);
   end Get_Free_Blocks;

   procedure Get_Free_Inodes
      (FS                 : System.Address;
       Free_Inodes        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
   is
      Data  : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Free_Inodes := Unsigned_64 (Data.Super.Unallocated_Inode_Count);
      Free_Unpriviledged := Unsigned_64 (Data.Super.Unallocated_Inode_Count);
      Lib.Synchronization.Release (Data.Mutex);
   end Get_Free_Inodes;

   function Get_Max_Length (FS : System.Address) return Unsigned_64 is
      pragma Unreferenced (FS);
   begin
      return Max_File_Name_Size;
   end Get_Max_Length;
   ----------------------------------------------------------------------------
   procedure Create_Node
      (FS       : System.Address;
       Relative : File_Inode_Number;
       Path     : String;
       Typ      : File_Type;
       Mode     : File_Mode;
       User     : Unsigned_32;
       Status   : out FS_Status)
   is
      Data     : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Perms    : constant  Unsigned_16 := Get_Permissions (Typ);
      Dir_Type : constant   Unsigned_8 := Get_Dir_Type (Typ);
      Has_64bit_Sz : Boolean renames Data.Has_64bit_Filesizes;
      Last_Component             : String_Acc;
      Target_Index, Parent_Index : Unsigned_32;
      Target_Inode, Parent_Inode : Inode_Acc := new Inode;
      Descriptor                 : Block_Group_Descriptor;
      Desc_Index                 : Unsigned_32;
      Ent                        : Directory_Entry;
      Name                       : String (1 .. 2);
      Temp                       : Natural;
      Success                    : Boolean;
      Ent_Data  : Operation_Data (1 .. Ent'Size / 8)
         with Import, Address => Ent'Address;
      Name_Data : Operation_Data (1 .. 2)
         with Import, Address => Name'Address;
   begin
      Lib.Synchronization.Seize (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Path'Length = 0 then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      --  Checking the file doesn't exist but the parent is found along perms.
      Inner_Open_Inode
         (Data           => Data,
          Relative       => Unsigned_32 (Relative),
          Path           => Path,
          Last_Component => Last_Component,
          Target_Index   => Target_Index,
          Target_Inode   => Target_Inode.all,
          Parent_Index   => Parent_Index,
          Parent_Inode   => Parent_Inode.all,
          Success        => Success);
      if Success or else Parent_Index = 0 or else Target_Index /= 0 then
         Status := FS_Invalid_Value;
         goto Cleanup;
      elsif not Check_User_Access (User, Parent_Inode.all, False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      end if;

      Allocate_Inode
         (FS_Data   => Data,
          Inode_Num => Target_Index,
          Success   => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      Target_Inode.all :=
         (Permissions         => Perms or Unsigned_16 (Mode),
          UID                 => Unsigned_16 (User),
          Size_Low            => 0,
          Access_Time_Epoch   => 0,
          Creation_Time_Epoch => 0,
          Modified_Time_Epoch => 0,
          Deleted_Time_Epoch  => 0,
          GID                 => 0,
          Hard_Link_Count     => 1,
          Sectors             => 0,
          Flags               => 0,
          OS_Specific_Value_1 => 0,
          Blocks              => (others => 0),
          Generation_Number   => 0,
          EAB                 => 0,
          Size_High           => 0,
          Fragment_Address    => 0,
          OS_Specific_Value_2 => (others => 0));

      Add_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Parent_Inode.all,
          Inode_Size  => Get_Size (Parent_Inode.all, Has_64bit_Sz),
          Inode_Index => Parent_Index,
          Added_Index => Target_Index,
          Dir_Type    => Dir_Type,
          Name        => Last_Component.all,
          Success     => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      if Typ = File_Directory then
         Ent := (Target_Index, 12, 1, Dir_Type);
         Write_To_Inode
            (FS_Data     => Data,
             Inode_Data  => Target_Inode.all,
             Inode_Num   => Target_Index,
             Inode_Size  => Get_Size (Target_Inode.all, Has_64bit_Sz),
             Offset      => 0,
             Data        => Ent_Data,
             Ret_Count   => Temp,
             Success     => Success);
         Name := ('.', Ada.Characters.Latin_1.NUL);
         Write_To_Inode
            (FS_Data     => Data,
             Inode_Data  => Target_Inode.all,
             Inode_Num   => Target_Index,
             Inode_Size  => Get_Size (Target_Inode.all, Has_64bit_Sz),
             Offset      => 8,
             Data        => Name_Data,
             Ret_Count   => Temp,
             Success     => Success);

         Ent :=
            (Inode_Index => Parent_Index,
             Entry_Count => Unsigned_16 (Data.Block_Size) - 12,
             Name_Length => 2,
             Dir_Type    => Dir_Type);
         Write_To_Inode
            (FS_Data     => Data,
             Inode_Data  => Target_Inode.all,
             Inode_Num   => Target_Index,
             Inode_Size  => Get_Size (Target_Inode.all, Has_64bit_Sz),
             Offset      => 12,
             Data        => Ent_Data,
             Ret_Count   => Temp,
             Success     => Success);
         Name := "..";
         Write_To_Inode
            (FS_Data     => Data,
             Inode_Data  => Target_Inode.all,
             Inode_Num   => Target_Index,
             Inode_Size  => Get_Size (Target_Inode.all, Has_64bit_Sz),
             Offset      => 12 + (Ent'Size / 8),
             Data        => Name_Data,
             Ret_Count   => Temp,
             Success     => Success);

         Desc_Index := Target_Index - 1 / Data.Super.Inodes_Per_Group;
         RW_Block_Group_Descriptor
            (Data             => Data,
             Descriptor_Index => Desc_Index,
             Result           => Descriptor,
             Write_Operation  => False,
             Success          => Success);
         Descriptor.Directory_Count := Descriptor.Directory_Count + 1;
         RW_Block_Group_Descriptor
            (Data             => Data,
             Descriptor_Index => Desc_Index,
             Result           => Descriptor,
             Write_Operation  => True,
             Success          => Success);
         Parent_Inode.Hard_Link_Count := Parent_Inode.Hard_Link_Count + 1;
         Target_Inode.Hard_Link_Count := Target_Inode.Hard_Link_Count + 1;
      end if;

      RW_Inode
         (Data            => Data,
          Inode_Index     => Target_Index,
          Result          => Target_Inode.all,
          Write_Operation => True,
          Success         => Success);
      RW_Inode
         (Data            => Data,
          Inode_Index     => Parent_Index,
          Result          => Parent_Inode.all,
          Write_Operation => True,
          Success         => Success);

      Status := (if Success then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Lib.Synchronization.Release (Data.Mutex);
      Free (Target_Inode);
      Free (Parent_Inode);
      Free (Last_Component);
   end Create_Node;

   procedure Create_Symbolic_Link
      (FS       : System.Address;
       Relative : File_Inode_Number;
       Path     : String;
       Target   : String;
       Mode     : Unsigned_32;
       User     : Unsigned_32;
       Status   : out FS_Status)
   is
      pragma Unreferenced (Mode);
      pragma Unreferenced (Relative);
      pragma Unreferenced (User);
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
      elsif Path'Length = 0 or Target'Length = 0 then
         Status := FS_Invalid_Value;
      else
         Status := FS_Not_Supported;
      end if;
      Lib.Synchronization.Release (Data.Mutex);
   end Create_Symbolic_Link;

   procedure Create_Hard_Link
      (FS              : System.Address;
       Relative_Path   : File_Inode_Number;
       Path            : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       User            : Unsigned_32;
       Status          : out FS_Status)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Last_Component                         : String_Acc;
      Path_Index, Target_Index, Parent_Index : Unsigned_32;
      Path_Inode, Target_Inode, Parent_Inode : Inode_Acc := new Inode;
      Success                                : Boolean;
   begin
      Lib.Synchronization.Seize (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Path'Length = 0 or Target'Length = 0 then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      --  Open the source.
      Inner_Open_Inode
         (Data           => Data,
          Relative       => Unsigned_32 (Relative_Path),
          Path           => Path,
          Last_Component => Last_Component,
          Target_Index   => Path_Index,
          Target_Inode   => Path_Inode.all,
          Parent_Index   => Parent_Index,
          Parent_Inode   => Parent_Inode.all,
          Success        => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;
      Free (Last_Component);

      --  Checking the target file doesn't exist but the parent is found.
      --  Also check some permissions.
      Inner_Open_Inode
         (Data           => Data,
          Relative       => Unsigned_32 (Relative_Target),
          Path           => Target,
          Last_Component => Last_Component,
          Target_Index   => Target_Index,
          Target_Inode   => Target_Inode.all,
          Parent_Index   => Parent_Index,
          Parent_Inode   => Parent_Inode.all,
          Success        => Success);
      if Success or else Parent_Index = 0 or else Target_Index /= 0 then
         Status := FS_Invalid_Value;
         goto Cleanup;
      elsif not Check_User_Access (User, Parent_Inode.all, False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      end if;

      --  Once we can commit to it, update the hard link count.
      RW_Inode
         (Data            => Data,
          Inode_Index     => Path_Index,
          Result          => Path_Inode.all,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;
      Path_Inode.Hard_Link_Count := Path_Inode.Hard_Link_Count + 1;
      RW_Inode
         (Data            => Data,
          Inode_Index     => Path_Index,
          Result          => Path_Inode.all,
          Write_Operation => True,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      Add_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Parent_Inode.all,
          Inode_Size  => Get_Size (Parent_Inode.all, Data.Has_64bit_Filesizes),
          Inode_Index => Parent_Index,
          Added_Index => Path_Index,
          Dir_Type    => Get_Dir_Type (File_Regular),
          Name        => Last_Component.all,
          Success     => Success);

      Status := (if Success then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Lib.Synchronization.Release (Data.Mutex);
      Free (Path_Inode);
      Free (Target_Inode);
      Free (Parent_Inode);
      Free (Last_Component);
   end Create_Hard_Link;

   procedure Rename
      (FS              : System.Address;
       Relative_Source : File_Inode_Number;
       Source          : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       Keep            : Boolean;
       User            : Unsigned_32;
       Status          : out FS_Status)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Last_Component                           : String_Acc;
      Source_Index, Target_Index               : Unsigned_32;
      Source_Parent_Index, Target_Parent_Index : Unsigned_32;
      Source_Inode, Target_Inode               : Inode_Acc := new Inode;
      Source_Parent_Inode, Target_Parent_Inode : Inode_Acc := new Inode;
      Target_Parent_Size                       : Unsigned_64;
      Success1, Success2                       : Boolean;
   begin
      Lib.Synchronization.Seize (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Source'Length = 0 or Target'Length = 0 then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      Inner_Open_Inode
         (Data         => Data,
          Relative     => Unsigned_32 (Relative_Source),
          Path         => Source,
          Last_Component   => Last_Component,
          Target_Index => Source_Index,
          Target_Inode => Source_Inode.all,
          Parent_Index => Source_Parent_Index,
          Parent_Inode => Source_Parent_Inode.all,
          Success      => Success1);
      Free (Last_Component);
      Inner_Open_Inode
         (Data         => Data,
          Relative     => Unsigned_32 (Relative_Target),
          Path         => Target,
          Last_Component   => Last_Component,
          Target_Index => Target_Index,
          Target_Inode => Target_Inode.all,
          Parent_Index => Target_Parent_Index,
          Parent_Inode => Target_Parent_Inode.all,
          Success      => Success2);

      --  Check that the source exists, that the parent of the target exists,
      --  and that we do not want to keep the file if it exists, along with
      --  permissions.
      if not Success1 or Target_Parent_Index = 0 or (Keep and Success2) then
         Status := FS_Invalid_Value;
         goto Cleanup;
      elsif not Check_User_Access (User, Target_Parent_Inode.all,
                                   False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      end if;

      Target_Parent_Size := Get_Size (Target_Parent_Inode.all,
                                      Data.Has_64bit_Filesizes);

      --  The target already exists so we nuke it from orbit.
      if Success2 then
         Delete_Directory_Entry
            (FS_Data     => Data,
             Inode_Data  => Target_Parent_Inode.all,
             Inode_Size  => Target_Parent_Size,
             Inode_Index => Target_Parent_Index,
             Added_Index => Target_Index,
             Success     => Success1);
         if not Success1 then
            Status := FS_IO_Failure;
            goto Cleanup;
         end if;
      end if;

      --  Add the directory entry on its place.
      Add_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Target_Parent_Inode.all,
          Inode_Size  => Target_Parent_Size,
          Inode_Index => Target_Parent_Index,
          Added_Index => Target_Index,
          Dir_Type    => Get_Dir_Type (File_Regular),
          Name        => Last_Component.all,
          Success     => Success1);

      Status := (if Success1 then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Lib.Synchronization.Release (Data.Mutex);
      Free (Source_Inode);
      Free (Target_Inode);
      Free (Source_Parent_Inode);
      Free (Target_Parent_Inode);
      Free (Last_Component);
   end Rename;

   procedure Unlink
      (FS       : System.Address;
       Relative : File_Inode_Number;
       Path     : String;
       User     : Unsigned_32;
       Status   : out FS_Status)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Last_Component           : String_Acc;
      Path_Index, Parent_Index : Unsigned_32;
      Path_Inode, Parent_Inode : Inode_Acc := new Inode;
      Success                  : Boolean;
   begin
      Lib.Synchronization.Seize (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Path'Length = 0 then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      Inner_Open_Inode
         (Data         => Data,
          Relative     => Unsigned_32 (Relative),
          Path         => Path,
          Last_Component   => Last_Component,
          Target_Index => Path_Index,
          Target_Inode => Path_Inode.all,
          Parent_Index => Parent_Index,
          Parent_Inode => Parent_Inode.all,
          Success      => Success);
      if not Success then
         Status := FS_Invalid_Value;
         goto Cleanup;
      elsif not Check_User_Access (User, Parent_Inode.all, False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      end if;

      Delete_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Parent_Inode.all,
          Inode_Size  => Get_Size (Parent_Inode.all, Data.Has_64bit_Filesizes),
          Inode_Index => Parent_Index,
          Added_Index => Path_Index,
          Success     => Success);

      Status := (if Success then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Lib.Synchronization.Release (Data.Mutex);
      Free (Path_Inode);
      Free (Parent_Inode);
      Free (Last_Component);
   end Unlink;

   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS_Data));
      Fetched_Inode : Inode_Acc := new Inode;
      Curr_Index    : Unsigned_64;
      Next_Index    : Unsigned_64;
      Entry_Count   : Unsigned_64;
      Entity        : Directory_Entity;
      Succ          : Boolean;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      Ret_Count := 0;
      Success   := FS_Success;
      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Fetched_Inode.all,
          Write_Operation => False,
          Success         => Succ);
      if not Succ then
         Success := FS_IO_Failure;
         goto Cleanup;
      elsif Get_Inode_Type (Fetched_Inode.all.Permissions) /= File_Directory
      then
         Success := FS_Invalid_Value;
         goto Cleanup;
      end if;

      Curr_Index  := 0;
      Next_Index  := 0;
      Entry_Count := 0;
      loop
         Inner_Read_Entry
            (FS_Data     => FS,
             Inode_Sz  => Get_Size (Fetched_Inode.all, FS.Has_64bit_Filesizes),
             File_Ino    => Fetched_Inode.all,
             Inode_Index => Curr_Index,
             Entity      => Entity,
             Next_Index  => Next_Index,
             Success     => Succ);
         if not Succ then
            exit;
         end if;

         Curr_Index := Next_Index;
         if Entry_Count >= Unsigned_64 (Offset) then
            if Ret_Count < Entities'Length then
               Entities (Entities'First + Ret_Count) := Entity;
            else
               exit;
            end if;
            Ret_Count := Ret_Count + 1;
         end if;
         Entry_Count := Entry_Count + 1;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Fetched_Inode);
   end Read_Entries;

   procedure Read_Symbolic_Link
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Path      : out String;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS_Data));
      Fetched_Inode : Inode_Acc := new Inode;
      Succ          : Boolean;
   begin
      Lib.Synchronization.Seize (FS.Mutex);
      Ret_Count := 0;
      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Fetched_Inode.all,
          Write_Operation => False,
          Success         => Succ);
      if Succ and then
         Get_Inode_Type (Fetched_Inode.all.Permissions) = File_Symbolic_Link
      then
         Inner_Read_Symbolic_Link
            (Ino       => Fetched_Inode.all,
             File_Size => Get_Size (Fetched_Inode.all, FS.Has_64bit_Filesizes),
             Path      => Path,
             Ret_Count => Ret_Count);
         Success := (if Ret_Count /= 0 then FS_Success else FS_IO_Failure);
      else
         Path      := (others => ' ');
         Ret_Count := 0;
         Success   := FS_Invalid_Value;
      end if;

      Lib.Synchronization.Release (FS.Mutex);
      Free (Fetched_Inode);
   end Read_Symbolic_Link;

   procedure Read
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS_Data));
      Fetched_Type  : File_Type;
      Fetched_Inode : Inode_Acc := new Inode;
      Succ : Boolean;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      Ret_Count := 0;
      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Fetched_Inode.all,
          Write_Operation => False,
          Success         => Succ);
      if not Succ then
         Success := FS_IO_Failure;
         goto Cleanup;
      end if;

      Fetched_Type := Get_Inode_Type (Fetched_Inode.all.Permissions);
      case Fetched_Type is
         when File_Regular   => null;
         when File_Directory => Success := FS_Is_Directory;  goto Cleanup;
         when others         => Success := FS_Not_Supported; goto Cleanup;
      end case;

      Read_From_Inode
         (FS_Data    => FS,
          Inode_Data => Fetched_Inode.all,
          Inode_Size => Get_Size (Fetched_Inode.all, FS.Has_64bit_Filesizes),
          Offset     => Offset,
          Data       => Data,
          Ret_Count  => Ret_Count,
          Success    => Succ);

      Success := (if Succ then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Fetched_Inode);
   end Read;

   procedure Write
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS_Data));
      Fetched_Inode : Inode_Acc := new Inode;
      Succ : Boolean;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      Ret_Count := 0;
      if FS.Is_Read_Only then
         Success := FS_RO_Failure;
         goto Cleanup;
      end if;

      --  FIXME: Check whether the passed ino was opened allowing write. For
      --  now, this depends on the checks done in the syscall bodies, which it
      --  shouldnt.

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Fetched_Inode.all,
          Write_Operation => False,
          Success         => Succ);
      if not Succ then
         Success := FS_IO_Failure;
         goto Cleanup;
      elsif Is_Immutable (Fetched_Inode.all) or
            Get_Inode_Type (Fetched_Inode.all.Permissions) /= File_Regular
      then
         Success := FS_Invalid_Value;
         goto Cleanup;
      end if;

      Write_To_Inode
         (FS_Data    => FS,
          Inode_Data => Fetched_Inode.all,
          Inode_Num  => Unsigned_32 (Ino),
          Inode_Size => Get_Size (Fetched_Inode.all, FS.Has_64bit_Filesizes),
          Offset     => Offset,
          Data       => Data,
          Ret_Count  => Ret_Count,
          Success    => Succ);

      Success := (if Succ then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Fetched_Inode);
   end Write;

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status)
   is
      package Align is new Lib.Alignment (Unsigned_64);
      FS   : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Blk  : constant Unsigned_64  := Unsigned_64 (Get_Block_Size (FS.Handle));
      Size : Unsigned_64;
      Inod : Inode_Acc := new Inode;
      Succ : Boolean;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Inod.all,
          Write_Operation => False,
          Success         => Succ);

      if Succ then
         Size := Get_Size (Inod.all, FS.Has_64bit_Filesizes);
         S    :=
            (Unique_Identifier => Ino,
             Type_Of_File      => Get_Inode_Type (Inod.Permissions),
             Mode              => File_Mode (Inod.Permissions and 8#777#),
             UID               => Unsigned_32 (Inod.UID),
             GID               => Unsigned_32 (Inod.GID),
             Hard_Link_Count   => Positive (Inod.Hard_Link_Count),
             Byte_Size         => Size,
             IO_Block_Size     => Get_Block_Size (FS.Handle),
             IO_Block_Count    => Align.Divide_Round_Up (Size, Blk),
             Creation_Time     => (Unsigned_64 (Inod.Creation_Time_Epoch), 0),
             Modification_Time => (Unsigned_64 (Inod.Modified_Time_Epoch), 0),
             Access_Time       => (Unsigned_64 (Inod.Access_Time_Epoch),   0));
         Success := FS_Success;
      else
         Success := FS_IO_Failure;
      end if;

      Lib.Synchronization.Release (FS.Mutex);
      Free (Inod);
   end Stat;

   procedure Truncate
      (Data     : System.Address;
       Ino      : File_Inode_Number;
       New_Size : Unsigned_64;
       Status   : out FS_Status)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Fetched      : Inode_Acc := new Inode;
      Fetched_Size : Unsigned_64;
      Success      : Boolean;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      if FS.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      end if;

      --  FIXME: Check whether the passed ino was opened allowing write. For
      --  now, this depends on the checks done in the syscall bodies, which it
      --  shouldnt.

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Fetched.all,
          Write_Operation => False,
          Success         => Success);
      if not Success                                              or else
         Get_Inode_Type (Fetched.all.Permissions) /= File_Regular or else
         Is_Immutable (Fetched.all)
      then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      Fetched_Size := Get_Size (Fetched.all, FS.Has_64bit_Filesizes);
      if Fetched_Size = New_Size then
         Success := True;
      else
         if Fetched_Size < New_Size then
            Grow_Inode
               (FS_Data     => FS,
                Inode_Data  => Fetched.all,
                Inode_Num   => Unsigned_32 (Ino),
                Start       => 0,
                Count       => New_Size,
                Success     => Success);
         else
            Success := False;
         end if;

         if Success then
            Set_Size (Fetched.all, New_Size, FS.Has_64bit_Filesizes, Success);
            if Success then
               RW_Inode
                  (Data            => FS,
                   Inode_Index     => Unsigned_32 (Ino),
                   Result          => Fetched.all,
                   Write_Operation => True,
                   Success         => Success);
            end if;
         end if;
      end if;

      Status := (if Success then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Fetched);
   end Truncate;

   procedure IO_Control
      (Data   : System.Address;
       Ino    : File_Inode_Number;
       Req    : Unsigned_64;
       Arg    : System.Address;
       Status : out FS_Status)
   is
      EXT_GETFLAGS : constant := 16#5600#;
      EXT_SETFLAGS : constant := 16#5601#;

      FS      : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Inod    : Inode_Acc := new Inode;
      Success : Boolean;
      Flags   : Unsigned_32 with Import, Address => Arg;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      --  FIXME: Check whether the passed ino was opened allowing write. For
      --  now, this depends on the checks done in the syscall bodies, which it
      --  shouldnt.

      if FS.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      end if;

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Inod.all,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
      else
         case Req is
            when EXT_GETFLAGS =>
               Flags  := Inod.Flags;
               Status := FS_Success;
            when EXT_SETFLAGS =>
               Inod.Flags := Flags;
               RW_Inode
                  (Data            => FS,
                   Inode_Index     => Unsigned_32 (Ino),
                   Result          => Inod.all,
                   Write_Operation => True,
                   Success         => Success);
               Status := (if Success then FS_Success else FS_IO_Failure);
            when others =>
               Status := FS_Invalid_Value;
         end case;
      end if;

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Inod);
   end IO_Control;

   procedure Change_Mode
      (Data   : System.Address;
       Ino    : File_Inode_Number;
       Mode   : File_Mode;
       Status : out FS_Status)
   is
      FS      : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Inod    : Inode_Acc := new Inode;
      Success : Boolean;
      Typ     : File_Type;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      --  FIXME: Check whether the passed ino was opened allowing write. For
      --  now, this depends on the checks done in the syscall bodies, which it
      --  shouldnt.

      if FS.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      end if;

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Inod.all,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
      else
         Typ              := Get_Inode_Type (Inod.Permissions);
         Inod.Permissions := Get_Permissions (Typ) or Unsigned_16 (Mode);
         RW_Inode
            (Data            => FS,
             Inode_Index     => Unsigned_32 (Ino),
             Result          => Inod.all,
             Write_Operation => True,
             Success         => Success);
         Status := (if Success then FS_Success else FS_IO_Failure);
      end if;

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Inod);
   end Change_Mode;

   procedure Change_Owner
      (Data   : System.Address;
       Ino    : File_Inode_Number;
       Owner  : Unsigned_32;
       Group  : Unsigned_32;
       Status : out FS_Status)
   is
      FS      : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Inod    : Inode_Acc := new Inode;
      Success : Boolean;
   begin

      --  FIXME: Check whether the passed ino was opened allowing write. For
      --  now, this depends on the checks done in the syscall bodies, which it
      --  shouldnt.

      Lib.Synchronization.Seize (FS.Mutex);

      if FS.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      end if;

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Inod.all,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
      else
         Inod.UID := Unsigned_16 (Owner);
         Inod.GID := Unsigned_16 (Group);
         RW_Inode
            (Data            => FS,
             Inode_Index     => Unsigned_32 (Ino),
             Result          => Inod.all,
             Write_Operation => True,
             Success         => Success);
         Status := (if Success then FS_Success else FS_IO_Failure);
      end if;

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Inod);
   end Change_Owner;

   procedure Change_Access_Times
      (Data               : System.Address;
       Ino                : File_Inode_Number;
       Access_Seconds     : Unsigned_64;
       Access_Nanoseconds : Unsigned_64;
       Modify_Seconds     : Unsigned_64;
       Modify_Nanoseconds : Unsigned_64;
       Status             : out FS_Status)
   is
      pragma Unreferenced (Access_Nanoseconds);
      pragma Unreferenced (Modify_Nanoseconds);

      FS      : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Inod    : Inode_Acc := new Inode;
      AS      : Unsigned_64 renames Access_Seconds;
      MS      : Unsigned_64 renames Modify_Seconds;
      Success : Boolean;
   begin
      Lib.Synchronization.Seize (FS.Mutex);

      if FS.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      end if;

      --  FIXME: Check whether the passed ino was opened allowing write. For
      --  now, this depends on the checks done in the syscall bodies, which it
      --  shouldnt.

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Inod.all,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
      else
         Inod.Access_Time_Epoch   := Unsigned_32 (AS and 16#FFFFFFFF#);
         Inod.Modified_Time_Epoch := Unsigned_32 (MS and 16#FFFFFFFF#);
         RW_Inode
            (Data            => FS,
             Inode_Index     => Unsigned_32 (Ino),
             Result          => Inod.all,
             Write_Operation => True,
             Success         => Success);
         Status := (if Success then FS_Success else FS_IO_Failure);
      end if;

   <<Cleanup>>
      Lib.Synchronization.Release (FS.Mutex);
      Free (Inod);
   end Change_Access_Times;

   function Synchronize (Data : System.Address) return FS_Status is
      FS_Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
   begin
      if Devices.Synchronize (FS_Data.Handle) then
         return FS_Success;
      else
         return FS_IO_Failure;
      end if;
   end Synchronize;

   function Synchronize
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status
   is
      pragma Unreferenced (Ino);
      pragma Unreferenced (Data_Only);
   begin
      return Synchronize (Data);
   end Synchronize;
   ----------------------------------------------------------------------------
   procedure Inner_Open_Inode
      (Data           : EXT_Data_Acc;
       Relative       : Unsigned_32;
       Path           : String;
       Last_Component : out String_Acc;
       Target_Index   : out Unsigned_32;
       Target_Inode   : out Inode;
       Parent_Index   : out Unsigned_32;
       Parent_Inode   : out Inode;
       Success        : out Boolean)
   is
      Name_Start             : Natural;
      Target_Type            : File_Type;
      Target_Sz              : Unsigned_64;
      Entity                 : Directory_Entity;
      First_I, Last_I        : Natural;
      Curr_Index, Next_Index : Unsigned_64;
      Symlink                : String (1 .. 60);
      Symlink_Len            : Natural;
   begin
      Last_Component := null;
      Name_Start := 0;
      if Is_Absolute (Path) then
         Target_Index := Root_Inode;
         Target_Inode := Data.Root;
         Parent_Index := Root_Inode;
         Parent_Inode := Data.Root;
         Target_Sz    := Get_Size (Target_Inode, Data.Has_64bit_Filesizes);
         Target_Type  := File_Directory;

         if Path'Length = 1 then
            goto Perfect_Hit_Return;
         end if;

         First_I := Path'First + 1;
         Last_I  := Path'First + 1;
      elsif Path'Length > 0 then
         RW_Inode
            (Data            => Data,
             Inode_Index     => Relative,
             Result          => Target_Inode,
             Write_Operation => False,
             Success         => Success);
         if not Success or else
            Get_Inode_Type (Target_Inode.Permissions) /= File_Directory
         then
            goto Absolute_Miss_Return;
         end if;

         Target_Index := Relative;
         Parent_Index := Relative;
         Parent_Inode := Target_Inode;
         Target_Sz    := Get_Size (Target_Inode, Data.Has_64bit_Filesizes);
         Target_Type  := File_Directory;

         if Path'Length = 0 then
            goto Perfect_Hit_Return;
         end if;

         First_I := Path'First;
         Last_I  := Path'First;
      else
         goto Absolute_Miss_Return;
      end if;

      while Last_I <= Path'Last loop
      <<Retry_Component>>
         while Last_I <= Path'Last and then Path (Last_I) /= '/' loop
            Last_I := Last_I + 1;
         end loop;
         if First_I = Last_I and First_I < Path'Last then
            Last_I  := Last_I  + 1;
            First_I := First_I + 1;
            goto Retry_Component;
         end if;

         if Target_Type /= File_Directory then
            goto Absolute_Miss_Return;
         end if;

         Name_Start   := First_I;
         Curr_Index   := 0;
         Next_Index   := 0;
         Parent_Index := Target_Index;
         Parent_Inode := Target_Inode;

         loop
            Inner_Read_Entry
               (FS_Data     => Data,
                Inode_Sz    => Target_Sz,
                File_Ino    => Parent_Inode,
                Inode_Index => Curr_Index,
                Entity      => Entity,
                Next_Index  => Next_Index,
                Success     => Success);
            if not Success then
               if Last_I >= Path'Last then
                  goto Target_Miss_Parent_Hit_Return;
               else
                  goto Absolute_Miss_Return;
               end if;
            end if;

            Curr_Index := Next_Index;

            if Entity.Name_Buffer (1 .. Entity.Name_Len) =
               Path (First_I .. Last_I - 1)
            then
               Target_Index := Unsigned_32 (Entity.Inode_Number);
               RW_Inode
                  (Data            => Data,
                   Inode_Index     => Target_Index,
                   Result          => Target_Inode,
                   Write_Operation => False,
                   Success         => Success);
               Target_Sz := Get_Size (Target_Inode, Data.Has_64bit_Filesizes);
               Target_Type := Get_Inode_Type (Target_Inode.Permissions);
               if not Success then
                  goto Absolute_Miss_Return;
               end if;

               if Last_I < Path'Last and Target_Type = File_Symbolic_Link then
                  Inner_Read_Symbolic_Link
                     (Target_Inode,
                      Target_Sz,
                      Symlink,
                      Symlink_Len);
                  if Symlink_Len = 0 then
                     goto Absolute_Miss_Return;
                  end if;

                  Inner_Open_Inode
                     (Data,
                      Parent_Index,
                      Symlink (1 .. Symlink_Len) & Path (Last_I .. Path'Last),
                      Last_Component,
                      Target_Index,
                      Target_Inode,
                      Parent_Index,
                      Parent_Inode,
                      Success);
                  return;
               end if;
               goto Next_Iteration;
            end if;
         end loop;

      <<Next_Iteration>>
         Last_I  := Last_I + 1;
         First_I := Last_I;
      end loop;

   <<Perfect_Hit_Return>>
      Success := True;
      goto Fix_Last_Component;

   <<Target_Miss_Parent_Hit_Return>>
      Target_Index := 0;
      Success := False;
      goto Fix_Last_Component;

   <<Absolute_Miss_Return>>
      Target_Index := 0;
      Parent_Index := 0;
      Success := False;
      return;

   <<Fix_Last_Component>>
      if Path'First <= Name_Start and Name_Start <= Path'Last then
         Last_Component := new String'(Path (Name_Start .. Path'Last));
      end if;
   end Inner_Open_Inode;

   procedure Inner_Read_Symbolic_Link
      (Ino       : Inode;
       File_Size : Unsigned_64;
       Path      : out String;
       Ret_Count : out Natural)
   is
      Final_Length : Natural;
      Str_Data     : Operation_Data (1 .. Path'Length)
         with Import, Address => Ino.Blocks'Address;
   begin
      Final_Length := Natural (File_Size);
      if Final_Length > 60 then
         Path      := (others => ' ');
         Ret_Count := 0;
         return;
      end if;

      for I in 1 .. Final_Length loop
         Path (Path'First + I - 1) := Character'Val (Str_Data (I));
      end loop;
      Ret_Count := Final_Length;
   end Inner_Read_Symbolic_Link;

   procedure Inner_Read_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Sz    : Unsigned_64;
       File_Ino    : Inode;
       Inode_Index : Unsigned_64;
       Entity      : out Directory_Entity;
       Next_Index  : out Unsigned_64;
       Success     : out Boolean)
   is
      Ret_Count : Natural;
      Dir       : Directory_Entry;
      Tmp_Index : Unsigned_64;
      Tmp_Type  : File_Type;
      Dir_Data  : Operation_Data (1 .. Directory_Entry'Size / 8)
         with Import, Address => Dir'Address;
   begin
      Read_From_Inode
         (FS_Data    => FS_Data,
          Inode_Data => File_Ino,
          Inode_Size => Inode_Sz,
          Offset     => Inode_Index,
          Data       => Dir_Data,
          Ret_Count  => Ret_Count,
          Success    => Success);
      if (not Success or Ret_Count /= Dir_Data'Length) or else
         Dir.Inode_Index = 0
      then
         goto Error_Return;
      end if;

      declare
         Dir_Name  : String (1 .. Natural (Dir.Name_Length));
         Name_Data : Operation_Data (1 .. Dir_Name'Length)
            with Import, Address => Dir_Name'Address;
      begin
         Tmp_Index := Inode_Index + (Directory_Entry'Size / 8);
         if (FS_Data.Super.Required_Features and Required_Directory_Types) /= 0
         then
            Tmp_Type := Get_Dir_Type (Dir.Dir_Type);
         else
            Tmp_Index := Tmp_Index - 1;
            Tmp_Type  := File_Regular;
         end if;

         Read_From_Inode
            (FS_Data    => FS_Data,
             Inode_Data => File_Ino,
             Inode_Size => Inode_Sz,
             Offset     => Tmp_Index,
             Data       => Name_Data,
             Ret_Count  => Ret_Count,
             Success    => Success);
         if not Success or Ret_Count /= Name_Data'Length then
            goto Error_Return;
         end if;

         Entity :=
            (Inode_Number => Unsigned_64 (Dir.Inode_Index),
             Name_Buffer  => <>,
             Name_Len     => Dir_Name'Length,
             Type_Of_File => Tmp_Type);
         Entity.Name_Buffer (1 .. Dir_Name'Length) := Dir_Name;
         Next_Index := Inode_Index + Unsigned_64 (Dir.Entry_Count);
         Success    := True;
         return;
      end;

   <<Error_Return>>
      Next_Index := 0;
      Success    := False;
   end Inner_Read_Entry;

   procedure RW_Superblock
      (Handle          : Device_Handle;
       Offset          : Unsigned_64;
       Super           : in out Superblock;
       Write_Operation : Boolean;
       Success         : out Boolean)
   is
      Ret_Count  : Natural;
      Super_Data : Operation_Data (1 .. Superblock'Size / 8)
         with Import, Address => Super'Address;
   begin
      if Write_Operation then
         Devices.Write
            (Handle    => Handle,
             Offset    => Offset,
             Data      => Super_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Devices.Read
            (Handle    => Handle,
             Offset    => Offset,
             Data      => Super_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      end if;

      Success := Success and (Ret_Count = Super_Data'Length);
   end RW_Superblock;

   procedure RW_Block_Group_Descriptor
      (Data             : EXT_Data_Acc;
       Descriptor_Index : Unsigned_32;
       Result           : in out Block_Group_Descriptor;
       Write_Operation  : Boolean;
       Success          : out Boolean)
   is
      Descr_Size  : constant Natural := Block_Group_Descriptor'Size / 8;
      Offset      : Unsigned_32 := Data.Block_Size;
      Ret_Count   : Natural;
      Result_Data : Operation_Data (1 .. Descr_Size)
         with Import, Address => Result'Address;
   begin
      if Data.Block_Size < 2048 then
         Offset := Offset * 2;
      end if;

      Offset := Offset + (Unsigned_32 (Descr_Size) * Descriptor_Index);

      if Write_Operation then
         Devices.Write
            (Handle    => Data.Handle,
             Offset    => Unsigned_64 (Offset),
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Devices.Read
            (Handle    => Data.Handle,
             Offset    => Unsigned_64 (Offset),
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      end if;

      if Success and (Ret_Count = Result_Data'Length) then
         Success := True;
      else
         Act_On_Policy (Data, "block group descriptor RW failure");
         Success := False;
      end if;
   end RW_Block_Group_Descriptor;

   procedure RW_Inode
      (Data            : EXT_Data_Acc;
       Inode_Index     : Unsigned_32;
       Result          : in out Inode;
       Write_Operation : Boolean;
       Success         : out Boolean)
   is
      Table_Index, Descriptor_Index : Unsigned_32;
      Block_Descriptor : Block_Group_Descriptor;
      Offset      : Unsigned_32;
      Ret_Count   : Natural;
      Result_Data : Operation_Data (1 .. Inode'Size / 8)
         with Import, Address => Result'Address;
   begin
      Table_Index      := (Inode_Index - 1) mod Data.Super.Inodes_Per_Group;
      Descriptor_Index := (Inode_Index - 1) / Data.Super.Inodes_Per_Group;

      RW_Block_Group_Descriptor
         (Data             => Data,
          Descriptor_Index => Descriptor_Index,
          Result           => Block_Descriptor,
          Write_Operation  => False,
          Success          => Success);
      if not Success then
         return;
      end if;

      Offset := Block_Descriptor.Inode_Table_Block * Data.Block_Size +
                Table_Index * Unsigned_32 (Data.Super.Inode_Size);

      if Write_Operation then
         Devices.Write
            (Handle    => Data.Handle,
             Offset    => Unsigned_64 (Offset),
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Devices.Read
            (Handle    => Data.Handle,
             Offset    => Unsigned_64 (Offset),
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      end if;
      if Success and (Ret_Count = Result_Data'Length) then
         Success := True;
      else
         Act_On_Policy (Data, "inode RW failure");
         Success := False;
      end if;
   end RW_Inode;

   function Get_Block_Index
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : Inode;
       Searched    : Unsigned_32) return Unsigned_32
   is
      Adjusted_Block        : Unsigned_32 := Searched;
      Block_Level           : constant Unsigned_32 := FS_Data.Block_Size / 4;
      Block_Index           : Unsigned_32;
      Single_Index          : Unsigned_32;
      Indirect_Offset       : Unsigned_32;
      Indirect_Block        : Unsigned_32;
      Double_Indirect       : Unsigned_32;
      Single_Indirect_Index : Unsigned_32;

      Single_Indirect_Index_Data : Operation_Data (1 .. 4)
         with Import, Address => Single_Indirect_Index'Address;
      Indirect_Block_Data : Operation_Data (1 .. 4)
         with Import, Address => Indirect_Block'Address;
      Block_Index_Data : Operation_Data (1 .. 4)
         with Import, Address => Block_Index'Address;

      Discard_1 : Natural;
      Discard_2 : Boolean;
   begin
      if Adjusted_Block < 12 then
         return Inode_Data.Blocks (Natural (Searched));
      else
         Adjusted_Block := Searched - 12;
      end if;

      if Adjusted_Block >= Block_Level then
         Adjusted_Block  := Adjusted_Block - Block_Level;
         Single_Index    := Adjusted_Block / Block_Level;
         Indirect_Offset := Adjusted_Block mod Block_Level;
         Indirect_Block  := 0;

         if Single_Index >= Block_Level then
            Adjusted_Block         := Adjusted_Block - (Block_Level ** 2);
            Double_Indirect        := Adjusted_Block / Block_Level;
            Indirect_Offset        := Adjusted_Block mod Block_Level;
            Single_Indirect_Index  := 0;

            Devices.Read
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Inode_Data.Blocks (14) *
                             FS_Data.Block_Size + Double_Indirect * 4),
                Data      => Single_Indirect_Index_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);
            Devices.Read
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Double_Indirect * FS_Data.Block_Size
                                          + Single_Indirect_Index * 4),
                Data      => Indirect_Block_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);
            Devices.Read
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Indirect_Block * FS_Data.Block_Size +
                             Indirect_Offset * 4),
                Data      => Block_Index_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);
            return Block_Index;
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Inode_Data.Blocks (13) *
                                       FS_Data.Block_Size + Single_Index * 4),
             Data      => Indirect_Block_Data,
             Ret_Count => Discard_1,
             Success   => Discard_2);
         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Indirect_Block * FS_Data.Block_Size +
                                       Indirect_Offset * 4),
             Data      => Block_Index_Data,
             Ret_Count => Discard_1,
             Success   => Discard_2);
         return Block_Index;
      end if;

      Devices.Read
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Inode_Data.Blocks (12) * FS_Data.Block_Size
                                    + Adjusted_Block * 4),
          Data      => Block_Index_Data,
          Ret_Count => Discard_1,
          Success   => Discard_2);
      return Block_Index;
   end Get_Block_Index;

   procedure Read_From_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : Inode;
       Inode_Size  : Unsigned_64;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean)
   is
      Final_Count    : Natural     := Data'Length;
      Final_Offset   : Unsigned_64 := Offset;
      Block_Searched : Unsigned_64;
      Block_Index    : Unsigned_32;
      Step_Size      : Unsigned_64;
      Bytes_Read     : Natural := 0;
   begin
      if Offset > Inode_Size then
         Ret_Count := 0;
         Success   := True;
         return;
      elsif Offset + Data'Length > Inode_Size then
         Final_Count := Natural (Inode_Size - Offset);
      end if;

      while Bytes_Read < Final_Count loop
         Final_Offset   := (Offset + Unsigned_64 (Bytes_Read));
         Block_Searched := Final_Offset / Unsigned_64 (FS_Data.Block_Size);
         Final_Offset   := Final_Offset mod Unsigned_64 (FS_Data.Block_Size);
         Step_Size     := Unsigned_64 (Final_Count) - Unsigned_64 (Bytes_Read);

         if Step_Size > Unsigned_64 (FS_Data.Block_Size) - Final_Offset then
            Step_Size := Unsigned_64 (FS_Data.Block_Size) - Final_Offset;
         end if;

         Block_Index := Get_Block_Index
            (FS_Data, Inode_Data, Unsigned_32 (Block_Searched));
         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Block_Index) *
                          Unsigned_64 (FS_Data.Block_Size) + Final_Offset,
             Data      => Data (Data'First + Bytes_Read ..
                                Data'First + Bytes_Read +
                                Natural (Step_Size) - 1),
             Ret_Count => Ret_Count,
             Success   => Success);
         if not Success then
            Act_On_Policy (FS_Data, "Error reading an inode");
            return;
         end if;

         Bytes_Read := Bytes_Read + Natural (Step_Size);
      end loop;

      Ret_Count := Final_Count;
      Success   := True;
   end Read_From_Inode;

   procedure Write_To_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Inode_Size  : Unsigned_64;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean)
   is
      Final_Offset   : Unsigned_64 := Offset;
      Block_Searched : Unsigned_64;
      Block_Index    : Unsigned_32;
      Step_Size      : Unsigned_64;
      Bytes_Read     : Natural := 0;
   begin
      if Offset + Unsigned_64 (Data'Length) > Inode_Size then
         Grow_Inode
            (FS_Data     => FS_Data,
             Inode_Data  => Inode_Data,
             Inode_Num   => Inode_Num,
             Start       => Offset,
             Count       => Unsigned_64 (Data'Length),
             Success     => Success);
         if not Success then
            goto Error_Return;
         end if;
         Set_Size
            (Ino        => Inode_Data,
             New_Size   => Offset + Unsigned_64 (Data'Length),
             Is_64_Bits => FS_Data.Has_64bit_Filesizes,
             Success    => Success);
         if not Success then
            goto Error_Return;
         end if;
         RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Num,
             Result          => Inode_Data,
             Write_Operation => True,
             Success         => Success);
         if not Success then
            goto Error_Return;
         end if;
      end if;

      while Bytes_Read < Data'Length loop
         Final_Offset   := (Offset + Unsigned_64 (Bytes_Read));
         Block_Searched := Final_Offset / Unsigned_64 (FS_Data.Block_Size);
         Final_Offset   := Final_Offset mod Unsigned_64 (FS_Data.Block_Size);
         Step_Size     := Unsigned_64 (Data'Length) - Unsigned_64 (Bytes_Read);

         if Step_Size > Unsigned_64 (FS_Data.Block_Size) - Final_Offset then
            Step_Size := Unsigned_64 (FS_Data.Block_Size) - Final_Offset;
         end if;

         Block_Index := Get_Block_Index
            (FS_Data, Inode_Data, Unsigned_32 (Block_Searched));
         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Block_Index) *
                          Unsigned_64 (FS_Data.Block_Size) + Final_Offset,
             Data      => Data (Data'First + Bytes_Read ..
                                Data'First + Bytes_Read +
                                Natural (Step_Size) - 1),
             Ret_Count => Ret_Count,
             Success   => Success);
         if not Success then
            goto Error_Return;
         end if;

         Bytes_Read := Bytes_Read + Natural (Step_Size);
      end loop;

      Ret_Count := Data'Length;
      Success   := True;
      return;

   <<Error_Return>>
      Act_On_Policy (FS_Data, "Error while writing to an inode");
      Ret_Count := 0;
      Success   := False;
   end Write_To_Inode;

   procedure Grow_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start       : Unsigned_64;
       Count       : Unsigned_64;
       Success     : out Boolean)
   is
      Offset : constant Unsigned_64 :=
         Shift_Right (Start and not Unsigned_64 (FS_Data.Block_Size - 1),
                      Natural (10 + FS_Data.Super.Block_Size_Log));
      BCount : constant Unsigned_64 :=
         Shift_Right ((Start and Unsigned_64 (FS_Data.Block_Size - 1)) +
                      Count + Unsigned_64 (FS_Data.Block_Size - 1),
                      Natural (10 + FS_Data.Super.Block_Size_Log));
   begin
      Assign_Inode_Blocks
         (FS_Data     => FS_Data,
          Inode_Data  => Inode_Data,
          Inode_Num   => Inode_Num,
          Start_Blk   => Unsigned_32 (Offset),
          Block_Count => Unsigned_32 (BCount),
          Success     => Success);
   end Grow_Inode;

   procedure Assign_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start_Blk   : Unsigned_32;
       Block_Count : Unsigned_32;
       Success     : out Boolean)
   is
      Ret_Blk : Unsigned_32;
   begin
      for I in 0 .. Block_Count - 1 loop
         Ret_Blk := Get_Block_Index (FS_Data, Inode_Data, Start_Blk + I);
         if Ret_Blk = 0 then
            Allocate_Block_For_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Inode_Data,
                Inode_Num  => Inode_Num,
                Ret_Block  => Ret_Blk,
                Success    => Success);
            if not Success then
               return;
            end if;
            Wire_Inode_Blocks
               (FS_Data     => FS_Data,
                Inode_Data  => Inode_Data,
                Inode_Num   => Inode_Num,
                Block_Index => Start_Blk + I,
                Wired_Block => Ret_Blk,
                Success     => Success);
            if not Success then
               return;
            end if;
         end if;
      end loop;

      RW_Inode
         (Data            => FS_Data,
          Inode_Index     => Inode_Num,
          Result          => Inode_Data,
          Write_Operation => True,
          Success         => Success);
   end Assign_Inode_Blocks;

   procedure Wire_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Block_Index : Unsigned_32;
       Wired_Block : Unsigned_32;
       Success     : out Boolean)
   is
      Adjusted_Block        : Unsigned_32 := Block_Index;
      Block_Level           : constant Unsigned_32 := FS_Data.Block_Size / 4;
      DBlock                : Unsigned_32 := Wired_Block;
      Single_Index          : Unsigned_32;
      Indirect_Offset       : Unsigned_32;
      Indirect_Block        : Unsigned_32;
      Double_Indirect       : Unsigned_32;
      Single_Indirect_Index : Unsigned_32;
      Temp                  : Unsigned_32;

      Single_Indirect_Index_Data : Operation_Data (1 .. 4)
         with Import, Address => Single_Indirect_Index'Address;
      Indirect_Block_Data : Operation_Data (1 .. 4)
         with Import, Address => Indirect_Block'Address;
      DBlock_Data : Operation_Data (1 .. 4)
         with Import, Address => DBlock'Address;

      Discard_1 : Natural;
      Discard_2 : Boolean;
   begin
      if Adjusted_Block < 12 then
         Inode_Data.Blocks (Natural (Adjusted_Block)) := Wired_Block;
         Success := True;
         return;
      else
         Adjusted_Block := Adjusted_Block - 12;
      end if;

      if Adjusted_Block >= Block_Level then
         Adjusted_Block  := Adjusted_Block - Block_Level;
         Single_Index    := Adjusted_Block / Block_Level;
         Indirect_Offset := Adjusted_Block mod Block_Level;
         Indirect_Block  := 0;

         if Single_Index >= Block_Level then
            Adjusted_Block         := Adjusted_Block - (Block_Level ** 2);
            Double_Indirect        := Adjusted_Block / Block_Level;
            Indirect_Offset        := Adjusted_Block mod Block_Level;
            Single_Indirect_Index  := 0;

            if Inode_Data.Blocks (14) = 0 then
               Allocate_Block_For_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Num,
                   Ret_Block  => Temp,
                   Success    => Discard_2);
               Inode_Data.Blocks (14) := Temp;
               RW_Inode
                  (Data            => FS_Data,
                   Inode_Index     => Inode_Num,
                   Result          => Inode_Data,
                   Write_Operation => True,
                   Success         => Discard_2);
            end if;

            Devices.Read
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Inode_Data.Blocks (14) *
                             FS_Data.Block_Size + Double_Indirect * 4),
                Data      => Single_Indirect_Index_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);

            if Single_Indirect_Index = 0 then
               Allocate_Block_For_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Num,
                   Ret_Block  => Single_Indirect_Index,
                   Success    => Discard_2);

               Devices.Write
                 (Handle    => FS_Data.Handle,
                  Offset    => Unsigned_64 (Inode_Data.Blocks (14) *
                               FS_Data.Block_Size + Double_Indirect * 4),
                  Data      => Single_Indirect_Index_Data,
                  Ret_Count => Discard_1,
                  Success   => Discard_2);
            end if;

            Devices.Read
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Double_Indirect * FS_Data.Block_Size
                                          + Single_Indirect_Index * 4),
                Data      => Indirect_Block_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);

            if Indirect_Block = 0 then
               Allocate_Block_For_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Num,
                   Ret_Block  => Temp,
                   Success    => Discard_2);

               Devices.Write
                  (Handle   => FS_Data.Handle,
                   Offset    => Unsigned_64 (Double_Indirect    *
                                             FS_Data.Block_Size +
                                             Single_Indirect_Index * 4),
                  Data      => Indirect_Block_Data,
                  Ret_Count => Discard_1,
                  Success   => Discard_2);

               Indirect_Block := Temp;
            end if;

            Devices.Write
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Indirect_Block * FS_Data.Block_Size +
                             Indirect_Offset * 4),
                Data      => DBlock_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);
            Success := True;
            return;
         end if;

         if Inode_Data.Blocks (13) = 0 then
            Allocate_Block_For_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Inode_Data,
                Inode_Num  => Inode_Num,
                Ret_Block  => Temp,
                Success    => Discard_2);
            Inode_Data.Blocks (13) := Temp;
            RW_Inode
               (Data            => FS_Data,
                Inode_Index     => Inode_Num,
                Result          => Inode_Data,
                Write_Operation => True,
                Success         => Discard_2);
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Inode_Data.Blocks (13) *
                                       FS_Data.Block_Size + Single_Index * 4),
             Data      => Indirect_Block_Data,
             Ret_Count => Discard_1,
             Success   => Discard_2);

         if Indirect_Block = 0 then
            Allocate_Block_For_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Inode_Data,
                Inode_Num  => Inode_Num,
                Ret_Block  => Indirect_Block,
                Success    => Discard_2);

            Devices.Write
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Inode_Data.Blocks (13) *
                                          FS_Data.Block_Size     +
                                          Single_Index * 4),
                Data      => Indirect_Block_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);
         end if;

         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Indirect_Block * FS_Data.Block_Size +
                                       Indirect_Offset * 4),
             Data      => DBlock_Data,
             Ret_Count => Discard_1,
             Success   => Discard_2);
         Success := True;
         return;
      end if;

      if Inode_Data.Blocks (12) = 0 then
         Allocate_Block_For_Inode
            (FS_Data    => FS_Data,
             Inode_Data => Inode_Data,
             Inode_Num  => Inode_Num,
             Ret_Block  => Temp,
             Success    => Discard_2);
         Inode_Data.Blocks (12) := Temp;
         RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Num,
             Result          => Inode_Data,
             Write_Operation => True,
             Success         => Discard_2);
      end if;

      Devices.Write
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Inode_Data.Blocks (12) * FS_Data.Block_Size
                                    + Adjusted_Block * 4),
          Data      => DBlock_Data,
          Ret_Count => Discard_1,
          Success   => Discard_2);
      Success := True;
      return;
   end Wire_Inode_Blocks;

   procedure Allocate_Block_For_Inode
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : in out Inode;
       Inode_Num  : Unsigned_32;
       Ret_Block  : out Unsigned_32;
       Success    : out Boolean)
   is
      Ret_Count  : Natural;
      Desc       : Block_Group_Descriptor;
      Curr_Block : Unsigned_32;
      Bitmap     : Operation_Data_Acc
         := new Operation_Data (1 .. Natural (FS_Data.Block_Size));
   begin
      for I in 0 .. FS_Data.Super.Block_Count loop
         RW_Block_Group_Descriptor
            (Data             => FS_Data,
             Descriptor_Index => I,
             Result           => Desc,
             Write_Operation  => False,
             Success          => Success);
         if not Success then
            goto Error_Return;
         end if;

         if Desc.Unallocated_Blocks = 0 then
            goto Next_Iteration;
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Block_Usage_Bitmap_Block *
                                       FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Ret_Count,
             Success   => Success);
         if not Success then
            goto Error_Return;
         end if;

         Curr_Block := 0;
         for J in Bitmap'Range loop
            if Bitmap (J) = 16#FF# then
               goto Next_Next_Iteration;
            end if;
            for Bit in 0 .. 7 loop
               if (Bitmap (J) and 2 ** Bit) = 0 then
                  Bitmap (J) := Bitmap (J) or 2 ** Bit;
                  Curr_Block := (I * FS_Data.Super.Blocks_Per_Group) +
                                 Unsigned_32 (J) * 8 + Unsigned_32 (Bit);
                  goto End_Search_Loop;
               end if;
            end loop;
         <<Next_Next_Iteration>>
         end loop;
      <<End_Search_Loop>>
         if Curr_Block = 0 then
            goto Next_Iteration;
         end if;

         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Block_Usage_Bitmap_Block *
                                       FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Ret_Count,
             Success   => Success);
         if not Success then
            goto Error_Return;
         end if;

         FS_Data.Super.Unallocated_Block_Count :=
            FS_Data.Super.Unallocated_Block_Count - 1;
         Desc.Unallocated_Blocks := Desc.Unallocated_Blocks - 1;

         Inode_Data.Sectors := Inode_Data.Sectors + (FS_Data.Block_Size /
                               Unsigned_32 (Get_Block_Size (FS_Data.Handle)));
         RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Num,
             Result          => Inode_Data,
             Write_Operation => True,
             Success         => Success);
         if not Success then
            goto Error_Return;
         end if;

         RW_Block_Group_Descriptor
            (Data             => FS_Data,
             Descriptor_Index => I,
             Result           => Desc,
             Write_Operation  => True,
             Success          => Success);
         if not Success then
            goto Error_Return;
         end if;
         Ret_Block := Curr_Block;
         Free (Bitmap);
         Success := True;
         return;
   <<Next_Iteration>>
      end loop;

   <<Error_Return>>
      Free (Bitmap);
      Ret_Block := 0;
      Success := False;
   end Allocate_Block_For_Inode;

   procedure Allocate_Inode
      (FS_Data   : EXT_Data_Acc;
       Inode_Num : out Unsigned_32;
       Success   : out Boolean)
   is
      Ret_Count  : Natural;
      Desc       : Block_Group_Descriptor;
      Curr_Block : Unsigned_32;
      Bitmap     : Operation_Data_Acc
         := new Operation_Data (1 .. Natural (FS_Data.Block_Size));
   begin
      for I in 0 .. FS_Data.Super.Inode_Count loop
         RW_Block_Group_Descriptor
            (Data             => FS_Data,
             Descriptor_Index => I,
             Result           => Desc,
             Write_Operation  => False,
             Success          => Success);
         if not Success then
            goto Error_Return;
         end if;

         if Desc.Unallocated_Inodes = 0 then
            goto Next_Iteration;
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Inode_Usage_Bitmap_Block *
                                       FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Ret_Count,
             Success   => Success);
         if not Success then
            goto Error_Return;
         end if;

         Curr_Block := 0;
         for J in Bitmap'Range loop
            if Bitmap (J) = 16#FF# then
               goto Next_Next_Iteration;
            end if;
            for Bit in 0 .. 7 loop
               if (Bitmap (J) and 2 ** Bit) = 0 then
                  Curr_Block := (I * FS_Data.Super.Inodes_Per_Group) +
                                 Unsigned_32 (J) * 8 + Unsigned_32 (Bit);
                  if Curr_Block > FS_Data.Super.First_Non_Reserved and
                     Curr_Block > 11
                  then
                     Bitmap (J) := Bitmap (J) or 2 ** Bit;
                     goto End_Search_Loop;
                  end if;
               end if;
            end loop;
         <<Next_Next_Iteration>>
         end loop;
      <<End_Search_Loop>>
         if Curr_Block = 0 then
            goto Next_Iteration;
         end if;

         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Inode_Usage_Bitmap_Block *
                                       FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Ret_Count,
             Success   => Success);
         if not Success then
            goto Error_Return;
         end if;

         FS_Data.Super.Unallocated_Inode_Count :=
            FS_Data.Super.Unallocated_Inode_Count - 1;
         Desc.Unallocated_Inodes := Desc.Unallocated_Inodes - 1;

         RW_Block_Group_Descriptor
            (Data             => FS_Data,
             Descriptor_Index => I,
             Result           => Desc,
             Write_Operation  => True,
             Success          => Success);
         if not Success then
            goto Error_Return;
         end if;
         Inode_Num := Curr_Block;
         Free (Bitmap);
         Success := True;
         return;
   <<Next_Iteration>>
      end loop;

   <<Error_Return>>
      Inode_Num := 0;
      Free (Bitmap);
      Success := False;
   end Allocate_Inode;

   procedure Add_Directory_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Size  : Unsigned_64;
       Inode_Index : Unsigned_32;
       Added_Index : Unsigned_32;
       Dir_Type    : Unsigned_8;
       Name        : String;
       Success     : out Boolean)
   is
      Buffer     : Operation_Data_Acc;
      Required   : Unsigned_64;
      Offset     : Natural;
      Contracted : Unsigned_64;
      Available  : Unsigned_64;
      Ret_Count  : Natural;
   begin
      Buffer := new Operation_Data (1 .. Natural (Inode_Size));
      Read_From_Inode
         (FS_Data    => FS_Data,
          Inode_Data => Inode_Data,
          Inode_Size => Inode_Size,
          Offset     => 0,
          Data       => Buffer.all,
          Ret_Count  => Ret_Count,
          Success    => Success);
      if not Success or Ret_Count /= Buffer'Length then
         Success := False;
         goto Cleanup;
      end if;

      Required := ((Directory_Entry'Size / 8) + Name'Length + 4) and not 3;
      Offset   := 0;

      while Offset < Natural (Inode_Size) loop
         declare
            Ent : Directory_Entry
               with Import, Address => Buffer (Buffer'First + Offset)'Address;
         begin
            Contracted :=
               ((Directory_Entry'Size / 8) +
                Unsigned_64 (Ent.Name_Length) + 3) and not 3;
            Available := Unsigned_64 (Ent.Entry_Count) - Contracted;

            if Available >= Required then
               Ent.Entry_Count := Unsigned_16 (Contracted);
               if Offset + Buffer'First + Offset + Natural (Contracted) >
                  Buffer'Length
               then
                  Success := False;
                  goto Cleanup;
               end if;

               declare
                  Ent2 : Directory_Entry with Import, Address =>
                     Buffer (Buffer'First + Offset +
                                  Natural (Contracted))'Address;
                  Name_Buf : String (1 .. Name'Length) with Address =>
                     Buffer (Buffer'First + Offset + Natural (Contracted) +
                             (Directory_Entry'Size / 8))'Address, Import;
               begin
                  Ent2 :=
                     (Inode_Index => Added_Index,
                      Entry_Count => Unsigned_16 (Available),
                      Name_Length => Name'Length,
                      Dir_Type    => Dir_Type);
                  Name_Buf := Name;

                  Write_To_Inode
                     (FS_Data    => FS_Data,
                      Inode_Num  => Inode_Index,
                      Inode_Data => Inode_Data,
                      Inode_Size => Inode_Size,
                      Offset     => 0,
                      Data       => Buffer.all,
                      Ret_Count  => Ret_Count,
                      Success    => Success);
                  if not Success or Ret_Count /= Buffer'Length then
                     Success := False;
                  end if;
                  goto Cleanup;
               end;
            end if;
            Offset := Offset + Natural (Ent.Entry_Count);
         end;
      end loop;

      Success := False;

   <<Cleanup>>
      Free (Buffer);
   end Add_Directory_Entry;

   procedure Delete_Directory_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Size  : Unsigned_64;
       Inode_Index : Unsigned_32;
       Added_Index : Unsigned_32;
       Success     : out Boolean)
   is
      Buffer                     : Operation_Data_Acc;
      Offset, Offset2, Ret_Count : Natural;
   begin
      Buffer := new Operation_Data (1 .. Natural (Inode_Size));
      Read_From_Inode
         (FS_Data    => FS_Data,
          Inode_Data => Inode_Data,
          Inode_Size => Inode_Size,
          Offset     => 0,
          Data       => Buffer.all,
          Ret_Count  => Ret_Count,
          Success    => Success);
      if not Success or Ret_Count /= Buffer'Length then
         Success := False;
         goto Cleanup;
      end if;

      Offset  := 0;
      Offset2 := 0;

      while Offset < Natural (Inode_Size) loop
         declare
            Ent : Directory_Entry
               with Import, Address => Buffer (Buffer'First + Offset)'Address;
            Ent2 : Directory_Entry
               with Import, Address => Buffer (Buffer'First + Offset2)'Address;
         begin
            if Ent.Inode_Index = Added_Index then
               Ent2.Entry_Count := Ent2.Entry_Count + Ent.Entry_Count;
               Write_To_Inode
                  (FS_Data    => FS_Data,
                   Inode_Num  => Inode_Index,
                   Inode_Data => Inode_Data,
                   Inode_Size => Inode_Size,
                   Offset     => 0,
                   Data       => Buffer.all,
                   Ret_Count  => Ret_Count,
                   Success    => Success);
               if not Success or Ret_Count /= Buffer'Length then
                  Success := False;
               end if;
               goto Cleanup;
            end if;
            Offset2 := Offset;
            Offset  := Offset + Natural (Ent.Entry_Count);
         end;
      end loop;

      Success := False;

   <<Cleanup>>
      Free (Buffer);
   end Delete_Directory_Entry;

   function Get_Dir_Type (Dir_Type : Unsigned_8) return File_Type is
   begin
      case Dir_Type is
         when 1      => return File_Regular;
         when 2      => return File_Directory;
         when 3      => return File_Character_Device;
         when 4      => return File_Block_Device;
         when others => return File_Symbolic_Link;
      end case;
   end Get_Dir_Type;

   function Get_Dir_Type (T : File_Type) return Unsigned_8 is
   begin
      case T is
         when File_Regular          => return 1;
         when File_Directory        => return 2;
         when File_Character_Device => return 3;
         when File_Block_Device     => return 4;
         when others                => return 5;
      end case;
   end Get_Dir_Type;

   function Get_Permissions (T : File_Type) return Unsigned_16 is
   begin
      case T is
         when File_Character_Device => return 16#2000#;
         when File_Directory        => return 16#4000#;
         when File_Block_Device     => return 16#6000#;
         when File_Regular          => return 16#8000#;
         when File_Symbolic_Link    => return 16#A000#;
      end case;
   end Get_Permissions;

   function Get_Inode_Type (Perms : Unsigned_16) return File_Type is
   begin
      case Perms and 16#F000# is
         when 16#2000# => return File_Character_Device;
         when 16#4000# => return File_Directory;
         when 16#6000# => return File_Block_Device;
         when 16#8000# => return File_Regular;
         when 16#A000# => return File_Symbolic_Link;
         when others   => return File_Regular; -- ???
      end case;
   end Get_Inode_Type;

   function Get_Inode_Type
      (T    : File_Type;
       Mode : Unsigned_32) return Unsigned_16
   is
      Ret : constant Unsigned_16 := Unsigned_16 (Mode and 16#FFF#);
   begin
      case T is
         when File_Character_Device => return Ret or 16#2000#;
         when File_Directory        => return Ret or 16#4000#;
         when File_Block_Device     => return Ret or 16#6000#;
         when File_Symbolic_Link    => return Ret or 16#A000#;
         when others                => return Ret or 16#8000#;
      end case;
   end Get_Inode_Type;

   function Get_Size (Ino : Inode; Is_64_Bits : Boolean) return Unsigned_64 is
   begin
      if Is_64_Bits then
         return Shift_Left (Unsigned_64 (Ino.Size_High), 32) or
                Shift_Left (Unsigned_64 (Ino.Size_Low),   0);
      else
         return Unsigned_64 (Ino.Size_Low);
      end if;
   end Get_Size;

   procedure Set_Size
      (Ino        : in out Inode;
       New_Size   : Unsigned_64;
       Is_64_Bits : Boolean;
       Success    : out Boolean)
   is
      L32 : constant Unsigned_32 := Unsigned_32 (New_Size and 16#FFFFFFFF#);
      H32 : constant Unsigned_32 := Unsigned_32 (Shift_Right (New_Size, 32));
   begin
      if Is_64_Bits then
         Ino.Size_Low  := L32;
         Ino.Size_High := H32;
         Success       := True;
      else
         Ino.Size_Low := L32;
         Success      := H32 = 0;
      end if;
   end Set_Size;

   procedure Act_On_Policy (Data : EXT_Data_Acc; Message : String) is
   begin
      case Data.Super.Error_Policy is
         when Policy_Ignore =>
            Lib.Messages.Put_Line (Message);
         when Policy_Remount_RO =>
            Lib.Messages.Put_Line (Message);
            Data.Is_Read_Only := True;
         when Policy_Panic =>
            Lib.Panic.Hard_Panic (Message);
         when others =>
            Lib.Panic.Hard_Panic ("ext is dead, and we killed it");
      end case;
   end Act_On_Policy;

   function Check_User_Access
      (User        : Unsigned_32;
       Inod        : Inode;
       Check_Read  : Boolean;
       Check_Write : Boolean;
       Check_Exec  : Boolean) return Boolean
   is
   begin
      if User /= 0 then
         if Unsigned_32 (Inod.UID) = User then
            if (Check_Read  and then ((Inod.Permissions and 8#400#) = 0)) or
               (Check_Write and then ((Inod.Permissions and 8#200#) = 0)) or
               (Check_Exec  and then ((Inod.Permissions and 8#100#) = 0))
            then
               return False;
            end if;
         else
            if (Check_Read  and then ((Inod.Permissions and 8#004#) = 0)) or
               (Check_Write and then ((Inod.Permissions and 8#002#) = 0)) or
               (Check_Exec  and then ((Inod.Permissions and 8#001#) = 0))
            then
               return False;
            end if;
         end if;
      end if;

      return True;
   end Check_User_Access;
end VFS.EXT;
