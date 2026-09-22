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

with Time;
with Arch.Clocks;
with Messages;
with Panic;
with Alignment;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;

package body VFS.EXT with SPARK_Mode => Off is
   package   Conv is new System.Address_To_Access_Conversions (EXT_Data);
   procedure Free is new Ada.Unchecked_Deallocation (EXT_Data, EXT_Data_Acc);
   procedure Free is new Ada.Unchecked_Deallocation (Inode,    Inode_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
      (Operation_Data, Operation_Data_Acc);

   procedure Probe
      (Handle        : Device_Handle;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Data_Addr     : out System.Address;
       Root_Ino      : out File_Inode_Number)
   is
      Sup      : Superblock;
      Data     : EXT_Data_Acc;
      Success  : Boolean;
      Is_RO    : Boolean;
      Blk_Size : Unsigned_32;
      Ino_Size : Unsigned_32;
      First_In : Unsigned_32;
      Groups   : Unsigned_64;
      Max_Mnts : Unsigned_16;
   begin
      Root_Ino  := Root_Inode;
      Data_Addr := Null_Address;

      RW_Superblock
         (Handle          => Handle,
          Offset          => Main_Superblock_Offset,
          Super           => Sup,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         return;
      end if;

      --  Check we support everything ext needs us to. The sizes are checked
      --  as well because everything below divides by them, and a superblock
      --  that came off a damaged device must not take the kernel down.
      if Sup.Signature /= EXT_Signature                           or else
         Sup.Block_Size_Log > 6                                   or else
         Sup.Block_Size_Log < Sup.Fragment_Size_Log               or else
         Sup.Major_Version > 1                                    or else
         Sup.Blocks_Per_Group = 0                                 or else
         Sup.Inodes_Per_Group = 0                                 or else
         Sup.Block_Count = 0                                      or else
         Sup.Inode_Count = 0                                      or else
         Sup.Block_Containing_Super >= Sup.Block_Count            or else
         (Sup.Required_Features and Required_Compression)    /= 0 or else
         (Sup.Required_Features and Required_Journal_Replay) /= 0 or else
         (Sup.Required_Features and Required_Journal_Device) /= 0
      then
         return;
      end if;

      Blk_Size := Shift_Left (Unsigned_32'(1024),
                              Natural (Sup.Block_Size_Log));

      --  Revision 0 does not have the fields holding the inode size and the
      --  first usable inode, they are fixed by the format instead.
      if Sup.Major_Version >= 1 then
         Ino_Size := Unsigned_32 (Sup.Inode_Size);
         First_In := Sup.First_Non_Reserved;
      else
         Ino_Size := Old_Inode_Size;
         First_In := Old_First_Ino;
      end if;
      if Ino_Size < Old_Inode_Size or else
         Ino_Size > Blk_Size       or else
         First_In < Root_Inode     or else
         First_In > Sup.Inode_Count
      then
         return;
      end if;

      --  Amount of block groups the filesystem is divided in.
      Groups :=
         (Unsigned_64 (Sup.Block_Count) -
          Unsigned_64 (Sup.Block_Containing_Super) +
          Unsigned_64 (Sup.Blocks_Per_Group) - 1) /
         Unsigned_64 (Sup.Blocks_Per_Group);
      if Groups = 0 or else Groups > Unsigned_64 (Unsigned_32'Last) then
         return;
      end if;

      --  A max mount count of zero or -1 means the mount check is disabled.
      Max_Mnts := Sup.Max_Mounts_Since_Check;
      Is_RO :=
         Do_Read_Only                                 or
         Devices.Is_Read_Only (Handle)                or
         Sup.Filesystem_State /= State_Clean          or
         (Max_Mnts /= 0 and then
          Max_Mnts /= Unsigned_16'Last and then
          Sup.Mounts_Since_Check > Max_Mnts)          or
         (Sup.RO_If_Not_Features and RO_Binary_Trees) /= 0;
      if Is_RO then
         Messages.Put_Line ("ext will be mounted RO, consider an fsck");
      end if;

      Data := new EXT_Data'
         (Mutex         => Synchronization.Unlocked_RW_Lock,
          Handle        => Handle,
          Super         => Sup,
          Is_Read_Only  => Is_RO,
          Do_Relatime   => Access_Policy = Relative_Update,
          Block_Size    => Blk_Size,
          Fragment_Size => Shift_Left (Unsigned_32'(1024),
                                       Natural (Sup.Fragment_Size_Log)),
          Root          => <>,
          Has_Sparse_Superblock =>
            (Sup.RO_If_Not_Features and RO_Sparse_Superblocks) /= 0,
          Has_64bit_Filesizes =>
            (Sup.RO_If_Not_Features and RO_64bit_Filesize) /= 0,
          Inode_Size          => Ino_Size,
          First_Inode         => First_In,
          First_Data_Block    => Sup.Block_Containing_Super,
          Block_Group_Count   => Unsigned_32 (Groups),
          Pointers_Per_Block  => Blk_Size / 4,
          Has_Directory_Types =>
            (Sup.Required_Features and Required_Directory_Types) /= 0,
          Search_Group        => 0,
          Memo_Lock           => Synchronization.Unlocked_Semaphore,
          Memo_Inode          => 0,
          Memo_Index          => 0,
          Memo_Offset         => 0);

      RW_Inode
         (Data            => Data,
          Inode_Index     => Root_Inode,
          Result          => Data.Root,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Free (Data);
         return;
      end if;

      Data_Addr := Conv.To_Address (Conv.Object_Pointer (Data));
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception probing an EXT filesystem");
         Data_Addr := Null_Address;
   end Probe;

   procedure Remount
      (FS            : System.Address;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Success       : out Boolean)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Synchronization.Seize_Writer (Data.Mutex);
      Data.Is_Read_Only := Do_Read_Only;
      Data.Do_Relatime  := Access_Policy = Relative_Update;
      Synchronization.Release_Writer (Data.Mutex);
      Success := True;
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (Data.Mutex);
         Messages.Put_Line ("Exception remounting an EXT filesystem");
         Success := False;
   end Remount;

   procedure Unmount (FS : in out System.Address) is
      Data : EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Synchronization.Seize_Writer (Data.Mutex);

      if not Data.Is_Read_Only then
         if Data.Super.Mounts_Since_Check /= Unsigned_16'Last then
            Data.Super.Mounts_Since_Check :=
               Data.Super.Mounts_Since_Check + 1;
         end if;
         Sync_Superblock (Data);
      end if;

      Free (Data);
      FS := System.Null_Address;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception unmounting an EXT filesystem");
         FS := System.Null_Address;
   end Unmount;
   ----------------------------------------------------------------------------
   procedure Get_Block_Size (FS : System.Address; Size : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Synchronization.Seize_Reader (Data.Mutex);
      Size := Unsigned_64 (Data.Block_Size);
      Synchronization.Release_Reader (Data.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (Data.Mutex);
         Messages.Put_Line ("Exception getting a EXT block size");
         Size := 0;
   end Get_Block_Size;

   procedure Get_Fragment_Size (FS : System.Address; Size : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Synchronization.Seize_Reader (Data.Mutex);
      Size := Unsigned_64 (Data.Fragment_Size);
      Synchronization.Release_Reader (Data.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (Data.Mutex);
         Messages.Put_Line ("Exception getting a EXT fragment size");
         Size := 0;
   end Get_Fragment_Size;

   procedure Get_Size (FS : System.Address; Size : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Synchronization.Seize_Reader (Data.Mutex);
      Size := Unsigned_64 (Data.Super.Block_Count) *
         Unsigned_64 (Data.Block_Size) /
         Unsigned_64 (Data.Fragment_Size);
      Synchronization.Release_Reader (Data.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (Data.Mutex);
         Messages.Put_Line ("Exception getting a EXT size");
         Size := 0;
   end Get_Size;

   procedure Get_Inode_Count (FS : System.Address; Count : out Unsigned_64) is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Synchronization.Seize_Reader (Data.Mutex);
      Count := Unsigned_64 (Data.Super.Inode_Count);
      Synchronization.Release_Reader (Data.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (Data.Mutex);
         Messages.Put_Line ("Exception getting a EXT inode count");
         Count := 0;
   end Get_Inode_Count;

   procedure Get_Free_Blocks
      (FS                : System.Address;
       Free_Blocks       : out Unsigned_64;
       Free_Unprivileged : out Unsigned_64)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Free : Unsigned_64;
      Rsvd : Unsigned_64;
   begin
      Synchronization.Seize_Reader (Data.Mutex);
      Free := Unsigned_64 (Data.Super.Unallocated_Block_Count);
      Rsvd := Unsigned_64 (Data.Super.Reserved_Count);
      Free_Blocks := Free;
      Free_Unprivileged := (if Free > Rsvd then Free - Rsvd else 0);
      Synchronization.Release_Reader (Data.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (Data.Mutex);
         Messages.Put_Line ("Exception getting EXT free blocks");
         Free_Blocks        := 0;
         Free_Unprivileged := 0;
   end Get_Free_Blocks;

   procedure Get_Free_Inodes
      (FS                : System.Address;
       Free_Inodes       : out Unsigned_64;
       Free_Unprivileged : out Unsigned_64)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Synchronization.Seize_Reader (Data.Mutex);
      Free_Inodes := Unsigned_64 (Data.Super.Unallocated_Inode_Count);
      Free_Unprivileged := Unsigned_64 (Data.Super.Unallocated_Inode_Count);
      Synchronization.Release_Reader (Data.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (Data.Mutex);
         Messages.Put_Line ("Exception getting EXT free inodes");
         Free_Inodes        := 0;
         Free_Unprivileged := 0;
   end Get_Free_Inodes;

   function Get_Max_Length (FS : System.Address) return Unsigned_64 is
      pragma Unreferenced (FS);
   begin
      return Max_File_Name_Size;
   end Get_Max_Length;
   ----------------------------------------------------------------------------
   procedure Create_Node
      (FS         : System.Address;
       Parent_Ino : File_Inode_Number;
       Name       : String;
       Kind       : File_Type;
       Mode       : File_Mode;
       User       : Unsigned_32;
       Status     : out FS_Status)
   is
      Data     : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Perms    : constant  Unsigned_16 := Get_Permissions (Kind);
      Dir_Type : constant   Unsigned_8 := Get_Dir_Type (Kind);
      Is_Dir   : constant      Boolean := Kind = File_Directory;
      Target_Index               : Unsigned_32 := 0;
      Target_Inode, Parent_Inode : Inode_Acc := new Inode;
      Buffer                     : Operation_Data_Acc := null;
      Temp                       : Natural;
      Success, Parent_Open       : Boolean;
      Discard                    : Boolean;
      Stamp                      : Unsigned_32;
   begin
      Synchronization.Seize_Writer (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Name'Length = 0 or else Name'Length > Max_File_Name_Size then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      --  Checking the file doesn't exist but the parent is found along perms.
      Inner_Open_Inode
         (Data           => Data,
          Parent_Index   => Unsigned_32 (Parent_Ino),
          Name           => Name,
          Target_Index   => Target_Index,
          Target_Inode   => Target_Inode.all,
          Parent_Inode   => Parent_Inode.all,
          Success        => Success,
          Parent_Open    => Parent_Open);
      if Success then
         Status := FS_Exists;
         goto Cleanup;
      elsif not Parent_Open then
         Status := FS_Not_Found;
         goto Cleanup;
      elsif Get_Inode_Type (Parent_Inode.Permissions) /= File_Directory then
         Status := FS_Not_Directory;
         goto Cleanup;
      elsif not Check_User_Access (User, Parent_Inode.all, False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      elsif Is_Dir and Parent_Inode.Hard_Link_Count >= Link_Max then
         Status := FS_Too_Many_Links;
         goto Cleanup;
      end if;

      Allocate_Inode
         (FS_Data      => Data,
          Is_Directory => Is_Dir,
          Goal         => (Unsigned_32 (Parent_Ino) - 1) /
                          Data.Super.Inodes_Per_Group,
          Inode_Num    => Target_Index,
          Success      => Success);
      if not Success then
         Status := FS_Full;
         goto Cleanup;
      end if;

      Stamp := Current_Epoch;
      Target_Inode.all :=
         (Permissions         => Perms or Unsigned_16 (Mode),
          UID                 => Unsigned_16 (User and 16#FFFF#),
          Size_Low            => 0,
          Access_Time_Epoch   => Stamp,
          Creation_Time_Epoch => Stamp,
          Modified_Time_Epoch => Stamp,
          Deleted_Time_Epoch  => 0,
          GID                 => Parent_Inode.GID,
          Hard_Link_Count     => (if Is_Dir then 2 else 1),
          Sectors             => 0,
          Flags               => 0,
          OS_Specific_Value_1 => 0,
          Blocks              => [others => 0],
          Generation_Number   => 0,
          EAB                 => 0,
          Size_High           => 0,
          Fragment_Address    => 0,
          OS_Specific_Value_2 => [others => 0]);

      RW_Inode
         (Data            => Data,
          Inode_Index     => Target_Index,
          Result          => Target_Inode.all,
          Write_Operation => True,
          Success         => Success);
      if not Success then
         goto Undo_Inode;
      end if;

      if Is_Dir then
         Buffer := new Operation_Data'
            (1 .. Natural (Data.Block_Size) => 0);
         Put_Dir_Entry
            (Buffer   => Buffer.all,
             Offset   => 0,
             Ino      => Target_Index,
             Rec_Len  => 12,
             Kind     => Get_Dir_Type (File_Directory),
             Name     => ".",
             Has_Type => Data.Has_Directory_Types);
         Put_Dir_Entry
            (Buffer   => Buffer.all,
             Offset   => 12,
             Ino      => Unsigned_32 (Parent_Ino),
             Rec_Len  => Natural (Data.Block_Size) - 12,
             Kind     => Get_Dir_Type (File_Directory),
             Name     => "..",
             Has_Type => Data.Has_Directory_Types);
         Write_To_Inode
            (FS_Data    => Data,
             Inode_Data => Target_Inode.all,
             Inode_Num  => Target_Index,
             Inode_Size => 0,
             Offset     => 0,
             Data       => Buffer.all,
             Ret_Count  => Temp,
             Success    => Success);
         if not Success then
            goto Undo_Inode;
         end if;
      end if;

      Add_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Parent_Inode.all,
          Inode_Size  => Get_Size (Parent_Inode.all, Data.Has_64bit_Filesizes),
          Inode_Index => Unsigned_32 (Parent_Ino),
          Added_Index => Target_Index,
          Dir_Type    => Dir_Type,
          Name        => Name,
          Success     => Success);
      if not Success then
         goto Undo_Inode;
      end if;

      if Is_Dir then
         Parent_Inode.Hard_Link_Count := Parent_Inode.Hard_Link_Count + 1;
      end if;
      Parent_Inode.Modified_Time_Epoch := Stamp;
      Parent_Inode.Creation_Time_Epoch := Stamp;
      RW_Inode
         (Data            => Data,
          Inode_Index     => Unsigned_32 (Parent_Ino),
          Result          => Parent_Inode.all,
          Write_Operation => True,
          Success         => Success);

      Status := (if Success then FS_Success else FS_IO_Failure);
      goto Cleanup;

   <<Undo_Inode>>
      Delete_Inode (Data, Target_Index, Target_Inode.all, Discard);
      Status := FS_IO_Failure;

   <<Cleanup>>
      Synchronization.Release_Writer (Data.Mutex);
      Free (Target_Inode);
      Free (Parent_Inode);
      Free (Buffer);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (Data.Mutex);
         Messages.Put_Line ("Exception while creating an EXT node");
         Status := FS_IO_Failure;
   end Create_Node;

   procedure Create_Symbolic_Link
      (FS         : System.Address;
       Parent_Ino : File_Inode_Number;
       Name       : String;
       Target     : String;
       Mode       : Unsigned_32;
       User       : Unsigned_32;
       Status     : out FS_Status)
   is
      Data     : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Perms    : constant  Unsigned_16 := Get_Permissions (File_Symbolic_Link);
      Dir_Type : constant   Unsigned_8 := Get_Dir_Type (File_Symbolic_Link);
      Target_Index               : Unsigned_32 := 0;
      Target_Inode, Parent_Inode : Inode_Acc := new Inode;
      Ret_Count                  : Natural;
      Success, Parent_Open       : Boolean;
      Discard                    : Boolean;
      Stamp                      : Unsigned_32;
   begin
      Synchronization.Seize_Writer (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Name'Length = 0 or else Target'Length = 0 or else
            Name'Length > Max_File_Name_Size
      then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      --  Checking the file doesn't exist but the parent is found along perms.
      Inner_Open_Inode
         (Data           => Data,
          Parent_Index   => Unsigned_32 (Parent_Ino),
          Name           => Name,
          Target_Index   => Target_Index,
          Target_Inode   => Target_Inode.all,
          Parent_Inode   => Parent_Inode.all,
          Success        => Success,
          Parent_Open    => Parent_Open);
      if Success then
         Status := FS_Exists;
         goto Cleanup;
      elsif not Parent_Open then
         Status := FS_Not_Found;
         goto Cleanup;
      elsif not Check_User_Access (User, Parent_Inode.all, False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      end if;

      Allocate_Inode
         (FS_Data      => Data,
          Is_Directory => False,
          Goal         => (Unsigned_32 (Parent_Ino) - 1) /
                          Data.Super.Inodes_Per_Group,
          Inode_Num    => Target_Index,
          Success      => Success);
      if not Success then
         Status := FS_Full;
         goto Cleanup;
      end if;

      Stamp := Current_Epoch;
      Target_Inode.all :=
         (Permissions         => Perms or Unsigned_16 (Mode and 8#777#),
          UID                 => Unsigned_16 (User and 16#FFFF#),
          Size_Low            => Unsigned_32 (Target'Length),
          Access_Time_Epoch   => Stamp,
          Creation_Time_Epoch => Stamp,
          Modified_Time_Epoch => Stamp,
          Deleted_Time_Epoch  => 0,
          GID                 => Parent_Inode.GID,
          Hard_Link_Count     => 1,
          Sectors             => 0,
          Flags               => 0,
          OS_Specific_Value_1 => 0,
          Blocks              => [others => 0],
          Generation_Number   => 0,
          EAB                 => 0,
          Size_High           => 0,
          Fragment_Address    => 0,
          OS_Specific_Value_2 => [others => 0]);

      --  EXT implements a shortcut for short symlinks, by putting them
      --  straight on the blocks array and having no blocks.
      if Target'Length <= Target_Inode.Blocks'Length * 4 then
         declare
            Str_Data : String (1 .. Target'Length)
               with Import, Address => Target_Inode.Blocks'Address;
         begin
            Str_Data := Target;
            Success := True;
         end;
      else
         declare
            Str_Data : Operation_Data (1 .. Target'Length)
               with Import, Address => Target (Target'First)'Address;
         begin
            Write_To_Inode
               (FS_Data    => Data,
                Inode_Data => Target_Inode.all,
                Inode_Num  => Target_Index,
                Inode_Size => 0,
                Offset     => 0,
                Data       => Str_Data,
                Ret_Count  => Ret_Count,
                Success    => Success);
         end;
      end if;
      if not Success then
         goto Undo_Inode;
      end if;

      --  Writing the target may have moved the size along, put it back to
      --  the length of the link.
      Set_Size
         (Ino        => Target_Inode.all,
          New_Size   => Unsigned_64 (Target'Length),
          Is_64_Bits => Data.Has_64bit_Filesizes,
          Success    => Success);
      if not Success then
         goto Undo_Inode;
      end if;

      RW_Inode
         (Data            => Data,
          Inode_Index     => Target_Index,
          Result          => Target_Inode.all,
          Write_Operation => True,
          Success         => Success);
      if not Success then
         goto Undo_Inode;
      end if;

      Add_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Parent_Inode.all,
          Inode_Size  => Get_Size (Parent_Inode.all, Data.Has_64bit_Filesizes),
          Inode_Index => Unsigned_32 (Parent_Ino),
          Added_Index => Target_Index,
          Dir_Type    => Dir_Type,
          Name        => Name,
          Success     => Success);
      if not Success then
         goto Undo_Inode;
      end if;

      Parent_Inode.Modified_Time_Epoch := Stamp;
      Parent_Inode.Creation_Time_Epoch := Stamp;
      RW_Inode
         (Data            => Data,
          Inode_Index     => Unsigned_32 (Parent_Ino),
          Result          => Parent_Inode.all,
          Write_Operation => True,
          Success         => Success);

      Status := (if Success then FS_Success else FS_IO_Failure);
      goto Cleanup;

   <<Undo_Inode>>
      Delete_Inode (Data, Target_Index, Target_Inode.all, Discard);
      Status := FS_IO_Failure;

   <<Cleanup>>
      Synchronization.Release_Writer (Data.Mutex);
      Free (Target_Inode);
      Free (Parent_Inode);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (Data.Mutex);
         Messages.Put_Line ("Exception while creating an EXT symlink");
         Status := FS_IO_Failure;
   end Create_Symbolic_Link;

   procedure Create_Hard_Link
      (FS            : System.Address;
       Source_Parent : File_Inode_Number;
       Source_Name   : String;
       Target_Parent : File_Inode_Number;
       Target_Name   : String;
       User          : Unsigned_32;
       Status        : out FS_Status)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Source_Index, Target_Index        : Unsigned_32 := 0;
      Source_Inode, Source_Parent_Inode : Inode_Acc := new Inode;
      Target_Inode, Target_Parent_Inode : Inode_Acc := new Inode;
      Success, Parent_Open              : Boolean;
      Stamp                             : Unsigned_32;
   begin
      Synchronization.Seize_Writer (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Source_Name'Length = 0 or else Target_Name'Length = 0 or else
            Target_Name'Length > Max_File_Name_Size
      then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      --  Open the source.
      Inner_Open_Inode
         (Data           => Data,
          Parent_Index   => Unsigned_32 (Source_Parent),
          Name           => Source_Name,
          Target_Index   => Source_Index,
          Target_Inode   => Source_Inode.all,
          Parent_Inode   => Source_Parent_Inode.all,
          Success        => Success,
          Parent_Open    => Parent_Open);
      if not Success then
         Status := FS_Not_Found;
         goto Cleanup;
      elsif Get_Inode_Type (Source_Inode.Permissions) = File_Directory then
         Status := FS_Is_Directory;
         goto Cleanup;
      end if;

      --  Checking the target file doesn't exist but the parent is found.
      --  Also check some permissions.
      Inner_Open_Inode
         (Data           => Data,
          Parent_Index   => Unsigned_32 (Target_Parent),
          Name           => Target_Name,
          Target_Index   => Target_Index,
          Target_Inode   => Target_Inode.all,
          Parent_Inode   => Target_Parent_Inode.all,
          Success        => Success,
          Parent_Open    => Parent_Open);
      if Success then
         Status := FS_Exists;
         goto Cleanup;
      elsif not Parent_Open then
         Status := FS_Not_Found;
         goto Cleanup;
      elsif not Check_User_Access
         (User, Target_Parent_Inode.all, False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      elsif Source_Inode.Hard_Link_Count >= Link_Max then
         Status := FS_Too_Many_Links;
         goto Cleanup;
      end if;

      Stamp := Current_Epoch;
      Add_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Target_Parent_Inode.all,
          Inode_Size  => Get_Size (Target_Parent_Inode.all,
                                   Data.Has_64bit_Filesizes),
          Inode_Index => Unsigned_32 (Target_Parent),
          Added_Index => Source_Index,
          Dir_Type    =>
            Get_Dir_Type (Get_Inode_Type (Source_Inode.Permissions)),
          Name        => Target_Name,
          Success     => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      Source_Inode.Hard_Link_Count := Source_Inode.Hard_Link_Count + 1;
      Source_Inode.Creation_Time_Epoch := Stamp;
      RW_Inode
         (Data            => Data,
          Inode_Index     => Source_Index,
          Result          => Source_Inode.all,
          Write_Operation => True,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      Target_Parent_Inode.Modified_Time_Epoch := Stamp;
      Target_Parent_Inode.Creation_Time_Epoch := Stamp;
      RW_Inode
         (Data            => Data,
          Inode_Index     => Unsigned_32 (Target_Parent),
          Result          => Target_Parent_Inode.all,
          Write_Operation => True,
          Success         => Success);

      Status := (if Success then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Synchronization.Release_Writer (Data.Mutex);
      Free (Source_Inode);
      Free (Source_Parent_Inode);
      Free (Target_Inode);
      Free (Target_Parent_Inode);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (Data.Mutex);
         Messages.Put_Line ("Exception while creating an EXT hardlink");
         Status := FS_IO_Failure;
   end Create_Hard_Link;

   procedure Rename
      (FS            : System.Address;
       Source_Parent : File_Inode_Number;
       Source_Name   : String;
       Target_Parent : File_Inode_Number;
       Target_Name   : String;
       Keep          : Boolean;
       User          : Unsigned_32;
       Status        : out FS_Status)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Same_Parent : constant Boolean := Source_Parent = Target_Parent;
      Source_Index, Target_Index               : Unsigned_32 := 0;
      Source_Inode, Target_Inode               : Inode_Acc := new Inode;
      Source_Parent_Inode, Target_Parent_Inode : Inode_Acc := new Inode;
      Src_P, Tgt_P                             : Inode_Acc;
      Source_Kind, Target_Kind                 : File_Type;
      Deleted, Stamp                           : Unsigned_32;
      Empty, Is_Descendant                     : Boolean;
      Success1, Success2, Parent_O1, Parent_O2 : Boolean;
   begin
      Synchronization.Seize_Writer (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Source_Name'Length = 0 or else Target_Name'Length = 0 or else
            Target_Name'Length > Max_File_Name_Size or else
            Source_Name = "." or else Source_Name = ".."   or else
            Target_Name = "." or else Target_Name = ".."
      then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      Inner_Open_Inode
         (Data         => Data,
          Parent_Index => Unsigned_32 (Source_Parent),
          Name         => Source_Name,
          Target_Index => Source_Index,
          Target_Inode => Source_Inode.all,
          Parent_Inode => Source_Parent_Inode.all,
          Success      => Success1,
          Parent_Open  => Parent_O1);
      Inner_Open_Inode
         (Data         => Data,
          Parent_Index => Unsigned_32 (Target_Parent),
          Name         => Target_Name,
          Target_Index => Target_Index,
          Target_Inode => Target_Inode.all,
          Parent_Inode => Target_Parent_Inode.all,
          Success      => Success2,
          Parent_Open  => Parent_O2);

      --  Check that the source exists, that the parent of the target exists,
      --  and that we do not want to keep the file if it exists, along with
      --  permissions.
      if not Success1 or not Parent_O1 or not Parent_O2 then
         Status := FS_Not_Found;
         goto Cleanup;
      elsif Keep and Success2 then
         Status := FS_Exists;
         goto Cleanup;
      elsif not Check_User_Access (User, Target_Parent_Inode.all,
                                   False, True, False)
         or else not Check_User_Access (User, Source_Parent_Inode.all,
                                        False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      end if;

      --  Renaming a name onto itself is asked to do nothing at all.
      if Success2 and then Source_Index = Target_Index then
         Status := FS_Success;
         goto Cleanup;
      end if;

      Source_Kind := Get_Inode_Type (Source_Inode.Permissions);

      --  A directory cannot be moved under itself: it would leave the tree
      --  and end up its own ancestor. Nothing moves meanwhile, the lock is
      --  held as a writer.
      if Source_Kind = File_Directory and not Same_Parent then
         Is_Under
            (FS_Data  => Data,
             Dir      => Unsigned_32 (Target_Parent),
             Ancestor => Source_Index,
             Result   => Is_Descendant,
             Success  => Success1);
         if not Success1 then
            Status := FS_IO_Failure;
            goto Cleanup;
         elsif Is_Descendant then
            Status := FS_Invalid_Value;
            goto Cleanup;
         end if;
      end if;

      --  When there is something in the way it has to be compatible with
      --  what is being moved onto it, and an occupied directory never is.
      if Success2 then
         Target_Kind := Get_Inode_Type (Target_Inode.Permissions);
         if Target_Kind = File_Directory and Source_Kind /= File_Directory then
            Status := FS_Is_Directory;
            goto Cleanup;
         elsif Source_Kind = File_Directory and
               Target_Kind /= File_Directory
         then
            Status := FS_Not_Directory;
            goto Cleanup;
         elsif Target_Kind = File_Directory then
            Is_Directory_Empty
               (FS_Data    => Data,
                Inode_Data => Target_Inode.all,
                Inode_Size => Get_Size (Target_Inode.all,
                                        Data.Has_64bit_Filesizes),
                Empty      => Empty,
                Success    => Success1);
            if not Success1 then
               Status := FS_IO_Failure;
               goto Cleanup;
            elsif not Empty then
               Status := FS_Not_Empty;
               goto Cleanup;
            end if;
         end if;
      end if;

      --  A directory moving under a new parent gives it one more link, unless
      --  it replaces a directory there.
      if Source_Kind = File_Directory and not Same_Parent and not Success2
         and Target_Parent_Inode.Hard_Link_Count >= Link_Max
      then
         Status := FS_Too_Many_Links;
         goto Cleanup;
      end if;

      --  Both names may live in the same directory, in which case there is
      --  only one inode and it must not be edited through two copies.
      Src_P := Source_Parent_Inode;
      Tgt_P := (if Same_Parent then Source_Parent_Inode
                else Target_Parent_Inode);
      Stamp := Current_Epoch;

      --  The new name goes in first, or the name in the way is pointed at
      --  what moves, and only then does the old name go, so that a new name
      --  that finds no room changes nothing. Two names point at the file
      --  meanwhile, which nothing sees under the lock.
      if Success2 then
         Set_Entry_Inode
            (FS_Data     => Data,
             Inode_Data  => Tgt_P.all,
             Inode_Size  => Get_Size (Tgt_P.all, Data.Has_64bit_Filesizes),
             Inode_Index => Unsigned_32 (Target_Parent),
             Name        => Target_Name,
             New_Ino     => Source_Index,
             Dir_Type    => Get_Dir_Type (Source_Kind),
             Success     => Success1);
      else
         Add_Directory_Entry
            (FS_Data     => Data,
             Inode_Data  => Tgt_P.all,
             Inode_Size  => Get_Size (Tgt_P.all, Data.Has_64bit_Filesizes),
             Inode_Index => Unsigned_32 (Target_Parent),
             Added_Index => Source_Index,
             Dir_Type    => Get_Dir_Type (Source_Kind),
             Name        => Target_Name,
             Success     => Success1);
      end if;
      if not Success1 then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      Delete_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Src_P.all,
          Inode_Size  => Get_Size (Src_P.all, Data.Has_64bit_Filesizes),
          Inode_Index => Unsigned_32 (Source_Parent),
          Name        => Source_Name,
          Deleted_Ino => Deleted,
          Success     => Success1);
      if not Success1 then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      if Success2 then
         --  Drop the link the replaced name held, and reap the inode when
         --  that was the last one. Without this every overwrite would leak
         --  an inode and all of its blocks.
         if Target_Kind = File_Directory then
            Target_Inode.Hard_Link_Count := 0;
            Delete_Inode (Data, Target_Index, Target_Inode.all, Success1);
            if Tgt_P.Hard_Link_Count > 2 then
               Tgt_P.Hard_Link_Count := Tgt_P.Hard_Link_Count - 1;
            end if;
         elsif Target_Inode.Hard_Link_Count > 1 then
            Target_Inode.Hard_Link_Count :=
               Target_Inode.Hard_Link_Count - 1;
            Target_Inode.Creation_Time_Epoch := Stamp;
            RW_Inode (Data, Target_Index, Target_Inode.all, True, Success1);
         else
            Delete_Inode (Data, Target_Index, Target_Inode.all, Success1);
         end if;
         if not Success1 then
            Status := FS_IO_Failure;
            goto Cleanup;
         end if;
      end if;

      --  A directory that changed parent points at the wrong one through its
      --  '..' entry, and both directories are one link off.
      if Source_Kind = File_Directory and not Same_Parent then
         Set_Parent_Entry
            (FS_Data     => Data,
             Inode_Data  => Source_Inode.all,
             Inode_Size  => Get_Size (Source_Inode.all,
                                      Data.Has_64bit_Filesizes),
             Inode_Index => Source_Index,
             New_Parent  => Unsigned_32 (Target_Parent),
             Success     => Success1);
         if not Success1 then
            Status := FS_IO_Failure;
            goto Cleanup;
         end if;
         if Src_P.Hard_Link_Count > 2 then
            Src_P.Hard_Link_Count := Src_P.Hard_Link_Count - 1;
         end if;
         Tgt_P.Hard_Link_Count := Tgt_P.Hard_Link_Count + 1;
      end if;

      Source_Inode.Creation_Time_Epoch := Stamp;
      RW_Inode (Data, Source_Index, Source_Inode.all, True, Success1);

      Src_P.Modified_Time_Epoch := Stamp;
      Src_P.Creation_Time_Epoch := Stamp;
      RW_Inode (Data, Unsigned_32 (Source_Parent), Src_P.all, True, Success2);
      Success1 := Success1 and Success2;

      if not Same_Parent then
         Tgt_P.Modified_Time_Epoch := Stamp;
         Tgt_P.Creation_Time_Epoch := Stamp;
         RW_Inode
            (Data, Unsigned_32 (Target_Parent), Tgt_P.all, True, Success2);
         Success1 := Success1 and Success2;
      end if;

      Status := (if Success1 then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Synchronization.Release_Writer (Data.Mutex);
      Free (Source_Inode);
      Free (Target_Inode);
      Free (Source_Parent_Inode);
      Free (Target_Parent_Inode);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (Data.Mutex);
         Messages.Put_Line ("Exception while renaming an EXT node");
         Status := FS_IO_Failure;
   end Rename;

   procedure Unlink
      (FS      : System.Address;
       Parent  : File_Inode_Number;
       Name    : String;
       User    : Unsigned_32;
       Do_Dirs : Boolean;
       Status  : out FS_Status)
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      Path_Index               : Unsigned_32 := 0;
      Path_Inode, Parent_Inode : Inode_Acc := new Inode;
      Success, Parent_Open     : Boolean;
      Empty                    : Boolean;
      Kind                     : File_Type;
      Deleted, Stamp           : Unsigned_32;
   begin
      Synchronization.Seize_Writer (Data.Mutex);

      if Data.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      elsif Name'Length = 0 or else Name = "." or else Name = ".." then
         Status := FS_Invalid_Value;
         goto Cleanup;
      end if;

      Inner_Open_Inode
         (Data         => Data,
          Parent_Index => Unsigned_32 (Parent),
          Name         => Name,
          Target_Index => Path_Index,
          Target_Inode => Path_Inode.all,
          Parent_Inode => Parent_Inode.all,
          Success      => Success,
          Parent_Open  => Parent_Open);
      if not Success then
         Status := FS_Not_Found;
         goto Cleanup;
      elsif not Check_User_Access (User, Parent_Inode.all, False, True, False)
      then
         Status := FS_Not_Allowed;
         goto Cleanup;
      end if;

      Kind := Get_Inode_Type (Path_Inode.Permissions);
      if Do_Dirs then
         if Kind /= File_Directory then
            Status := FS_Not_Directory;
            goto Cleanup;
         end if;
         Is_Directory_Empty
            (FS_Data    => Data,
             Inode_Data => Path_Inode.all,
             Inode_Size => Get_Size (Path_Inode.all,
                                     Data.Has_64bit_Filesizes),
             Empty      => Empty,
             Success    => Success);
         if not Success then
            Status := FS_IO_Failure;
            goto Cleanup;
         elsif not Empty then
            Status := FS_Not_Empty;
            goto Cleanup;
         end if;
      elsif Kind = File_Directory then
         Status := FS_Is_Directory;
         goto Cleanup;
      end if;

      Stamp := Current_Epoch;
      Delete_Directory_Entry
         (FS_Data     => Data,
          Inode_Data  => Parent_Inode.all,
          Inode_Size  => Get_Size (Parent_Inode.all, Data.Has_64bit_Filesizes),
          Inode_Index => Unsigned_32 (Parent),
          Name        => Name,
          Deleted_Ino => Deleted,
          Success     => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      --  Now that no name is left pointing at it, hand the inode and every
      --  block it owns back to the filesystem. Not doing this used to leak
      --  the whole file on every single unlink.
      if Kind = File_Directory then
         Path_Inode.Hard_Link_Count := 0;
         Delete_Inode (Data, Path_Index, Path_Inode.all, Success);
         if Parent_Inode.Hard_Link_Count > 2 then
            Parent_Inode.Hard_Link_Count := Parent_Inode.Hard_Link_Count - 1;
         end if;
      elsif Path_Inode.Hard_Link_Count > 1 then
         Path_Inode.Hard_Link_Count := Path_Inode.Hard_Link_Count - 1;
         Path_Inode.Creation_Time_Epoch := Stamp;
         RW_Inode (Data, Path_Index, Path_Inode.all, True, Success);
      else
         Delete_Inode (Data, Path_Index, Path_Inode.all, Success);
      end if;
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      end if;

      Parent_Inode.Modified_Time_Epoch := Stamp;
      Parent_Inode.Creation_Time_Epoch := Stamp;
      RW_Inode
         (Data            => Data,
          Inode_Index     => Unsigned_32 (Parent),
          Result          => Parent_Inode.all,
          Write_Operation => True,
          Success         => Success);

      Status := (if Success then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Synchronization.Release_Writer (Data.Mutex);
      Free (Path_Inode);
      Free (Parent_Inode);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (Data.Mutex);
         Messages.Put_Line ("Exception while unlinking an EXT node");
         Status := FS_IO_Failure;
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
      Curr_Index, Next_Index, Entry_Count : Unsigned_64;
      Inode_Sz : Unsigned_64;
      Cursor   : Map_Cursor := Empty_Cursor;
      Entity   : Directory_Entity;
      Succ     : Boolean;
   begin
      Synchronization.Seize_Reader (FS.Mutex);

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
         Success := FS_Not_Directory;
         goto Cleanup;
      end if;

      Inode_Sz := Get_Size (Fetched_Inode.all, FS.Has_64bit_Filesizes);
      Curr_Index  := 0;
      Entry_Count := 0;
      Synchronization.Seize (FS.Memo_Lock);
      if FS.Memo_Inode = Unsigned_32 (Ino) and then
         FS.Memo_Index = Unsigned_64 (Offset)
      then
         Curr_Index  := FS.Memo_Offset;
         Entry_Count := Unsigned_64 (Offset);
      end if;
      Synchronization.Release (FS.Memo_Lock);

      loop
         exit when Entry_Count >= Unsigned_64 (Offset) and
                   Ret_Count >= Entities'Length;

         Inner_Read_Entry
            (FS_Data     => FS,
             Inode_Sz    => Inode_Sz,
             File_Ino    => Fetched_Inode.all,
             Inode_Index => Curr_Index,
             Cursor      => Cursor,
             Entity      => Entity,
             Next_Index  => Next_Index,
             Success     => Succ);
         exit when not Succ;

         if Entry_Count >= Unsigned_64 (Offset) then
            Entities (Entities'First + Ret_Count) := Entity;
            Ret_Count := Ret_Count + 1;
         end if;
         Entry_Count := Entry_Count + 1;
         Curr_Index  := Next_Index;
      end loop;

      Synchronization.Seize (FS.Memo_Lock);
      FS.Memo_Inode  := Unsigned_32 (Ino);
      FS.Memo_Index  := Entry_Count;
      FS.Memo_Offset := Curr_Index;
      Synchronization.Release (FS.Memo_Lock);

   <<Cleanup>>
      Close_Cursor (Cursor);
      Synchronization.Release_Reader (FS.Mutex);
      Free (Fetched_Inode);
   exception
      when Constraint_Error =>
         Close_Cursor (Cursor);
         Synchronization.Release_Reader (FS.Mutex);
         Messages.Put_Line ("Exception while reading EXT entries");
         Ret_Count := 0;
         Success   := FS_IO_Failure;
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
      Synchronization.Seize_Reader (FS.Mutex);
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
            (FS_Data   => FS,
             Ino       => Fetched_Inode.all,
             File_Size => Get_Size (Fetched_Inode.all, FS.Has_64bit_Filesizes),
             Path      => Path,
             Ret_Count => Ret_Count);
         Success := (if Ret_Count /= 0 then FS_Success else FS_IO_Failure);
      else
         Path      := [others => ' '];
         Ret_Count := 0;
         Success   := FS_Invalid_Value;
      end if;

      Synchronization.Release_Reader (FS.Mutex);
      Free (Fetched_Inode);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (FS.Mutex);
         Messages.Put_Line ("Exception while reading an EXT symlink");
         Path      := [others => ' '];
         Ret_Count := 0;
         Success   := FS_IO_Failure;
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
      Synchronization.Seize_Reader (FS.Mutex);

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
      Synchronization.Release_Reader (FS.Mutex);
      Free (Fetched_Inode);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (FS.Mutex);
         Messages.Put_Line ("Exception while reading EXT data");
         Data      := [others => 0];
         Ret_Count := 0;
         Success   := FS_IO_Failure;
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
      Synchronization.Seize_Writer (FS.Mutex);

      Ret_Count := 0;
      if FS.Is_Read_Only then
         Success := FS_RO_Failure;
         goto Cleanup;
      end if;

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Fetched_Inode.all,
          Write_Operation => False,
          Success         => Succ);
      if not Succ then
         Success := FS_IO_Failure;
         goto Cleanup;
      elsif Is_Immutable (Fetched_Inode.all) then
         Success := FS_RO_Failure;
         goto Cleanup;
      elsif Get_Inode_Type (Fetched_Inode.all.Permissions) /= File_Regular
      then
         Success := FS_Is_Directory;
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
      Synchronization.Release_Writer (FS.Mutex);
      Free (Fetched_Inode);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (FS.Mutex);
         Messages.Put_Line ("Exception while writing EXT data");
         Ret_Count := 0;
         Success   := FS_IO_Failure;
   end Write;

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status)
   is
      package Align is new Alignment (Unsigned_64);
      FS   : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Blk  : Unsigned_64;
      Size : Unsigned_64;
      Inod : Inode_Acc := new Inode;
      Succ : Boolean;
   begin
      Blk := Unsigned_64 (Get_Block_Size (FS.Handle));
      Synchronization.Seize_Reader (FS.Mutex);

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
             Hard_Link_Count   => Positive (Unsigned_16'Max
                                            (Inod.Hard_Link_Count, 1)),
             Byte_Size         => Size,
             IO_Block_Size     => Get_Block_Size (FS.Handle),
             IO_Block_Count    => Align.Divide_Round_Up (Size, Blk),
             Birth_Time        => (Unsigned_64 (Inod.Creation_Time_Epoch), 0),
             Modification_Time => (Unsigned_64 (Inod.Modified_Time_Epoch), 0),
             Access_Time       => (Unsigned_64 (Inod.Access_Time_Epoch),   0),
             Change_Time       => (Unsigned_64 (Inod.Creation_Time_Epoch), 0));
         Success := FS_Success;
      else
         Success := FS_IO_Failure;
      end if;

      Synchronization.Release_Reader (FS.Mutex);
      Free (Inod);
   exception
      when Constraint_Error =>
         Synchronization.Release_Reader (FS.Mutex);
         Messages.Put_Line ("Exception while doing an EXT stat");
         Success := FS_IO_Failure;
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
      Block_Sz     : Unsigned_64;
      Tail         : Unsigned_64;
      Zeroes       : Operation_Data_Acc := null;
      Ret_Count    : Natural;
      Success      : Boolean;
   begin
      Synchronization.Seize_Writer (FS.Mutex);
      Block_Sz := Unsigned_64 (FS.Block_Size);

      if FS.Is_Read_Only then
         Status := FS_RO_Failure;
         goto Cleanup;
      end if;

      RW_Inode
         (Data            => FS,
          Inode_Index     => Unsigned_32 (Ino),
          Result          => Fetched.all,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         Status := FS_IO_Failure;
         goto Cleanup;
      elsif Get_Inode_Type (Fetched.all.Permissions) /= File_Regular then
         Status := FS_Is_Directory;
         goto Cleanup;
      elsif Is_Immutable (Fetched.all) then
         Status := FS_RO_Failure;
         goto Cleanup;
      end if;

      Fetched_Size := Get_Size (Fetched.all, FS.Has_64bit_Filesizes);
      Success      := True;

      if Fetched_Size > New_Size then
         --  Wipe whatever is left over in the block the file now ends in,
         --  so that growing it again shows zeroes and not old contents.
         Tail := New_Size mod Block_Sz;
         if Tail /= 0 then
            Tail := Unsigned_64'Min (Block_Sz - Tail, Fetched_Size - New_Size);
            Zeroes := new Operation_Data'(1 .. Natural (Tail) => 0);
            Write_To_Inode
               (FS_Data    => FS,
                Inode_Data => Fetched.all,
                Inode_Num  => Unsigned_32 (Ino),
                Inode_Size => Fetched_Size,
                Offset     => New_Size,
                Data       => Zeroes.all,
                Ret_Count  => Ret_Count,
                Success    => Success);
            Free (Zeroes);
         end if;

         --  Then give back every block that is now past the end of the file.
         if Success then
            Free_Blocks_From
               (FS_Data    => FS,
                Inode_Data => Fetched.all,
                From_Block => (New_Size + Block_Sz - 1) / Block_Sz,
                Success    => Success);
         end if;
      end if;

      --  Growing needs nothing done to the blocks: the gap is a hole, which
      --  reads as zeroes and gets filled in when something writes to it.
      if Success and Fetched_Size /= New_Size then
         Set_Size (Fetched.all, New_Size, FS.Has_64bit_Filesizes, Success);
      end if;

      if Success then
         Fetched.Modified_Time_Epoch := Current_Epoch;
         Fetched.Creation_Time_Epoch := Fetched.Modified_Time_Epoch;
         RW_Inode
            (Data            => FS,
             Inode_Index     => Unsigned_32 (Ino),
             Result          => Fetched.all,
             Write_Operation => True,
             Success         => Success);
      end if;

      Status := (if Success then FS_Success else FS_IO_Failure);

   <<Cleanup>>
      Synchronization.Release_Writer (FS.Mutex);
      Free (Fetched);
      Free (Zeroes);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (FS.Mutex);
         Messages.Put_Line ("Exception while doing an EXT truncation");
         Status := FS_IO_Failure;
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
      case Req is
         when EXT_GETFLAGS =>
            Synchronization.Seize_Reader (FS.Mutex);
            RW_Inode
               (Data            => FS,
                Inode_Index     => Unsigned_32 (Ino),
                Result          => Inod.all,
                Write_Operation => False,
                Success         => Success);
            if Success then
               Flags  := Inod.Flags;
               Status := FS_Success;
            else
               Status := VFS.FS_IO_Failure;
            end if;
            Synchronization.Release_Reader (FS.Mutex);
         when EXT_SETFLAGS =>
            Synchronization.Seize_Writer (FS.Mutex);
            RW_Inode
               (Data            => FS,
                Inode_Index     => Unsigned_32 (Ino),
                Result          => Inod.all,
                Write_Operation => False,
                Success         => Success);
            if not Success then
               Status := VFS.FS_IO_Failure;
            elsif not FS.Is_Read_Only then
               --  The hash index bit is not the caller's to hand out: this
               --  driver keeps no index, so a directory carrying the bit
               --  would be searched through an index that does not exist.
               Inod.Flags := Flags and not Unsigned_32'(Flags_Hash_Index);
               Inod.Creation_Time_Epoch := Current_Epoch;
               RW_Inode
                  (Data            => FS,
                   Inode_Index     => Unsigned_32 (Ino),
                   Result          => Inod.all,
                   Write_Operation => True,
                   Success         => Success);
               Status := (if Success then FS_Success else FS_IO_Failure);
            else
               Status := FS_RO_Failure;
            end if;
            Synchronization.Release_Writer (FS.Mutex);
         when others =>
            Status := FS_Invalid_Value;
      end case;

      Free (Inod);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while doing an EXT ioctl");
         Status := FS_IO_Failure;
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
      Kind    : File_Type;
   begin
      Synchronization.Seize_Writer (FS.Mutex);

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
         Kind             := Get_Inode_Type (Inod.Permissions);
         Inod.Permissions := Get_Inode_Type (Kind, Mode);
         Inod.Creation_Time_Epoch := Current_Epoch;
         RW_Inode
            (Data            => FS,
             Inode_Index     => Unsigned_32 (Ino),
             Result          => Inod.all,
             Write_Operation => True,
             Success         => Success);
         Status := (if Success then FS_Success else FS_IO_Failure);
      end if;

   <<Cleanup>>
      Synchronization.Release_Writer (FS.Mutex);
      Free (Inod);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (FS.Mutex);
         Messages.Put_Line ("Exception while doing an EXT mode change");
         Status := FS_IO_Failure;
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
      Changed : Boolean := False;
   begin
      Synchronization.Seize_Writer (FS.Mutex);

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
         goto Cleanup;
      end if;

      --  Either of the two is left alone when it is does not fit per POSIX.
      if Owner <= Unsigned_32 (Unsigned_16'Last) then
         Inod.UID := Unsigned_16 (Owner);
         Changed  := True;
      end if;
      if Group <= Unsigned_32 (Unsigned_16'Last) then
         Inod.GID := Unsigned_16 (Group);
         Changed  := True;
      end if;

      if Changed then
         Inod.Creation_Time_Epoch := Current_Epoch;
         RW_Inode
            (Data            => FS,
             Inode_Index     => Unsigned_32 (Ino),
             Result          => Inod.all,
             Write_Operation => True,
             Success         => Success);
         Status := (if Success then FS_Success else FS_IO_Failure);
      else
         Status := FS_Success;
      end if;

   <<Cleanup>>
      Synchronization.Release_Writer (FS.Mutex);
      Free (Inod);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (FS.Mutex);
         Messages.Put_Line ("Exception while doing an EXT owner change");
         Status := FS_IO_Failure;
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
      Synchronization.Seize_Writer (FS.Mutex);

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
         Inod.Access_Time_Epoch   := Unsigned_32 (AS and 16#FFFFFFFF#);
         Inod.Modified_Time_Epoch := Unsigned_32 (MS and 16#FFFFFFFF#);
         Inod.Creation_Time_Epoch := Current_Epoch;
         RW_Inode
            (Data            => FS,
             Inode_Index     => Unsigned_32 (Ino),
             Result          => Inod.all,
             Write_Operation => True,
             Success         => Success);
         Status := (if Success then FS_Success else FS_IO_Failure);
      end if;

   <<Cleanup>>
      Synchronization.Release_Writer (FS.Mutex);
      Free (Inod);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (FS.Mutex);
         Messages.Put_Line ("Exception while doing an EXT access time change");
         Status := FS_IO_Failure;
   end Change_Access_Times;

   procedure Synchronize (Data : System.Address; Status : out FS_Status) is
      FS_Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Success : Boolean;
   begin
      Synchronization.Seize_Writer (FS_Data.Mutex);
      Sync_Superblock (FS_Data);
      Devices.Synchronize (FS_Data.Handle, Success);
      Status := (if Success then FS_Success else FS_IO_Failure);
      Synchronization.Release_Writer (FS_Data.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release_Writer (FS_Data.Mutex);
         Messages.Put_Line ("Exception while doing an EXT sync");
         Status := FS_IO_Failure;
   end Synchronize;

   procedure Synchronize
      (Data      : System.Address;
       Ino       : File_Inode_Number;
       Data_Only : Boolean;
       Status    : out FS_Status)
   is
      FS_Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (Data));
      Succ : Boolean;
      Offset : Unsigned_64;
   begin
      if Data_Only then
         Synchronization.Seize_Reader (FS_Data.Mutex);
         Get_Inode_Index (FS_Data, Unsigned_32 (Ino), Offset, Succ);
         if Succ then
            Devices.Synchronize
               (FS_Data.Handle, Offset, Unsigned_64 (FS_Data.Inode_Size),
                Succ);
            Status := (if Succ then FS_Success else FS_IO_Failure);
         else
            Status := FS_Invalid_Value;
         end if;
         Synchronization.Release_Reader (FS_Data.Mutex);
      else
         Synchronize (Data, Status);
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while doing an EXT partial sync");
         Status := FS_IO_Failure;
   end Synchronize;
   ----------------------------------------------------------------------------
   procedure Inner_Open_Inode
      (Data         : EXT_Data_Acc;
       Parent_Index : Unsigned_32;
       Name         : String;
       Target_Index : out Unsigned_32;
       Target_Inode : out Inode;
       Parent_Inode : out Inode;
       Success      : out Boolean;
       Parent_Open  : out Boolean)
   is
      Entity : Directory_Entity;
      Cursor : Map_Cursor := Empty_Cursor;
      Curr_Index, Next_Index, Parent_Sz : Unsigned_64;
   begin
      Target_Index := 0;
      Parent_Open  := False;

      RW_Inode
         (Data            => Data,
          Inode_Index     => Parent_Index,
          Result          => Parent_Inode,
          Write_Operation => False,
          Success         => Success);
      if not Success then
         return;
      elsif Get_Inode_Type (Parent_Inode.Permissions) /= File_Directory then
         Success := False;
         return;
      end if;

      Parent_Open := True;
      Parent_Sz   := Get_Size (Parent_Inode, Data.Has_64bit_Filesizes);
      Curr_Index  := 0;
      loop
         Inner_Read_Entry
            (FS_Data     => Data,
             Inode_Sz    => Parent_Sz,
             File_Ino    => Parent_Inode,
             Inode_Index => Curr_Index,
             Cursor      => Cursor,
             Entity      => Entity,
             Next_Index  => Next_Index,
             Success     => Success);
         if not Success then
            goto Failure_Return;
         end if;

         Curr_Index := Next_Index;

         if Entity.Name_Buffer (1 .. Entity.Name_Len) = Name then
            Target_Index := Unsigned_32 (Entity.Inode_Number);
            RW_Inode
               (Data            => Data,
                Inode_Index     => Target_Index,
                Result          => Target_Inode,
                Write_Operation => False,
                Success         => Success);
            if not Success then
               goto Failure_Return;
            end if;
            exit;
         end if;
      end loop;

      Close_Cursor (Cursor);
      Success := True;
      return;

   <<Failure_Return>>
      Close_Cursor (Cursor);
      Target_Index := 0;
      Success := False;
   exception
      when Constraint_Error =>
         Close_Cursor (Cursor);
         Messages.Put_Line ("Exception while opening an EXT inode");
         Target_Index := 0;
         Success      := False;
         Parent_Open  := False;
   end Inner_Open_Inode;

   procedure Is_Directory_Empty
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : Inode;
       Inode_Size : Unsigned_64;
       Empty      : out Boolean;
       Success    : out Boolean)
   is
      Cursor : Map_Cursor := Empty_Cursor;
      Curr_Index, Next_Index : Unsigned_64 := 0;
      Entity : Directory_Entity;
      Succ   : Boolean;
   begin
      Empty   := True;
      Success := True;

      loop
         Inner_Read_Entry
            (FS_Data     => FS_Data,
             Inode_Sz    => Inode_Size,
             File_Ino    => Inode_Data,
             Inode_Index => Curr_Index,
             Cursor      => Cursor,
             Entity      => Entity,
             Next_Index  => Next_Index,
             Success     => Succ);
         exit when not Succ;

         if Entity.Name_Buffer (1 .. Entity.Name_Len) /= "." and then
            Entity.Name_Buffer (1 .. Entity.Name_Len) /= ".."
         then
            Empty := False;
            exit;
         end if;
         Curr_Index := Next_Index;
      end loop;

      Close_Cursor (Cursor);
   exception
      when Constraint_Error =>
         Close_Cursor (Cursor);
         Messages.Put_Line ("Exception while checking an EXT directory");
         Empty   := False;
         Success := False;
   end Is_Directory_Empty;

   procedure Inner_Read_Symbolic_Link
      (FS_Data   : EXT_Data_Acc;
       Ino       : Inode;
       File_Size : Unsigned_64;
       Path      : out String;
       Ret_Count : out Natural)
   is
      Success      : Boolean;
      Final_Length : Natural;
   begin
      Path := [others => ' '];
      if File_Size >= Unsigned_64 (Path'Length) then
         Final_Length := Path'Length;
      else
         Final_Length := Natural (File_Size);
      end if;
      if Final_Length = 0 then
         Ret_Count := 0;
         return;
      end if;

      if Is_Fast_Symlink (Ino) then
         declare
            Str_Data : Operation_Data (1 .. Ino.Blocks'Length * 4)
               with Import, Address => Ino.Blocks'Address;
         begin
            if Final_Length > Str_Data'Length then
               Final_Length := Str_Data'Length;
            end if;
            for I in 1 .. Final_Length loop
               Path (Path'First + I - 1) := Character'Val (Str_Data (I));
            end loop;
         end;
      else
         declare
            Str_Data : Operation_Data (1 .. Final_Length)
               with Import, Address => Path (Path'First)'Address;
         begin
            Read_From_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Ino,
                Inode_Size => File_Size,
                Offset     => 0,
                Data       => Str_Data,
                Ret_Count  => Final_Length,
                Success    => Success);
            if not Success then
               Ret_Count := 0;
               return;
            end if;
         end;
      end if;

      Ret_Count := Final_Length;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while reading an EXT symbolic link");
         Ret_Count := 0;
   end Inner_Read_Symbolic_Link;

   procedure Inner_Read_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Sz    : Unsigned_64;
       File_Ino    : Inode;
       Inode_Index : Unsigned_64;
       Cursor      : in out Map_Cursor;
       Entity      : out Directory_Entity;
       Next_Index  : out Unsigned_64;
       Success     : out Boolean)
   is
      Block_Sz  : Unsigned_64;
      Offset    : Unsigned_64 := Inode_Index;
      Window    : Operation_Data (1 .. Max_Dir_Entry_Size);
      Avail     : Unsigned_64;
      Ret_Count : Natural;
      Ino_Num   : Unsigned_32;
      Rec_Len, Name_Len, Copied : Natural;
      Kind_Byte : Unsigned_8;
      Succ      : Boolean;
   begin
      Block_Sz   := Unsigned_64 (FS_Data.Block_Size);
      Entity     := (0, [others => ' '], 0, File_Regular);
      Next_Index := 0;
      Success    := False;

      while Offset < Inode_Sz loop
         --  A directory record never straddles a block, so a window that
         --  reaches either the end of the block or the longest possible
         --  record always holds the whole of it. Fetching header and name
         --  together is nice.
         Avail := Unsigned_64'Min
            (Unsigned_64 (Max_Dir_Entry_Size),
             Unsigned_64'Min (Block_Sz - (Offset mod Block_Sz),
                              Inode_Sz - Offset));
         exit when Avail < Unsigned_64 (Dir_Entry_Header);

         Read_From_Inode
            (FS_Data    => FS_Data,
             Inode_Data => File_Ino,
             Inode_Size => Inode_Sz,
             Offset     => Offset,
             Data       => Window (1 .. Natural (Avail)),
             Cursor     => Cursor,
             Ret_Count  => Ret_Count,
             Success    => Succ);
         exit when not Succ or else Ret_Count /= Natural (Avail);

         Get_Dir_Entry
            (Buffer   => Window (1 .. Natural (Avail)),
             Offset   => 0,
             Has_Type => FS_Data.Has_Directory_Types,
             Ino      => Ino_Num,
             Rec_Len  => Rec_Len,
             Name_Len => Name_Len,
             Kind     => Kind_Byte);

         --  A record that is too short to hold a header, is not a multiple
         --  of four, or reaches out of its own block cannot be stepped over
         --  safely, so the walk has to stop instead of guessing.
         exit when Rec_Len < Dir_Entry_Header or else
                   (Rec_Len mod 4) /= 0       or else
                   (Offset mod Block_Sz) + Unsigned_64 (Rec_Len) > Block_Sz;

         if Ino_Num /= 0 and then Name_Len /= 0 and then
            Dir_Entry_Header + Name_Len <= Rec_Len and then
            Dir_Entry_Header + Name_Len <= Natural (Avail)
         then
            Copied := Natural'Min (Name_Len, Entity.Name_Buffer'Length);
            Entity :=
               (Inode_Number => Unsigned_64 (Ino_Num),
                Name_Buffer  => [others => ' '],
                Name_Len     => Copied,
                Type_Of_File =>
                  (if FS_Data.Has_Directory_Types
                   then Get_Dir_Type (Kind_Byte) else File_Regular));
            for I in 1 .. Copied loop
               Entity.Name_Buffer (I) :=
                  Character'Val (Window (Dir_Entry_Header + I));
            end loop;

            Next_Index := Offset + Unsigned_64 (Rec_Len);
            Success    := True;
            return;
         end if;

         --  A record with no inode is how ext marks free room inside a
         --  directory.
         Offset := Offset + Unsigned_64 (Rec_Len);
      end loop;

      Next_Index := 0;
      Success    := False;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while reading an EXT entry");
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
      Succ       : Devices.Dev_Status;
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
             Success   => Succ);
      else
         Devices.Read
            (Handle    => Handle,
             Offset    => Offset,
             Data      => Super_Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
      end if;

      Success := (Succ = Devices.Dev_Success) and
                 (Ret_Count = Super_Data'Length);
   end RW_Superblock;

   procedure Sync_Superblock (Data : EXT_Data_Acc) is
      Success : Boolean;
   begin
      if Data.Is_Read_Only then
         return;
      end if;

      Data.Super.Last_Write_Epoch := Current_Epoch;
      RW_Superblock
         (Handle          => Data.Handle,
          Offset          => Main_Superblock_Offset,
          Super           => Data.Super,
          Write_Operation => True,
          Success         => Success);
      if not Success then
         Act_On_Policy (Data, "superblock write error");
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while syncing an EXT superblock");
   end Sync_Superblock;

   procedure RW_Block_Group_Descriptor
      (Data             : EXT_Data_Acc;
       Descriptor_Index : Unsigned_32;
       Result           : in out Block_Group_Descriptor;
       Write_Operation  : Boolean;
       Success          : out Boolean)
   is
      Succ        : Devices.Dev_Status;
      Descr_Size  : constant Unsigned_64 := Block_Group_Descriptor'Size / 8;
      Offset      : Unsigned_64;
      Ret_Count   : Natural;
      Result_Data : Operation_Data (1 .. Natural (Descr_Size))
         with Import, Address => Result'Address;
   begin
      if Descriptor_Index >= Data.Block_Group_Count then
         Success := False;
         return;
      end if;

      Offset := (Unsigned_64 (Data.First_Data_Block) + 1) *
                Unsigned_64 (Data.Block_Size) +
                Descr_Size * Unsigned_64 (Descriptor_Index);

      if Write_Operation then
         Devices.Write
            (Handle    => Data.Handle,
             Offset    => Offset,
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
      else
         Devices.Read
            (Handle    => Data.Handle,
             Offset    => Offset,
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
      end if;

      if (Succ = Devices.Dev_Success) and (Ret_Count = Result_Data'Length) then
         Success := True;
      else
         Act_On_Policy (Data, "block group descriptor RW failure");
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while doing R/W on an EXT block group");
         Success := False;
   end RW_Block_Group_Descriptor;

   procedure Get_Inode_Index
      (Data        : EXT_Data_Acc;
       Inode_Index : Unsigned_32;
       Result      : out Unsigned_64;
       Success     : out Boolean)
   is
      Table_Index, Descriptor_Index : Unsigned_32;
      Block_Descriptor : Block_Group_Descriptor;
   begin
      Result := 0;

      if Inode_Index < 1 or else Inode_Index > Data.Super.Inode_Count then
         Success := False;
         return;
      end if;

      Table_Index      := (Inode_Index - 1) mod Data.Super.Inodes_Per_Group;
      Descriptor_Index := (Inode_Index - 1) / Data.Super.Inodes_Per_Group;

      RW_Block_Group_Descriptor
         (Data             => Data,
          Descriptor_Index => Descriptor_Index,
          Result           => Block_Descriptor,
          Write_Operation  => False,
          Success          => Success);
      if Success then
         Result :=
            Unsigned_64 (Block_Descriptor.Inode_Table_Block) *
            Unsigned_64 (Data.Block_Size) + Unsigned_64 (Table_Index) *
            Unsigned_64 (Data.Inode_Size);
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while getting index of an EXT inode");
         Result := 0;
         Success := False;
   end Get_Inode_Index;

   procedure RW_Inode
      (Data            : EXT_Data_Acc;
       Inode_Index     : Unsigned_32;
       Result          : in out Inode;
       Write_Operation : Boolean;
       Success         : out Boolean)
   is
      Succ        : Devices.Dev_Status;
      Offset      : Unsigned_64;
      Ret_Count   : Natural;
      Result_Data : Operation_Data (1 .. Inode'Size / 8)
         with Import, Address => Result'Address;
   begin
      Get_Inode_Index
         (Data        => Data,
          Inode_Index => Inode_Index,
          Result      => Offset,
          Success     => Success);
      if not Success then
         return;
      end if;

      if Write_Operation then
         Devices.Write
            (Handle    => Data.Handle,
             Offset    => Offset,
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
      else
         Devices.Read
            (Handle    => Data.Handle,
             Offset    => Offset,
             Data      => Result_Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
      end if;
      if (Succ = Devices.Dev_Success) and (Ret_Count = Result_Data'Length) then
         Success := True;
      else
         Act_On_Policy (Data, "inode RW failure");
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while doing R/W on an EXT inode");
         Success := False;
   end RW_Inode;

   procedure Close_Cursor (Cursor : in out Map_Cursor) is
   begin
      Free (Cursor.L1_Data);
      Free (Cursor.L2_Data);
      Cursor.L1_Block := 0;
      Cursor.L2_Block := 0;
   end Close_Cursor;

   procedure Read_Pointer_Block
      (FS_Data : EXT_Data_Acc;
       Block   : Unsigned_32;
       Buffer  : in out Operation_Data_Acc;
       Cached  : in out Unsigned_32;
       Success : out Boolean)
   is
      Ret_Count : Natural;
      Succ      : Devices.Dev_Status;
   begin
      if Block = 0 then
         Success := False;
         return;
      elsif Buffer /= null and then Cached = Block then
         Success := True;
         return;
      end if;

      if Buffer = null then
         Buffer := new Operation_Data (1 .. Natural (FS_Data.Block_Size));
      end if;

      Cached := 0;
      Devices.Read
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Block) * Unsigned_64 (FS_Data.Block_Size),
          Data      => Buffer.all,
          Ret_Count => Ret_Count,
          Success   => Succ);
      if Succ /= Devices.Dev_Success or else Ret_Count /= Buffer'Length then
         Success := False;
         return;
      end if;

      Cached  := Block;
      Success := True;
   exception
      when Constraint_Error =>
         Cached  := 0;
         Success := False;
   end Read_Pointer_Block;

   function Get_Pointer
      (Buffer : Operation_Data;
       Index  : Unsigned_32) return Unsigned_32
   is
      Base : Natural;
   begin
      Base := Buffer'First + Natural (Index) * 4;
      return Unsigned_32 (Buffer (Base))                    or
             Shift_Left (Unsigned_32 (Buffer (Base + 1)),  8) or
             Shift_Left (Unsigned_32 (Buffer (Base + 2)), 16) or
             Shift_Left (Unsigned_32 (Buffer (Base + 3)), 24);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Pointer;

   procedure Set_Pointer
      (Buffer : in out Operation_Data;
       Index  : Unsigned_32;
       Value  : Unsigned_32)
   is
      Base : Natural;
   begin
      Base := Buffer'First + Natural (Index) * 4;
      Buffer (Base)     := Unsigned_8 (Value and 16#FF#);
      Buffer (Base + 1) := Unsigned_8 (Shift_Right (Value,  8) and 16#FF#);
      Buffer (Base + 2) := Unsigned_8 (Shift_Right (Value, 16) and 16#FF#);
      Buffer (Base + 3) := Unsigned_8 (Shift_Right (Value, 24) and 16#FF#);
   exception
      when Constraint_Error =>
         null;
   end Set_Pointer;

   procedure Fetch_Pointer
      (FS_Data : EXT_Data_Acc;
       Block   : Unsigned_32;
       Index   : Unsigned_64;
       Value   : out Unsigned_32;
       Success : out Boolean)
   is
      Raw       : Operation_Data (1 .. 4);
      Ret_Count : Natural;
      Succ      : Devices.Dev_Status;
   begin
      Value := 0;
      if Block = 0 then
         Success := False;
         return;
      end if;

      Devices.Read
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Block) * Unsigned_64 (FS_Data.Block_Size) +
                       Index * 4,
          Data      => Raw,
          Ret_Count => Ret_Count,
          Success   => Succ);
      if Succ /= Devices.Dev_Success or else Ret_Count /= Raw'Length then
         Success := False;
         return;
      end if;

      Value   := Get_Pointer (Raw, 0);
      Success := True;
   exception
      when Constraint_Error =>
         Value   := 0;
         Success := False;
   end Fetch_Pointer;

   procedure Put_Pointer
      (FS_Data : EXT_Data_Acc;
       Block   : Unsigned_32;
       Index   : Unsigned_64;
       Value   : Unsigned_32;
       Success : out Boolean)
   is
      Raw       : Operation_Data (1 .. 4) := [others => 0];
      Ret_Count : Natural;
      Succ      : Devices.Dev_Status;
   begin
      if Block = 0 then
         Success := False;
         return;
      end if;

      Set_Pointer (Raw, 0, Value);
      Devices.Write
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Block) * Unsigned_64 (FS_Data.Block_Size) +
                       Index * 4,
          Data      => Raw,
          Ret_Count => Ret_Count,
          Success   => Succ);
      Success := Succ = Devices.Dev_Success and then Ret_Count = Raw'Length;
   exception
      when Constraint_Error =>
         Success := False;
   end Put_Pointer;

   procedure Get_Block_Index
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : Inode;
       Searched   : Unsigned_32;
       Cursor     : in out Map_Cursor;
       Result     : out Unsigned_32;
       Success    : out Boolean)
   is
      Per_Blk : Unsigned_64;
      Idx     : Unsigned_64;
      Mid     : Unsigned_32;
      Succ    : Boolean;
   begin
      Per_Blk := Unsigned_64 (FS_Data.Pointers_Per_Block);
      Idx     := Unsigned_64 (Searched);
      Result  := 0;
      Success := True;

      --  Twelve blocks are named by the inode itself.
      if Idx < 12 then
         Result := Inode_Data.Blocks (Natural (Idx));
         return;
      end if;
      Idx := Idx - 12;

      --  One block of pointers to blocks.
      if Idx < Per_Blk then
         if Inode_Data.Blocks (12) = 0 then
            return;
         end if;
         Read_Pointer_Block (FS_Data, Inode_Data.Blocks (12),
                             Cursor.L1_Data, Cursor.L1_Block, Succ);
         if not Succ then
            Success := False;
            return;
         end if;
         Result := Get_Pointer (Cursor.L1_Data.all, Unsigned_32 (Idx));
         return;
      end if;
      Idx := Idx - Per_Blk;

      --  One block of pointers to blocks of pointers to blocks.
      if Idx < Per_Blk * Per_Blk then
         if Inode_Data.Blocks (13) = 0 then
            return;
         end if;
         Read_Pointer_Block (FS_Data, Inode_Data.Blocks (13),
                             Cursor.L2_Data, Cursor.L2_Block, Succ);
         if not Succ then
            Success := False;
            return;
         end if;
         Mid := Get_Pointer (Cursor.L2_Data.all, Unsigned_32 (Idx / Per_Blk));
         if Mid = 0 then
            return;
         end if;
         Read_Pointer_Block (FS_Data, Mid, Cursor.L1_Data, Cursor.L1_Block,
                             Succ);
         if not Succ then
            Success := False;
            return;
         end if;
         Result := Get_Pointer (Cursor.L1_Data.all,
                                Unsigned_32 (Idx mod Per_Blk));
         return;
      end if;
      Idx := Idx - Per_Blk * Per_Blk;

      --  And one more level on top of that. The topmost block only changes
      --  once every Per_Blk squared blocks, so it is fetched a pointer at a
      --  time and both cached levels are left to the busy ones.
      if Idx < Per_Blk * Per_Blk * Per_Blk then
         if Inode_Data.Blocks (14) = 0 then
            return;
         end if;
         Fetch_Pointer (FS_Data, Inode_Data.Blocks (14),
                        Idx / (Per_Blk * Per_Blk), Mid, Succ);
         if not Succ then
            Success := False;
            return;
         elsif Mid = 0 then
            return;
         end if;
         Read_Pointer_Block (FS_Data, Mid, Cursor.L2_Data, Cursor.L2_Block,
                             Succ);
         if not Succ then
            Success := False;
            return;
         end if;
         Mid := Get_Pointer (Cursor.L2_Data.all,
                             Unsigned_32 ((Idx / Per_Blk) mod Per_Blk));
         if Mid = 0 then
            return;
         end if;
         Read_Pointer_Block (FS_Data, Mid, Cursor.L1_Data, Cursor.L1_Block,
                             Succ);
         if not Succ then
            Success := False;
            return;
         end if;
         Result := Get_Pointer (Cursor.L1_Data.all,
                                Unsigned_32 (Idx mod Per_Blk));
         return;
      end if;

      --  Past what the format can address at all.
      Success := False;
   exception
      when Constraint_Error =>
         Result  := 0;
         Success := False;
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
      Cursor : Map_Cursor := Empty_Cursor;
   begin
      Read_From_Inode
         (FS_Data    => FS_Data,
          Inode_Data => Inode_Data,
          Inode_Size => Inode_Size,
          Offset     => Offset,
          Data       => Data,
          Cursor     => Cursor,
          Ret_Count  => Ret_Count,
          Success    => Success);
      Close_Cursor (Cursor);
   end Read_From_Inode;

   procedure Read_From_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : Inode;
       Inode_Size  : Unsigned_64;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Cursor      : in out Map_Cursor;
       Ret_Count   : out Natural;
       Success     : out Boolean)
   is
      Block_Sz     : Unsigned_64;
      Succ         : Devices.Dev_Status;
      Ok           : Boolean;
      Final_Count  : Natural;
      Done         : Natural := 0;
      Dev_Count    : Natural;
      Count        : Natural;
      Pos, In_Blk, Chunk, Run, Want : Unsigned_64;
      Logical      : Unsigned_64;
      Phys, Next_P : Unsigned_32;
   begin
      Block_Sz  := Unsigned_64 (FS_Data.Block_Size);
      Ret_Count := 0;
      Success   := True;
      if Data'Length = 0 or else Offset >= Inode_Size then
         return;
      end if;

      Final_Count := Data'Length;
      if Unsigned_64 (Data'Length) > Inode_Size - Offset then
         Final_Count := Natural (Inode_Size - Offset);
      end if;

      while Done < Final_Count loop
         Pos     := Offset + Unsigned_64 (Done);
         Logical := Pos / Block_Sz;
         In_Blk  := Pos mod Block_Sz;
         if Logical > Unsigned_64 (Unsigned_32'Last) then
            Success := False;
            exit;
         end if;

         Get_Block_Index
            (FS_Data, Inode_Data, Unsigned_32 (Logical), Cursor, Phys, Ok);
         if not Ok then
            Success := False;
            exit;
         end if;

         --  Walk forward while the blocks stay next to each other on the
         --  device, so that a run of them is fetched with one operation
         --  instead of one per block.
         Want := (In_Blk + Unsigned_64 (Final_Count - Done) + Block_Sz - 1) /
                 Block_Sz;
         Run  := 1;
         while Run < Want and then Logical + Run <=
               Unsigned_64 (Unsigned_32'Last)
         loop
            Get_Block_Index
               (FS_Data, Inode_Data, Unsigned_32 (Logical + Run), Cursor,
                Next_P, Ok);
            exit when not Ok;
            if Phys = 0 then
               exit when Next_P /= 0;
            else
               exit when Unsigned_64 (Next_P) /= Unsigned_64 (Phys) + Run;
            end if;
            Run := Run + 1;
         end loop;

         Chunk := Run * Block_Sz - In_Blk;
         if Chunk > Unsigned_64 (Final_Count - Done) then
            Chunk := Unsigned_64 (Final_Count - Done);
         end if;
         Count := Natural (Chunk);

         if Phys = 0 then
            Data (Data'First + Done .. Data'First + Done + Count - 1) :=
               [others => 0];
         else
            Devices.Read
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Phys) * Block_Sz + In_Blk,
                Data      => Data (Data'First + Done ..
                                   Data'First + Done + Count - 1),
                Ret_Count => Dev_Count,
                Success   => Succ);
            if Succ /= Devices.Dev_Success or else Dev_Count /= Count then
               Act_On_Policy (FS_Data, "error reading an inode");
               Success := False;
               exit;
            end if;
         end if;

         Done := Done + Count;
      end loop;

      Ret_Count := (if Success then Done else 0);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while reading from an EXT inode");
         Ret_Count := 0;
         Success   := False;
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
      Cursor : Map_Cursor := Empty_Cursor;
   begin
      Write_To_Inode
         (FS_Data    => FS_Data,
          Inode_Data => Inode_Data,
          Inode_Num  => Inode_Num,
          Inode_Size => Inode_Size,
          Offset     => Offset,
          Data       => Data,
          Cursor     => Cursor,
          Ret_Count  => Ret_Count,
          Success    => Success);
      Close_Cursor (Cursor);
   end Write_To_Inode;

   procedure Write_To_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Inode_Size  : Unsigned_64;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Cursor      : in out Map_Cursor;
       Ret_Count   : out Natural;
       Success     : out Boolean)
   is
      Block_Sz     : Unsigned_64;
      Goal         : Unsigned_32;
      Succ         : Devices.Dev_Status;
      Ok           : Boolean;
      Done         : Natural := 0;
      Dev_Count    : Natural;
      Count        : Natural;
      Pos, In_Blk, Chunk, Run, Want : Unsigned_64;
      Logical      : Unsigned_64;
      Head_Blk, Tail_Blk, Head_Off, Tail_Off : Unsigned_64;
      Head_New     : Boolean := False;
      Tail_New     : Boolean := False;
      Phys, Next_P : Unsigned_32;
   begin
      Block_Sz  := Unsigned_64 (FS_Data.Block_Size);
      Goal      := (if Inode_Num > 0
                    then (Inode_Num - 1) / FS_Data.Super.Inodes_Per_Group
                    else 0);
      Ret_Count := 0;
      Success   := True;
      if Data'Length = 0 then
         return;
      end if;

      --  Anything a directory holds may have moved, so the shortcut that
      --  remembers where a scan stopped cannot be trusted any more.
      Invalidate_Memo (FS_Data);

      --  Move the end of the file first, so that a failure halfway through
      --  leaves a size that the blocks below it can back.
      if Offset + Unsigned_64 (Data'Length) > Inode_Size then
         Set_Size
            (Ino        => Inode_Data,
             New_Size   => Offset + Unsigned_64 (Data'Length),
             Is_64_Bits => FS_Data.Has_64bit_Filesizes,
             Success    => Ok);
         if not Ok then
            Success := False;
            goto Finish;
         end if;
      end if;

      --  A block the allocator hands out still holds whatever the file that
      --  owned it last left in it. Whatever this write does not cover in a
      --  block it has just been given therefore has to be wiped: otherwise
      --  extending the file later shows the tail of a deleted one, and a
      --  symlink, whose target is read up to the first NUL rather than by
      --  size, reads back with rubbish stuck to the end of it. Note which
      --  edge blocks are missing now, before they are handed out.
      Head_Blk := Offset / Block_Sz;
      Head_Off := Offset mod Block_Sz;
      Tail_Blk := (Offset + Unsigned_64 (Data'Length) - 1) / Block_Sz;
      Tail_Off := (Offset + Unsigned_64 (Data'Length)) mod Block_Sz;
      if Head_Off /= 0 and then Head_Blk <= Unsigned_64 (Unsigned_32'Last) then
         Get_Block_Index
            (FS_Data, Inode_Data, Unsigned_32 (Head_Blk), Cursor, Phys, Ok);
         Head_New := Ok and then Phys = 0;
      end if;
      if Tail_Off /= 0 and then Tail_Blk <= Unsigned_64 (Unsigned_32'Last) then
         Get_Block_Index
            (FS_Data, Inode_Data, Unsigned_32 (Tail_Blk), Cursor, Phys, Ok);
         Tail_New := Ok and then Phys = 0;
      end if;

      --  Ask for every block the write needs at once. Doing it one block at
      --  a time reads and rewrites a whole bitmap per block, and hands out
      --  blocks that need not be next to each other; in one go the bitmap is
      --  touched once and the file comes out laid contiguously, which is
      --  what lets reads of it afterwards be merged.
      Grow_Inode
         (FS_Data    => FS_Data,
          Inode_Data => Inode_Data,
          Inode_Num  => Inode_Num,
          Start      => Offset,
          Count      => Unsigned_64 (Data'Length),
          Success    => Ok);
      if not Ok then
         Success := False;
         goto Finish;
      end if;
      Cursor.L1_Block := 0;
      Cursor.L2_Block := 0;

      if Head_New then
         Zero_Inode_Part
            (FS_Data, Inode_Data, Head_Blk, 0, Head_Off, Cursor, Ok);
         if not Ok then
            Success := False;
            goto Finish;
         end if;
      end if;
      if Tail_New then
         Zero_Inode_Part
            (FS_Data, Inode_Data, Tail_Blk, Tail_Off, Block_Sz, Cursor, Ok);
         if not Ok then
            Success := False;
            goto Finish;
         end if;
      end if;

      while Done < Data'Length loop
         Pos     := Offset + Unsigned_64 (Done);
         Logical := Pos / Block_Sz;
         In_Blk  := Pos mod Block_Sz;
         if Logical > Unsigned_64 (Unsigned_32'Last) then
            Success := False;
            exit;
         end if;

         Get_Block_Index
            (FS_Data, Inode_Data, Unsigned_32 (Logical), Cursor, Phys, Ok);
         if not Ok then
            Success := False;
            exit;
         end if;

         --  Blocks are given out here rather than up front, which is what
         --  makes writing into the middle of a hole work at all instead of
         --  landing on block zero.
         if Phys = 0 then
            Allocate_Block_For_Inode (FS_Data, Inode_Data, Goal, Phys, Ok);
            if not Ok then
               Success := False;
               exit;
            end if;
            Wire_Inode_Blocks
               (FS_Data, Inode_Data, Unsigned_32 (Logical), Phys, Cursor, Ok);
            if not Ok then
               Success := False;
               exit;
            end if;
         end if;

         Want := (In_Blk + Unsigned_64 (Data'Length - Done) + Block_Sz - 1) /
                 Block_Sz;
         Run  := 1;
         while Run < Want and then Logical + Run <=
               Unsigned_64 (Unsigned_32'Last)
         loop
            Get_Block_Index
               (FS_Data, Inode_Data, Unsigned_32 (Logical + Run), Cursor,
                Next_P, Ok);
            exit when not Ok or else Next_P = 0 or else
                      Unsigned_64 (Next_P) /= Unsigned_64 (Phys) + Run;
            Run := Run + 1;
         end loop;

         Chunk := Run * Block_Sz - In_Blk;
         if Chunk > Unsigned_64 (Data'Length - Done) then
            Chunk := Unsigned_64 (Data'Length - Done);
         end if;
         Count := Natural (Chunk);

         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Phys) * Block_Sz + In_Blk,
             Data      => Data (Data'First + Done ..
                                Data'First + Done + Count - 1),
             Ret_Count => Dev_Count,
             Success   => Succ);
         if Succ /= Devices.Dev_Success or else Dev_Count /= Count then
            Success := False;
            exit;
         end if;

         Done := Done + Count;
      end loop;

   <<Finish>>
      Inode_Data.Modified_Time_Epoch := Current_Epoch;
      Inode_Data.Creation_Time_Epoch := Inode_Data.Modified_Time_Epoch;
      RW_Inode
         (Data            => FS_Data,
          Inode_Index     => Inode_Num,
          Result          => Inode_Data,
          Write_Operation => True,
          Success         => Ok);

      if not Success or not Ok then
         Act_On_Policy (FS_Data, "error while writing to an inode");
         Success   := False;
         Ret_Count := 0;
      else
         Ret_Count := Done;
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while writing to an EXT inode");
         Ret_Count := 0;
         Success   := False;
   end Write_To_Inode;

   procedure Patch_Cursor
      (Cursor : in out Map_Cursor;
       Block  : Unsigned_32;
       Index  : Unsigned_64;
       Value  : Unsigned_32)
   is
   begin
      --  Keep a cached copy of a pointer block in step with a write that
      --  just went to the device, so that a run of allocations does not
      --  throw away and refetch the very block it keeps writing to.
      if Block /= 0 and then Cursor.L1_Data /= null and then
         Cursor.L1_Block = Block
      then
         Set_Pointer (Cursor.L1_Data.all, Unsigned_32 (Index), Value);
      end if;
      if Block /= 0 and then Cursor.L2_Data /= null and then
         Cursor.L2_Block = Block
      then
         Set_Pointer (Cursor.L2_Data.all, Unsigned_32 (Index), Value);
      end if;
   exception
      when Constraint_Error =>
         Cursor.L1_Block := 0;
         Cursor.L2_Block := 0;
   end Patch_Cursor;

   procedure Drop_Cursor_Block
      (Cursor : in out Map_Cursor;
       Block  : Unsigned_32)
   is
   begin
      if Cursor.L1_Block = Block then
         Cursor.L1_Block := 0;
      end if;
      if Cursor.L2_Block = Block then
         Cursor.L2_Block := 0;
      end if;
   end Drop_Cursor_Block;

   procedure Zero_Out_Block
      (FS_Data : EXT_Data_Acc;
       Block   : Unsigned_32;
       Success : out Boolean)
   is
      Buffer    : Operation_Data_Acc;
      Ret_Count : Natural;
      Succ      : Devices.Dev_Status;
   begin
      if Block = 0 then
         Success := False;
         return;
      end if;

      Buffer := new Operation_Data'(1 .. Natural (FS_Data.Block_Size) => 0);
      Devices.Write
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Block) * Unsigned_64 (FS_Data.Block_Size),
          Data      => Buffer.all,
          Ret_Count => Ret_Count,
          Success   => Succ);
      Success := Succ = Devices.Dev_Success and then
                 Ret_Count = Buffer'Length;
      Free (Buffer);
   exception
      when Constraint_Error =>
         Free (Buffer);
         Success := False;
   end Zero_Out_Block;

   procedure Zero_Inode_Part
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : Inode;
       Logical    : Unsigned_64;
       From       : Unsigned_64;
       To         : Unsigned_64;
       Cursor     : in out Map_Cursor;
       Success    : out Boolean)
   is
      Block_Sz  : Unsigned_64;
      Buffer    : Operation_Data_Acc := null;
      Ret_Count : Natural;
      Succ      : Devices.Dev_Status;
      Phys      : Unsigned_32;
   begin
      Block_Sz := Unsigned_64 (FS_Data.Block_Size);
      Success  := True;
      if From >= To or else To > Block_Sz or else
         Logical > Unsigned_64 (Unsigned_32'Last)
      then
         return;
      end if;

      Get_Block_Index
         (FS_Data, Inode_Data, Unsigned_32 (Logical), Cursor, Phys, Success);
      if not Success or else Phys = 0 then
         return;
      end if;

      Buffer := new Operation_Data'(1 .. Natural (To - From) => 0);
      Devices.Write
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Phys) * Block_Sz + From,
          Data      => Buffer.all,
          Ret_Count => Ret_Count,
          Success   => Succ);
      Success := Succ = Devices.Dev_Success and then
                 Ret_Count = Buffer'Length;
      Free (Buffer);
   exception
      when Constraint_Error =>
         Free (Buffer);
         Success := False;
   end Zero_Inode_Part;

   procedure Grow_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start       : Unsigned_64;
       Count       : Unsigned_64;
       Success     : out Boolean)
   is
      Block_Sz    : Unsigned_64;
      First, Last : Unsigned_64;
   begin
      Block_Sz := Unsigned_64 (FS_Data.Block_Size);

      if Count = 0 then
         Success := True;
         return;
      end if;

      First := Start / Block_Sz;
      Last  := (Start + Count - 1) / Block_Sz;
      if Last > Unsigned_64 (Unsigned_32'Last) then
         Success := False;
         return;
      end if;

      Assign_Inode_Blocks
         (FS_Data     => FS_Data,
          Inode_Data  => Inode_Data,
          Inode_Num   => Inode_Num,
          Start_Blk   => Unsigned_32 (First),
          Block_Count => Unsigned_32 (Last - First + 1),
          Success     => Success);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while growing an EXT inode");
         Success := False;
   end Grow_Inode;

   procedure Assign_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start_Blk   : Unsigned_32;
       Block_Count : Unsigned_32;
       Success     : out Boolean)
   is
      Goal       : Unsigned_32;
      Per_Sector : Unsigned_32;
      Cursor     : Map_Cursor := Empty_Cursor;
      Index, Need, Got, First, Blk : Unsigned_32;
   begin
      Goal := (if Inode_Num > 0
               then (Inode_Num - 1) / FS_Data.Super.Inodes_Per_Group
               else 0);
      Per_Sector := FS_Data.Block_Size / Sector_Unit;

      Success := True;
      if Block_Count = 0 then
         return;
      end if;

      Index := 0;
      while Index < Block_Count loop
         Get_Block_Index
            (FS_Data, Inode_Data, Start_Blk + Index, Cursor, Blk, Success);
         exit when not Success;

         if Blk /= 0 then
            Index := Index + 1;
         else
            --  Count how many blocks in a row are still missing and ask for
            --  all of them at once. That reads the bitmap once instead of
            --  once per block, and lays the file out contiguously, which is
            --  what lets reads of it later be merged into single operations.
            Need := 1;
            while Index + Need < Block_Count loop
               Get_Block_Index
                  (FS_Data, Inode_Data, Start_Blk + Index + Need, Cursor,
                   Blk, Success);
               exit when not Success or else Blk /= 0;
               Need := Need + 1;
            end loop;
            exit when not Success;

            Allocate_Blocks (FS_Data, Goal, Need, First, Got, Success);
            exit when not Success;

            Inode_Data.Sectors := Inode_Data.Sectors + Got * Per_Sector;
            for J in 0 .. Got - 1 loop
               Wire_Inode_Blocks
                  (FS_Data, Inode_Data, Start_Blk + Index + J, First + J,
                   Cursor, Success);
               exit when not Success;
            end loop;
            exit when not Success;

            Index := Index + Got;
         end if;
      end loop;

      Close_Cursor (Cursor);
      if Success then
         RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Num,
             Result          => Inode_Data,
             Write_Operation => True,
             Success         => Success);
      end if;
   exception
      when Constraint_Error =>
         Close_Cursor (Cursor);
         Messages.Put_Line ("Exception while assigning EXT inode blocks");
         Success := False;
   end Assign_Inode_Blocks;

   procedure Ensure_Pointer_Root
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : in out Inode;
       Goal       : Unsigned_32;
       Which      : Natural;
       Cursor     : in out Map_Cursor;
       Success    : out Boolean)
   is
      Blk : Unsigned_32;
   begin
      if Inode_Data.Blocks (Which) /= 0 then
         Success := True;
         return;
      end if;

      Allocate_Block_For_Inode (FS_Data, Inode_Data, Goal, Blk, Success);
      if not Success then
         return;
      end if;

      --  A block of pointers has to read as all zeroes to start with, else
      --  whatever the block used to hold would pass for block numbers.
      Zero_Out_Block (FS_Data, Blk, Success);
      if not Success then
         return;
      end if;

      Drop_Cursor_Block (Cursor, Blk);
      Inode_Data.Blocks (Which) := Blk;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while rooting an EXT pointer block");
         Success := False;
   end Ensure_Pointer_Root;

   procedure Ensure_Child
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : in out Inode;
       Goal       : Unsigned_32;
       Parent     : Unsigned_32;
       Index      : Unsigned_64;
       Cursor     : in out Map_Cursor;
       Child      : out Unsigned_32;
       Success    : out Boolean)
   is
   begin
      Fetch_Pointer (FS_Data, Parent, Index, Child, Success);
      if not Success or else Child /= 0 then
         return;
      end if;

      Allocate_Block_For_Inode (FS_Data, Inode_Data, Goal, Child, Success);
      if not Success then
         return;
      end if;
      Zero_Out_Block (FS_Data, Child, Success);
      if not Success then
         return;
      end if;
      Drop_Cursor_Block (Cursor, Child);

      Put_Pointer (FS_Data, Parent, Index, Child, Success);
      Patch_Cursor (Cursor, Parent, Index, Child);
   end Ensure_Child;

   procedure Wire_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Block_Index : Unsigned_32;
       Wired_Block : Unsigned_32;
       Cursor      : in out Map_Cursor;
       Success     : out Boolean)
   is
      Per_Blk  : Unsigned_64;
      Goal     : Unsigned_32;
      Idx      : Unsigned_64;
      Top, Mid : Unsigned_32;
   begin
      Per_Blk := Unsigned_64 (FS_Data.Pointers_Per_Block);
      Goal    := (if Wired_Block >= FS_Data.First_Data_Block
                  then (Wired_Block - FS_Data.First_Data_Block) /
                       FS_Data.Super.Blocks_Per_Group
                  else 0);
      Idx     := Unsigned_64 (Block_Index);
      Success := False;

      if Idx < 12 then
         Inode_Data.Blocks (Natural (Idx)) := Wired_Block;
         Success := True;
         return;
      end if;
      Idx := Idx - 12;

      if Idx < Per_Blk then
         Ensure_Pointer_Root (FS_Data, Inode_Data, Goal, 12, Cursor, Success);
         if not Success then
            return;
         end if;
         Top := Inode_Data.Blocks (12);
         Put_Pointer (FS_Data, Top, Idx, Wired_Block, Success);
         Patch_Cursor (Cursor, Top, Idx, Wired_Block);
         return;
      end if;
      Idx := Idx - Per_Blk;

      if Idx < Per_Blk * Per_Blk then
         Ensure_Pointer_Root (FS_Data, Inode_Data, Goal, 13, Cursor, Success);
         if not Success then
            return;
         end if;
         Ensure_Child
            (FS_Data, Inode_Data, Goal, Inode_Data.Blocks (13),
             Idx / Per_Blk, Cursor, Mid, Success);
         if not Success then
            return;
         end if;
         Put_Pointer (FS_Data, Mid, Idx mod Per_Blk, Wired_Block, Success);
         Patch_Cursor (Cursor, Mid, Idx mod Per_Blk, Wired_Block);
         return;
      end if;
      Idx := Idx - Per_Blk * Per_Blk;

      if Idx < Per_Blk * Per_Blk * Per_Blk then
         Ensure_Pointer_Root (FS_Data, Inode_Data, Goal, 14, Cursor, Success);
         if not Success then
            return;
         end if;
         Ensure_Child
            (FS_Data, Inode_Data, Goal, Inode_Data.Blocks (14),
             Idx / (Per_Blk * Per_Blk), Cursor, Top, Success);
         if not Success then
            return;
         end if;
         Ensure_Child
            (FS_Data, Inode_Data, Goal, Top,
             (Idx / Per_Blk) mod Per_Blk, Cursor, Mid, Success);
         if not Success then
            return;
         end if;
         Put_Pointer (FS_Data, Mid, Idx mod Per_Blk, Wired_Block, Success);
         Patch_Cursor (Cursor, Mid, Idx mod Per_Blk, Wired_Block);
         return;
      end if;

      Success := False;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while wiring blocks of an EXT inode");
         Success := False;
   end Wire_Inode_Blocks;

   procedure Allocate_Blocks
      (FS_Data   : EXT_Data_Acc;
       Goal      : Unsigned_32;
       Wanted    : Unsigned_32;
       Ret_Block : out Unsigned_32;
       Ret_Count : out Unsigned_32;
       Success   : out Boolean)
   is
      Start_Group : Unsigned_32;
      Desc      : Block_Group_Descriptor;
      Bitmap    : Operation_Data_Acc;
      Dev_Count : Natural;
      Succ      : Devices.Dev_Status;
      Group, Limit, Bit, Found, Taken, Ask : Unsigned_32;
      Byte      : Natural;
      Ok        : Boolean;
      Base      : Unsigned_64;
   begin
      Start_Group := (if Goal < FS_Data.Block_Group_Count then Goal else 0);
      Ret_Block := 0;
      Ret_Count := 0;
      Success   := False;
      if Wanted = 0 then
         return;
      end if;

      Bitmap := new Operation_Data (1 .. Natural (FS_Data.Block_Size));

      for N in 0 .. FS_Data.Block_Group_Count - 1 loop
         Group := (Start_Group + N) mod FS_Data.Block_Group_Count;
         RW_Block_Group_Descriptor (FS_Data, Group, Desc, False, Ok);
         if not Ok then
            goto Cleanup;
         end if;
         if Desc.Unallocated_Blocks = 0 then
            goto Next_Group;
         end if;

         Limit := FS_Data.Super.Blocks_Per_Group;
         Base  := Unsigned_64 (FS_Data.First_Data_Block) +
                  Unsigned_64 (Group) * Unsigned_64 (Limit);
         if Base + Unsigned_64 (Limit) >
            Unsigned_64 (FS_Data.Super.Block_Count)
         then
            Limit := Unsigned_32
               (Unsigned_64 (FS_Data.Super.Block_Count) - Base);
         end if;
         if Limit > FS_Data.Block_Size * 8 then
            Limit := FS_Data.Block_Size * 8;
         end if;
         if Limit = 0 then
            goto Next_Group;
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Block_Usage_Bitmap_Block) *
                          Unsigned_64 (FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Dev_Count,
             Success   => Succ);
         if Succ /= Devices.Dev_Success or else Dev_Count /= Bitmap'Length then
            goto Cleanup;
         end if;

         --  Bits go least significant first inside each byte, the way EXT
         --  writes them. Walking them the other way round, as used to
         --  happen, marks one block as taken and hands back a different one.
         Found := Limit;
         Bit   := 0;
         while Bit < Limit loop
            Byte := Bitmap'First + Natural (Bit / 8);
            if (Bit mod 8) = 0 and then Bitmap (Byte) = 16#FF# then
               Bit := Bit + 8;
            else
               if (Bitmap (Byte) and
                   Shift_Left (Unsigned_8'(1), Natural (Bit mod 8))) = 0
               then
                  Found := Bit;
                  exit;
               end if;
               Bit := Bit + 1;
            end if;
         end loop;
         if Found >= Limit then
            goto Next_Group;
         end if;

         --  Take as many blocks in a row as were asked for and are free.
         Ask :=
            Unsigned_32'Min (Wanted, Unsigned_32 (Desc.Unallocated_Blocks));
         Taken := 0;
         while Taken < Ask and then Found + Taken < Limit loop
            Byte := Bitmap'First + Natural ((Found + Taken) / 8);
            exit when (Bitmap (Byte) and
                       Shift_Left (Unsigned_8'(1),
                                   Natural ((Found + Taken) mod 8))) /= 0;
            Bitmap (Byte) := Bitmap (Byte) or
               Shift_Left (Unsigned_8'(1), Natural ((Found + Taken) mod 8));
            Taken := Taken + 1;
         end loop;
         if Taken = 0 then
            goto Next_Group;
         end if;

         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Block_Usage_Bitmap_Block) *
                          Unsigned_64 (FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Dev_Count,
             Success   => Succ);
         if Succ /= Devices.Dev_Success or else Dev_Count /= Bitmap'Length then
            goto Cleanup;
         end if;

         Desc.Unallocated_Blocks :=
            Desc.Unallocated_Blocks - Unsigned_16 (Taken);
         RW_Block_Group_Descriptor (FS_Data, Group, Desc, True, Ok);
         if not Ok then
            goto Cleanup;
         end if;

         if FS_Data.Super.Unallocated_Block_Count >= Taken then
            FS_Data.Super.Unallocated_Block_Count :=
               FS_Data.Super.Unallocated_Block_Count - Taken;
         else
            FS_Data.Super.Unallocated_Block_Count := 0;
         end if;

         FS_Data.Search_Group := Group;
         Ret_Block := Unsigned_32 (Base + Unsigned_64 (Found));
         Ret_Count := Taken;
         Success   := True;
         goto Cleanup;

      <<Next_Group>>
      end loop;

   <<Cleanup>>
      Free (Bitmap);
   exception
      when Constraint_Error =>
         Free (Bitmap);
         Messages.Put_Line ("Exception while allocating EXT blocks");
         Ret_Block := 0;
         Ret_Count := 0;
         Success   := False;
   end Allocate_Blocks;

   procedure Allocate_Block
      (FS_Data   : EXT_Data_Acc;
       Goal      : Unsigned_32;
       Ret_Block : out Unsigned_32;
       Success   : out Boolean)
   is
      Count : Unsigned_32;
   begin
      Allocate_Blocks (FS_Data, Goal, 1, Ret_Block, Count, Success);
      Success := Success and then Count = 1;
   end Allocate_Block;

   procedure Allocate_Block_For_Inode
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : in out Inode;
       Goal       : Unsigned_32;
       Ret_Block  : out Unsigned_32;
       Success    : out Boolean)
   is
   begin
      Allocate_Block (FS_Data, Goal, Ret_Block, Success);
      if Success then
         Inode_Data.Sectors :=
            Inode_Data.Sectors + (FS_Data.Block_Size / Sector_Unit);
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while allocating an EXT block");
         Ret_Block := 0;
         Success   := False;
   end Allocate_Block_For_Inode;

   procedure Allocate_Inode
      (FS_Data      : EXT_Data_Acc;
       Is_Directory : Boolean;
       Goal         : Unsigned_32;
       Inode_Num    : out Unsigned_32;
       Success      : out Boolean)
   is
      Start_Group : Unsigned_32;
      Per_Group : Unsigned_32;
      Desc      : Block_Group_Descriptor;
      Bitmap    : Operation_Data_Acc;
      Dev_Count : Natural;
      Succ      : Devices.Dev_Status;
      Group, Limit, Bit, Found : Unsigned_32;
      Candidate : Unsigned_64;
      Byte      : Natural;
      Ok        : Boolean;
   begin
      Start_Group := (if Goal < FS_Data.Block_Group_Count then Goal else 0);
      Per_Group   := FS_Data.Super.Inodes_Per_Group;
      Inode_Num := 0;
      Success   := False;

      Bitmap := new Operation_Data (1 .. Natural (FS_Data.Block_Size));

      for N in 0 .. FS_Data.Block_Group_Count - 1 loop
         Group := (Start_Group + N) mod FS_Data.Block_Group_Count;
         RW_Block_Group_Descriptor (FS_Data, Group, Desc, False, Ok);
         if not Ok then
            goto Cleanup;
         end if;
         if Desc.Unallocated_Inodes = 0 then
            goto Next_Group;
         end if;

         Limit := Per_Group;
         if Limit > FS_Data.Block_Size * 8 then
            Limit := FS_Data.Block_Size * 8;
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Inode_Usage_Bitmap_Block) *
                          Unsigned_64 (FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Dev_Count,
             Success   => Succ);
         if Succ /= Devices.Dev_Success or else Dev_Count /= Bitmap'Length then
            goto Cleanup;
         end if;

         --  Inodes are numbered from one, so bit b of group g stands for
         --  inode g * per_group + b + 1.
         Found := Limit;
         Bit   := 0;
         if Group = 0 and then FS_Data.First_Inode > 1 then
            Bit := FS_Data.First_Inode - 1;
         end if;
         while Bit < Limit loop
            Candidate := Unsigned_64 (Group) * Unsigned_64 (Per_Group) +
                         Unsigned_64 (Bit) + 1;
            exit when Candidate > Unsigned_64 (FS_Data.Super.Inode_Count);
            Byte := Bitmap'First + Natural (Bit / 8);
            if (Bit mod 8) = 0 and then Bitmap (Byte) = 16#FF# then
               Bit := Bit + 8;
            else
               if (Bitmap (Byte) and
                   Shift_Left (Unsigned_8'(1), Natural (Bit mod 8))) = 0
               then
                  Found := Bit;
                  exit;
               end if;
               Bit := Bit + 1;
            end if;
         end loop;
         if Found >= Limit then
            goto Next_Group;
         end if;

         Byte := Bitmap'First + Natural (Found / 8);
         Bitmap (Byte) := Bitmap (Byte) or
            Shift_Left (Unsigned_8'(1), Natural (Found mod 8));

         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Inode_Usage_Bitmap_Block) *
                          Unsigned_64 (FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Dev_Count,
             Success   => Succ);
         if Succ /= Devices.Dev_Success or else Dev_Count /= Bitmap'Length then
            goto Cleanup;
         end if;

         Desc.Unallocated_Inodes := Desc.Unallocated_Inodes - 1;
         if Is_Directory then
            Desc.Directory_Count := Desc.Directory_Count + 1;
         end if;
         RW_Block_Group_Descriptor (FS_Data, Group, Desc, True, Ok);
         if not Ok then
            goto Cleanup;
         end if;

         if FS_Data.Super.Unallocated_Inode_Count /= 0 then
            FS_Data.Super.Unallocated_Inode_Count :=
               FS_Data.Super.Unallocated_Inode_Count - 1;
         end if;

         Inode_Num := Group * Per_Group + Found + 1;
         Success   := True;
         goto Cleanup;

      <<Next_Group>>
      end loop;

   <<Cleanup>>
      Free (Bitmap);
   exception
      when Constraint_Error =>
         Free (Bitmap);
         Messages.Put_Line ("Exception while allocating an EXT inode");
         Inode_Num := 0;
         Success   := False;
   end Allocate_Inode;

   procedure Free_Blocks
      (FS_Data : EXT_Data_Acc;
       First   : Unsigned_32;
       Count   : Unsigned_32;
       Success : out Boolean)
   is
      Per_Group : Unsigned_32;
      Bit_Limit : Unsigned_32;
      Desc      : Block_Group_Descriptor;
      Bitmap    : Operation_Data_Acc;
      Dev_Count : Natural;
      Succ      : Devices.Dev_Status;
      Ok        : Boolean;
      Done, Blk, Group, Bit, Here, Stepped, Cleared : Unsigned_32;
      Byte      : Natural;
      Mask      : Unsigned_8;
   begin
      Per_Group := FS_Data.Super.Blocks_Per_Group;
      Bit_Limit := Unsigned_32'Min (Per_Group, FS_Data.Block_Size * 8);

      Success := True;
      if Count = 0 then
         return;
      elsif First < FS_Data.First_Data_Block or else
            Unsigned_64 (First) + Unsigned_64 (Count) >
            Unsigned_64 (FS_Data.Super.Block_Count)
      then
         --  Refuse a block number that is not one of ours rather than
         --  clearing a bit somewhere else in the filesystem.
         Success := False;
         return;
      end if;

      Bitmap := new Operation_Data (1 .. Natural (FS_Data.Block_Size));
      Done   := 0;

      while Done < Count loop
         Blk   := First + Done;
         Group := (Blk - FS_Data.First_Data_Block) / Per_Group;
         Bit   := (Blk - FS_Data.First_Data_Block) mod Per_Group;

         RW_Block_Group_Descriptor (FS_Data, Group, Desc, False, Ok);
         if not Ok then
            Success := False;
            goto Cleanup;
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Desc.Block_Usage_Bitmap_Block) *
                          Unsigned_64 (FS_Data.Block_Size),
             Data      => Bitmap.all,
             Ret_Count => Dev_Count,
             Success   => Succ);
         if Succ /= Devices.Dev_Success or else Dev_Count /= Bitmap'Length then
            Success := False;
            goto Cleanup;
         end if;

         --  Clear every bit of the run that falls in this group, then move
         --  on to the next one. Runs are the common case when a file is
         --  thrown away, so one pass over a bitmap covers many blocks.
         Stepped := 0;
         Cleared := 0;
         Here    := Bit;
         while Done + Stepped < Count and then Here < Bit_Limit loop
            Byte := Bitmap'First + Natural (Here / 8);
            Mask := Shift_Left (Unsigned_8'(1), Natural (Here mod 8));
            if (Bitmap (Byte) and Mask) /= 0 then
               Bitmap (Byte) := Bitmap (Byte) and not Mask;
               Cleared := Cleared + 1;
            end if;
            Here    := Here + 1;
            Stepped := Stepped + 1;
         end loop;
         if Stepped = 0 then
            Success := False;
            goto Cleanup;
         end if;

         if Cleared /= 0 then
            Devices.Write
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Desc.Block_Usage_Bitmap_Block) *
                             Unsigned_64 (FS_Data.Block_Size),
                Data      => Bitmap.all,
                Ret_Count => Dev_Count,
                Success   => Succ);
            if Succ /= Devices.Dev_Success or else
               Dev_Count /= Bitmap'Length
            then
               Success := False;
               goto Cleanup;
            end if;

            if Unsigned_32 (Desc.Unallocated_Blocks) + Cleared <=
               Unsigned_32 (Unsigned_16'Last)
            then
               Desc.Unallocated_Blocks :=
                  Desc.Unallocated_Blocks + Unsigned_16 (Cleared);
            else
               Desc.Unallocated_Blocks := Unsigned_16'Last;
            end if;
            RW_Block_Group_Descriptor (FS_Data, Group, Desc, True, Ok);
            if not Ok then
               Success := False;
               goto Cleanup;
            end if;

            FS_Data.Super.Unallocated_Block_Count :=
               FS_Data.Super.Unallocated_Block_Count + Cleared;
         end if;

         Done := Done + Stepped;
      end loop;

   <<Cleanup>>
      Free (Bitmap);
   exception
      when Constraint_Error =>
         Free (Bitmap);
         Messages.Put_Line ("Exception while freeing EXT blocks");
         Success := False;
   end Free_Blocks;

   procedure Free_Block
      (FS_Data : EXT_Data_Acc;
       Block   : Unsigned_32;
       Success : out Boolean)
   is
   begin
      Free_Blocks (FS_Data, Block, 1, Success);
   end Free_Block;

   procedure Free_Inode_Number
      (FS_Data      : EXT_Data_Acc;
       Inode_Num    : Unsigned_32;
       Is_Directory : Boolean;
       Success      : out Boolean)
   is
      Per_Group : Unsigned_32;
      Desc      : Block_Group_Descriptor;
      Bitmap    : Operation_Data_Acc;
      Dev_Count : Natural;
      Succ      : Devices.Dev_Status;
      Ok        : Boolean;
      Group, Bit : Unsigned_32;
      Byte      : Natural;
      Mask      : Unsigned_8;
   begin
      Per_Group := FS_Data.Super.Inodes_Per_Group;
      Success := False;
      if Inode_Num < 1 or else Inode_Num > FS_Data.Super.Inode_Count then
         return;
      end if;

      Group := (Inode_Num - 1) / Per_Group;
      Bit   := (Inode_Num - 1) mod Per_Group;
      if Bit >= FS_Data.Block_Size * 8 then
         return;
      end if;

      RW_Block_Group_Descriptor (FS_Data, Group, Desc, False, Ok);
      if not Ok then
         return;
      end if;

      Bitmap := new Operation_Data (1 .. Natural (FS_Data.Block_Size));
      Devices.Read
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Desc.Inode_Usage_Bitmap_Block) *
                       Unsigned_64 (FS_Data.Block_Size),
          Data      => Bitmap.all,
          Ret_Count => Dev_Count,
          Success   => Succ);
      if Succ /= Devices.Dev_Success or else Dev_Count /= Bitmap'Length then
         goto Cleanup;
      end if;

      Byte := Bitmap'First + Natural (Bit / 8);
      Mask := Shift_Left (Unsigned_8'(1), Natural (Bit mod 8));
      if (Bitmap (Byte) and Mask) = 0 then
         Success := True;
         goto Cleanup;
      end if;
      Bitmap (Byte) := Bitmap (Byte) and not Mask;

      Devices.Write
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Desc.Inode_Usage_Bitmap_Block) *
                       Unsigned_64 (FS_Data.Block_Size),
          Data      => Bitmap.all,
          Ret_Count => Dev_Count,
          Success   => Succ);
      if Succ /= Devices.Dev_Success or else Dev_Count /= Bitmap'Length then
         goto Cleanup;
      end if;

      if Desc.Unallocated_Inodes /= Unsigned_16'Last then
         Desc.Unallocated_Inodes := Desc.Unallocated_Inodes + 1;
      end if;
      if Is_Directory and then Desc.Directory_Count /= 0 then
         Desc.Directory_Count := Desc.Directory_Count - 1;
      end if;
      RW_Block_Group_Descriptor (FS_Data, Group, Desc, True, Ok);
      if not Ok then
         goto Cleanup;
      end if;

      FS_Data.Super.Unallocated_Inode_Count :=
         FS_Data.Super.Unallocated_Inode_Count + 1;
      Success := True;

   <<Cleanup>>
      Free (Bitmap);
   exception
      when Constraint_Error =>
         Free (Bitmap);
         Messages.Put_Line ("Exception while freeing an EXT inode");
         Success := False;
   end Free_Inode_Number;

   procedure Free_Indirect_Tree
      (FS_Data : EXT_Data_Acc;
       Block   : in out Unsigned_32;
       Level   : Natural;
       Start   : Unsigned_64;
       Freed   : in out Unsigned_64;
       Success : out Boolean)
   is
      Per_Blk   : Unsigned_64;
      Span      : Unsigned_64 := 1;
      Buffer    : Operation_Data_Acc := null;
      Dirty     : Boolean := False;
      Dev_Count : Natural;
      Succ      : Devices.Dev_Status;
      Ok        : Boolean;
      First_Entry, Sub_Start : Unsigned_64;
      Sub, Child, Run_Start, Run_Len : Unsigned_32;
   begin
      Per_Blk := Unsigned_64 (FS_Data.Pointers_Per_Block);
      Success := True;
      if Block = 0 then
         return;
      elsif Level < 1 or else Level > 3 then
         Success := False;
         return;
      end if;

      for I in 2 .. Level loop
         Span := Span * Per_Blk;
      end loop;

      First_Entry := Start / Span;
      if First_Entry >= Per_Blk then
         --  The cut falls past everything this subtree covers.
         return;
      end if;

      Buffer := new Operation_Data (1 .. Natural (FS_Data.Block_Size));
      Devices.Read
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Block) * Unsigned_64 (FS_Data.Block_Size),
          Data      => Buffer.all,
          Ret_Count => Dev_Count,
          Success   => Succ);
      if Succ /= Devices.Dev_Success or else Dev_Count /= Buffer'Length then
         Success := False;
         goto Cleanup;
      end if;

      Run_Start := 0;
      Run_Len   := 0;
      for I in First_Entry .. Per_Blk - 1 loop
         Sub := Get_Pointer (Buffer.all, Unsigned_32 (I));
         if Sub /= 0 then
            if Level = 1 then
               --  Gather neighbouring blocks so that a whole run of them is
               --  given back with a single pass over a bitmap.
               if Run_Len /= 0 and then Sub = Run_Start + Run_Len then
                  Run_Len := Run_Len + 1;
               else
                  if Run_Len /= 0 then
                     Free_Blocks (FS_Data, Run_Start, Run_Len, Ok);
                     Success := Success and Ok;
                     Freed   := Freed + Unsigned_64 (Run_Len);
                  end if;
                  Run_Start := Sub;
                  Run_Len   := 1;
               end if;
               Set_Pointer (Buffer.all, Unsigned_32 (I), 0);
               Dirty := True;
            else
               Child     := Sub;
               Sub_Start := (if I = First_Entry then Start - I * Span else 0);
               Free_Indirect_Tree
                  (FS_Data, Child, Level - 1, Sub_Start, Freed, Ok);
               Success := Success and Ok;
               if Child /= Sub then
                  Set_Pointer (Buffer.all, Unsigned_32 (I), Child);
                  Dirty := True;
               end if;
            end if;
         end if;
      end loop;

      if Run_Len /= 0 then
         Free_Blocks (FS_Data, Run_Start, Run_Len, Ok);
         Success := Success and Ok;
         Freed   := Freed + Unsigned_64 (Run_Len);
      end if;

      if Start = 0 then
         Free_Block (FS_Data, Block, Ok);
         Success := Success and Ok;
         Freed   := Freed + 1;
         Block   := 0;
      elsif Dirty then
         Devices.Write
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Block) *
                          Unsigned_64 (FS_Data.Block_Size),
             Data      => Buffer.all,
             Ret_Count => Dev_Count,
             Success   => Succ);
         if Succ /= Devices.Dev_Success or else Dev_Count /= Buffer'Length then
            Success := False;
         end if;
      end if;

   <<Cleanup>>
      Free (Buffer);
   exception
      when Constraint_Error =>
         Free (Buffer);
         Messages.Put_Line ("Exception while freeing an EXT indirect block");
         Success := False;
   end Free_Indirect_Tree;

   procedure Free_Blocks_From
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : in out Inode;
       From_Block : Unsigned_64;
       Success    : out Boolean)
   is
      Per_Blk : Unsigned_64;
      Freed  : Unsigned_64 := 0;
      Sects  : Unsigned_64;
      Base, Start : Unsigned_64;
      Run_Start, Run_Len : Unsigned_32;
      Ok     : Boolean;
   begin
      Per_Blk := Unsigned_64 (FS_Data.Pointers_Per_Block);
      Success := True;

      --  The twelve blocks the inode names itself.
      Run_Start := 0;
      Run_Len   := 0;
      for I in 0 .. 11 loop
         if Unsigned_64 (I) >= From_Block and then
            Inode_Data.Blocks (I) /= 0
         then
            if Run_Len /= 0 and then
               Inode_Data.Blocks (I) = Run_Start + Run_Len
            then
               Run_Len := Run_Len + 1;
            else
               if Run_Len /= 0 then
                  Free_Blocks (FS_Data, Run_Start, Run_Len, Ok);
                  Success := Success and Ok;
                  Freed   := Freed + Unsigned_64 (Run_Len);
               end if;
               Run_Start := Inode_Data.Blocks (I);
               Run_Len   := 1;
            end if;
            Inode_Data.Blocks (I) := 0;
         end if;
      end loop;
      if Run_Len /= 0 then
         Free_Blocks (FS_Data, Run_Start, Run_Len, Ok);
         Success := Success and Ok;
         Freed   := Freed + Unsigned_64 (Run_Len);
      end if;

      --  Then each of the three trees of pointers, in turn.
      Base := 12;
      if From_Block < Base + Per_Blk then
         Start := (if From_Block > Base then From_Block - Base else 0);
         Free_Indirect_Tree
            (FS_Data, Inode_Data.Blocks (12), 1, Start, Freed, Ok);
         Success := Success and Ok;
      end if;

      Base := 12 + Per_Blk;
      if From_Block < Base + Per_Blk * Per_Blk then
         Start := (if From_Block > Base then From_Block - Base else 0);
         Free_Indirect_Tree
            (FS_Data, Inode_Data.Blocks (13), 2, Start, Freed, Ok);
         Success := Success and Ok;
      end if;

      Base := 12 + Per_Blk + Per_Blk * Per_Blk;
      if From_Block < Base + Per_Blk * Per_Blk * Per_Blk then
         Start := (if From_Block > Base then From_Block - Base else 0);
         Free_Indirect_Tree
            (FS_Data, Inode_Data.Blocks (14), 3, Start, Freed, Ok);
         Success := Success and Ok;
      end if;

      Sects := Freed * Unsigned_64 (FS_Data.Block_Size / Sector_Unit);
      if Unsigned_64 (Inode_Data.Sectors) > Sects then
         Inode_Data.Sectors := Inode_Data.Sectors - Unsigned_32 (Sects);
      else
         Inode_Data.Sectors := 0;
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while trimming an EXT inode");
         Success := False;
   end Free_Blocks_From;

   procedure Delete_Inode
      (FS_Data    : EXT_Data_Acc;
       Inode_Num  : Unsigned_32;
       Inode_Data : in out Inode;
       Success    : out Boolean)
   is
      Kind : constant File_Type := Get_Inode_Type (Inode_Data.Permissions);
      Ok   : Boolean;
   begin
      Success := True;

      --  A symlink short enough to be kept inside the inode owns no blocks.
      if Kind /= File_Symbolic_Link or else not Is_Fast_Symlink (Inode_Data)
      then
         Free_Blocks_From (FS_Data, Inode_Data, 0, Success);
      end if;

      Inode_Data.Hard_Link_Count    := 0;
      Inode_Data.Deleted_Time_Epoch := Current_Epoch;
      Inode_Data.Size_Low           := 0;
      Inode_Data.Size_High          := 0;
      Inode_Data.Sectors            := 0;
      Inode_Data.Blocks             := [others => 0];

      RW_Inode
         (Data            => FS_Data,
          Inode_Index     => Inode_Num,
          Result          => Inode_Data,
          Write_Operation => True,
          Success         => Ok);
      Success := Success and Ok;

      Free_Inode_Number (FS_Data, Inode_Num, Kind = File_Directory, Ok);
      Success := Success and Ok;
   end Delete_Inode;

   procedure Is_Under
      (FS_Data  : EXT_Data_Acc;
       Dir      : Unsigned_32;
       Ancestor : Unsigned_32;
       Result   : out Boolean;
       Success  : out Boolean)
   is
      Current      : Unsigned_32 := Dir;
      Parent_Index : Unsigned_32;
      Steps        : Unsigned_32 := 0;
      Parent_Inode, Current_Inode : Inode_Acc := new Inode;
      Found, Is_Dir               : Boolean;
   begin
      Result  := False;
      Success := True;

      --  Every step goes up a directory, so a walk longer than there are
      --  inodes means the '..' entries loop.
      loop
         if Current = Ancestor then
            Result := True;
            exit;
         elsif Current = Root_Inode then
            exit;
         elsif Steps > FS_Data.Super.Inode_Count then
            Success := False;
            exit;
         end if;

         Inner_Open_Inode
            (Data         => FS_Data,
             Parent_Index => Current,
             Name         => "..",
             Target_Index => Parent_Index,
             Target_Inode => Parent_Inode.all,
             Parent_Inode => Current_Inode.all,
             Success      => Found,
             Parent_Open  => Is_Dir);
         if not Found or not Is_Dir then
            Success := False;
            exit;
         elsif Parent_Index = Current then
            exit;
         end if;

         Current := Parent_Index;
         Steps   := Steps + 1;
      end loop;

      Free (Parent_Inode);
      Free (Current_Inode);
   exception
      when Constraint_Error =>
         Free (Parent_Inode);
         Free (Current_Inode);
         Messages.Put_Line ("Exception while walking up an EXT directory");
         Result  := False;
         Success := False;
   end Is_Under;

   procedure Get_Dir_Entry
      (Buffer   : Operation_Data;
       Offset   : Natural;
       Has_Type : Boolean;
       Ino      : out Unsigned_32;
       Rec_Len  : out Natural;
       Name_Len : out Natural;
       Kind     : out Unsigned_8)
   is
      Base : Natural;
   begin
      Base := Buffer'First + Offset;
      Ino := Unsigned_32 (Buffer (Base))                       or
             Shift_Left (Unsigned_32 (Buffer (Base + 1)),  8)  or
             Shift_Left (Unsigned_32 (Buffer (Base + 2)), 16)  or
             Shift_Left (Unsigned_32 (Buffer (Base + 3)), 24);
      Rec_Len := Natural (Buffer (Base + 4)) +
                 Natural (Buffer (Base + 5)) * 256;
      if Has_Type then
         Name_Len := Natural (Buffer (Base + 6));
         Kind     := Buffer (Base + 7);
      else
         Name_Len := Natural (Buffer (Base + 6)) +
                     Natural (Buffer (Base + 7)) * 256;
         Kind     := 0;
      end if;
   exception
      when Constraint_Error =>
         Ino      := 0;
         Rec_Len  := 0;
         Name_Len := 0;
         Kind     := 0;
   end Get_Dir_Entry;

   procedure Put_Dir_Entry
      (Buffer   : in out Operation_Data;
       Offset   : Natural;
       Ino      : Unsigned_32;
       Rec_Len  : Natural;
       Kind     : Unsigned_8;
       Name     : String;
       Has_Type : Boolean)
   is
      Base : Natural;
   begin
      Base := Buffer'First + Offset;
      Buffer (Base)     := Unsigned_8 (Ino and 16#FF#);
      Buffer (Base + 1) := Unsigned_8 (Shift_Right (Ino,  8) and 16#FF#);
      Buffer (Base + 2) := Unsigned_8 (Shift_Right (Ino, 16) and 16#FF#);
      Buffer (Base + 3) := Unsigned_8 (Shift_Right (Ino, 24) and 16#FF#);
      Buffer (Base + 4) := Unsigned_8 (Rec_Len mod 256);
      Buffer (Base + 5) := Unsigned_8 ((Rec_Len / 256) mod 256);
      if Has_Type then
         Buffer (Base + 6) := Unsigned_8 (Name'Length);
         Buffer (Base + 7) := Kind;
      else
         Buffer (Base + 6) := Unsigned_8 (Name'Length mod 256);
         Buffer (Base + 7) := Unsigned_8 (Name'Length / 256);
      end if;
      for I in 1 .. Name'Length loop
         Buffer (Base + Dir_Entry_Header + I - 1) :=
            Character'Pos (Name (Name'First + I - 1));
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Put_Dir_Entry;

   procedure Set_Dir_Rec_Len
      (Buffer  : in out Operation_Data;
       Offset  : Natural;
       Rec_Len : Natural)
   is
      Base : Natural;
   begin
      Base := Buffer'First + Offset;
      Buffer (Base + 4) := Unsigned_8 (Rec_Len mod 256);
      Buffer (Base + 5) := Unsigned_8 ((Rec_Len / 256) mod 256);
   exception
      when Constraint_Error =>
         null;
   end Set_Dir_Rec_Len;

   procedure Set_Dir_Inode
      (Buffer : in out Operation_Data;
       Offset : Natural;
       Ino    : Unsigned_32)
   is
      Base : Natural;
   begin
      Base := Buffer'First + Offset;
      Buffer (Base)     := Unsigned_8 (Ino and 16#FF#);
      Buffer (Base + 1) := Unsigned_8 (Shift_Right (Ino,  8) and 16#FF#);
      Buffer (Base + 2) := Unsigned_8 (Shift_Right (Ino, 16) and 16#FF#);
      Buffer (Base + 3) := Unsigned_8 (Shift_Right (Ino, 24) and 16#FF#);
   exception
      when Constraint_Error =>
         null;
   end Set_Dir_Inode;

   procedure Drop_Hash_Index
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Index : Unsigned_32;
       Success     : out Boolean)
   is
   begin
      Success := True;
      if (Inode_Data.Flags and Flags_Hash_Index) /= 0 then
         Inode_Data.Flags :=
            Inode_Data.Flags and not Unsigned_32'(Flags_Hash_Index);
         RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Index,
             Result          => Inode_Data,
             Write_Operation => True,
             Success         => Success);
      end if;
   end Drop_Hash_Index;

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
      Block_Sz  : Unsigned_64;
      Needed    : Natural;
      Buffer    : Operation_Data_Acc := null;
      Cursor    : Map_Cursor := Empty_Cursor;
      Blk_Off   : Unsigned_64 := 0;
      Append_At : Unsigned_64;
      Offset, Ret_Count : Natural;
      Rec_Len, Name_Len, Actual : Natural;
      Ino_Num   : Unsigned_32;
      Kind_Byte : Unsigned_8;
      Ok        : Boolean;
   begin
      Block_Sz := Unsigned_64 (FS_Data.Block_Size);
      Needed   := ((Dir_Entry_Header + Name'Length + 3) / 4) * 4;

      Success := False;
      if Name'Length = 0 or else Name'Length > Max_File_Name_Size or else
         Unsigned_64 (Needed) > Block_Sz or else Added_Index = 0
      then
         return;
      end if;

      Drop_Hash_Index (FS_Data, Inode_Data, Inode_Index, Ok);
      if not Ok then
         return;
      end if;
      Invalidate_Memo (FS_Data);

      Buffer := new Operation_Data (1 .. Natural (Block_Sz));

      --  Look for room one block at a time.
      while Blk_Off + Block_Sz <= Inode_Size loop
         Read_From_Inode
            (FS_Data    => FS_Data,
             Inode_Data => Inode_Data,
             Inode_Size => Inode_Size,
             Offset     => Blk_Off,
             Data       => Buffer.all,
             Cursor     => Cursor,
             Ret_Count  => Ret_Count,
             Success    => Ok);
         exit when not Ok or else Ret_Count /= Buffer'Length;

         Offset := 0;
         while Offset + Dir_Entry_Header <= Natural (Block_Sz) loop
            Get_Dir_Entry
               (Buffer   => Buffer.all,
                Offset   => Offset,
                Has_Type => FS_Data.Has_Directory_Types,
                Ino      => Ino_Num,
                Rec_Len  => Rec_Len,
                Name_Len => Name_Len,
                Kind     => Kind_Byte);
            exit when Rec_Len < Dir_Entry_Header or else
                      (Rec_Len mod 4) /= 0       or else
                      Offset + Rec_Len > Natural (Block_Sz);

            --  A record with no inode is free room.
            if Ino_Num = 0 then
               Actual := 0;
            else
               Actual := ((Dir_Entry_Header + Name_Len + 3) / 4) * 4;
            end if;

            if Actual <= Rec_Len and then Rec_Len - Actual >= Needed then
               if Actual /= 0 then
                  Set_Dir_Rec_Len (Buffer.all, Offset, Actual);
               end if;
               Put_Dir_Entry
                  (Buffer   => Buffer.all,
                   Offset   => Offset + Actual,
                   Ino      => Added_Index,
                   Rec_Len  => Rec_Len - Actual,
                   Kind     => Dir_Type,
                   Name     => Name,
                   Has_Type => FS_Data.Has_Directory_Types);

               Write_To_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Index,
                   Inode_Size => Inode_Size,
                   Offset     => Blk_Off,
                   Data       => Buffer.all,
                   Cursor     => Cursor,
                   Ret_Count  => Ret_Count,
                   Success    => Ok);
               Success := Ok and then Ret_Count = Buffer'Length;
               goto Cleanup;
            end if;

            Offset := Offset + Rec_Len;
         end loop;

         Blk_Off := Blk_Off + Block_Sz;
      end loop;

      --  Nowhere left to put it, so the directory gains a block holding a
      --  single record that spans the whole of it.
      Append_At := ((Inode_Size + Block_Sz - 1) / Block_Sz) * Block_Sz;
      Buffer.all := [others => 0];
      Put_Dir_Entry
         (Buffer   => Buffer.all,
          Offset   => 0,
          Ino      => Added_Index,
          Rec_Len  => Natural (Block_Sz),
          Kind     => Dir_Type,
          Name     => Name,
          Has_Type => FS_Data.Has_Directory_Types);
      Write_To_Inode
         (FS_Data    => FS_Data,
          Inode_Data => Inode_Data,
          Inode_Num  => Inode_Index,
          Inode_Size => Inode_Size,
          Offset     => Append_At,
          Data       => Buffer.all,
          Cursor     => Cursor,
          Ret_Count  => Ret_Count,
          Success    => Ok);
      Success := Ok and then Ret_Count = Buffer'Length;

   <<Cleanup>>
      Close_Cursor (Cursor);
      Free (Buffer);
   exception
      when Constraint_Error =>
         Close_Cursor (Cursor);
         Free (Buffer);
         Messages.Put_Line ("Exception while adding an EXT dir entry");
         Success := False;
   end Add_Directory_Entry;

   procedure Delete_Directory_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Size  : Unsigned_64;
       Inode_Index : Unsigned_32;
       Name        : String;
       Deleted_Ino : out Unsigned_32;
       Success     : out Boolean)
   is
      Block_Sz  : Unsigned_64;
      Buffer    : Operation_Data_Acc := null;
      Cursor    : Map_Cursor := Empty_Cursor;
      Blk_Off   : Unsigned_64 := 0;
      Offset, Ret_Count : Natural;
      Rec_Len, Name_Len : Natural;
      Prev, Prev_Len    : Natural;
      Has_Prev, Matches : Boolean;
      Ino_Num   : Unsigned_32;
      Kind_Byte : Unsigned_8;
      Ok        : Boolean;
   begin
      Block_Sz    := Unsigned_64 (FS_Data.Block_Size);
      Deleted_Ino := 0;
      Success     := False;
      if Name'Length = 0 or else Name'Length > Max_File_Name_Size then
         return;
      end if;

      Drop_Hash_Index (FS_Data, Inode_Data, Inode_Index, Ok);
      if not Ok then
         return;
      end if;
      Invalidate_Memo (FS_Data);

      Buffer := new Operation_Data (1 .. Natural (Block_Sz));

      while Blk_Off + Block_Sz <= Inode_Size loop
         Read_From_Inode
            (FS_Data    => FS_Data,
             Inode_Data => Inode_Data,
             Inode_Size => Inode_Size,
             Offset     => Blk_Off,
             Data       => Buffer.all,
             Cursor     => Cursor,
             Ret_Count  => Ret_Count,
             Success    => Ok);
         exit when not Ok or else Ret_Count /= Buffer'Length;

         --  Records are merged into the one before them, and a record never
         --  reaches past its own block, so the search for a predecessor
         --  restarts at every block.
         Offset   := 0;
         Prev     := 0;
         Prev_Len := 0;
         Has_Prev := False;
         while Offset + Dir_Entry_Header <= Natural (Block_Sz) loop
            Get_Dir_Entry
               (Buffer   => Buffer.all,
                Offset   => Offset,
                Has_Type => FS_Data.Has_Directory_Types,
                Ino      => Ino_Num,
                Rec_Len  => Rec_Len,
                Name_Len => Name_Len,
                Kind     => Kind_Byte);
            exit when Rec_Len < Dir_Entry_Header or else
                      (Rec_Len mod 4) /= 0       or else
                      Offset + Rec_Len > Natural (Block_Sz);

            Matches := Ino_Num /= 0 and then Name_Len = Name'Length and then
                       Offset + Dir_Entry_Header + Name_Len <=
                       Natural (Block_Sz);
            if Matches then
               for I in 1 .. Name_Len loop
                  if Character'Val
                        (Buffer (Buffer'First + Offset +
                                 Dir_Entry_Header + I - 1)) /=
                     Name (Name'First + I - 1)
                  then
                     Matches := False;
                     exit;
                  end if;
               end loop;
            end if;

            if Matches then
               Deleted_Ino := Ino_Num;
               if Has_Prev then
                  --  Hand the room back to the record before it.
                  Set_Dir_Rec_Len (Buffer.all, Prev, Prev_Len + Rec_Len);
               else
                  Set_Dir_Inode (Buffer.all, Offset, 0);
               end if;

               Write_To_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Index,
                   Inode_Size => Inode_Size,
                   Offset     => Blk_Off,
                   Data       => Buffer.all,
                   Cursor     => Cursor,
                   Ret_Count  => Ret_Count,
                   Success    => Ok);
               Success := Ok and then Ret_Count = Buffer'Length;
               goto Cleanup;
            end if;

            Prev     := Offset;
            Prev_Len := Rec_Len;
            Has_Prev := True;
            Offset   := Offset + Rec_Len;
         end loop;

         Blk_Off := Blk_Off + Block_Sz;
      end loop;

   <<Cleanup>>
      Close_Cursor (Cursor);
      Free (Buffer);
   exception
      when Constraint_Error =>
         Close_Cursor (Cursor);
         Free (Buffer);
         Messages.Put_Line ("Exception while deleting an EXT dir entry");
         Success := False;
   end Delete_Directory_Entry;

   procedure Set_Parent_Entry
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Size  : Unsigned_64;
       Inode_Index : Unsigned_32;
       New_Parent  : Unsigned_32;
       Success     : out Boolean)
   is
      Block_Sz  : Unsigned_64;
      Buffer    : Operation_Data_Acc := null;
      Offset, Ret_Count : Natural;
      Rec_Len, Name_Len : Natural;
      Ino_Num   : Unsigned_32;
      Kind_Byte : Unsigned_8;
      Ok        : Boolean;
   begin
      Block_Sz := Unsigned_64 (FS_Data.Block_Size);
      Success  := False;
      if Inode_Size < Block_Sz then
         return;
      end if;

      Buffer := new Operation_Data (1 .. Natural (Block_Sz));
      Read_From_Inode
         (FS_Data    => FS_Data,
          Inode_Data => Inode_Data,
          Inode_Size => Inode_Size,
          Offset     => 0,
          Data       => Buffer.all,
          Ret_Count  => Ret_Count,
          Success    => Ok);
      if not Ok or else Ret_Count /= Buffer'Length then
         goto Cleanup;
      end if;

      --  The '..' of a directory that moved has to name its new parent, or
      --  everything walking upwards out of it lands in the old one.
      Offset := 0;
      while Offset + Dir_Entry_Header <= Natural (Block_Sz) loop
         Get_Dir_Entry
            (Buffer   => Buffer.all,
             Offset   => Offset,
             Has_Type => FS_Data.Has_Directory_Types,
             Ino      => Ino_Num,
             Rec_Len  => Rec_Len,
             Name_Len => Name_Len,
             Kind     => Kind_Byte);
         exit when Rec_Len < Dir_Entry_Header or else
                   (Rec_Len mod 4) /= 0       or else
                   Offset + Rec_Len > Natural (Block_Sz);

         if Ino_Num /= 0 and then Name_Len = 2 and then
            Buffer (Buffer'First + Offset + Dir_Entry_Header) =
               Character'Pos ('.') and then
            Buffer (Buffer'First + Offset + Dir_Entry_Header + 1) =
               Character'Pos ('.')
         then
            Set_Dir_Inode (Buffer.all, Offset, New_Parent);
            Write_To_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Inode_Data,
                Inode_Num  => Inode_Index,
                Inode_Size => Inode_Size,
                Offset     => 0,
                Data       => Buffer.all,
                Ret_Count  => Ret_Count,
                Success    => Ok);
            Success := Ok and then Ret_Count = Buffer'Length;
            goto Cleanup;
         end if;

         Offset := Offset + Rec_Len;
      end loop;

   <<Cleanup>>
      Free (Buffer);
   exception
      when Constraint_Error =>
         Free (Buffer);
         Messages.Put_Line ("Exception while repointing an EXT directory");
         Success := False;
   end Set_Parent_Entry;

   procedure Set_Entry_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Size  : Unsigned_64;
       Inode_Index : Unsigned_32;
       Name        : String;
       New_Ino     : Unsigned_32;
       Dir_Type    : Unsigned_8;
       Success     : out Boolean)
   is
      Block_Sz  : Unsigned_64;
      Buffer    : Operation_Data_Acc := null;
      Block_Off : Unsigned_64 := 0;
      Offset, Ret_Count : Natural;
      Rec_Len, Name_Len : Natural;
      Ino_Num   : Unsigned_32;
      Kind_Byte : Unsigned_8;
      Ok        : Boolean;
      Matches   : Boolean;
   begin
      Block_Sz := Unsigned_64 (FS_Data.Block_Size);
      Success  := False;
      Buffer   := new Operation_Data (1 .. Natural (Block_Sz));

      --  A directory record never straddles a block, so each block is looked
      --  at whole and the one holding the name is written back whole.
      while Block_Off + Block_Sz <= Inode_Size loop
         Read_From_Inode
            (FS_Data    => FS_Data,
             Inode_Data => Inode_Data,
             Inode_Size => Inode_Size,
             Offset     => Block_Off,
             Data       => Buffer.all,
             Ret_Count  => Ret_Count,
             Success    => Ok);
         if not Ok or else Ret_Count /= Buffer'Length then
            goto Cleanup;
         end if;

         Offset := 0;
         while Offset + Dir_Entry_Header <= Natural (Block_Sz) loop
            Get_Dir_Entry
               (Buffer   => Buffer.all,
                Offset   => Offset,
                Has_Type => FS_Data.Has_Directory_Types,
                Ino      => Ino_Num,
                Rec_Len  => Rec_Len,
                Name_Len => Name_Len,
                Kind     => Kind_Byte);
            exit when Rec_Len < Dir_Entry_Header or else
                      (Rec_Len mod 4) /= 0       or else
                      Offset + Rec_Len > Natural (Block_Sz);

            Matches := Ino_Num /= 0 and then Name_Len = Name'Length and then
                       Dir_Entry_Header + Name_Len <= Rec_Len;
            if Matches then
               for I in 1 .. Name_Len loop
                  if Buffer (Buffer'First + Offset + Dir_Entry_Header + I - 1)
                     /= Character'Pos (Name (Name'First + I - 1))
                  then
                     Matches := False;
                     exit;
                  end if;
               end loop;
            end if;

            if Matches then
               Set_Dir_Inode (Buffer.all, Offset, New_Ino);
               if FS_Data.Has_Directory_Types then
                  Buffer (Buffer'First + Offset + 7) := Dir_Type;
               end if;
               Write_To_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Index,
                   Inode_Size => Inode_Size,
                   Offset     => Block_Off,
                   Data       => Buffer.all,
                   Ret_Count  => Ret_Count,
                   Success    => Ok);
               Success := Ok and then Ret_Count = Buffer'Length;
               goto Cleanup;
            end if;

            Offset := Offset + Rec_Len;
         end loop;

         Block_Off := Block_Off + Block_Sz;
      end loop;

   <<Cleanup>>
      Free (Buffer);
   exception
      when Constraint_Error =>
         Free (Buffer);
         Messages.Put_Line ("Exception while repointing an EXT entry");
         Success := False;
   end Set_Entry_Inode;

   procedure Invalidate_Memo (FS_Data : EXT_Data_Acc) is
   begin
      Synchronization.Seize (FS_Data.Memo_Lock);
      FS_Data.Memo_Inode  := 0;
      FS_Data.Memo_Index  := 0;
      FS_Data.Memo_Offset := 0;
      Synchronization.Release (FS_Data.Memo_Lock);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while resetting the EXT scan memo");
   end Invalidate_Memo;

   function Get_Dir_Type (Dir_Type : Unsigned_8) return File_Type is
   begin
      case Dir_Type is
         when 1      => return File_Regular;
         when 2      => return File_Directory;
         when 3      => return File_Character_Device;
         when 4      => return File_Block_Device;
         when 7      => return File_Symbolic_Link;
         when others => return File_Regular;
      end case;
   end Get_Dir_Type;

   function Get_Dir_Type (T : File_Type) return Unsigned_8 is
   begin
      case T is
         when File_Regular          => return 1;
         when File_Directory        => return 2;
         when File_Character_Device => return 3;
         when File_Block_Device     => return 4;
         when File_Symbolic_Link    => return 7;
      end case;
   exception
      when Constraint_Error =>
         return 0;
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
   exception
      when Constraint_Error =>
         return 0;
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
      Ret : Unsigned_16;
   begin
      Ret := Unsigned_16 (Mode and 16#FFF#);
      case T is
         when File_Character_Device => return Ret or 16#2000#;
         when File_Directory        => return Ret or 16#4000#;
         when File_Block_Device     => return Ret or 16#6000#;
         when File_Regular          => return Ret or 16#8000#;
         when File_Symbolic_Link    => return Ret or 16#A000#;
      end case;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Inode_Type;

   function Get_Size (Ino : Inode; Is_64_Bits : Boolean) return Unsigned_64 is
   begin
      --  The upper half of the size is only a size for a plain file. For
      --  anything else that field is i_dir_acl.
      if Is_64_Bits and then
         Get_Inode_Type (Ino.Permissions) = File_Regular
      then
         return Shift_Left (Unsigned_64 (Ino.Size_High), 32) or
                Unsigned_64 (Ino.Size_Low);
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
      L32, H32 : Unsigned_32;
   begin
      L32 := Unsigned_32 (New_Size and 16#FFFFFFFF#);
      H32 := Unsigned_32 (Shift_Right (New_Size, 32));

      if Is_64_Bits and then
         Get_Inode_Type (Ino.Permissions) = File_Regular
      then
         Ino.Size_Low  := L32;
         Ino.Size_High := H32;
         Success       := True;
      else
         Ino.Size_Low := L32;
         Success      := H32 = 0;
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while setting size of an EXT inode");
         Success := False;
   end Set_Size;

   function Current_Epoch return Unsigned_32 is
      Stamp : Time.Timestamp;
   begin
      Arch.Clocks.Get_Real_Time (Stamp);
      return Unsigned_32 (Stamp.Seconds and 16#FFFFFFFF#);
   exception
      when Constraint_Error =>
         return 0;
   end Current_Epoch;

   procedure Act_On_Policy (Data : EXT_Data_Acc; Message : String) is
   begin
      case Data.Super.Error_Policy is
         when Policy_Ignore =>
            Messages.Put_Line (Message);
         when Policy_Remount_RO | Policy_Panic =>
            Messages.Put_Line (Message);
            Data.Is_Read_Only := True;
         when others =>
            Messages.Put_Line (Message);
            Messages.Put_Line ("Undetected policy: We are remounting RO");
            Data.Is_Read_Only := True;
      end case;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception while acting on ext policy");
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
