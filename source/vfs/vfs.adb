--  vfs.adb: FS and register dispatching.
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

with Ada.Unchecked_Deallocation;
with VFS.Dev;
with VFS.EXT;
with VFS.FAT;

package body VFS is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Init is
      pragma SPARK_Mode (Off);
   begin
      Mounts := new Mount_Registry'[others =>
         (Mounted_Dev => Devices.Error_Handle,
          Mounted_FS  => FS_EXT,
          FS_Data     => System.Null_Address,
          Path_Length => 0,
          Path_Buffer => [others => ' '],
          Base_Key    => Error_Handle,
          Base_Ino    => 0,
          Root_Ino    => 0)];
      Mounts_Mutex := Synchronization.Unlocked_Semaphore;
      Root_Idx     := Error_Handle;
   end Init;

   procedure Mount
      (Device_Name   : String;
       Mount_Path    : String;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Success       : out FS_Status)
   is
      pragma SPARK_Mode (Off);
      Name : String renames Device_Name;
   begin
      for FS in FS_Type'Range loop
         if FS /= FS_DEV then
            Mount (Name, Mount_Path, FS, Do_Read_Only, Access_Policy, Success);
            exit when Success = FS_Success;
         end if;
      end loop;
   end Mount;

   procedure Mount
      (Device_Name   : String;
       Mount_Path    : String;
       FS            : FS_Type;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Success       : out FS_Status)
   is
      De         : constant Device_Handle := Devices.Fetch (Device_Name);
      Free_I     : FS_Handle;
      FS_Data    : System.Address;
      Key        : FS_Handle := Error_Handle;
      Ino        : File_Inode_Number := 0;
      Root_Inode : File_Inode_Number;
   begin
      if not Is_Absolute (Mount_Path)           or
         Mount_Path'Length > Path_Buffer_Length or
         De = Devices.Error_Handle
      then
         Success := FS_Invalid_Value;
         return;
      end if;

      if Mount_Path /= "/" then
         Open
            (Path       => Mount_Path,
             Key        => Key,
             Ino        => Ino,
             Success    => Success,
             User       => 0,
             Want_Read  => True,
             Want_Write => False);
         if Success /= FS_Success then
            return;
         end if;
      end if;

      Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev = De then
            Success := FS_Invalid_Value;
            goto Cleanup;
         elsif Mounts (I).Mounted_Dev = Devices.Error_Handle then
            Free_I := I;
            goto Try_Probe;
         end if;
      end loop;

      Success := FS_Full;
      goto Cleanup;

   <<Try_Probe>>
      case FS is
         when FS_DEV =>
            Dev.Probe (De, Do_Read_Only, Access_Policy, FS_Data, Root_Inode);
         when FS_EXT =>
            EXT.Probe (De, Do_Read_Only, Access_Policy, FS_Data, Root_Inode);
         when FS_FAT =>
            FAT.Probe (De, Do_Read_Only, Access_Policy, FS_Data, Root_Inode);
      end case;

      if FS_Data /= System.Null_Address then
         Mounts (Free_I) :=
            (Mounted_Dev => De,
             Mounted_FS  => FS,
             FS_Data     => FS_Data,
             Path_Length => Mount_Path'Length,
             Path_Buffer => [others => ' '],
             Base_Key    => Key,
             Base_Ino    => Ino,
             Root_Ino    => Root_Inode);

         Mounts (Free_I).Path_Buffer (1 .. Mount_Path'Length) := Mount_Path;
         Success := FS_Success;
         if Root_Idx = Error_Handle then
            Root_Idx := Free_I;
         end if;
      else
         Success := FS_Invalid_Value;
      end if;

   <<Cleanup>>
      Synchronization.Release (Mounts_Mutex);
   end Mount;

   procedure Unmount (Path : String; Force : Boolean; Success : out Boolean) is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      Success := False;
      Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = Path
         then
            case Mounts (I).Mounted_FS is
               when FS_DEV => Dev.Unmount (Mounts (I).FS_Data);
               when FS_EXT => EXT.Unmount (Mounts (I).FS_Data);
               when FS_FAT => FAT.Unmount (Mounts (I).FS_Data);
            end case;

            if Force or Mounts (I).FS_Data = Null_Address then
               Mounts (I).Mounted_Dev := Devices.Error_Handle;
               Success := True;
            end if;
            exit;
         end if;
      end loop;
      Synchronization.Release (Mounts_Mutex);
   end Unmount;

   procedure Get_Mount
      (Path   : String;
       Match  : out Natural;
       Handle : out FS_Handle)
   is
   begin
      Match  := 0;
      Handle := VFS.Error_Handle;

      Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle     and then
            Path'Length >= Mounts (I).Path_Length              and then
            Match < Mounts (I).Path_Length                     and then
            Path'First < Natural'Last - Mounts (I).Path_Length and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) =
            Path (Path'First .. Path'First + Mounts (I).Path_Length - 1)
         then
            Handle := I;
            Match  := Mounts (I).Path_Length;
         end if;
      end loop;
      Synchronization.Release (Mounts_Mutex);

      if Match > 0 then
         Match := Match - 1;
      end if;
   end Get_Mount;

   procedure Get_Root (FS : out FS_Handle; Ino : out File_Inode_Number) is
   begin
      if Root_Idx /= Error_Handle then
         FS  := Root_Idx;
         Ino := Mounts (Root_Idx).Root_Ino;
      else
         FS  := Error_Handle;
         Ino := 0;
      end if;
   end Get_Root;

   procedure Pivot_Root
      (New_Mount : String;
       Old_Mount : String;
       Success   : out Boolean)
   is
      New_Idx : FS_Handle := Error_Handle;
      Succ    : FS_Status;
      Key     : FS_Handle;
      Ino     : File_Inode_Number;
   begin
      Success := False;

      if Root_Idx = Error_Handle then
         return;
      end if;

      Synchronization.Seize (Mounts_Mutex);

      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = New_Mount
         then
            New_Idx := I;
         end if;
      end loop;

      if New_Idx /= Error_Handle and then New_Idx /= Root_Idx then
         Open
            (Key        => New_Idx,
             Relative   => Mounts (New_Idx).Root_Ino,
             Path       => "./" & Old_Mount,
             Final_Key  => Key,
             Ino        => Ino,
             Success    => Succ,
             User       => 0,
             Want_Read  => True,
             Want_Write => False);
         if Succ /= FS_Success then
            goto Cleanup;
         end if;

         Mounts (Root_Idx).Path_Buffer (1 .. Old_Mount'Length) := Old_Mount;
         Mounts (Root_Idx).Path_Length := Old_Mount'Length;
         Mounts (Root_Idx).Base_Key    := Key;
         Mounts (Root_Idx).Base_Ino    := Ino;

         Root_Idx := New_Idx;

         Mounts (Root_Idx).Path_Buffer (1) := '/';
         Mounts (Root_Idx).Path_Length := 1;
         Mounts (Root_Idx).Base_Key    := Error_Handle;
         Mounts (Root_Idx).Base_Ino    := 0;
         Success := True;
      end if;

   <<Cleanup>>
      Synchronization.Release (Mounts_Mutex);
   end Pivot_Root;

   procedure List_All (List : out Mountpoint_Arr; Total : out Natural) is
      pragma Annotate (GNATprove, False_Positive, "range check might fail",
                       "in List? how?");

      Curr_Index : Natural := 0;
   begin
      Total := 0;
      List  := [others => Error_Handle];

      Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle then
            if Curr_Index < List'Length then
               List (List'First + Curr_Index) := I;
               Curr_Index := Curr_Index + 1;
            end if;
            Total := Total + 1;
         end if;
      end loop;
      Synchronization.Release (Mounts_Mutex);
   end List_All;

   function Get_Backing_FS (Key : FS_Handle) return FS_Type is
   begin
      return Mounts (Key).Mounted_FS;
   end Get_Backing_FS;

   function Get_Backing_FS_Data (Key : FS_Handle) return System.Address is
   begin
      return Mounts (Key).FS_Data;
   end Get_Backing_FS_Data;

   function Get_Backing_Device (Key : FS_Handle) return Device_Handle is
   begin
      return Mounts (Key).Mounted_Dev;
   end Get_Backing_Device;

   procedure Get_Mount_Point
      (Key    : FS_Handle;
       Name   : out String;
       Length : out Natural)
   is
   begin
      Name := [others => ' '];
      Name (Name'First .. Name'First + Mounts (Key).Path_Length - 1) :=
         Mounts (Key).Path_Buffer (1 .. Mounts (Key).Path_Length);
      Length := Mounts (Key).Path_Length;
   end Get_Mount_Point;

   procedure Remount
      (Key           : FS_Handle;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Success       : out Boolean)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
      Data : constant System.Address := Mounts (Key).FS_Data;
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Remount (Data, Do_Read_Only, Access_Policy, Success);
         when FS_EXT =>
            EXT.Remount (Data, Do_Read_Only, Access_Policy, Success);
         when FS_FAT =>
            FAT.Remount (Data, Do_Read_Only, Access_Policy, Success);
      end case;
   end Remount;
   ----------------------------------------------------------------------------
   procedure Get_Block_Size (Key : FS_Handle; Size : out Unsigned_64) is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Get_Block_Size (Mounts (Key).FS_Data, Size);
         when FS_EXT => EXT.Get_Block_Size (Mounts (Key).FS_Data, Size);
         when FS_FAT => Size := FAT.Get_Block_Size (Mounts (Key).FS_Data);
      end case;
   end Get_Block_Size;

   procedure Get_Fragment_Size (Key : FS_Handle; Size : out Unsigned_64) is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Get_Fragment_Size (Mounts (Key).FS_Data, Size);
         when FS_EXT => EXT.Get_Fragment_Size (Mounts (Key).FS_Data, Size);
         when FS_FAT => Size := FAT.Get_Fragment_Size (Mounts (Key).FS_Data);
      end case;
   end Get_Fragment_Size;

   procedure Get_Size (Key : FS_Handle; Size : out Unsigned_64) is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Get_Size (Mounts (Key).FS_Data, Size);
         when FS_EXT => EXT.Get_Size (Mounts (Key).FS_Data, Size);
         when FS_FAT => Size := FAT.Get_Size (Mounts (Key).FS_Data);
      end case;
   end Get_Size;

   procedure Get_Inode_Count (Key : FS_Handle; Count : out Unsigned_64) is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Get_Inode_Count (Mounts (Key).FS_Data, Count);
         when FS_EXT => EXT.Get_Inode_Count (Mounts (Key).FS_Data, Count);
         when FS_FAT => Count := FAT.Get_Inode_Count (Mounts (Key).FS_Data);
      end case;
   end Get_Inode_Count;

   procedure Get_Free_Blocks
      (Key                : FS_Handle;
       Free_Blocks        : out Unsigned_64;
       Free_Unprivileged : out Unsigned_64)
   is
      FB : Unsigned_64 renames Free_Blocks;
      FU : Unsigned_64 renames Free_Unprivileged;
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Get_Free_Blocks (Mounts (Key).FS_Data, FB, FU);
         when FS_EXT => EXT.Get_Free_Blocks (Mounts (Key).FS_Data, FB, FU);
         when FS_FAT => FAT.Get_Free_Blocks (Mounts (Key).FS_Data, FB, FU);
      end case;
   end Get_Free_Blocks;

   procedure Get_Free_Inodes
      (Key                : FS_Handle;
       Free_Inodes        : out Unsigned_64;
       Free_Unprivileged : out Unsigned_64)
   is
      FI : Unsigned_64 renames Free_Inodes;
      FU : Unsigned_64 renames Free_Unprivileged;
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Get_Free_Inodes (Mounts (Key).FS_Data, FI, FU);
         when FS_EXT => EXT.Get_Free_Inodes (Mounts (Key).FS_Data, FI, FU);
         when FS_FAT => FAT.Get_Free_Inodes (Mounts (Key).FS_Data, FI, FU);
      end case;
   end Get_Free_Inodes;

   procedure Get_Max_Length (Key : FS_Handle; Length : out Unsigned_64) is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Length := Dev.Get_Max_Length (Mounts (Key).FS_Data);
         when FS_EXT => Length := EXT.Get_Max_Length (Mounts (Key).FS_Data);
         when FS_FAT => Length := FAT.Get_Max_Length (Mounts (Key).FS_Data);
      end case;
   end Get_Max_Length;
   ----------------------------------------------------------------------------
   procedure Open
      (Key        : FS_Handle;
       Relative   : File_Inode_Number;
       Path       : String;
       Final_Key  : out FS_Handle;
       Ino        : out File_Inode_Number;
       Success    : out FS_Status;
       User       : Unsigned_32;
       Want_Read  : Boolean;
       Want_Write : Boolean;
       Do_Follow  : Boolean := True)
   is
      pragma Annotate
         (GNATprove,
          False_Positive,
          "precondition might fail, cannot prove Key /= Error_Handle",
          "I think this is not the case");

      pragma Annotate
         (GNATprove,
          False_Positive,
          "range check might fail",
          "Proven in preconditions");

      type String_Acc is access String;

      procedure Free1 is new Ada.Unchecked_Deallocation (String, String_Acc);
      procedure Free2 is new Ada.Unchecked_Deallocation
         (Directory_Entities, Directory_Entities_Acc);

      Orig_Key        :         FS_Handle := Error_Handle;
      Orig_Ino        : File_Inode_Number := 0;
      Actual_Key      :         FS_Handle := Key;
      Actual_Ino      : File_Inode_Number := Relative;
      Path_Idx, Path_Last, Entries_Offset, Entries_Count : Natural;
      Entry_Stat      : File_Stat;
      Symlink_Len     : Natural;
      Symlink_Path    : String_Acc := null;
      Dir_Entries     : Directory_Entities_Acc := new Directory_Entities'
         [1 .. 20 =>
            (Inode_Number => 0,
             Name_Buffer  => [others => ' '],
             Name_Len     => 0,
             Type_Of_File => File_Regular)];
   begin
      if Path'Length = 0 or Root_Idx = Error_Handle then
         goto Invalid_Value_Return;
      end if;

      if Is_Absolute (Path) then
         Actual_Key := Root_Idx;
         Actual_Ino := Mounts (Root_Idx).Root_Ino;
      end if;

      Path_Idx := 0;
      loop
         --  Clear slashes ('/') at the front of path.
         --  If we end up with nothing at the end, it means we already hit
         --  our destination, and we were just dealing with some trailing
         --  slashes. Ex: /usr/home////
         while Path'First < Natural'Last - Path_Idx and then
               Path'First + Path_Idx <= Path'Last   and then
               Path (Path'First + Path_Idx) = '/'
         loop
            Path_Idx := Path_Idx + 1;
         end loop;
         if Path'First >= Natural'Last - Path_Idx then
            goto Invalid_Value_Return;
         end if;
         if Path'First + Path_Idx > Path'Last then
            exit;
         end if;

         --  Find the next component of Path, by iterating until we find the
         --  next slash.
         Path_Last := Path_Idx;
         while Path'First < Natural'Last - Path_Last and then
               Path'First + Path_Last <= Path'Last   and then
               Path (Path'First + Path_Last) /= '/'
         loop
            Path_Last := Path_Last + 1;
         end loop;
         if Path'First >= Natural'Last - Path_Last then
            goto Invalid_Value_Return;
         end if;
         Path_Last := Path_Last - 1;

         --  Read the entries of current directory, check if the component
         --  of path is found.
         Entries_Offset := 0;
         loop
            Read_Entries
               (Key       => Actual_Key,
                Ino       => Actual_Ino,
                Offset    => Entries_Offset,
                Entities  => Dir_Entries.all,
                Ret_Count => Entries_Count,
                Success   => Success);
            if (Success = FS_Success and Entries_Count = 0) or
               Success /= FS_Success
            then
               exit;
            end if;

            for Ent of Dir_Entries (1 .. Entries_Count) loop
               if Ent.Name_Buffer (1 .. Ent.Name_Len) =
                  Path (Path'First + Path_Idx .. Path'First + Path_Last)
               then
                  Orig_Key   := Actual_Key;
                  Orig_Ino   := Actual_Ino;
                  Actual_Ino := File_Inode_Number (Ent.Inode_Number);
                  goto Found_Entry;
               end if;
            end loop;

            if Entries_Offset <= Natural'Last - Entries_Count then
               Entries_Offset := Entries_Offset + Entries_Count;
            end if;
         end loop;

         goto Not_Found_Return;

   <<Found_Entry>>
         --  Get the stat for several checks.
         VFS.Stat (Actual_Key, Actual_Ino, Entry_Stat, Success);
         if Success /= FS_Success then
            goto Invalid_Value_Return;
         end if;

         --  We have found a suitable next step, neat!
         --  If we are at the end of the road, we can try to follow applicable
         --  symlinks by recursion, and mount replacement, and be done.
         if Path'First + Path_Last = Path'Last then
            if Entry_Stat.Type_Of_File = File_Symbolic_Link and Do_Follow then
               if Entry_Stat.Byte_Size > Unsigned_64 (Natural'Last) then
                  goto Invalid_Value_Return;
               end if;

               Symlink_Path :=
                  new String'(1 .. Natural (Entry_Stat.Byte_Size) => ' ');
               Read_Symbolic_Link
                  (Key       => Actual_Key,
                   Ino       => Actual_Ino,
                   Path      => Symlink_Path.all,
                   Ret_Count => Symlink_Len,
                   Success   => Success);
               if Success /= FS_Success or else
                  Symlink_Path'Length < Symlink_Len
               then
                  goto Invalid_Value_Return;
               end if;

               Open
                  (Key        => Orig_Key,
                   Relative   => Orig_Ino,
                   Path       => Symlink_Path (1 .. Symlink_Len),
                   Final_Key  => Final_Key,
                   Ino        => Ino,
                   Success    => Success,
                   User       => User,
                   Want_Read  => Want_Read,
                   Want_Write => Want_Write,
                   Do_Follow  => Do_Follow);
               goto Cleanup_Only_Return;
            elsif Entry_Stat.Type_Of_File = File_Directory then
               --  Check whether we are dealing with a mount.
               for I in Mounts'Range loop
                  if Mounts (I).Base_Key = Actual_Key and
                     Mounts (I).Base_Ino = Actual_Ino
                  then
                     Actual_Key := I;
                     Actual_Ino := Mounts (I).Root_Ino;
                     exit;
                  end if;
               end loop;
            end if;
            exit;
         end if;

         --  If we have found the next component and we are NOT at the end of
         --  path, then we prepare to go again. We do that by making sure
         --  we are at a symlink or directory.
         case Entry_Stat.Type_Of_File is
            when File_Directory =>
               for I in Mounts'Range loop
                  if Mounts (I).Base_Key = Actual_Key and
                     Mounts (I).Base_Ino = Actual_Ino
                  then
                     Actual_Key := I;
                     Actual_Ino := Mounts (I).Root_Ino;
                     VFS.Stat (Actual_Key, Actual_Ino, Entry_Stat, Success);
                     if Success /= FS_Success then
                        goto Invalid_Value_Return;
                     end if;
                     exit;
                  end if;
               end loop;

               --  Check that we have a permission to keep going.
               if not Can_Access_File
                  (User       => User,
                   File_Owner => Entry_Stat.UID,
                   Mode       => Entry_Stat.Mode,
                   Want_Read  => True,
                   Want_Write => False,
                   Want_Exec  => False)
               then
                  goto Not_Allowed_Return;
               end if;
            when File_Symbolic_Link =>
               if Entry_Stat.Byte_Size > Unsigned_64 (Natural'Last) then
                  goto Invalid_Value_Return;
               end if;

               Symlink_Path :=
                  new String'(1 .. Natural (Entry_Stat.Byte_Size) => ' ');
               Read_Symbolic_Link
                  (Key       => Actual_Key,
                   Ino       => Actual_Ino,
                   Path      => Symlink_Path.all,
                   Ret_Count => Symlink_Len,
                   Success   => Success);
               if Success /= FS_Success or else
                  Symlink_Path'Length < Symlink_Len
               then
                  goto Invalid_Value_Return;
               end if;

               Open
                  (Key        => Orig_Key,
                   Relative   => Orig_Ino,
                   Path       => Symlink_Path (1 .. Symlink_Len) & "/" &
                              Path (Path'First + Path_Last + 1 .. Path'Last),
                   Final_Key  => Final_Key,
                   Ino        => Ino,
                   Success    => Success,
                   User       => User,
                   Want_Read  => Want_Read,
                   Want_Write => Want_Write,
                   Do_Follow  => Do_Follow);
               goto Cleanup_Only_Return;
            when others =>
               goto Invalid_Value_Return;
         end case;
         Path_Idx := Path_Last + 1;
      end loop;

      --  Check that we can actually open with what is wanted.
      VFS.Stat (Actual_Key, Actual_Ino, Entry_Stat, Success);
      if Success /= FS_Success then
         goto Invalid_Value_Return;
      end if;
      if not Can_Access_File
         (User       => User,
          File_Owner => Entry_Stat.UID,
          Mode       => Entry_Stat.Mode,
          Want_Read  => Want_Read,
          Want_Write => Want_Write,
          Want_Exec  => False)
      then
         goto Not_Allowed_Return;
      end if;

      Free1 (Symlink_Path);
      Free2 (Dir_Entries);
      Final_Key := Actual_Key;
      Ino       := Actual_Ino;
      Success   := FS_Success;
      return;

   <<Not_Found_Return>>
      Free1 (Symlink_Path);
      Free2 (Dir_Entries);
      Final_Key := Error_Handle;
      Ino       := 0;
      Success   := FS_Not_Found;
      return;

   <<Invalid_Value_Return>>
      Free1 (Symlink_Path);
      Free2 (Dir_Entries);
      Final_Key := Error_Handle;
      Ino       := 0;
      Success   := FS_Invalid_Value;
      return;

   <<Not_Allowed_Return>>
      Free1 (Symlink_Path);
      Free2 (Dir_Entries);
      Final_Key := Error_Handle;
      Ino       := 0;
      Success   := FS_Not_Allowed;
      return;

   <<Cleanup_Only_Return>>
      Free1 (Symlink_Path);
      Free2 (Dir_Entries);
   end Open;

   procedure Create_Node
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       Kind     : File_Type;
       Mode     : File_Mode;
       User     : Unsigned_32;
       Group    : Unsigned_32;
       Status   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Create_Node
               (Mounts (Key).FS_Data, Relative, Path, Kind, Mode, User, Group,
                Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Create_Node;

   procedure Create_Symbolic_Link
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       Target   : String;
       Mode     : Unsigned_32;
       User     : Unsigned_32;
       Status   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Create_Symbolic_Link
               (Mounts (Key).FS_Data, Relative, Path, Target, Mode, User,
                Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Create_Symbolic_Link;

   procedure Create_Hard_Link
      (Key             : FS_Handle;
       Relative_Path   : File_Inode_Number;
       Path            : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       User            : Unsigned_32;
       Status          : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Create_Hard_Link
               (Mounts (Key).FS_Data, Relative_Path, Path, Relative_Target,
                Target, User, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Create_Hard_Link;

   procedure Rename
      (Key             : FS_Handle;
       Relative_Source : File_Inode_Number;
       Source          : String;
       Relative_Target : File_Inode_Number;
       Target          : String;
       Keep            : Boolean;
       User            : Unsigned_32;
       Status          : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Rename
               (Mounts (Key).FS_Data, Relative_Source, Source, Relative_Target,
                Target, Keep, User, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Rename;

   procedure Unlink
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       User     : Unsigned_32;
       Do_Dir   : Boolean;
       Status   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Unlink (Mounts (Key).FS_Data, Relative, Path, User, Do_Dir,
                        Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Unlink;

   procedure Read_Entries
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Read_Entries
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Entities,
                Ret_Count,
                Success);
         when FS_EXT =>
            EXT.Read_Entries
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Entities,
                Ret_Count,
                Success);
         when FS_FAT =>
            FAT.Read_Entries
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Entities,
                Ret_Count,
                Success);
      end case;
   end Read_Entries;

   procedure Read_Symbolic_Link
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Path      : out String;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Read_Symbolic_Link
               (Mounts (Key).FS_Data, Ino, Path, Ret_Count, Success);
         when others =>
            Path      := [others => ' '];
            Ret_Count := 0;
            Success   := FS_Not_Supported;
      end case;
   end Read_Symbolic_Link;

   procedure Read
      (Key         : FS_Handle;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Read
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Is_Blocking,
                Success);
         when FS_EXT =>
            EXT.Read
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Success);
         when FS_FAT =>
            FAT.Read
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Success);
      end case;
   end Read;

   procedure Write
      (Key         : FS_Handle;
       Ino         : File_Inode_Number;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean;
       Success     : out FS_Status)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Write
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Is_Blocking,
                Success);
         when FS_EXT =>
            EXT.Write
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Success);
         when others =>
            Ret_Count := 0;
            Success   := FS_Not_Supported;
      end case;
   end Write;

   procedure Stat
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       Stat_Val : out File_Stat;
       Success  : out FS_Status)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Stat (Mounts (Key).FS_Data, Ino, Stat_Val, Success);
         when FS_EXT =>
            EXT.Stat (Mounts (Key).FS_Data, Ino, Stat_Val, Success);
         when FS_FAT =>
            FAT.Stat (Mounts (Key).FS_Data, Ino, Stat_Val, Success);
      end case;
   end Stat;

   procedure Truncate
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       New_Size : Unsigned_64;
       Status   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Truncate (Mounts (Key).FS_Data, Ino, New_Size, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Truncate;

   procedure IO_Control
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Request   : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Status    : out FS_Status)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.IO_Control
               (Mounts (Key).FS_Data, Ino, Request, Arg, Has_Extra, Extra,
                Status);
         when FS_EXT =>
            EXT.IO_Control
               (Mounts (Key).FS_Data, Ino, Request, Arg, Status);
            Has_Extra := False;
            Extra     := 0;
         when others =>
            Has_Extra := False;
            Extra     := 0;
            Status    := FS_Not_Supported;
      end case;
   end IO_Control;

   procedure Mmap
      (Key     : FS_Handle;
       Ino     : File_Inode_Number;
       Map     : Memory.MMU.Page_Table_Acc;
       Offset  : Unsigned_64;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions;
       Status  : out FS_Status)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
      pragma Unreferenced (Offset);
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Mmap
               (Mounts (Key).FS_Data, Ino, Map, Address, Length, Flags,
                Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Mmap;

   procedure Poll
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Poll
               (Mounts (Key).FS_Data, Ino, Can_Read, Can_Write, Is_Error);
         when others =>
            Can_Read  := True;
            Can_Write := True;
            Is_Error  := False;
      end case;
   end Poll;

   procedure Synchronize (Key : FS_Handle; Status : out FS_Status) is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Synchronize (Mounts (Key).FS_Data, Status);
         when FS_EXT => EXT.Synchronize (Mounts (Key).FS_Data, Status);
         when FS_FAT => Status := FS_Not_Supported;
      end case;
   end Synchronize;

   procedure Synchronize
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Data_Only : Boolean;
       Status    : out FS_Status)
   is
      pragma Annotate (GNATprove, False_Positive, "precondition might fail",
         "No it does not");
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Synchronize (Mounts (Key).FS_Data, Ino, Data_Only, Status);
         when FS_EXT =>
            EXT.Synchronize (Mounts (Key).FS_Data, Ino, Data_Only, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Synchronize;

   procedure Change_Mode
      (Key    : FS_Handle;
       Ino    : File_Inode_Number;
       Mode   : File_Mode;
       Status : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Change_Mode (Mounts (Key).FS_Data, Ino, Mode, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Change_Mode;

   procedure Change_Owner
      (Key    : FS_Handle;
       Ino    : File_Inode_Number;
       Owner  : Unsigned_32;
       Group  : Unsigned_32;
       Status : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Change_Owner
               (Mounts (Key).FS_Data, Ino, Owner, Group, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Change_Owner;

   procedure Check_Access
      (Key         : FS_Handle;
       Ino         : File_Inode_Number;
       Exists_Only : Boolean;
       Can_Read    : Boolean;
       Can_Write   : Boolean;
       Can_Exec    : Boolean;
       Real_UID    : Unsigned_32;
       Status      : out FS_Status)
   is
      Ino_Stat : File_Stat;
   begin
      Stat (Key, Ino, Ino_Stat, Status);
      if Status /= FS_Success or else Exists_Only then
         return;
      end if;

      if Can_Access_File
         (User       => Real_UID,
          File_Owner => Ino_Stat.UID,
          Mode       => Ino_Stat.Mode,
          Want_Read  => Can_Read,
          Want_Write => Can_Write,
          Want_Exec  => Can_Exec)
      then
         Status := FS_Success;
      else
         Status := FS_Not_Allowed;
      end if;
   end Check_Access;

   procedure Change_Access_Times
      (Key                : FS_Handle;
       Ino                : File_Inode_Number;
       Access_Seconds     : Unsigned_64;
       Access_Nanoseconds : Unsigned_64;
       Modify_Seconds     : Unsigned_64;
       Modify_Nanoseconds : Unsigned_64;
       Status             : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Change_Access_Times
               (Mounts (Key).FS_Data, Ino, Access_Seconds, Access_Nanoseconds,
                Modify_Seconds, Modify_Nanoseconds, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Change_Access_Times;
   ----------------------------------------------------------------------------
   procedure Open
      (Path       : String;
       Key        : out FS_Handle;
       Ino        : out File_Inode_Number;
       Success    : out FS_Status;
       User       : Unsigned_32;
       Want_Read  : Boolean;
       Want_Write : Boolean;
       Do_Follow  : Boolean := True)
   is
   begin
      if Root_Idx = Error_Handle then
         Key     := Error_Handle;
         Ino     := 0;
         Success := FS_Invalid_Value;
         return;
      end if;

      Open
         (Key        => Root_Idx,
          Relative   => Mounts (Root_Idx).Root_Ino,
          Path       => Path,
          Final_Key  => Key,
          Ino        => Ino,
          Success    => Success,
          User       => User,
          Want_Read  => Want_Read,
          Want_Write => Want_Write,
          Do_Follow  => Do_Follow);
   end Open;

   procedure Synchronize (Success : out Boolean) is
      Status : FS_Status;
   begin
      Success := True;

      Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle then
            Synchronize (I, Status);
            if Status /= FS_Success then
               Success := False;
            end if;
         end if;
      end loop;
      Synchronization.Release (Mounts_Mutex);
   end Synchronize;

   procedure Create_Node
      (Path    : String;
       Kind    : File_Type;
       Mode    : File_Mode;
       Success : out FS_Status;
       User    : Unsigned_32;
       Group   : Unsigned_32)
   is
   begin
      if Root_Idx = Error_Handle then
         Success := FS_Invalid_Value;
         return;
      end if;

      Create_Node
         (Key      => Root_Idx,
          Relative => Mounts (Root_Idx).Root_Ino,
          Path     => Path,
          Kind     => Kind,
          Mode     => Mode,
          User     => User,
          Group    => Group,
          Status   => Success);
   end Create_Node;
   ----------------------------------------------------------------------------
   function Is_Absolute (Path : String) return Boolean is
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute;

   function Apply_Umask (Mode, Umask : File_Mode) return File_Mode is
   begin
      return File_Mode (Unsigned_32 (Mode) and not Unsigned_32 (Umask));
   end Apply_Umask;

   function Can_Access_File
      (User       : Unsigned_32;
       File_Owner : Unsigned_32;
       Mode       : File_Mode;
       Want_Read  : Boolean;
       Want_Write : Boolean;
       Want_Exec  : Boolean) return Boolean
   is
   begin
      if User = 0 then
         return True;
      end if;

      if File_Owner = User then
         if (Want_Read  and then ((Mode and 8#400#) = 0)) or
            (Want_Write and then ((Mode and 8#200#) = 0)) or
            (Want_Exec  and then ((Mode and 8#100#) = 0))
         then
            return False;
         end if;
      else
         if (Want_Read  and then ((Mode and 8#004#) = 0)) or
            (Want_Write and then ((Mode and 8#002#) = 0)) or
            (Want_Exec  and then ((Mode and 8#001#) = 0))
         then
            return False;
         end if;
      end if;

      return True;
   end Can_Access_File;
end VFS;
