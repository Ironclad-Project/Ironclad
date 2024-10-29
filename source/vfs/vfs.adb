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

with VFS.Dev;
with VFS.EXT;
with VFS.FAT;

package body VFS is
   pragma Suppress (All_Checks); --  Unit passes SPARK AoRTE.

   procedure Init is
      pragma SPARK_Mode (Off);
   begin
      Mounts := new Mount_Registry'(others =>
         (Mounted_Dev => Devices.Error_Handle,
          Mounted_FS  => FS_EXT,
          FS_Data     => System.Null_Address,
          Path_Length => 0,
          Path_Buffer => (others => ' ')));
      Mounts_Mutex := Lib.Synchronization.Unlocked_Semaphore;
   end Init;

   procedure Mount
      (Device_Name  : String;
       Mount_Path   : String;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
   is
      pragma SPARK_Mode (Off);
      Name : String renames Device_Name;
   begin
      for FS in FS_Type'Range loop
         if FS /= FS_DEV then
            Mount (Name, Mount_Path, FS, Do_Read_Only, Do_Relatime, Success);
            exit when Success;
         end if;
      end loop;
   end Mount;

   procedure Mount
      (Device_Name  : String;
       Mount_Path   : String;
       FS           : FS_Type;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
   is
      De      : constant Device_Handle := Devices.Fetch (Device_Name);
      Free_I  :              FS_Handle := VFS.Error_Handle;
      FS_Data : System.Address;
   begin
      Success := False;

      if not Is_Absolute (Mount_Path)           or
         Mount_Path'Length > Path_Buffer_Length or
         De = Devices.Error_Handle
      then
         return;
      end if;

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev = De then
            goto Cleanup;
         elsif Mounts (I).Mounted_Dev = Devices.Error_Handle then
            Free_I := I;
            goto Try_Probe;
         end if;
      end loop;

      goto Cleanup;

   <<Try_Probe>>
      case FS is
         when FS_DEV => Dev.Probe (De, Do_Read_Only, Do_Relatime, FS_Data);
         when FS_EXT => EXT.Probe (De, Do_Read_Only, Do_Relatime, FS_Data);
         when FS_FAT => FAT.Probe (De, Do_Read_Only, FS_Data);
      end case;

      if FS_Data /= System.Null_Address then
         Mounts (Free_I) :=
            (Mounted_Dev => De,
             Mounted_FS  => FS,
             FS_Data     => FS_Data,
             Path_Length => Mount_Path'Length,
             Path_Buffer => (others => ' '));
         Mounts (Free_I).Path_Buffer (1 .. Mount_Path'Length) := Mount_Path;
         Success := True;
      end if;

   <<Cleanup>>
      Lib.Synchronization.Release (Mounts_Mutex);
   end Mount;

   procedure Unmount (Path : String; Force : Boolean; Success : out Boolean) is
   begin
      Success := False;
      Lib.Synchronization.Seize (Mounts_Mutex);
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
      Lib.Synchronization.Release (Mounts_Mutex);
   end Unmount;

   procedure Get_Mount
      (Path   : String;
       Match  : out Natural;
       Handle : out FS_Handle)
   is
   begin
      Match  := 0;
      Handle := VFS.Error_Handle;

      Lib.Synchronization.Seize (Mounts_Mutex);
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
      Lib.Synchronization.Release (Mounts_Mutex);

      if Match > 0 then
         Match := Match - 1;
      end if;
   end Get_Mount;

   procedure List_All (List : out Mountpoint_Arr; Total : out Natural) is
      pragma Annotate (GNATprove, False_Positive, "range check might fail",
                       "in List? how?");

      Curr_Index : Natural := 0;
   begin
      Total := 0;
      List  := (others => Error_Handle);

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle then
            if Curr_Index < List'Length then
               List (List'First + Curr_Index) := I;
               Curr_Index := Curr_Index + 1;
            end if;
            Total := Total + 1;
         end if;
      end loop;
      Lib.Synchronization.Release (Mounts_Mutex);
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
      Name := (others => ' ');
      Name (Name'First .. Name'First + Path_Buffer_Length - 1) :=
         Mounts (Key).Path_Buffer;
      Length := Mounts (Key).Path_Length;
   end Get_Mount_Point;

   procedure Remount
      (Key          : FS_Handle;
       Do_Read_Only : Boolean;
       Do_Relatime  : Boolean;
       Success      : out Boolean)
   is
      Data : constant System.Address := Mounts (Key).FS_Data;
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Remount (Data, Do_Read_Only, Do_Relatime, Success);
         when FS_EXT => EXT.Remount (Data, Do_Read_Only, Do_Relatime, Success);
         when FS_FAT => FAT.Remount (Data, Do_Read_Only, Do_Relatime, Success);
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
       Free_Unpriviledged : out Unsigned_64)
   is
      FB : Unsigned_64 renames Free_Blocks;
      FU : Unsigned_64 renames Free_Unpriviledged;
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
       Free_Unpriviledged : out Unsigned_64)
   is
      FI : Unsigned_64 renames Free_Inodes;
      FU : Unsigned_64 renames Free_Unpriviledged;
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
   begin
      Final_Key := Key;

      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Open
               (FS         => Mounts (Key).FS_Data,
                Relative   => Relative,
                Path       => Path,
                Ino        => Ino,
                Success    => Success,
                User       => User,
                Want_Read  => Want_Read,
                Want_Write => Want_Write,
                Do_Follow  => Do_Follow);
         when FS_EXT =>
            EXT.Open
               (FS         => Mounts (Key).FS_Data,
                Relative   => Relative,
                Path       => Path,
                Ino        => Ino,
                Success    => Success,
                User       => User,
                Want_Read  => Want_Read,
                Want_Write => Want_Write,
                Do_Follow  => Do_Follow);
         when FS_FAT =>
            FAT.Open
               (FS      => Mounts (Key).FS_Data,
                Path    => Path,
                Ino     => Ino,
                Success => Success);
      end case;
   end Open;

   procedure Create_Node
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       Typ      : File_Type;
       Mode     : File_Mode;
       User     : Unsigned_32;
       Status   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Create_Node
               (Mounts (Key).FS_Data, Relative, Path, Typ, Mode, User, Status);
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
       Status   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Unlink (Mounts (Key).FS_Data, Relative, Path, User, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Unlink;

   procedure Close (Key : FS_Handle; Ino : File_Inode_Number) is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => Dev.Close (Mounts (Key).FS_Data, Ino);
         when FS_EXT => EXT.Close (Mounts (Key).FS_Data, Ino);
         when FS_FAT => FAT.Close (Mounts (Key).FS_Data, Ino);
      end case;
   end Close;

   procedure Read_Entries
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
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
            Path      := (others => ' ');
            Ret_Count := 0;
            Success   := FS_Not_Supported;
      end case;
   end Read_Symbolic_Link;

   procedure Read
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Read
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
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
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Write
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
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
      (Key     : FS_Handle;
       Ino     : File_Inode_Number;
       Request : Unsigned_64;
       Arg     : System.Address;
       Status  : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.IO_Control
               (Mounts (Key).FS_Data, Ino, Request, Arg, Status);
         when FS_EXT =>
            EXT.IO_Control
               (Mounts (Key).FS_Data, Ino, Request, Arg, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end IO_Control;

   function Synchronize (Key : FS_Handle) return FS_Status is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV => return Dev.Synchronize (Mounts (Key).FS_Data);
         when FS_EXT => return EXT.Synchronize (Mounts (Key).FS_Data);
         when FS_FAT => return FS_Not_Supported;
      end case;
   end Synchronize;

   function Synchronize
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            return Dev.Synchronize (Mounts (Key).FS_Data, Ino, Data_Only);
         when FS_EXT =>
            return EXT.Synchronize (Mounts (Key).FS_Data, Ino, Data_Only);
         when others =>
            return FS_Not_Supported;
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
   begin
      case Mounts (Key).Mounted_FS is
         when FS_DEV =>
            Dev.Check_Access
               (Mounts (Key).FS_Data, Ino, Exists_Only, Can_Read, Can_Write,
                Can_Exec, Real_UID, Status);
         when FS_EXT =>
            EXT.Check_Access
               (Mounts (Key).FS_Data, Ino, Exists_Only, Can_Read, Can_Write,
                Can_Exec, Real_UID, Status);
         when FS_FAT =>
            Status := FS_Not_Supported;
      end case;
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
      Rela_Key    : FS_Handle;
      Match_Count : Natural;
   begin
      Get_Mount (Path, Match_Count, Rela_Key);
      if Rela_Key /= Error_Handle and Path'First < Natural'Last - Match_Count
      then
         Open
            (Key        => Rela_Key,
             Relative   => 0,
             Path       => Path (Path'First + Match_Count .. Path'Last),
             Final_Key  => Key,
             Ino        => Ino,
             Success    => Success,
             User       => User,
             Want_Read  => Want_Read,
             Want_Write => Want_Write,
             Do_Follow  => Do_Follow);
      else
         Key     := Error_Handle;
         Ino     := 0;
         Success := FS_Invalid_Value;
      end if;
   end Open;

   procedure Synchronize (Success : out Boolean) is
   begin
      Success := True;

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle then
            if Synchronize (I) = FS_IO_Failure then
               Success := False;
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Mounts_Mutex);
   end Synchronize;

   procedure Create_Node
      (Path    : String;
       Typ     : File_Type;
       Mode    : File_Mode;
       Success : out FS_Status;
       User    : Unsigned_32)
   is
      Match_Count : Natural;
      Handle      : FS_Handle;
   begin
      Get_Mount (Path, Match_Count, Handle);
      if Handle /= Error_Handle and Path'First < Natural'Last - Match_Count
      then
         Create_Node
            (Handle, 0, Path (Path'First + Match_Count .. Path'Last), Typ,
             Mode, User, Success);
      else
         Success := FS_Invalid_Value;
      end if;
   end Create_Node;
   ----------------------------------------------------------------------------
   function Is_Absolute (Path : String) return Boolean is
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute;
end VFS;
