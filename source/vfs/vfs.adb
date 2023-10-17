--  vfs.adb: FS and register dispatching.
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

with VFS.EXT;
with VFS.FAT;
with VFS.QNX;
with Ada.Characters.Latin_1;

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
       Success      : out Boolean)
   is
      pragma SPARK_Mode (Off);
   begin
      Mount (Device_Name, Mount_Path, FS_EXT, Do_Read_Only, Success);
      if Success then return; end if;
      Mount (Device_Name, Mount_Path, FS_FAT, Do_Read_Only, Success);
      if Success then return; end if;
      Mount (Device_Name, Mount_Path, FS_QNX, Do_Read_Only, Success);
   end Mount;

   procedure Mount
      (Device_Name  : String;
       Mount_Path   : String;
       FS           : FS_Type;
       Do_Read_Only : Boolean;
       Success      : out Boolean)
   is
      Dev     : constant Device_Handle := Devices.Fetch (Device_Name);
      Free_I  :              FS_Handle := VFS.Error_Handle;
      FS_Data : System.Address;
   begin
      if not Is_Absolute (Mount_Path)           or
         Mount_Path'Length > Path_Buffer_Length or
         Dev = Devices.Error_Handle
      then
         Success := False;
         return;
      end if;

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev = Dev then
            Free_I := VFS.Error_Handle;
            goto Return_End;
         elsif Mounts (I).Mounted_Dev = Devices.Error_Handle then
            Free_I := I;
         end if;
      end loop;
      if Free_I = VFS.Error_Handle then
         goto Return_End;
      end if;

      case FS is
         when FS_EXT => VFS.EXT.Probe (Dev, Do_Read_Only, FS_Data);
         when FS_FAT => VFS.FAT.Probe (Dev, Do_Read_Only, FS_Data);
         when FS_QNX => VFS.QNX.Probe (Dev, Do_Read_Only, FS_Data);
      end case;
      Mounts (Free_I).Mounted_FS := FS;

      if FS_Data /= System.Null_Address then
         Mounts (Free_I).FS_Data := FS_Data;
         Mounts (Free_I).Mounted_Dev := Dev;
         Mounts (Free_I).Path_Length := Mount_Path'Length;
         Mounts (Free_I).Path_Buffer (1 .. Mount_Path'Length) := Mount_Path;
      else
         Free_I := VFS.Error_Handle;
      end if;

   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
      Success := Free_I /= VFS.Error_Handle;
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
               when FS_EXT => EXT.Unmount (Mounts (I).FS_Data);
               when FS_FAT => FAT.Unmount (Mounts (I).FS_Data);
               when FS_QNX => QNX.Unmount (Mounts (I).FS_Data);
            end case;

            if Force or Mounts (I).FS_Data = Null_Address then
               Mounts (I).Mounted_Dev := Devices.Error_Handle;
               Success := True;
            end if;
            goto Return_End;
         end if;
      end loop;
   <<Return_End>>
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

   procedure List_All (List : out Mountpoint_Info_Arr; Total : out Natural) is
      pragma Annotate (GNATprove, False_Positive, "range check might fail",
                       "in List? how?");

      Curr_Index        : Natural := 0;
      Dev_Name          : String (1 .. Devices.Max_Name_Length);
      Dev_Len, Path_Len : Natural;
   begin
      Total := 0;
      List  := (others => (FS_EXT, (others => ' '), 0, (others => ' '), 0));

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle then
            Total := Total + 1;
            if Curr_Index < List'Length then
               Devices.Fetch_Name (Mounts (I).Mounted_Dev, Dev_Name, Dev_Len);
               Path_Len := Mounts (I).Path_Length;

               List (List'First + Curr_Index) :=
                  (Inner_Type   => Mounts (I).Mounted_FS,
                   Source       => Dev_Name (1 .. 20),
                   Source_Len   => Dev_Len,
                   Location     => (others => Ada.Characters.Latin_1.NUL),
                   Location_Len => Path_Len);
               if Path_Len <= List (List'First + Curr_Index).Location'Length
               then
                  List (List'First + Curr_Index).Location (1 .. Path_Len) :=
                     Mounts (I).Path_Buffer (1 .. Path_Len);
               else
                  List (List'First + Curr_Index).Location (1 .. 20) :=
                     Mounts (I).Path_Buffer (1 .. 20);
               end if;
               Curr_Index := Curr_Index + 1;
            end if;
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
   ----------------------------------------------------------------------------
   procedure Open
      (Key       : FS_Handle;
       Relative  : File_Inode_Number;
       Path      : String;
       Ino       : out File_Inode_Number;
       Success   : out FS_Status;
       User      : Unsigned_32;
       Do_Follow : Boolean := True)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Open
               (FS        => Mounts (Key).FS_Data,
                Relative  => Relative,
                Path      => Path,
                Ino       => Ino,
                Success   => Success,
                User      => User,
                Do_Follow => Do_Follow);
         when FS_FAT =>
            FAT.Open
               (FS      => Mounts (Key).FS_Data,
                Path    => Path,
                Ino     => Ino,
                Success => Success);
         when FS_QNX =>
            Ino     := 0;
            Success := FS_Not_Supported;
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

   function Create_Symbolic_Link
      (Key      : FS_Handle;
       Relative : File_Inode_Number;
       Path     : String;
       Target   : String;
       Mode     : Unsigned_32;
       User     : Unsigned_32) return FS_Status
   is
      Status : FS_Status;
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Create_Symbolic_Link
               (Mounts (Key).FS_Data, Relative, Path, Target, Mode, User,
                Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
      return Status;
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
         when FS_EXT => EXT.Close (Mounts (Key).FS_Data, Ino);
         when FS_FAT => FAT.Close (Mounts (Key).FS_Data, Ino);
         when FS_QNX => null;
      end case;
   end Close;

   procedure Read_Entries
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Natural;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status;
       User      : Unsigned_32)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Read_Entries
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Entities,
                Ret_Count,
                Success,
                User);
         when FS_FAT =>
            FAT.Read_Entries
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Entities,
                Ret_Count,
                Success);
         when FS_QNX =>
            Entities  := (others => (0, (others => ' '), 0, File_Regular));
            Ret_Count := 0;
            Success   := FS_Not_Supported;
      end case;
   end Read_Entries;

   procedure Read_Symbolic_Link
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Path      : out String;
       Ret_Count : out Natural;
       Success   : out FS_Status;
       User      : Unsigned_32)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Read_Symbolic_Link
               (Mounts (Key).FS_Data, Ino, Path, Ret_Count, Success, User);
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
       Success   : out FS_Status;
       User      : Unsigned_32)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Read
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Success,
                User);
         when FS_FAT =>
            FAT.Read
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Success);
         when FS_QNX =>
            Data      := (others => 0);
            Ret_Count := 0;
            Success   := FS_Not_Supported;
      end case;
   end Read;

   procedure Write
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status;
       User      : Unsigned_32)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Write
               (Mounts (Key).FS_Data,
                Ino,
                Offset,
                Data,
                Ret_Count,
                Success,
                User);
         when others =>
            Ret_Count := 0;
            Success   := FS_Not_Supported;
      end case;
   end Write;

   procedure Stat
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       Stat_Val : out File_Stat;
       Success  : out FS_Status;
       User     : Unsigned_32)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Stat (Mounts (Key).FS_Data, Ino, Stat_Val, User, Success);
         when FS_FAT =>
            FAT.Stat (Mounts (Key).FS_Data, Ino, Stat_Val, Success);
         when FS_QNX =>
            Stat_Val :=
               (Unique_Identifier => 0,
                Type_Of_File      => File_Regular,
                Mode              => 0,
                UID               => 0,
                GID               => 0,
                Hard_Link_Count   => 1,
                Byte_Size         => 0,
                IO_Block_Size     => 0,
                IO_Block_Count    => 0,
                Creation_Time     => (0, 0),
                Modification_Time => (0, 0),
                Access_Time       => (0, 0));
            Success := FS_Not_Supported;
      end case;
   end Stat;

   procedure Truncate
      (Key      : FS_Handle;
       Ino      : File_Inode_Number;
       New_Size : Unsigned_64;
       User     : Unsigned_32;
       Status   : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Truncate (Mounts (Key).FS_Data, Ino, New_Size, User, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Truncate;

   procedure IO_Control
      (Key     : FS_Handle;
       Ino     : File_Inode_Number;
       Request : Unsigned_64;
       Arg     : System.Address;
       User    : Unsigned_32;
       Status  : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.IO_Control
               (Mounts (Key).FS_Data, Ino, Request, Arg, User, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end IO_Control;

   function Synchronize (Key : FS_Handle) return FS_Status is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT => return EXT.Synchronize (Mounts (Key).FS_Data);
         when others => return FS_Not_Supported;
      end case;
   end Synchronize;

   function Synchronize
      (Key       : FS_Handle;
       Ino       : File_Inode_Number;
       Data_Only : Boolean) return FS_Status
   is
   begin
      case Mounts (Key).Mounted_FS is
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
       User   : Unsigned_32;
       Status : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Change_Mode (Mounts (Key).FS_Data, Ino, Mode, User, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Change_Mode;

   procedure Change_Owner
      (Key    : FS_Handle;
       Ino    : File_Inode_Number;
       Owner  : Unsigned_32;
       Group  : Unsigned_32;
       User   : Unsigned_32;
       Status : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Change_Owner
               (Mounts (Key).FS_Data, Ino, Owner, Group, User, Status);
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
       User        : Unsigned_32;
       Status      : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Check_Access
               (Mounts (Key).FS_Data, Ino, Exists_Only, Can_Read, Can_Write,
                Can_Exec, User, Status);
         when others =>
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
       User               : Unsigned_32;
       Status             : out FS_Status)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_EXT =>
            EXT.Change_Access_Times
               (Mounts (Key).FS_Data, Ino, Access_Seconds, Access_Nanoseconds,
                Modify_Seconds, Modify_Nanoseconds, User, Status);
         when others =>
            Status := FS_Not_Supported;
      end case;
   end Change_Access_Times;
   ----------------------------------------------------------------------------
   procedure Open
      (Path      : String;
       Key       : out FS_Handle;
       Ino       : out File_Inode_Number;
       Success   : out FS_Status;
       User      : Unsigned_32;
       Do_Follow : Boolean := True)
   is
      Match_Count : Natural;
   begin
      Get_Mount (Path, Match_Count, Key);
      if Key /= Error_Handle and Path'First < Natural'Last - Match_Count
      then
         Open
            (Key,
             0,
             Path (Path'First + Match_Count .. Path'Last),
             Ino, Success, User, Do_Follow);
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
