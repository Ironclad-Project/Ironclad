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
with VFS.USTAR;
with Lib.Synchronization;

package body VFS with SPARK_Mode => Off is
   Path_Buffer_Length : constant := 100;
   type Mount_Container is record
      Mounted_Dev : Device_Handle;
      Mounted_FS  : FS_Type;
      FS_Data     : System.Address;
      Path_Length : Natural;
      Path_Buffer : String (1 .. Path_Buffer_Length);
   end record;
   type Mount_Arr is array (FS_Handle range 1 .. 5) of Mount_Container;
   type Mount_Arr_Acc is access Mount_Arr;

   Mounts       : Mount_Arr_Acc;
   Mounts_Mutex : aliased Lib.Synchronization.Binary_Semaphore;

   procedure Init is
   begin
      Mounts       := new Mount_Arr'(others =>
         (Mounted_Dev => Devices.Error_Handle,
          Mounted_FS  => FS_USTAR,
          FS_Data     => System.Null_Address,
          Path_Length => 0,
          Path_Buffer => (others => ' ')));
      Mounts_Mutex := Lib.Synchronization.Unlocked_Semaphore;
   end Init;

   function Mount (Name, Path : String; FS : FS_Type) return Boolean is
      Dev     : constant Device_Handle := Devices.Fetch (Name);
      FS_Data : System.Address         := System.Null_Address;
      Free_I  : FS_Handle              := VFS.Error_Handle;
   begin
      if not Is_Absolute (Path) or Path'Length > Path_Buffer_Length or
         Dev = Devices.Error_Handle
      then
         return False;
      end if;

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev = Dev then
            goto Return_End;
         elsif Mounts (I).Mounted_Dev = Devices.Error_Handle then
            Free_I := I;
         end if;
      end loop;
      if Free_I = VFS.Error_Handle then
         goto Return_End;
      end if;

      case FS is
         when FS_USTAR =>
            FS_Data := VFS.USTAR.Probe (Dev);
            Mounts (Free_I).Mounted_FS := FS_USTAR;
         when FS_EXT =>
            FS_Data := VFS.EXT.Probe (Dev);
            Mounts (Free_I).Mounted_FS := FS_EXT;
      end case;

      if FS_Data /= System.Null_Address then
         Mounts (Free_I).FS_Data := FS_Data;
         Mounts (Free_I).Mounted_Dev                    := Dev;
         Mounts (Free_I).Path_Length                    := Path'Length;
         Mounts (Free_I).Path_Buffer (1 .. Path'Length) := Path;
      else
         Free_I := VFS.Error_Handle;
      end if;

   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
      return Free_I /= VFS.Error_Handle;
   end Mount;

   function Mount (Name, Path : String) return Boolean is
   begin
      return Mount (Name, Path, FS_EXT) or else Mount (Name, Path, FS_USTAR);
   end Mount;

   function Unmount (Path : String; Force : Boolean) return Boolean is
      Success : Boolean := False;
   begin
      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = Path
         then
            case Mounts (I).Mounted_FS is
               when FS_USTAR => USTAR.Unmount (Mounts (I).FS_Data);
               when FS_EXT   => EXT.Unmount   (Mounts (I).FS_Data);
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
      return Success;
   end Unmount;

   function Get_Mount (Path : String) return FS_Handle is
      Returned : FS_Handle := VFS.Error_Handle;
   begin
      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= Devices.Error_Handle and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = Path
         then
            Returned := I;
            goto Return_End;
         end if;
      end loop;
   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
      return Returned;
   end Get_Mount;

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

   function Open (Key : FS_Handle; Path : String) return System.Address is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR => return USTAR.Open (Mounts (Key).FS_Data, Path);
         when FS_EXT   => return EXT.Open   (Mounts (Key).FS_Data, Path);
      end case;
   end Open;

   function Create
      (Key  : FS_Handle;
       Path : String;
       Mode : Unsigned_32) return System.Address
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR => return Null_Address;
         when FS_EXT   => return EXT.Create (Mounts (Key).FS_Data, Path, Mode);
      end case;
   end Create;

   procedure Close (Key : FS_Handle; Obj : in out System.Address) is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR => Obj := Null_Address;
         when FS_EXT   => EXT.Close (Mounts (Key).FS_Data, Obj);
      end case;
   end Close;

   procedure Read_Entries
      (Key       : FS_Handle;
       Obj       : System.Address;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR =>
            USTAR.Read_Entries
               (Mounts (Key).FS_Data,
                Obj,
                Entities,
                Ret_Count,
                Success);
         when FS_EXT =>
            EXT.Read_Entries
               (Mounts (Key).FS_Data,
                Obj,
                Entities,
                Ret_Count,
                Success);
      end case;
   end Read_Entries;

   procedure Read_Symbolic_Link
      (Key       : FS_Handle;
       Obj       : System.Address;
       Path      : out String;
       Ret_Count : out Natural)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR =>
            USTAR.Read_Symbolic_Link
               (Mounts (Key).FS_Data, Obj, Path, Ret_Count);
         when FS_EXT =>
            EXT.Read_Symbolic_Link
               (Mounts (Key).FS_Data, Obj, Path, Ret_Count);
      end case;
   end Read_Symbolic_Link;

   function Create_Symbolic_Link
      (Key          : FS_Handle;
       Path, Target : String;
       Mode         : Unsigned_32) return System.Address
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR =>
            return System.Null_Address;
         when FS_EXT =>
            return EXT.Create_Symbolic_Link
               (Mounts (Key).FS_Data,
                Path,
                Target,
                Mode);
      end case;
   end Create_Symbolic_Link;

   function Create_Directory
      (Key  : FS_Handle;
       Path : String;
       Mode : Unsigned_32) return System.Address
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR =>
            return System.Null_Address;
         when FS_EXT =>
            return EXT.Create_Directory (Mounts (Key).FS_Data, Path, Mode);
      end case;
   end Create_Directory;

   procedure Read
      (Key       : FS_Handle;
       Obj       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR =>
            USTAR.Read
               (Mounts (Key).FS_Data,
                Obj,
                Offset,
                Data,
                Ret_Count,
                Success);
         when FS_EXT =>
            EXT.Read
               (Mounts (Key).FS_Data,
                Obj,
                Offset,
                Data,
                Ret_Count,
                Success);
      end case;
   end Read;

   procedure Write
      (Key       : FS_Handle;
       Obj       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR =>
            Ret_Count := 0;
            Success   := False;
         when FS_EXT =>
            EXT.Write
               (Mounts (Key).FS_Data,
                Obj,
                Offset,
                Data,
                Ret_Count,
                Success);
      end case;
   end Write;

   function Stat
      (Key  : FS_Handle;
       Obj  : System.Address;
       S    : out File_Stat) return Boolean
   is
   begin
      case Mounts (Key).Mounted_FS is
         when FS_USTAR => return USTAR.Stat (Mounts (Key).FS_Data, Obj, S);
         when FS_EXT   => return EXT.Stat   (Mounts (Key).FS_Data, Obj, S);
      end case;
   end Stat;
   ----------------------------------------------------------------------------
   function Is_Absolute (Path : String) return Boolean is
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute;

   function Is_Canonical (Path : String) return Boolean is
      Previous : Character := ' ';
   begin
      if not Is_Absolute (Path) or else Path (Path'Last) = '/' then
         return False;
      end if;

      for C of Path loop
         if (Previous = '/' and C = '/') or (Previous = '.' and C = '.') then
            return False;
         end if;
         Previous := C;
      end loop;

      return True;
   end Is_Canonical;
end VFS;
