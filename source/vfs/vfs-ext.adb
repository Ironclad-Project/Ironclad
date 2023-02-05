--  vfs-ext.adb: Linux Extended FS driver.
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

with Lib.Messages;
with Lib.Panic;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;

package body VFS.EXT with SPARK_Mode => Off is
   package   Conv   is new System.Address_To_Access_Conversions (EXT_Data);
   procedure Free_1 is new Ada.Unchecked_Deallocation (EXT_Data, EXT_Data_Acc);

   function Probe (Handle : Device_Handle) return System.Address is
      Is_RO : Boolean      := Devices.Is_Read_Only (Handle);
      Data  : EXT_Data_Acc := new EXT_Data'(Handle => Handle, others => <>);
      Sup   : Superblock renames Data.Super;
   begin
      --  Check signature and conditions we do not support yet.
      if not Read_Superblock (Data) or Sup.Signature /= 16#EF53# or
         Sup.Block_Size_Log < Sup.Fragment_Size_Log
      then
         Free_1 (Data);
         return Null_Address;
      end if;

      if Sup.Filesystem_State /= State_Clean or
         Sup.Mounts_Since_Check > Sup.Max_Mounts_Since_Check
      then
         Lib.Messages.Warn ("ext may be damaged, forcing read-only");
         Is_RO := True;
      end if;

      Data.Is_Read_Only  := Is_RO;
      Data.Block_Size    := Shift_Left (1024, Natural (Sup.Block_Size_Log));
      Data.Fragment_Size := Shift_Left (1024, Natural (Sup.Fragment_Size_Log));
      Data.Error_Policy  := Sup.Error_Policy;

      if not Is_RO and Sup.Mounts_Since_Check /= Unsigned_16'Last then
         Sup.Mounts_Since_Check := Sup.Mounts_Since_Check + 1;
         if not Write_Superblock (Data) then
            Lib.Messages.Warn ("ext failed superblock write, going read-only");
            Data.Is_Read_Only := True;
         end if;
      end if;

      return Conv.To_Address (Conv.Object_Pointer (Data));
   end Probe;

   function Open (FS : System.Address; Path : String) return System.Address is
      FS_Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv.To_Pointer (FS));
      pragma Unreferenced (FS_Data);
      pragma Unreferenced (Path);
   begin
      return Null_Address;
   end Open;

   function Create
      (FS   : System.Address;
       Path : String;
       Mode : Unsigned_32) return System.Address
   is
      pragma Unreferenced (FS);
      pragma Unreferenced (Path);
      pragma Unreferenced (Mode);
   begin
      return Null_Address;
   end Create;

   function Create_Symbolic_Link
      (FS           : System.Address;
       Path, Target : String;
       Mode         : Unsigned_32) return System.Address
   is
      pragma Unreferenced (FS);
      pragma Unreferenced (Path);
      pragma Unreferenced (Target);
      pragma Unreferenced (Mode);
   begin
      return Null_Address;
   end Create_Symbolic_Link;

   function Create_Directory
      (FS   : System.Address;
       Path : String;
       Mode : Unsigned_32) return System.Address
   is
      pragma Unreferenced (FS);
      pragma Unreferenced (Path);
      pragma Unreferenced (Mode);
   begin
      return Null_Address;
   end Create_Directory;

   procedure Close (FS : System.Address; Obj : out System.Address) is
      pragma Unreferenced (FS);
      pragma Unreferenced (Obj);
   begin
      Obj := Null_Address;
   end Close;

   procedure Read_Entries
      (FS_Data   : System.Address;
       Obj       : System.Address;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (FS_Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Entities);
   begin
      Ret_Count := 0;
      Success   := False;
   end Read_Entries;

   procedure Read
      (FS_Data   : System.Address;
       Obj       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (FS_Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Data);
   begin
      Ret_Count := 0;
      Success   := False;
   end Read;

   procedure Write
      (FS_Data   : System.Address;
       Obj       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (FS_Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Data);
   begin
      Ret_Count := 0;
      Success   := False;
   end Write;

   function Stat
      (Data : System.Address;
       Obj  : System.Address;
       S    : out File_Stat) return Boolean
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (S);
   begin
      return False;
   end Stat;

   function Read_Superblock (Data : EXT_Data_Acc) return Boolean is
      Super_Data : Operation_Data (1 .. Superblock'Size / 8)
         with Address => Data.Super'Address;
      Ret_Count : Natural;
      Success   : Boolean;
   begin
      Devices.Read
         (Handle    => Data.Handle,
          Offset    => Superblock_Offset,
          Data      => Super_Data,
          Ret_Count => Ret_Count,
          Success   => Success);
      return Success and (Ret_Count = Super_Data'Length);
   end Read_Superblock;

   function Write_Superblock (Data : EXT_Data_Acc) return Boolean is
      Super_Data : Operation_Data (1 .. Superblock'Size / 8)
         with Address => Data.Super'Address;
      Ret_Count : Natural;
      Success   : Boolean;
   begin
      Devices.Write
         (Handle    => Data.Handle,
          Offset    => Superblock_Offset,
          Data      => Super_Data,
          Ret_Count => Ret_Count,
          Success   => Success);
      return Success and (Ret_Count /= Super_Data'Length);
   end Write_Superblock;

   procedure Act_On_Policy (Data : EXT_Data_Acc; Message : String) is
   begin
      case Data.Error_Policy is
         when Policy_Ignore =>
            null;
         when Policy_Remount_RO =>
            Lib.Messages.Warn (Message & ", we will go read-only");
            Data.Is_Read_Only := True;
         when Policy_Panic =>
            Lib.Panic.Hard_Panic ("ext says: " & Message);
         when others =>
            Lib.Panic.Hard_Panic ("ext is dead, and we killed it: " & Message);
      end case;
   end Act_On_Policy;
end VFS.EXT;
