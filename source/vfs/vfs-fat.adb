--  vfs-fat.adb: FAT-series FS driver.
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

with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Lib.Alignment;

package body VFS.FAT with SPARK_Mode => Off is
   package   Conv_1 is new System.Address_To_Access_Conversions (FAT_Data);
   package   Conv_2 is new System.Address_To_Access_Conversions (FAT_File);
   procedure Free_1 is new Ada.Unchecked_Deallocation (FAT_Data, FAT_Data_Acc);
   procedure Free_2 is new Ada.Unchecked_Deallocation (FAT_File, FAT_File_Acc);

   function Probe
      (Handle       : Device_Handle;
       Do_Read_Only : Boolean) return System.Address
   is
      pragma Unreferenced (Do_Read_Only);

      Data      : FAT_Data_Acc;
      BP        : BIOS_Parameter_Block;
      BP_Data   : Operation_Data (1 .. BP'Size / 8) with Address => BP'Address;
      Ret_Count : Natural;
      Success   : Boolean;
   begin
      Devices.Read
         (Handle    => Handle,
          Offset    => 0,
          Data      => BP_Data,
          Ret_Count => Ret_Count,
          Success   => Success);
      if not Success or Ret_Count /= BP_Data'Length or
         BP.Boot_Signature /= Boot_Signature        or
         (BP.Signature /= Signature_1 and BP.Signature /= Signature_2)
      then
         return System.Null_Address;
      end if;

      Data := new FAT_Data'
         (Handle         => Handle,
          Is_Read_Only   => Is_Read_Only (Handle),
          BPB            => BP,
          Sector_Count   => Unsigned_32 (BP.Sector_Count),
          FAT_Offset     => Unsigned_32 (BP.Reserved_Sectors),
          Cluster_Offset => Unsigned_32 (BP.Reserved_Sectors) +
                            Unsigned_32 (BP.FAT_Count) * BP.Sectors_Per_FAT);
      if Data.Sector_Count = 0 then
         Data.Sector_Count := BP.Large_Sector_Count;
      end if;

      return Conv_1.To_Address (Conv_1.Object_Pointer (Data));
   end Probe;

   procedure Unmount (FS : in out System.Address) is
      Data : FAT_Data_Acc := FAT_Data_Acc (Conv_1.To_Pointer (FS));
   begin
      Free_1 (Data);
      FS := System.Null_Address;
   end Unmount;

   procedure Open
      (FS      : System.Address;
       Path    : String;
       Ino     : out File_Inode_Number;
       Success : out FS_Status)
   is
      Data : constant FAT_Data_Acc := FAT_Data_Acc (Conv_1.To_Pointer (FS));
      Result     : FAT_File_Acc;
      Cluster    : Unsigned_32 := Data.BPB.Root_Entry_Cluster;
      Inner_Type : File_Type   := File_Directory;
      Index      : Unsigned_64;
      Ent        : Directory_Entry;
      First_I    : Natural;
      Last_I     : Natural;
   begin
      if Path'Length = 0 then
         goto End_Return;
      end if;

      First_I := Path'First;
      Last_I  := Path'First;

      loop
         if Last_I >= Path'Last then
            exit;
         end if;
         while Last_I <= Path'Last and then Path (Last_I) /= '/' loop
            Last_I := Last_I + 1;
         end loop;

         if Inner_Type /= File_Directory then
            goto Error_Return;
         end if;

         Index := 0;
         loop
            if not Read_Directory_Entry (Data, Cluster, Index, Ent) then
               goto Error_Return;
            end if;

            if Index = Unsigned_64 (Data.BPB.Sectors_Per_Cluster) * 16 then
               Index := 0;
               if not Get_Next_Cluster (Data, Cluster, Cluster) then
                  goto Error_Return;
               end if;
            end if;

            if Ent.Attributes = 0 then
               goto Error_Return;
            end if;

            --  TODO: Handle LFN.
            if (Ent.Attributes and Directory_LFN) /= Directory_LFN and then
               Are_Paths_Equal (Path (First_I .. Last_I - 1), Ent)
            then
               Inner_Type := Get_Type (Ent.Attributes);
               Cluster := Shift_Left (Unsigned_32 (Ent.First_Cluster_High), 32)
                          or Unsigned_32 (Ent.First_Cluster_Low);
               goto Next_Iteration;
            end if;

            Index := Index + 1;
         end loop;

      <<Next_Iteration>>
         Last_I  := Last_I + 1;
         First_I := Last_I;
      end loop;

   <<End_Return>>
      Result := new FAT_File'(Cluster, Inner_Type, Ent);
      Ino := File_Inode_Number (To_Integer
         (Conv_2.To_Address (Conv_2.Object_Pointer (Result))));
      Success := FS_Success;
      return;

   <<Error_Return>>
      Ino     := 0;
      Success := FS_Invalid_Value;
   end Open;

   procedure Close (FS : System.Address; Ino : File_Inode_Number) is
      pragma Unreferenced (FS);
      File : FAT_File_Acc := FAT_File_Acc
         (Conv_2.To_Pointer (To_Address (Integer_Address (Ino))));
   begin
      Free_2 (File);
   end Close;

   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      FS : constant FAT_Data_Acc := FAT_Data_Acc (Conv_1.To_Pointer (FS_Data));
      File : constant FAT_File_Acc := FAT_File_Acc
         (Conv_2.To_Pointer (To_Address (Integer_Address (Ino))));
      Cluster : Unsigned_32 := File.Begin_Cluster;
      Index   : Unsigned_64 := 0;
      Ent     : Directory_Entry;
      Composed     : String (1 .. 12);
      Composed_Len : Natural;
      Temp         : Natural;
   begin
      Ret_Count := 0;
      if File.Inner_Type /= File_Directory then
         Success := FS_Invalid_Value;
         return;
      end if;

      Success := FS_Success;
      loop
         if not Read_Directory_Entry (FS, Cluster, Index, Ent) then
            Success := FS_IO_Failure;
            return;
         end if;

         if Index = Unsigned_64 (FS.BPB.Sectors_Per_Cluster) * 16 then
            Index := 0;
            if not Get_Next_Cluster (FS, Cluster, Cluster) then
               return;
            end if;
         end if;

         if Ent.Attributes = 0 then
            exit;
         end if;

         --  TODO: Handle LFN.
         if (Ent.Attributes and Directory_LFN) /= Directory_LFN then
            if Ret_Count < Entities'Length then
               Composed_Len := Compose_Path (Ent, Composed);
               Temp := Entities'First + Ret_Count;
               Entities (Temp).Inode_Number :=
                  Unsigned_64 (Ent.First_Cluster_Low);
               Entities (Temp).Name_Buffer (1 .. Composed_Len) :=
                  Composed (1 .. Composed_Len);
               Entities (Temp).Name_Len     := Composed_Len;
               Entities (Temp).Type_Of_File := Get_Type (Ent.Attributes);
            end if;
            Ret_Count := Ret_Count + 1;
         end if;

         Index := Index + 1;
      end loop;
   end Read_Entries;

   procedure Read
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      FS : constant FAT_Data_Acc := FAT_Data_Acc (Conv_1.To_Pointer (FS_Data));
      File  : constant FAT_File_Acc := FAT_File_Acc
         (Conv_2.To_Pointer (To_Address (Integer_Address (Ino))));
      Cluster_Offset : Unsigned_64;
      Cluster        : Unsigned_32 := File.Begin_Cluster;
      Cluster_Sz     : constant Unsigned_32 :=
         Unsigned_32 (FS.BPB.Sectors_Per_Cluster) * Sector_Size;
      Final_Offset   : Unsigned_64 := Offset;
      Step_Size      : Natural;
      Discard        : Natural;
      Final_Count    : Natural     := Data'Length;
      Succ           : Boolean;
   begin
      Ret_Count := 0;
      if File.Inner_Type /= File_Regular then
         Success := FS_Invalid_Value;
         return;
      end if;

      if Offset + Data'Length > Unsigned_64 (File.FS_Entry.Size) then
         Final_Count := Natural (Unsigned_64 (File.FS_Entry.Size) - Offset);
      end if;

      Success := FS_Success;
      while Ret_Count < Final_Count loop
         Step_Size := Natural (Unsigned_64 (Final_Count) -
                      Unsigned_64 (Ret_Count));
         if Unsigned_64 (Step_Size) > Unsigned_64 (Cluster_Sz) - Final_Offset
         then
            Step_Size := Natural (Unsigned_64 (Cluster_Sz) - Final_Offset);
         end if;

         Cluster_Offset := Cluster_To_Disk_Offset
            (Cluster             => Cluster,
             Cluster_Begin       => FS.Cluster_Offset,
             Sectors_Per_Cluster => Unsigned_32 (FS.BPB.Sectors_Per_Cluster));

         Devices.Read
            (Handle    => FS.Handle,
             Offset    => Cluster_Offset + Final_Offset,
             Data      => Data (Data'First + Ret_Count ..
                                Data'First + Ret_Count - 1 + Step_Size),
             Ret_Count => Discard,
             Success   => Succ);
         if not Succ then
            Success := FS_IO_Failure;
            return;
         end if;

         Final_Offset := 0;
         if not Get_Next_Cluster (FS, Cluster, Cluster) then
            Success := FS_IO_Failure;
            return;
         end if;

         Ret_Count := Ret_Count + Step_Size;
      end loop;
   end Read;

   function Stat
      (Data : System.Address;
       Ino  : File_Inode_Number;
       S    : out File_Stat) return FS_Status
   is
      package Align is new Lib.Alignment (Unsigned_32);
      FS   : constant FAT_Data_Acc := FAT_Data_Acc (Conv_1.To_Pointer (Data));
      File : constant FAT_File_Acc := FAT_File_Acc
         (Conv_2.To_Pointer (To_Address (Integer_Address (Ino))));
      Blk  : constant Unsigned_32  := Unsigned_32 (Get_Block_Size (FS.Handle));
      Cnt  : constant Unsigned_32  :=
         Align.Divide_Round_Up (File.FS_Entry.Size, Blk);
   begin
      S :=
         (Unique_Identifier => File_Inode_Number (File.Begin_Cluster),
          Type_Of_File      => File.Inner_Type,
          Mode              => 8#755#,
          UID               => 0,
          GID               => 0,
          Hard_Link_Count   => 1,
          Byte_Size         => Unsigned_64 (File.FS_Entry.Size),
          IO_Block_Size     => Devices.Get_Block_Size (FS.Handle),
          IO_Block_Count    => Unsigned_64 (Cnt),
          Creation_Time     => (0, 0),
          Modification_Time => (0, 0),
          Access_Time       => (0, 0));
      return FS_Success;
   end Stat;
   ----------------------------------------------------------------------------
   function Read_Directory_Entry
      (Data    : FAT_Data_Acc;
       Cluster : Unsigned_32;
       Index   : Unsigned_64;
       Result  : out Directory_Entry) return Boolean
   is
      Offset      : Unsigned_64;
      Ret_Count   : Natural;
      Success     : Boolean;
      Result_Data : Operation_Data (1 .. Directory_Entry'Size / 8)
         with Address => Result'Address;
   begin
      Offset := Cluster_To_Disk_Offset
         (Cluster             => Cluster,
          Cluster_Begin       => Data.Cluster_Offset,
          Sectors_Per_Cluster => Unsigned_32 (Data.BPB.Sectors_Per_Cluster));

      Devices.Read
         (Handle    => Data.Handle,
          Offset    => Offset + Index * 32,
          Data      => Result_Data,
          Ret_Count => Ret_Count,
          Success   => Success);
      return Success and Ret_Count = Result_Data'Length;
   end Read_Directory_Entry;

   function Get_Next_Cluster
      (Data          : FAT_Data_Acc;
       Cluster_Index : Unsigned_32;
       Returned      : out Unsigned_32) return Boolean
   is
      Limit, Offset : Unsigned_64;
      Ret_Count     : Natural;
      Success       : Boolean;
      Returned_Data : Operation_Data (1 .. 4) with Address => Returned'Address;
   begin
      Limit  := (Unsigned_64 (Data.BPB.Sectors_Per_FAT) * Sector_Size) / 4;
      Offset := Sector_To_Disk_Offset (Data.FAT_Offset);

      if Unsigned_64 (Cluster_Index) >= Limit then
         Returned := 0;
         return False;
      end if;

      Devices.Read
         (Handle    => Data.Handle,
          Offset    => Offset + (Unsigned_64 (Cluster_Index) * 4),
          Data      => Returned_Data,
          Ret_Count => Ret_Count,
          Success   => Success);
      if not Success or Ret_Count /= Returned_Data'Length then
         return False;
      end if;

      return Returned < 16#FFFFFFF8#;
   end Get_Next_Cluster;

   function Are_Paths_Equal
      (Base : String;
       Ent  : Directory_Entry) return Boolean
   is
      Offset       : constant := Character'Pos ('A') - Character'Pos ('a');
      Base_Copy    : String   := Base;
      Composed     : String (1 .. 12);
      Composed_Len : Natural;
   begin
      for C of Base_Copy loop
         if C in 'a' .. 'z' then
            C := Character'Val (Character'Pos (C) + Offset);
         end if;
      end loop;

      Composed_Len := Compose_Path (Ent, Composed);
      return Base_Copy = Composed (1 .. Composed_Len);
   end Are_Paths_Equal;

   function Compose_Path
      (Ent    : Directory_Entry;
       Result : out String) return Natural
   is
      Ret          : Natural;
      Ent_Name_Len : Natural  := 0;
      Ent_Ext_Len  : Natural  := 0;
   begin
      for C of Ent.File_Name loop
         if C = ' ' then
            exit;
         end if;
         Ent_Name_Len := Ent_Name_Len + 1;
      end loop;

      for C of Ent.File_Extension loop
         if C = ' ' then
            exit;
         end if;
         Ent_Ext_Len := Ent_Ext_Len + 1;
      end loop;

      if Ent_Ext_Len /= 0 then
         Ret := Ent_Name_Len + 1 + Ent_Ext_Len;
         Result (Result'First .. Result'First + Ret - 1) :=
            Ent.File_Name (1 .. Ent_Name_Len) & "." &
            Ent.File_Extension (1 .. Ent_Ext_Len);
      else
         Ret := Ent_Name_Len;
         Result (Result'First .. Result'First + Ret - 1) :=
            Ent.File_Name (1 .. Ent_Name_Len);
      end if;

      return Ret;
   end Compose_Path;
end VFS.FAT;
