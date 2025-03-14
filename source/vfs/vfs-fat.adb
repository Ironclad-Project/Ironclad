--  vfs-fat.adb: FAT FS driver.
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

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Lib.Alignment;
with Lib.Time;

package body VFS.FAT with SPARK_Mode => Off is
   package   Conv is new System.Address_To_Access_Conversions (FAT_Data);
   procedure Free is new Ada.Unchecked_Deallocation (FAT_Data, FAT_Data_Acc);

   --  FAT doesnt have a notion of inodes, but Ironclad requires it, thats how
   --  POSIX works. So we will need to translate inode to file and viceversa.
   --  Inode numbers will translate to the disk offset of the directory entry
   --  except when refering to root, which has the hardcoded Inode of 2.
   Root_PseudoInode : constant := 2;

   --  All files in FAT have the same permissions, since FAT does not handle
   --  permissions.
   File_Permissions : constant := 8#755#;

   procedure Probe
      (Handle        : Device_Handle;
       Do_Read_Only  : Boolean;
       Access_Policy : Access_Time_Policy;
       Data_Addr     : out System.Address;
       Root_Ino      : out File_Inode_Number)
   is
      pragma Unreferenced (Do_Read_Only);
      pragma Unreferenced (Access_Policy);

      Data      : FAT_Data_Acc;
      BP        : BIOS_Parameter_Block;
      Ret_Count : Natural;
      Success   : Devices.Dev_Status;
   begin
      declare
         BP_Data   : Operation_Data (1 .. BP'Size / 8)
            with Import, Address => BP'Address;
      begin
         Devices.Read (Handle, 0, BP_Data, Ret_Count, Success);
         if Success /= Devices.Dev_Success or
            Ret_Count /= BP_Data'Length    or
            BP.Boot_Signature /= Boot_Signature
         then
            Data_Addr := System.Null_Address;
            Root_Ino  := 0;
            return;
         end if;
      end;

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

      Data_Addr := Conv.To_Address (Conv.Object_Pointer (Data));
      Root_Ino  := Root_PseudoInode;
   exception
      when Constraint_Error =>
         Data_Addr := System.Null_Address;
         Root_Ino  := 0;
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
      Data : FAT_Data_Acc := FAT_Data_Acc (Conv.To_Pointer (FS));
   begin
      Free (Data);
      FS := System.Null_Address;
   end Unmount;
   ----------------------------------------------------------------------------
   function Get_Block_Size (FS : System.Address) return Unsigned_64 is
      pragma Unreferenced (FS);
   begin
      return Sector_Size;
   end Get_Block_Size;

   function Get_Fragment_Size (FS : System.Address) return Unsigned_64 is
      pragma Unreferenced (FS);
   begin
      return Sector_Size;
   end Get_Fragment_Size;

   function Get_Size (FS : System.Address) return Unsigned_64 is
      Data : constant FAT_Data_Acc := FAT_Data_Acc (Conv.To_Pointer (FS));
   begin
      return Unsigned_64 (Data.Sector_Count);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Size;

   function Get_Inode_Count (FS : System.Address) return Unsigned_64 is
      pragma Unreferenced (FS);
   begin
      return 420;
   end Get_Inode_Count;

   procedure Get_Free_Blocks
      (FS                 : System.Address;
       Free_Blocks        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
   is
      pragma Unreferenced (FS);
   begin
      Free_Blocks        := 1;
      Free_Unpriviledged := 1;
   end Get_Free_Blocks;

   procedure Get_Free_Inodes
      (FS                 : System.Address;
       Free_Inodes        : out Unsigned_64;
       Free_Unpriviledged : out Unsigned_64)
   is
      pragma Unreferenced (FS);
   begin
      Free_Inodes        := 1;
      Free_Unpriviledged := 1;
   end Get_Free_Inodes;

   function Get_Max_Length (FS : System.Address) return Unsigned_64 is
      pragma Unreferenced (FS);
   begin
      return 64;
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
      FS : constant FAT_Data_Acc := FAT_Data_Acc (Conv.To_Pointer (FS_Data));
      Disk_Off : Unsigned_64;
      Cluster : Unsigned_32 := 0;
      Total   : Natural     := 0;
      Index   : Unsigned_64 := 0;
      Ent     : Directory_Entry;
      Composed     : String (1 .. 12);
      Composed_Len : Natural;
      Temp         : Natural;
      Success2     : Boolean;
   begin
      Ret_Count := 0;
      Success   := FS_Success;

      if Ino = Root_PseudoInode then
         Cluster := FS.BPB.Root_Entry_Cluster;
      else
         Read_Directory_Entry
            (Data        => FS,
             Disk_Offset => Unsigned_64 (Ino),
             Result      => Ent,
             Success     => Success2);
         if not Success2 or Get_Type (Ent.Attributes) /= File_Directory then
            Success := FS_Invalid_Value;
            return;
         end if;

         --  TODO: Handle 64 bit cluster numbers.
         Cluster := Unsigned_32 (Ent.First_Cluster_Low);
      end if;

      loop
         Read_Directory_Entry (FS, Cluster, Index, Disk_Off, Ent, Success2);
         if not Success2 then
            Success := FS_IO_Failure;
            return;
         end if;

         if Index = Unsigned_64 (FS.BPB.Sectors_Per_Cluster) * 16 then
            Index := 0;
            Get_Next_Cluster (FS, Cluster, Cluster, Success2);
            if not Success2 then
               return;
            end if;
         end if;

         if Ent.Attributes = 0 then
            exit;
         end if;

         --  TODO: Handle LFN.
         if Total >= Offset and
            (Ent.Attributes and Directory_LFN) /= Directory_LFN
         then
            if Ret_Count < Entities'Length then
               Compose_Path (Ent, Composed, Composed_Len);
               Temp := Entities'First + Ret_Count;
               Entities (Temp).Inode_Number := Disk_Off;
               Entities (Temp).Name_Buffer (1 .. Composed_Len) :=
                  Composed (1 .. Composed_Len);
               Entities (Temp).Name_Len     := Composed_Len;
               Entities (Temp).Type_Of_File := Get_Type (Ent.Attributes);
            end if;

            Total     := Total     + 1;
            Ret_Count := Ret_Count + 1;
         end if;

         Index := Index + 1;
      end loop;
   exception
      when Constraint_Error =>
         Ret_Count := 0;
         Success   := FS_IO_Failure;
   end Read_Entries;

   procedure Read
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out FS_Status)
   is
      FS : constant FAT_Data_Acc := FAT_Data_Acc (Conv.To_Pointer (FS_Data));
      Ent : Directory_Entry;
      Success2 : Boolean;
      Cluster        : Unsigned_32;
      Cluster_Offset : Unsigned_64;
      Cluster_Sz     : Unsigned_32;
      Final_Offset   : Unsigned_64 := Offset;
      Final_Count    : Natural;
      Step_Size      : Natural;
      Discard        : Natural;
      Succ           : Boolean;
      Succ2          : Devices.Dev_Status;
   begin
      Data        := [others => 0];
      Ret_Count   := 0;
      Success     := FS_Success;
      Final_Count := Data'Length;
      Cluster_Sz   :=
         Unsigned_32 (FS.BPB.Sectors_Per_Cluster) * Sector_Size;

      if Ino = Root_PseudoInode then
         Success := FS_Is_Directory;
         return;
      else
         Read_Directory_Entry
            (Data        => FS,
             Disk_Offset => Unsigned_64 (Ino),
             Result      => Ent,
             Success     => Success2);
         if not Success2 or Get_Type (Ent.Attributes) = File_Directory then
            Success := FS_Invalid_Value;
            return;
         end if;

         --  TODO: Handle 64 bit cluster numbers.
         Cluster := Unsigned_32 (Ent.First_Cluster_Low);
      end if;

      if Offset + Data'Length > Unsigned_64 (Ent.Size) then
         Final_Count := Natural (Unsigned_64 (Ent.Size) - Offset);
      end if;

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
             Success   => Succ2);
         if Succ2 /= Devices.Dev_Success then
            Success := FS_IO_Failure;
            return;
         end if;

         Final_Offset := 0;
         Get_Next_Cluster (FS, Cluster, Cluster, Succ);
         if not Succ then
            Success := FS_IO_Failure;
            return;
         end if;

         Ret_Count := Ret_Count + Step_Size;
      end loop;
   exception
      when Constraint_Error =>
         Ret_Count := 0;
         Success   := FS_IO_Failure;
   end Read;

   procedure Stat
      (Data    : System.Address;
       Ino     : File_Inode_Number;
       S       : out File_Stat;
       Success : out FS_Status)
   is
      package Align is new Lib.Alignment (Unsigned_32);

      FS : constant FAT_Data_Acc := FAT_Data_Acc (Conv.To_Pointer (Data));
      Ent : Directory_Entry;
      Success2 : Boolean;
      Blk, Cnt : Unsigned_32;

      C1 :  Unsigned_8 renames Ent.Creation_Time_1;
      C2 : Unsigned_16 renames Ent.Creation_Time_2;
      C3 : Unsigned_16 renames Ent.Creation_Time_3;
   begin
      if Ino = Root_PseudoInode then
         S :=
            (Unique_Identifier => File_Inode_Number
               (FS.BPB.Root_Entry_Cluster),
             Type_Of_File      => File_Directory,
             Mode              => File_Permissions,
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
      else
         Read_Directory_Entry
            (Data        => FS,
             Disk_Offset => Unsigned_64 (Ino),
             Result      => Ent,
             Success     => Success2);
         if not Success2 then
            Success := FS_Invalid_Value;
            return;
         end if;

         Blk := Unsigned_32 (Get_Block_Size (FS.Handle));
         Cnt := Align.Divide_Round_Up (Ent.Size, Blk);

         S :=
            (Unique_Identifier => Ino,
             Type_Of_File      => Get_Type (Ent.Attributes),
             Mode              => File_Permissions,
             UID               => 0,
             GID               => 0,
             Hard_Link_Count   => 1,
             Byte_Size         => Unsigned_64 (Ent.Size),
             IO_Block_Size     => Devices.Get_Block_Size (FS.Handle),
             IO_Block_Count    => Unsigned_64 (Cnt),
             Change_Time       => (0, 0),
             Modification_Time => (0, 0),
             Birth_Time        => (0, 0),
             Access_Time       => (0, 0));

         S.Birth_Time.Seconds_Since_Epoch :=
            Lib.Time.Time_To_Epoch
               (Year    => Natural (Shift_Right (C3, 9) and 2#1111111#) + 1980,
                Month   => Natural (Shift_Right (C3, 5)  and 2#0001111#),
                Day     => Natural (Shift_Right (C3, 0)  and 2#0011111#) + 1,
                Hours   => Natural (Shift_Right (C2, 10) and 2#0011111#),
                Minutes => Natural (Shift_Right (C2,  4) and 2#0111111#),
                Seconds => Natural (Shift_Right (C2,  0) and 2#0011111#) * 2);
         S.Birth_Time.Additional_Nanoseconds := Unsigned_64 (C1) * 100_000_000;
         S.Change_Time       := S.Birth_Time;
         S.Modification_Time := S.Birth_Time;
         S.Access_Time       := S.Birth_Time;
      end if;

      Success := FS_Success;
   exception
      when Constraint_Error =>
         Success := FS_IO_Failure;
   end Stat;
   ----------------------------------------------------------------------------
   procedure Read_Directory_Entry
      (Data        : FAT_Data_Acc;
       Cluster     : Unsigned_32;
       Index       : Unsigned_64;
       Disk_Offset : out Unsigned_64;
       Result      : out Directory_Entry;
       Success     : out Boolean)
   is
      Offset      : Unsigned_64;
      Ret_Count   : Natural;
      Succ        : Devices.Dev_Status;
      Result_Data : Operation_Data (1 .. Directory_Entry'Size / 8)
         with Import, Address => Result'Address;
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
          Success   => Succ);
      Disk_Offset := Offset + Index * 32;
      Success     := Succ = Devices.Dev_Success and
                     Ret_Count = Result_Data'Length;
   exception
      when Constraint_Error =>
         Disk_Offset := 0;
         Success     := False;
   end Read_Directory_Entry;

   procedure Read_Directory_Entry
      (Data        : FAT_Data_Acc;
       Disk_Offset : Unsigned_64;
       Result      : out Directory_Entry;
       Success     : out Boolean)
   is
      Ret_Count   : Natural;
      Succ        : Devices.Dev_Status;
      Result_Data : Operation_Data (1 .. Directory_Entry'Size / 8)
         with Import, Address => Result'Address;
   begin
      Devices.Read
         (Handle    => Data.Handle,
          Offset    => Disk_Offset,
          Data      => Result_Data,
          Ret_Count => Ret_Count,
          Success   => Succ);
      Success := Succ = Devices.Dev_Success and
                 Ret_Count = Result_Data'Length;
   exception
      when Constraint_Error =>
         Success := False;
   end Read_Directory_Entry;

   procedure Get_Next_Cluster
      (Data          : FAT_Data_Acc;
       Cluster_Index : Unsigned_32;
       Returned      : out Unsigned_32;
       Success       : out Boolean)
   is
      Limit, Offset : Unsigned_64;
      Ret_Count     : Natural;
      Succ          : Devices.Dev_Status;
      Returned_Data : Operation_Data (1 .. 4) with Address => Returned'Address;
   begin
      Limit  := (Unsigned_64 (Data.BPB.Sectors_Per_FAT) * Sector_Size) / 4;
      Offset := Sector_To_Disk_Offset (Data.FAT_Offset);

      if Unsigned_64 (Cluster_Index) >= Limit then
         Returned := 0;
         Success  := False;
      else
         Devices.Read
            (Handle    => Data.Handle,
             Offset    => Offset + (Unsigned_64 (Cluster_Index) * 4),
             Data      => Returned_Data,
             Ret_Count => Ret_Count,
             Success   => Succ);
         if (Succ = Devices.Dev_Success) or (Ret_Count /= Returned_Data'Length)
         then
            Success := False;
         else
            Success := Returned < 16#FFFFFFF8#;
         end if;
      end if;
   exception
      when Constraint_Error =>
         Returned := 0;
         Success  := False;
   end Get_Next_Cluster;

   procedure Compose_Path
      (Ent    : Directory_Entry;
       Result : out String;
       Length : out Natural)
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

      Length := Ret;
   exception
      when Constraint_Error =>
         Result := [others => ' '];
         Length := 0;
   end Compose_Path;
end VFS.FAT;
