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
with Lib.Alignment;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements; use System.Storage_Elements;

package body VFS.EXT with SPARK_Mode => Off is
   package   Conv_1 is new System.Address_To_Access_Conversions (EXT_Data);
   package   Conv_2 is new System.Address_To_Access_Conversions (EXT_File);
   procedure Free_1 is new Ada.Unchecked_Deallocation (EXT_Data, EXT_Data_Acc);
   procedure Free_2 is new Ada.Unchecked_Deallocation (EXT_File, EXT_File_Acc);

   function Probe (Handle : Device_Handle) return System.Address is
      Data    : EXT_Data_Acc := new EXT_Data'(Handle => Handle, others => <>);
      Sup     : Superblock renames Data.Super;
      Success : Boolean;
   begin
      --  Check we support everything ext needs us to.
      if not RW_Superblock (Data, Main_Superblock_Offset, False)  or
         Sup.Signature /= EXT_Signature                           or
         Sup.Block_Size_Log < Sup.Fragment_Size_Log               or
         Sup.Major_Version > 1                                    or
         (Sup.Required_Features and Required_Compression)    /= 0 or
         (Sup.Required_Features and Required_Journal_Replay) /= 0 or
         (Sup.Required_Features and Required_Journal_Device) /= 0
      then
         goto Error_Return;
      end if;

      --  Check for the features we do support.
      Data.Has_Sparse_Superblock :=
         (Sup.RO_If_Not_Features and RO_Sparse_Superblocks) /= 0;
      Data.Has_64bit_Filesizes :=
         (Sup.RO_If_Not_Features and RO_64bit_Filesize) /= 0;

      --  Check under which conditions we have to RO.
      Data.Is_Read_Only :=
         Devices.Is_Read_Only (Handle)                       or
         Sup.Filesystem_State /= State_Clean                 or
         Sup.Mounts_Since_Check > Sup.Max_Mounts_Since_Check or
         (Sup.RO_If_Not_Features and RO_Binary_Trees) /= 0;
      if Data.Is_Read_Only then
         Lib.Messages.Warn ("ext will be mounted RO, consider an fsck");
      end if;

      Data.Block_Size    := Shift_Left (1024, Natural (Sup.Block_Size_Log));
      Data.Fragment_Size := Shift_Left (1024, Natural (Sup.Fragment_Size_Log));

      --  Read the root inode.
      Success := RW_Inode
         (Data            => Data,
          Inode_Index     => 2,
          Result          => Data.Root,
          Write_Operation => False);
      if not Success then
         goto Error_Return;
      end if;

      Data.Mutex := Lib.Synchronization.Unlocked_Semaphore;
      return Conv_1.To_Address (Conv_1.Object_Pointer (Data));

   <<Error_Return>>
      Free_1 (Data);
      return Null_Address;
   end Probe;

   procedure Unmount (FS : in out System.Address) is
      Data : EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS));
   begin
      Lib.Synchronization.Seize (Data.Mutex);

      if not Data.Is_Read_Only then
         if Data.Super.Mounts_Since_Check /= Unsigned_16'Last then
            Data.Super.Mounts_Since_Check := Data.Super.Mounts_Since_Check + 1;
            if not RW_Superblock (Data, Main_Superblock_Offset, True) then
               Act_On_Policy (Data, "superblock write error");
            end if;
         end if;
      end if;

      Free_1 (Data);
      FS := System.Null_Address;
   end Unmount;

   procedure Open
      (FS      : System.Address;
       Path    : String;
       Ino     : out File_Inode_Number;
       Success : out Boolean)
   is
      Data   : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS));
      Result : EXT_File_Acc;
      Entity : Directory_Entity;
      Searched  :       Inode := Data.Root;
      Search_Sz : Unsigned_64 := Get_Size (Searched, Data.Has_64bit_Filesizes);
      Searched_Type : File_Type := File_Directory;
      Inode_Num : Unsigned_32 := 2;
      First_I, Last_I        : Natural;
      Curr_Index, Next_Index : Unsigned_64;
      Symlink                : String (1 .. 60);
      Symlink_Len            : Natural;
   begin
      if Path'Length = 0 then
         goto End_Return;
      end if;

      First_I := Path'First;
      Last_I  := Path'First;

      Lib.Synchronization.Seize (Data.Mutex);
      loop
         if Last_I >= Path'Last then
            exit;
         end if;
         while Last_I <= Path'Last and then Path (Last_I) /= '/' loop
            Last_I := Last_I + 1;
         end loop;

         if Searched_Type /= File_Directory then
            goto Error_Return;
         end if;

         Success    := True;
         Curr_Index := 0;
         Next_Index := 0;
         loop
            Inner_Read_Entry
               (FS_Data     => Data,
                Inode_Sz    => Search_Sz,
                File_Ino    => Searched,
                Inode_Index => Curr_Index,
                Entity      => Entity,
                Next_Index  => Next_Index,
                Success     => Success);
            if not Success then
               goto Error_Return;
            else
               Curr_Index := Next_Index;
            end if;

            if Entity.Name_Buffer (1 .. Entity.Name_Len) =
               Path (First_I .. Last_I - 1)
            then
               Inode_Num := Unsigned_32 (Entity.Inode_Number);
               Success := RW_Inode
                  (Data            => Data,
                   Inode_Index     => Inode_Num,
                   Result          => Searched,
                   Write_Operation => False);
               Search_Sz     := Get_Size (Searched, Data.Has_64bit_Filesizes);
               Searched_Type := Get_Inode_Type (Searched.Permissions);
               if not Success then
                  goto Error_Return;
               end if;

               if Last_I < Path'Last and Searched_Type = File_Symbolic_Link
               then
                  Inner_Read_Symbolic_Link
                     (Searched,
                      Search_Sz,
                      Symlink,
                      Symlink_Len);
                  if Symlink_Len = 0 then
                     goto Error_Return;
                  end if;
                  Lib.Synchronization.Release (Data.Mutex);
                  Open
                     (FS,
                      Symlink (1 .. Symlink_Len) & Path (Last_I .. Path'Last),
                      Ino,
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

   <<End_Return>>
      Result := new EXT_File'
         (Size           => Search_Sz,
          Inode_Number   => Inode_Num,
          Inner_Inode    => Searched,
          Inode_Type     => Searched_Type,
          Is_Immutable   => (Searched.Flags and Flags_Immutable)   /= 0,
          Is_Append_Only => (Searched.Flags and Flags_Append_Only) /= 0);
      Lib.Synchronization.Release (Data.Mutex);
      Ino := File_Inode_Number (To_Integer
         (Conv_2.To_Address (Conv_2.Object_Pointer (Result))));
      Success := True;
      return;

   <<Error_Return>>
      Lib.Synchronization.Release (Data.Mutex);
      Ino     := 0;
      Success := False;
   end Open;

   function Create_Regular
      (FS   : System.Address;
       Path : String;
       Mode : Unsigned_32) return Boolean
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS));
      pragma Unreferenced (Mode);
   begin
      if Data.Is_Read_Only or Path'Length = 0 then
         return False;
      end if;

      return False;
   end Create_Regular;

   function Create_Symbolic_Link
      (FS           : System.Address;
       Path, Target : String;
       Mode         : Unsigned_32) return Boolean
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS));
      pragma Unreferenced (Mode);
   begin
      if Data.Is_Read_Only or Path'Length = 0 or Target'Length = 0 then
         return False;
      end if;

      return False;
   end Create_Symbolic_Link;

   function Create_Directory
      (FS   : System.Address;
       Path : String;
       Mode : Unsigned_32) return Boolean
   is
      Data : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS));
      pragma Unreferenced (Mode);
   begin
      if Data.Is_Read_Only or Path'Length = 0 then
         return False;
      end if;

      return False;
   end Create_Directory;

   function Delete (FS : System.Address; Path : String) return Boolean is
      pragma Unreferenced (FS);
      pragma Unreferenced (Path);
   begin
      return False;
   end Delete;

   procedure Close (FS : System.Address; Ino : File_Inode_Number) is
      pragma Unreferenced (FS);
      File : EXT_File_Acc := EXT_File_Acc (Conv_2.To_Pointer
         (To_Address (Integer_Address (Ino))));
   begin
      Free_2 (File);
   end Close;

   procedure Read_Entries
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Entities  : out Directory_Entities;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS_Data));
      Fi : constant EXT_File_Acc := EXT_File_Acc (Conv_2.To_Pointer
         (To_Address (Integer_Address (Ino))));
      Curr_Index, Next_Index : Unsigned_64 := 0;
      Ent : Directory_Entity;
   begin
      if Fi.Inode_Type /= File_Directory then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      if not FS.Is_Read_Only then
         Lib.Synchronization.Seize (FS.Mutex);
      end if;
      Ret_Count := 0;
      loop
         Inner_Read_Entry
            (FS_Data     => FS,
             Inode_Sz    => Fi.Size,
             File_Ino    => Fi.Inner_Inode,
             Inode_Index => Curr_Index,
             Entity      => Ent,
             Next_Index  => Next_Index,
             Success     => Success);
         if not Success then
            exit;
         end if;

         Curr_Index := Next_Index;
         if Ret_Count < Entities'Length then
            Entities (Entities'First + Ret_Count) := Ent;
         end if;
         Ret_Count := Ret_Count + 1;
      end loop;
      Success := True;
      if not FS.Is_Read_Only then
         Lib.Synchronization.Release (FS.Mutex);
      end if;
   end Read_Entries;

   procedure Read_Symbolic_Link
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Path      : out String;
       Ret_Count : out Natural)
   is
      pragma Unreferenced (FS_Data);
      File : constant EXT_File_Acc :=  EXT_File_Acc (Conv_2.To_Pointer
         (To_Address (Integer_Address (Ino))));
   begin
      if File.Inode_Type = File_Symbolic_Link then
         Inner_Read_Symbolic_Link
            (Ino       => File.Inner_Inode,
             File_Size => File.Size,
             Path      => Path,
             Ret_Count => Ret_Count);
      else
         Path      := (others => ' ');
         Ret_Count := 0;
      end if;
   end Read_Symbolic_Link;

   procedure Read
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS_Data));
      File : constant EXT_File_Acc :=  EXT_File_Acc (Conv_2.To_Pointer
         (To_Address (Integer_Address (Ino))));
   begin
      if File.Inode_Type /= File_Regular then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      if not FS.Is_Read_Only then
         Lib.Synchronization.Seize (FS.Mutex);
      end if;
      Read_From_Inode
         (FS_Data    => FS,
          Inode_Data => File.Inner_Inode,
          Inode_Size => File.Size,
          Offset     => Offset,
          Data       => Data,
          Ret_Count  => Ret_Count,
          Success    => Success);
      if not FS.Is_Read_Only then
         Lib.Synchronization.Release (FS.Mutex);
      end if;
   end Read;

   procedure Write
      (FS_Data   : System.Address;
       Ino       : File_Inode_Number;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      FS : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (FS_Data));
      Fi : constant EXT_File_Acc :=  EXT_File_Acc (Conv_2.To_Pointer
         (To_Address (Integer_Address (Ino))));
   begin
      if Fi.Inode_Type /= File_Regular or FS.Is_Read_Only or Fi.Is_Immutable
      then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      Lib.Synchronization.Seize (FS.Mutex);
      Write_To_Inode
         (FS_Data    => FS,
          Inode_Data => Fi.Inner_Inode,
          Inode_Num  => Fi.Inode_Number,
          Inode_Size => Fi.Size,
          Offset     => Offset,
          Data       => Data,
          Ret_Count  => Ret_Count,
          Success    => Success);
      Lib.Synchronization.Release (FS.Mutex);
   end Write;

   function Stat
      (Data : System.Address;
       Ino  : File_Inode_Number;
       S    : out File_Stat) return Boolean
   is
      package Align is new Lib.Alignment (Unsigned_64);
      FS   : constant EXT_Data_Acc := EXT_Data_Acc (Conv_1.To_Pointer (Data));
      File : constant EXT_File_Acc :=  EXT_File_Acc (Conv_2.To_Pointer
         (To_Address (Integer_Address (Ino))));
      Blk  : constant Unsigned_64  := Unsigned_64 (Get_Block_Size (FS.Handle));
      Inod : Inode renames File.Inner_Inode;
   begin
      S :=
         (Unique_Identifier => File_Inode_Number (File.Inode_Number),
          Type_Of_File      => Get_Inode_Type (Inod.Permissions),
          Mode              => Unsigned_32 (Inod.Permissions),
          Hard_Link_Count   => Positive (Inod.Hard_Link_Count),
          Byte_Size         => File.Size,
          IO_Block_Size     => Get_Block_Size (FS.Handle),
          IO_Block_Count    => Align.Divide_Round_Up (File.Size, Blk),
          Creation_Time     => (Unsigned_64 (Inod.Creation_Time_Epoch), 0),
          Modification_Time => (Unsigned_64 (Inod.Modified_Time_Epoch), 0),
          Access_Time       => (Unsigned_64 (Inod.Access_Time_Epoch),   0));
      return True;
   end Stat;
   ----------------------------------------------------------------------------
   function RW_Superblock
      (Data            : EXT_Data_Acc;
       Offset          : Unsigned_64;
       Write_Operation : Boolean) return Boolean
   is
      Super_Data : Operation_Data (1 .. Superblock'Size / 8)
         with Address => Data.Super'Address;
      Ret_Count : Natural;
      Success   : Boolean;
   begin
      if Write_Operation then
         Devices.Write
            (Handle    => Data.Handle,
             Offset    => Offset,
             Data      => Super_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Devices.Read
            (Handle    => Data.Handle,
             Offset    => Offset,
             Data      => Super_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      end if;

      return Success and (Ret_Count = Super_Data'Length);
   end RW_Superblock;

   function RW_Block_Group_Descriptor
      (Data             : EXT_Data_Acc;
       Descriptor_Index : Unsigned_32;
       Result           : in out Block_Group_Descriptor;
       Write_Operation  : Boolean) return Boolean
   is
      Descr_Size  : constant Natural := Block_Group_Descriptor'Size / 8;
      Offset      : Unsigned_32 := Data.Block_Size;
      Ret_Count   : Natural;
      Success     : Boolean;
      Result_Data : Operation_Data (1 .. Descr_Size) with Address =>
         Result'Address;
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
         return True;
      else
         Act_On_Policy (Data, "block group descriptor RW failure");
         return False;
      end if;
   end RW_Block_Group_Descriptor;

   function RW_Inode
      (Data            : EXT_Data_Acc;
       Inode_Index     : Unsigned_32;
       Result          : in out Inode;
       Write_Operation : Boolean) return Boolean
   is
      Table_Index, Descriptor_Index : Unsigned_32;
      Block_Descriptor : Block_Group_Descriptor;
      Offset      : Unsigned_32;
      Ret_Count   : Natural;
      Success     : Boolean;
      Result_Data : Operation_Data (1 .. Inode'Size / 8) with Address =>
         Result'Address;
   begin
      Table_Index      := (Inode_Index - 1) mod Data.Super.Inodes_Per_Group;
      Descriptor_Index := (Inode_Index - 1) / Data.Super.Inodes_Per_Group;

      Success := RW_Block_Group_Descriptor
         (Data             => Data,
          Descriptor_Index => Descriptor_Index,
          Result           => Block_Descriptor,
          Write_Operation  => False);
      if not Success then
         return False;
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
         return True;
      else
         Act_On_Policy (Data, "inode RW failure");
         return False;
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
         with Address => Single_Indirect_Index'Address;
      Indirect_Block_Data : Operation_Data (1 .. 4)
         with Address => Indirect_Block'Address;
      Block_Index_Data : Operation_Data (1 .. 4)
         with Address => Block_Index'Address;

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

   procedure Inner_Read_Symbolic_Link
      (Ino       : Inode;
       File_Size : Unsigned_64;
       Path      : out String;
       Ret_Count : out Natural)
   is
      Final_Length : Natural;
      Str_Data     : Operation_Data (1 .. Path'Length)
         with Address => Ino.Blocks'Address;
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
         with Address => Dir'Address;
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
            with Address => Dir_Name'Address;
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
         Success := Grow_Inode
            (FS_Data     => FS_Data,
             Inode_Data  => Inode_Data,
             Inode_Num   => Inode_Num,
             Start       => Offset,
             Count       => Unsigned_64 (Data'Length));
         if not Success then
            goto Error_Return;
         end if;
         Success := Set_Size
            (Ino        => Inode_Data,
             New_Size   => Offset + Unsigned_64 (Data'Length),
             Is_64_Bits => FS_Data.Has_64bit_Filesizes);
         if not Success then
            goto Error_Return;
         end if;
         Success := RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Num,
             Result          => Inode_Data,
             Write_Operation => True);
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

   function Grow_Inode
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start       : Unsigned_64;
       Count       : Unsigned_64) return Boolean
   is
      Offset : constant Unsigned_64 :=
         Shift_Right (Start and not Unsigned_64 (FS_Data.Block_Size - 1),
                      Natural (10 + FS_Data.Super.Block_Size_Log));
      BCount : constant Unsigned_64 :=
         Shift_Right (Start and Unsigned_64 (FS_Data.Block_Size - 1) + Count +
                      Unsigned_64 (FS_Data.Block_Size - 1),
                      Natural (10 + FS_Data.Super.Block_Size_Log));
   begin
      return Assign_Inode_Blocks
         (FS_Data     => FS_Data,
          Inode_Data  => Inode_Data,
          Inode_Num   => Inode_Num,
          Start_Blk   => Unsigned_32 (Offset),
          Block_Count => Unsigned_32 (BCount));
   end Grow_Inode;

   function Assign_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Start_Blk   : Unsigned_32;
       Block_Count : Unsigned_32) return Boolean
   is
      Success : Boolean;
      Ret_Blk : Unsigned_32;
   begin
      if Block_Count = 0 then
         return True;
      end if;

      for I in 0 .. Block_Count - 1 loop
         Ret_Blk := Get_Block_Index (FS_Data, Inode_Data, Start_Blk + I);
         if Ret_Blk = 0 then
            Success := Allocate_Block_For_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Inode_Data,
                Inode_Num  => Inode_Num,
                Ret_Block  => Ret_Blk);
            if not Success then
               return False;
            end if;
            Success := Wire_Inode_Blocks
               (FS_Data     => FS_Data,
                Inode_Data  => Inode_Data,
                Inode_Num   => Inode_Num,
                Block_Index => Start_Blk + I,
                Wired_Block => Ret_Blk);
            if not Success then
               return False;
            end if;
         end if;
      end loop;

      Success := RW_Inode
         (Data            => FS_Data,
          Inode_Index     => Inode_Num,
          Result          => Inode_Data,
          Write_Operation => True);
      return Success;
   end Assign_Inode_Blocks;

   function Wire_Inode_Blocks
      (FS_Data     : EXT_Data_Acc;
       Inode_Data  : in out Inode;
       Inode_Num   : Unsigned_32;
       Block_Index : Unsigned_32;
       Wired_Block : Unsigned_32) return Boolean
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
         with Address => Single_Indirect_Index'Address;
      Indirect_Block_Data : Operation_Data (1 .. 4)
         with Address => Indirect_Block'Address;
      DBlock_Data : Operation_Data (1 .. 4)
         with Address => DBlock'Address;

      Discard_1 : Natural;
      Discard_2 : Boolean;
   begin
      if Adjusted_Block < 12 then
         Inode_Data.Blocks (Natural (Adjusted_Block)) := Wired_Block;
         return True;
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
               Discard_2 := Allocate_Block_For_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Num,
                   Ret_Block  => Temp);
               Inode_Data.Blocks (14) := Temp;
               Discard_2 := RW_Inode
                  (Data            => FS_Data,
                   Inode_Index     => Inode_Num,
                   Result          => Inode_Data,
                   Write_Operation => True);
            end if;

            Devices.Read
               (Handle    => FS_Data.Handle,
                Offset    => Unsigned_64 (Inode_Data.Blocks (14) *
                             FS_Data.Block_Size + Double_Indirect * 4),
                Data      => Single_Indirect_Index_Data,
                Ret_Count => Discard_1,
                Success   => Discard_2);

            if Single_Indirect_Index = 0 then
               Discard_2 := Allocate_Block_For_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Num,
                   Ret_Block  => Single_Indirect_Index);

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
               Discard_2 := Allocate_Block_For_Inode
                  (FS_Data    => FS_Data,
                   Inode_Data => Inode_Data,
                   Inode_Num  => Inode_Num,
                   Ret_Block  => Temp);

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
            return True;
         end if;

         if Inode_Data.Blocks (13) = 0 then
            Discard_2 := Allocate_Block_For_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Inode_Data,
                Inode_Num  => Inode_Num,
                Ret_Block  => Temp);
            Inode_Data.Blocks (13) := Temp;
            Discard_2 := RW_Inode
               (Data            => FS_Data,
                Inode_Index     => Inode_Num,
                Result          => Inode_Data,
                Write_Operation => True);
         end if;

         Devices.Read
            (Handle    => FS_Data.Handle,
             Offset    => Unsigned_64 (Inode_Data.Blocks (13) *
                                       FS_Data.Block_Size + Single_Index * 4),
             Data      => Indirect_Block_Data,
             Ret_Count => Discard_1,
             Success   => Discard_2);

         if Indirect_Block = 0 then
            Discard_2 := Allocate_Block_For_Inode
               (FS_Data    => FS_Data,
                Inode_Data => Inode_Data,
                Inode_Num  => Inode_Num,
                Ret_Block  => Indirect_Block);

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
         return True;
      end if;

      if Inode_Data.Blocks (12) = 0 then
         Discard_2 := Allocate_Block_For_Inode
            (FS_Data    => FS_Data,
             Inode_Data => Inode_Data,
             Inode_Num  => Inode_Num,
             Ret_Block  => Temp);
         Inode_Data.Blocks (12) := Temp;
         Discard_2 := RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Num,
             Result          => Inode_Data,
             Write_Operation => True);
      end if;

      Devices.Write
         (Handle    => FS_Data.Handle,
          Offset    => Unsigned_64 (Inode_Data.Blocks (12) * FS_Data.Block_Size
                                    + Adjusted_Block * 4),
          Data      => DBlock_Data,
          Ret_Count => Discard_1,
          Success   => Discard_2);
      return True;
   end Wire_Inode_Blocks;

   function Allocate_Block_For_Inode
      (FS_Data    : EXT_Data_Acc;
       Inode_Data : in out Inode;
       Inode_Num  : Unsigned_32;
       Ret_Block  : out Unsigned_32) return Boolean
   is
      Success    : Boolean;
      Ret_Count  : Natural;
      Desc       : Block_Group_Descriptor;
      Bitmap     : Operation_Data (1 .. Natural (FS_Data.Block_Size));
      Curr_Block : Unsigned_32;
   begin
      for I in 0 .. FS_Data.Super.Block_Count loop
         Success := RW_Block_Group_Descriptor
            (Data             => FS_Data,
             Descriptor_Index => I,
             Result           => Desc,
             Write_Operation  => False);
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
             Data      => Bitmap,
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
               if (Bitmap (J) and 2 ** Bit) /= 0 then
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
             Data      => Bitmap,
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
         Success := RW_Inode
            (Data            => FS_Data,
             Inode_Index     => Inode_Num,
             Result          => Inode_Data,
             Write_Operation => True);
         if not Success then
            goto Error_Return;
         end if;

         Success := RW_Block_Group_Descriptor
            (Data             => FS_Data,
             Descriptor_Index => I,
             Result           => Desc,
             Write_Operation  => True);
         if not Success then
            goto Error_Return;
         end if;
         Ret_Block := Curr_Block;
         return True;
   <<Next_Iteration>>
      end loop;

   <<Error_Return>>
      Ret_Block := 0;
      return False;
   end Allocate_Block_For_Inode;

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

   function Set_Size
      (Ino        : out Inode;
       New_Size   : Unsigned_64;
       Is_64_Bits : Boolean) return Boolean
   is
      L32 : constant Unsigned_32 := Unsigned_32 (New_Size and 16#FFFFFFFF#);
      H32 : constant Unsigned_32 := Unsigned_32 (Shift_Right (New_Size, 32));
   begin
      if Is_64_Bits then
         Ino.Size_Low  := L32;
         Ino.Size_High := H32;
         return True;
      else
         Ino.Size_Low := L32;
         return H32 = 0;
      end if;
   end Set_Size;

   procedure Act_On_Policy (Data : EXT_Data_Acc; Message : String) is
   begin
      case Data.Super.Error_Policy is
         when Policy_Ignore =>
            Lib.Messages.Warn (Message);
         when Policy_Remount_RO =>
            Lib.Messages.Warn (Message);
            Data.Is_Read_Only := True;
         when Policy_Panic =>
            Lib.Panic.Hard_Panic (Message);
         when others =>
            Lib.Panic.Hard_Panic ("ext is dead, and we killed it");
      end case;
   end Act_On_Policy;
end VFS.EXT;
