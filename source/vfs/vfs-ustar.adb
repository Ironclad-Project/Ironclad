--  vfs-ustar.adb: USTAR FS driver.
--  Copyright (C) 2021 streaksu
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

with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;
with Memory.Physical;
with System; use System;
with Interfaces.C;
with Lib.Alignment;

package body VFS.USTAR with SPARK_Mode => Off is
   type USTAR_Padding is array (Natural range <>) of Boolean with Pack;
   USTAR_Signature : constant String := "ustar ";
   type USTAR_Header is record
      Name         : String (1 .. 100);
      Mode         : String (1 .. 8);
      UID          : String (1 .. 8);
      GID          : String (1 .. 8);
      Size         : String (1 .. 12);
      MTime        : String (1 .. 12);
      Checksum     : String (1 .. 8);
      File_Type    : Unsigned_8;
      Link_Name    : String (1 .. 100);
      Signature    : String (1 .. 6);
      Version      : String (1 .. 2);
      Owner        : String (1 .. 32);
      Device_Major : String (1 .. 8);
      Device_Minor : String (1 .. 8);
      Prefix       : String (1 .. 155);
      Unused       : USTAR_Padding (1 .. 352);
   end record;
   for USTAR_Header use record
      Name         at 0 range    0 ..  799;
      Mode         at 0 range  800 ..  863;
      UID          at 0 range  864 ..  927;
      GID          at 0 range  928 ..  991;
      Size         at 0 range  992 .. 1087;
      MTime        at 0 range 1088 .. 1183;
      Checksum     at 0 range 1184 .. 1247;
      File_Type    at 0 range 1248 .. 1255;
      Link_Name    at 0 range 1256 .. 2055;
      Signature    at 0 range 2056 .. 2103;
      Version      at 0 range 2104 .. 2119;
      Owner        at 0 range 2120 .. 2375;
      Device_Major at 0 range 2376 .. 2439;
      Device_Minor at 0 range 2440 .. 2503;
      Prefix       at 0 range 2504 .. 3743;
      Unused       at 0 range 3744 .. 4095;
   end record;
   for USTAR_Header'Size use 4096; -- Padding.

   --  USTAR file types.
   USTAR_Regular_File  : constant := 16#30#;
   USTAR_Symbolic_Link : constant := 16#32#;
   USTAR_Directory     : constant := 16#35#;

   --  Since the files are constant and we dont have to worry about write, we
   --  can just cache the locations of all the files at boot.
   type USTAR_Cached_Files is array (Natural range <>) of aliased USTAR_File;
   type USTAR_Cached_Files_Acc is access USTAR_Cached_Files;
   type USTAR_Data is record
      Key   : Positive;
      Cache : USTAR_Cached_Files_Acc;
   end record;
   type USTAR_Data_Acc is access USTAR_Data;

   function Probe (Key : Positive) return System.Address is
      Header          : USTAR_Header;
      Byte_Size       : constant Natural := Header'Size / 8;
      Byte_Size_64    : constant Unsigned_64 := Unsigned_64 (Byte_Size);
      Header_Data     : Operation_Data (1 .. Byte_Size)
         with Import, Address => Header'Address;
      Data            : USTAR_Data_Acc;
      Header_Index    : Unsigned_64 := 0;
      Size, Jump      : Natural;
      Linked_Name_Len : Natural;
      Name_Len        : Natural;
      File_Count      : Natural := 0;
      Ret_Count       : Natural;
      Success         : Boolean;
   begin
      loop
         VFS.Read
            (Key       => Key,
             Offset    => Header_Index,
             Data      => Header_Data,
             Ret_Count => Ret_Count,
             Success   => Success);

         if not Success or else Ret_Count /= Byte_Size or else
            Header.Signature /= USTAR_Signature
         then
            exit;
         else
            File_Count := File_Count + 1;
         end if;

         Size := Octal_To_Decimal (Header.Size);
         Jump := Size;
         if Jump mod Byte_Size /= 0 then
            Jump := Jump + (Byte_Size - (Jump mod Byte_Size));
         end if;
         Jump := Jump + Byte_Size;
         Header_Index := Header_Index + Unsigned_64 (Jump);
      end loop;

      if File_Count = 0 then
         return System.Null_Address;
      end if;
      Data := new USTAR_Data'(Key, new USTAR_Cached_Files (1 .. File_Count));

      Header_Index := 0;
      for Cache_File of Data.Cache.all loop
         Name_Len        := 0;
         Linked_Name_Len := 0;
         Read
            (Key       => Key,
             Offset    => Header_Index,
             Data      => Header_Data,
             Ret_Count => Ret_Count,
             Success   => Success);
         Size := Octal_To_Decimal (Header.Size);
         for C of Header.Name loop
            exit when C = Ada.Characters.Latin_1.NUL;
            Name_Len := Name_Len + 1;
         end loop;
         case Header.File_Type is
            when USTAR_Symbolic_Link =>
               for C of Header.Link_Name loop
                  exit when C = Ada.Characters.Latin_1.NUL;
                  Linked_Name_Len := Linked_Name_Len + 1;
               end loop;
               Cache_File :=
                  (Name      => Header.Name,
                   Name_Len  => Name_Len,
                   Start     => Header_Index + Header.Link_Name'Position,
                   Size      => Linked_Name_Len,
                   File_Type => Header.File_Type,
                   Mode      => Octal_To_Decimal (Header.Mode));
            when USTAR_Directory =>
               Cache_File :=
                  (Name      => Header.Name,
                   Name_Len  => Name_Len - 1, --  USTAR appends / to dir names.
                   Start     => Header_Index + Byte_Size_64,
                   Size      => Size,
                   File_Type => Header.File_Type,
                   Mode      => Octal_To_Decimal (Header.Mode));
            when others =>
               Cache_File :=
                  (Name      => Header.Name,
                   Name_Len  => Name_Len,
                   Start     => Header_Index + Byte_Size_64,
                   Size      => Size,
                   File_Type => Header.File_Type,
                   Mode      => Octal_To_Decimal (Header.Mode));
         end case;

         Jump := Size;
         if Jump mod Byte_Size /= 0 then
            Jump := Jump + (Byte_Size - (Jump mod Byte_Size));
         end if;
         Jump := Jump + Byte_Size;
         Header_Index := Header_Index + Unsigned_64 (Jump);
      end loop;

      return Data.all'Address;
   end Probe;

   function Open (FS : System.Address; Path : String) return System.Address is
      FS_Data : USTAR_Data with Address => FS, Import;
      Data    : USTAR_File_Acc;
   begin
      if Fetch_Header (FS, Path, Data) then
         return Data.all'Address;
      else
         return System.Null_Address;
      end if;
   end Open;

   function Check_Permissions
      (FS        : System.Address;
       Path      : String;
       Exists    : Boolean;
       Can_Read  : Boolean;
       Can_Write : Boolean;
       Can_Exec  : Boolean) return Boolean
   is
      pragma Unreferenced (Can_Read);
      pragma Unreferenced (Can_Exec);
      Data : USTAR_File_Acc;
   begin
      if Can_Write then
         return False;
      elsif not Fetch_Header (FS, Path, Data) then
         return not Exists;
      else
         return True;
      end if;
   end Check_Permissions;

   procedure Close (FS : System.Address; File_Ptr : System.Address) is
      pragma Unreferenced (FS);
   begin
      Memory.Physical.Free (Interfaces.C.size_t (To_Integer (File_Ptr)));
   end Close;

   procedure Read
      (FS_Data   : System.Address;
       Obj       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      FS_Data2   : USTAR_Data with Address => FS_Data, Import;
      File_Data  : USTAR_File with Address => Obj,     Import;
      Real_Count : Natural := Data'Length;
   begin
      if Offset + Unsigned_64 (Real_Count) > Unsigned_64 (File_Data.Size) then
         Real_Count := Natural (Unsigned_64 (File_Data.Size) - Offset);
      end if;
      VFS.Read (
         Key       => FS_Data2.Key,
         Offset    => File_Data.Start + Offset,
         Ret_Count => Ret_Count,
         Success   => Success,
         Data      => Data (Data'First .. Data'First + Real_Count - 1)
      );
   end Read;

   function Stat
      (Data : System.Address;
       Obj  : System.Address;
       S    : out File_Stat) return Boolean
   is
      package A is new Lib.Alignment (Unsigned_64);
      FS_Data   : USTAR_Data with Address => Data, Import;
      File_Data : USTAR_File with Address => Obj,  Import;
      Han : constant Devices.Device_Handle := Get_Backing_Device (FS_Data.Key);
      Block_Size : constant Natural := Devices.Get_Block_Size (Han);
   begin
      S :=
         (Unique_Identifier => File_Data.Start,
          Type_Of_File      => <>,
          Mode              => Unsigned_32 (File_Data.Mode),
          Hard_Link_Count   => 1,
          Byte_Size         => Unsigned_64 (File_Data.Size),
          IO_Block_Size     => Block_Size,
          IO_Block_Count    => A.Divide_Round_Up
            (Unsigned_64 (File_Data.Size), Unsigned_64 (Block_Size)));

      case File_Data.File_Type is
         when USTAR_Regular_File  => S.Type_Of_File := File_Regular;
         when USTAR_Symbolic_Link => S.Type_Of_File := File_Symbolic_Link;
         when USTAR_Directory     => S.Type_Of_File := File_Directory;
         when others              => null;
      end case;

      return True;
   end Stat;

   function Fetch_Header
      (FS   : System.Address;
       Path : String;
       Data : out USTAR_File_Acc) return Boolean
   is
      FS_Data : USTAR_Data with Address => FS, Import;
   begin
      for I in FS_Data.Cache'Range loop
         if Path'Length = FS_Data.Cache (I).Name_Len and
            FS_Data.Cache (I).Name (1 .. Path'Length) = Path
         then
            Data := FS_Data.Cache (I)'Access;
            return True;
         end if;
      end loop;
      Data := null;
      return False;
   end Fetch_Header;

   function Octal_To_Decimal (Octal : String) return Natural is
      Result : Natural := 0;
   begin
      for C of Octal loop
         exit when C = Ada.Characters.Latin_1.NUL;
         Result := Result * 8;
         Result := Result + (Character'Pos (C) - Character'Pos ('0'));
      end loop;
      return Result;
   end Octal_To_Decimal;
end VFS.USTAR;
