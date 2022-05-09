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

package body VFS.USTAR is
   --  USTAR structures.
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
   --  USTAR_Hard_Link     : constant := 16#31#;
   --  USTAR_Symbolic_Link : constant := 16#32#;
   --  USTAR_Char_Device   : constant := 16#33#;
   --  USTAR_Block_Device  : constant := 16#34#;
   --  USTAR_Directory     : constant := 16#35#;
   --  USTAR_FIFO          : constant := 16#36#;
   --  USTAR_GNU_Long_Path : constant := 16#4C#;

   type USTAR_Data is record
      Dev : VFS.Device.Device_Data;
   end record;
   type USTAR_Data_Acc is access USTAR_Data;

   type USTAR_File is record
      Name  : String (1 .. 100);
      Mode  : Natural;
      Start : Unsigned_64;
      Size  : Natural;
   end record;
   type USTAR_File_Acc is access USTAR_File;

   function Probe (Dev : Device.Device_Data) return System.Address is
      First_Header : USTAR_Header;
      Byte_Size    : constant Unsigned_64 := First_Header'Size / 8;
      Data : constant USTAR_Data_Acc := new USTAR_Data'(Dev => Dev);
   begin
      if Dev.Read.all (Dev.Data, 0, Byte_Size, First_Header'Address) /=
         Byte_Size
      then
         return System.Null_Address;
      end if;

      if First_Header.Signature = USTAR_Signature then
         return Data.all'Address;
      end if;

      return System.Null_Address;
   end Probe;

   function Open (FS : System.Address; Path : String) return System.Address is
      FS_Data : USTAR_Data with Address => FS, Import;

      Header       : USTAR_Header;
      Header_Index : Unsigned_64 := 0;
      Byte_Size    : constant Unsigned_64 := Header'Size / 8;
      Result_Name  : String (1 .. 100);
      Result_Index : Unsigned_64;
      Result_Size  : Natural;
      Result_Mode  : Natural;
   begin
      --  Find the USTAR header corresponding with the name, if at all.
      --  TODO: Support GNU long names and symbolic links.
      loop
         if FS_Data.Dev.Read.all (FS_Data.Dev.Data, Header_Index, Byte_Size,
            Header'Address) /= Byte_Size
         then
            return System.Null_Address;
         end if;

         declare
            Size : constant Natural := Octal_To_Decimal (Header.Size);
            Mode : constant Natural := Octal_To_Decimal (Header.Mode);
            Jump :          Natural := Size;
         begin
            exit when Header.Signature /= USTAR_Signature;
            if Header.Name (1 .. Path'Length) = Path then
               if Header.File_Type = USTAR_Regular_File then
                  Result_Name  := Header.Name;
                  Result_Index := Header_Index + Byte_Size;
                  Result_Size  := Size;
                  Result_Mode  := Mode;
               else
                  Result_Index := 0;
               end if;
               goto Found_File;
            end if;
            --  Calculate where is the next header and add EOA empty record
            if Jump mod Natural (Byte_Size) /= 0 then
               Jump := Jump + (Natural (Byte_Size) -
                       (Jump mod Natural (Byte_Size)));
            end if;
            Jump := Jump + Natural (Byte_Size);
            Header_Index := Header_Index + Unsigned_64 (Jump);
         end;
      end loop;
      return System.Null_Address;

   <<Found_File>>
      --  Found it, but it's not a file we can open, so we fail.
      if Result_Index = 0 then
         return System.Null_Address;
      end if;
      --  Allocate the final data and return.
      declare
         Result_Data : constant USTAR_File_Acc := new USTAR_File'(
            Name  => Result_Name,
            Start => Result_Index,
            Size  => Result_Size,
            Mode  => Result_Mode
         );
      begin
         return Result_Data.all'Address;
      end;
   end Open;

   procedure Close (FS : System.Address; File_Ptr : System.Address) is
      pragma Unreferenced (FS);
   begin
      Memory.Physical.Free (To_Integer (File_Ptr));
   end Close;

   function Read
      (Data   : System.Address;
       Obj    : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      FS_Data    : USTAR_Data with Address => Data, Import;
      File_Data  : USTAR_File with Address => Obj,  Import;
      Real_Count : Unsigned_64 := Count;
   begin
      if Offset + Real_Count > Unsigned_64 (File_Data.Size) then
         Real_Count := Unsigned_64 (File_Data.Size) - Offset;
      end if;
      return FS_Data.Dev.Read.all (
         FS_Data.Dev.Data,
         File_Data.Start + Offset,
         Real_Count,
         Desto
      );
   end Read;

   function Stat
      (Data : System.Address;
       Obj  : System.Address;
       S    : out File_Stat) return Boolean
   is
      pragma Unreferenced (Data);
      File_Data : USTAR_File with Address => Obj, Import;
   begin
      S := (
         Unique_Identifier => File_Data.Start,
         Type_Of_File      => File_Regular,
         Mode              => Unsigned_32 (File_Data.Mode),
         Hard_Link_Count   => 1,
         Byte_Size         => Unsigned_64 (File_Data.Size),
         IO_Block_Size     => 512,
         IO_Block_Count    => (Unsigned_64 (File_Data.Size) + 512 - 1) / 512
      );
      return True;
   end Stat;

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
