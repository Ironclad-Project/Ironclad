--  devices-ramdev.adb: RAM devices.
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

with Interfaces; use Interfaces;
with System; use System;
with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;
with Memory.Physical;
with Memory; use Memory;

package body Devices.Ramdev is
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

   --  Ramdev data.
   type Ramdev_Data is record
      Start_Address : System.Address;
      Size          : Virtual_Address;
      Is_USTAR      : Boolean;
   end record;
   type Ramdev_Data_Acc is access Ramdev_Data;

   type Ramdev_Object is record
      Name  : String (1 .. 100);
      Mode  : Natural;
      Start : System.Address;
      Size  : Natural;
   end record;
   type Ramdev_Object_Acc is access Ramdev_Object;

   function Init_Module
      (Module : Arch.Stivale2.Module;
       Name   : Root_Name) return Root
   is
      Start : constant Virtual_Address := To_Integer (Module.Begin_Address);
      End2  : constant Virtual_Address := To_Integer (Module.End_Address);
      Data  : constant Ramdev_Data_Acc := new Ramdev_Data'(
         Start_Address => To_Address (Start + Memory_Offset),
         Size          => End2 - Start,
         Is_USTAR      => False
      );
      First_Header : USTAR_Header with Address => Data.Start_Address;
   begin
      --  Check if we are doing USTAR, and load the object accordingly.
      if First_Header.Signature = USTAR_Signature then
         return (
            Name     => Name,
            Data     => Data.all'Address,
            Init     => Ramdev_Init'Access,
            Unload   => Ramdev_Unload'Access,
            Sync     => null,
            Create   => null,
            Open     => USTAR_Open'Access,
            Close    => USTAR_Close'Access,
            Read     => USTAR_Read'Access,
            Write    => null,
            Get_Size => USTAR_Get_Size'Access
         );
      else
         return (
            Name     => Name,
            Data     => Data.all'Address,
            Init     => Ramdev_Init'Access,
            Unload   => Ramdev_Unload'Access,
            Sync     => null,
            Create   => null,
            Open     => null,
            Close    => null,
            Read     => Raw_Ramdev_Read'Access,
            Write    => null,
            Get_Size => Raw_Ramdev_Get_Size'Access
         );
      end if;
   end Init_Module;

   function Ramdev_Init (Data : Root_Data) return Root_Data is
      Real_Data    : Ramdev_Data  with Address => Data;
      First_Header : USTAR_Header with Address => Real_Data.Start_Address;
   begin
      --  Check if we are doing a USTAR.
      Real_Data.Is_USTAR := First_Header.Signature = USTAR_Signature;
      return Data;
   end Ramdev_Init;

   procedure Ramdev_Unload (Data : Root_Data) is
   begin
      Memory.Physical.Free (To_Integer (Data));
   end Ramdev_Unload;
   ----------------------------------------------------------------------------
   function Raw_Ramdev_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : System.Address;
       Count  : Positive;
       Desto  : System.Address) return Natural
   is
      Data2     : Ramdev_Data with Address => Data;
      Data_Addr : constant System.Address  := Data2.Start_Address;
      Data_Sz   : constant Natural         := Natural (Data2.Size);
      Result    : array (1 .. Count)   of Unsigned_8 with Address => Desto;
      Real_Data : array (1 .. Data_Sz) of Unsigned_8 with Address => Data_Addr;
      Offset2   : constant Natural := Natural (To_Integer (Offset)) + Count;
      To_Write  : Natural := Count;
      pragma Unreferenced (Obj);
   begin
      if Offset2 > Data_Sz then
         To_Write := To_Write - (Offset2 - Data_Sz);
      end if;

      for I in 1 .. To_Write loop
         Result (I) := Real_Data (Natural (To_Integer (Offset)) + I);
      end loop;

      return To_Write;
   end Raw_Ramdev_Read;

   function Raw_Ramdev_Get_Size
      (Data : Root_Data;
       Obj  : Object) return Natural is
      Data2 : Ramdev_Data with Address => Data;
      pragma Unreferenced (Obj);
   begin
      return Natural (Data2.Size);
   end Raw_Ramdev_Get_Size;
   ----------------------------------------------------------------------------
   function USTAR_Open (Data : Root_Data; Name : String) return Object is
      Real_Data      : Ramdev_Data with Address => Data;
      Header_Address : System.Address := Real_Data.Start_Address;
      Header_Sz      : constant Integer := USTAR_Header'Size / 8;

      Result_Name  : String (1 .. 100);
      Result_Start : System.Address;
      Result_Size  : Natural;
      Result_Mode  : Natural;
   begin
      --  Find the USTAR header corresponding with the name, if at all.
      --  TODO: Support GNU long names and symbolic links.
      loop
         declare
            Header : USTAR_Header with Address => Header_Address;
            Size : constant Natural := Octal_To_Decimal (Header.Size);
            Mode : constant Natural := Octal_To_Decimal (Header.Mode);
            Jump :          Natural := Size;
         begin
            exit when Header.Signature /= USTAR_Signature;

            if Header.Name (1 .. Name'Length) = Name then
               if Header.File_Type = USTAR_Regular_File then
                  Result_Name  := Header.Name;
                  Result_Start := Header'Address + Storage_Offset (Header_Sz);
                  Result_Size  := Size;
                  Result_Mode  := Mode;
               else
                  Result_Start := System.Null_Address;
               end if;
               goto Found_File;
            end if;

            --  Calculate where is the next header and add EOA empty record
            if Jump mod Header_Sz /= 0 then
               Jump := Jump + (Header_Sz - (Jump mod Header_Sz));
            end if;
            Jump := Jump + Header_Sz;

            Header_Address := Header_Address + Storage_Offset (Jump);
         end;
      end loop;
      return System.Null_Address;

   <<Found_File>>
      --  Found it, but it's not a file we can open, so we fail.
      if Result_Start = System.Null_Address then
         return System.Null_Address;
      end if;

      --  Allocate the final data and return.
      declare
         Result_Data : constant Ramdev_Object_Acc := new Ramdev_Object'(
            Name  => Result_Name,
            Start => Result_Start,
            Size  => Result_Size,
            Mode  => Result_Mode
         );
      begin
         return Result_Data.all'Address;
      end;
   end USTAR_Open;

   procedure USTAR_Close (Data : Root_Data; Obj : Object) is
      pragma Unreferenced (Data);
   begin
      --  Free the data.
      Memory.Physical.Free (To_Integer (Obj));
   end USTAR_Close;

   function USTAR_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : System.Address;
       Count  : Positive;
       Desto  : System.Address) return Natural
   is
      Obj2      : Ramdev_Object with Address => Obj;
      Data_Addr : constant System.Address  := Obj2.Start;
      Data_Sz   : constant Natural         := Obj2.Size;
      Result    : array (1 .. Count)   of Unsigned_8 with Address => Desto;
      Real_Data : array (1 .. Data_Sz) of Unsigned_8 with Address => Data_Addr;
      Offset2   : constant Natural := Natural (To_Integer (Offset)) + Count;
      To_Read  : Natural := Count;
      pragma Unreferenced (Data);
   begin
      --  Do not answer to device reads.
      if Obj = System.Null_Address then
         return 0;
      end if;

      if Offset2 > Data_Sz then
         To_Read := To_Read - (Offset2 - Data_Sz);
      end if;

      for I in 1 .. To_Read loop
         Result (I) := Real_Data (Natural (To_Integer (Offset)) + I);
      end loop;

      return To_Read;
   end USTAR_Read;

   function USTAR_Get_Size
      (Data : Root_Data;
       Obj  : Object) return Natural is
      Data2 : Ramdev_Data with Address => Data;
      Obj2  : Ramdev_Object with Address => Obj;
   begin
      if Obj = System.Null_Address then
         return Natural (Data2.Size);
      else
         return Obj2.Size;
      end if;
   end USTAR_Get_Size;

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
end Devices.Ramdev;
