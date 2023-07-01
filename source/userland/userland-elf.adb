--  userland-elf.adb: ELF loading.
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

with System.Storage_Elements; use System.Storage_Elements;
with Memory; use Memory;
with Arch.MMU;
with Lib.Alignment;
with Devices;

package body Userland.ELF with SPARK_Mode => Off is
   type ELF_ID_Field is array (Natural range <>) of Unsigned_8;
   ELF_Signature : constant ELF_ID_Field (1 .. 4) :=
      (16#7F#, Character'Pos ('E'), Character'Pos ('L'), Character'Pos ('F'));
   type ELF_Header is record
      Identifier           : ELF_ID_Field (1 .. 16);
      ELF_Type             : Unsigned_16;
      Machine              : Unsigned_16;
      Version              : Unsigned_32;
      Entrypoint           : System.Address;
      Program_Header_List  : Unsigned_64;
      Section_Header_List  : Unsigned_64;
      Flags                : Unsigned_32;
      Header_Size          : Unsigned_16;
      Program_Header_Size  : Unsigned_16;
      Program_Header_Count : Unsigned_16;
      Section_Header_Size  : Unsigned_16;
      Section_Header_Count : Unsigned_16;
      Section_Names_Index  : Unsigned_16;
   end record;
   for ELF_Header use record
      Identifier           at 0 range   0 .. 127;
      ELF_Type             at 0 range 128 .. 143;
      Machine              at 0 range 144 .. 159;
      Version              at 0 range 160 .. 191;
      Entrypoint           at 0 range 192 .. 255;
      Program_Header_List  at 0 range 256 .. 319;
      Section_Header_List  at 0 range 320 .. 383;
      Flags                at 0 range 384 .. 415;
      Header_Size          at 0 range 416 .. 431;
      Program_Header_Size  at 0 range 432 .. 447;
      Program_Header_Count at 0 range 448 .. 463;
      Section_Header_Size  at 0 range 464 .. 479;
      Section_Header_Count at 0 range 480 .. 495;
      Section_Names_Index  at 0 range 496 .. 511;
   end record;
   for ELF_Header'Size use 512;

   function Load_ELF
      (FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Map    : Memory.Virtual.Page_Map_Acc;
       Base   : Unsigned_64) return Parsed_ELF
   is
      use VFS;

      Header       : ELF_Header;
      Header_Bytes : constant Natural := ELF_Header'Size / 8;
      Header_Data  : Devices.Operation_Data (1 .. Header_Bytes)
         with Import, Address => Header'Address;

      Result : Parsed_ELF := (
         Was_Loaded  => False,
         Entrypoint  => System.Null_Address,
         Linker_Path => null,
         Vector => (
            Entrypoint => 0,
            Program_Headers => 0,
            Program_Header_Count => 0,
            Program_Header_Size => 0
         ),
         Exec_Stack => True
      );
      Ret_Count : Natural;
      Pos       : Unsigned_64 := 0;
      Discard   : Boolean;
      Success   : FS_Status;
   begin
      --  Read and check the header.
      VFS.Read (FS, Ino, Pos, Header_Data, Ret_Count, Success, 0);
      Pos := Pos + Unsigned_64 (Ret_Count);
      if Success /= FS_Success or Ret_Count /= Header_Bytes or
         Header.Identifier (1 .. 4) /= ELF_Signature
      then
         return Result;
      end if;

      --  Assign the data we already know.
      Result.Entrypoint := Header.Entrypoint + Storage_Offset (Base);
      Result.Vector.Entrypoint := Unsigned_64 (To_Integer (Result.Entrypoint));
      Result.Vector.Program_Header_Size  := Program_Header'Size / 8;
      Result.Vector.Program_Header_Count :=
         Unsigned_64 (Header.Program_Header_Count);

      --  Loop the program headers and either load them, or get info.
      declare
         PHDRs : array (1 .. Header.Program_Header_Count) of Program_Header;
         HSize : constant Unsigned_64 :=
            Unsigned_64 (Header.Program_Header_Size);
         RSize : constant Unsigned_64 := HSize * PHDRs'Length;
         PHDRs_Data : Devices.Operation_Data (1 .. Natural (RSize))
            with Import, Address => PHDRs'Address;
      begin
         if HSize = 0 or PHDRs'Length = 0 then
            return Result;
         end if;

         Pos := Header.Program_Header_List;
         VFS.Read (FS, Ino, Pos, PHDRs_Data, Ret_Count, Success, 0);
         Pos := Pos + Unsigned_64 (Ret_Count);
         if Success /= FS_Success or Ret_Count /= Natural (RSize) then
            return Result;
         end if;

         for HDR of PHDRs loop
            case HDR.Segment_Type is
               when Program_Loadable_Segment =>
                  if not Load_Header (FS, Ino, HDR, Map, Base) then
                     return Result;
                  end if;
               when Program_Header_Table_Segment =>
                  Result.Vector.Program_Headers := Base + HDR.Virt_Address;
               when Program_Interpreter_Segment =>
                  Result.Linker_Path := Get_Linker (FS, Ino, HDR);
               when Program_GNU_Stack =>
                  Result.Exec_Stack := (HDR.Flags and Flags_Executable) /= 0;
               when others =>
                  null;
            end case;
         end loop;

         --  Return success.
         Result.Was_Loaded := True;
         return Result;
      end;
   end Load_ELF;

   --  Get the linker path string from a given interpreter program header.
   function Get_Linker
      (FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Header : Program_Header) return String_Acc
   is
      use VFS;
      Discard  : Unsigned_64;
      Ret : constant String_Acc := new String (1 .. Header.File_Size_Bytes);
      Ret_Data : Devices.Operation_Data (1 .. Header.File_Size_Bytes)
         with Import, Address => Ret (1)'Address;
      Ret_Count : Natural;
      Discard2  : Boolean;
      Success   : FS_Status;
   begin
      VFS.Read (FS, Ino, Header.Offset, Ret_Data, Ret_Count, Success, 0);
      if Success = FS_Success and Ret_Count = Header.File_Size_Bytes then
         return Ret;
      else
         return null;
      end if;
   end Get_Linker;

   --  Load and map a loadable program header to memory.
   function Load_Header
      (FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Header : Program_Header;
       Map    : Memory.Virtual.Page_Map_Acc;
       Base   : Unsigned_64) return Boolean
   is
      use VFS;
      package Align1 is new Lib.Alignment (Unsigned_64);
      package Align2 is new Lib.Alignment (Integer_Address);

      MisAlign : constant Unsigned_64 :=
         Header.Virt_Address and (Memory.Virtual.Page_Size - 1);
      Load_Size : constant Unsigned_64 := MisAlign + Header.Mem_Size_Bytes;
      ELF_Virtual : constant Virtual_Address :=
         Virtual_Address (Base + Header.Virt_Address);
      Flags : constant Arch.MMU.Page_Permissions := (
         User_Accesible => True,
         Read_Only      => (Header.Flags and Flags_Write)       = 0,
         Executable     => (Header.Flags and Flags_Executable) /= 0,
         Global         => False,
         Write_Through  => False
      );
      Ret_Count : Natural;
      Discard   : Boolean;
      Success   : FS_Status;
      Result    : Virtual_Address;
   begin
      if not Memory.Virtual.Check_Userland_Mappability (ELF_Virtual, Load_Size)
      then
         return False;
      end if;

      if not Memory.Virtual.Map_Memory_Backed_Region
         (Map      => Map,
          Virtual  => Align2.Align_Down (ELF_Virtual,
                      Memory.Virtual.Page_Size),
          Length   => Align1.Align_Up (Load_Size, Memory.Virtual.Page_Size),
          Flags    => Flags,
          Writing  => Result)
      then
         return False;
      end if;

      declare
         Load2 : Devices.Operation_Data (1 .. Header.File_Size_Bytes)
            with Import, Address => To_Address (Result) +
                                    Storage_Offset (MisAlign);
      begin
         VFS.Read (FS, Ino, Header.Offset, Load2, Ret_Count, Success, 0);
         return Success = FS_Success and Ret_Count = Header.File_Size_Bytes;
      end;
   end Load_Header;
end Userland.ELF;
