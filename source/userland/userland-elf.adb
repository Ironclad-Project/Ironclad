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
with Memory.Physical;
with Memory; use Memory;

package body Userland.ELF is
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

   Program_Loadable_Segment    : constant := 1;
   Program_Interpreter_Segment : constant := 3;
   type Program_Header is record
      Segment_Type    : Unsigned_32;
      Flags           : Unsigned_32;
      Offset          : Unsigned_64;
      Virt_Address    : Unsigned_64;
      Phys_Address    : System.Address;
      File_Size_Bytes : Natural;
      Mem_Size_Bytes  : Unsigned_64;
      Alignment       : Unsigned_64;
   end record;
   for Program_Header use record
      Segment_Type    at 0 range  0  ..  31;
      Flags           at 0 range 32  ..  63;
      Offset          at 0 range 64  .. 127;
      Virt_Address    at 0 range 128 .. 191;
      Phys_Address    at 0 range 192 .. 255;
      File_Size_Bytes at 0 range 256 .. 319;
      Mem_Size_Bytes  at 0 range 320 .. 383;
      Alignment       at 0 range 384 .. 447;
   end record;
   for Program_Header'Size use 448;

   --  Memory base to load all executables.
   ELF_Base : constant := 16#40000000#;

   --  Get the linker path string from a given interpreter program header.
   function Get_Linker
      (File_D : FS.File.FD;
       Header : Program_Header) return access String is
   begin
      return Ret : access String := new String (1 .. Header.File_Size_Bytes)
      do
         FS.File.Set_Index (File_D, Natural (Header.Offset));
         FS.File.Read (File_D, Header.File_Size_Bytes, Ret.all'Address);
      end return;
   end Get_Linker;

   --  Load and map a loadable program header to memory.
   procedure Load_Header
      (File_D : FS.File.FD;
       Header : Program_Header;
       Map    : in out Memory.Virtual.Page_Map;
       Base   : Unsigned_64) is
      MisAlign : constant Unsigned_64 :=
         Header.Virt_Address and (Memory.Virtual.Page_Size - 1);
      Load_Size : constant Unsigned_64 := MisAlign + Header.Mem_Size_Bytes;
      Load : array (1 .. Load_Size) of Unsigned_8
         with Address => To_Address (Memory.Physical.Alloc (Size (Load_Size)));
      ELF_Virtual : constant Virtual_Address :=
         Virtual_Address (Base + Header.Virt_Address);
      Flags : constant Memory.Virtual.Page_Flags :=
         (Present => True, Read_Write => True, User_Supervisor => True,
          Write_Through => False, Cache_Disable => False,
          Accessed => False, Dirty => False, PAT => False,
          Global => False);
   begin
      Memory.Virtual.Map_Range (Map, ELF_Virtual,
         To_Integer (Load'Address) - Memory.Memory_Offset, Load_Size, Flags,
         False);
      FS.File.Set_Index (File_D, Natural (Header.Offset));
      FS.File.Read (File_D, Header.File_Size_Bytes, Load'Address);
   end Load_Header;

   function Load_ELF
      (File_D : FS.File.FD;
       Map    : in out Memory.Virtual.Page_Map;
       Base   : Unsigned_64) return Parsed_ELF is
      Header : ELF_Header;
      Result : Parsed_ELF := (False, System.Null_Address, null);
   begin
      --  Read and check the header.
      FS.File.Read (File_D, Header'Size / 8, Header'Address);
      if FS.File.Is_Error (File_D) then
         return Result;
      end if;
      if Header.Identifier (1 .. 4) /= ELF_Signature then
         return Result;
      end if;

      --  Assign the data we already know.
      Result.Entrypoint := Header.Entrypoint + Storage_Offset (ELF_Base);

      --  Loop the program headers and either load them, or get info.
      declare
         PHDRs : array (1 .. Header.Program_Header_Count) of Program_Header;
         HSize : constant Natural := Natural (Header.Program_Header_Size);
      begin
         FS.File.Set_Index (File_D, Natural (Header.Program_Header_List));
         FS.File.Read (File_D, HSize * PHDRs'Length, PHDRs'Address);
         if FS.File.Is_Error (File_D) then
            return Result;
         end if;

         for HDR of PHDRs loop
            case HDR.Segment_Type is
               when Program_Loadable_Segment =>
                  Load_Header (File_D, HDR, Map, Base);
               when Program_Interpreter_Segment =>
                  Result.Linker_Path := Get_Linker (File_D, HDR);
               when others =>
                  null;
            end case;
         end loop;

         --  Return success.
         Result.Was_Loaded := True;
         return Result;
      end;
   end Load_ELF;
end Userland.ELF;
