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
with FS.File; use FS.File;
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

   function Load_ELF
      (File_D : FS.File.FD;
       Map    : in out Memory.Virtual.Page_Map;
       Base   : Unsigned_64) return Parsed_ELF
   is
      Header : ELF_Header;
      Result : Parsed_ELF := (
         Was_Loaded  => False,
         Entrypoint  => System.Null_Address,
         Linker_Path => null,
         Vector => (
            Entrypoint => 0,
            Program_Headers => 0,
            Program_Header_Count => 0,
            Program_Header_Size => 0
         ));
      Header_Bytes : constant Natural := ELF_Header'Size / 8;
   begin
      --  Read and check the header.

      if FS.File.Read (File_D, Header_Bytes, Header'Address) /= Header_Bytes
      then
         return Result;
      end if;
      if Header.Identifier (1 .. 4) /= ELF_Signature then
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
         HSize : constant Natural := Natural (Header.Program_Header_Size);
         RSize : constant Natural := HSize * PHDRs'Length;
      begin
         if HSize = 0 or PHDRs'Length = 0 then
            return Result;
         end if;

         FS.File.Set_Index (File_D, Natural (Header.Program_Header_List));

         if FS.File.Read (File_D, RSize, PHDRs'Address) /= RSize then
            return Result;
         end if;

         for HDR of PHDRs loop
            case HDR.Segment_Type is
               when Program_Loadable_Segment =>
                  if not Load_Header (File_D, HDR, Map, Base) then
                     return Result;
                  end if;
               when Program_Header_Table_Segment =>
                  Result.Vector.Program_Headers := Base + HDR.Virt_Address;
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

   function Open_And_Load_ELF
      (Path : String;
       Map  : in out Memory.Virtual.Page_Map;
       Base : Unsigned_64) return Parsed_ELF
   is
      FD : constant FS.File.FD := FS.File.Open (Path, FS.File.Access_R);
      Loaded_ELF : Parsed_ELF := (
         Was_Loaded  => False,
         Entrypoint  => System.Null_Address,
         Linker_Path => null,
         Vector => (
            Entrypoint => 0,
            Program_Headers => 0,
            Program_Header_Count => 0,
            Program_Header_Size => 0
         ));
   begin
      if FD = FS.File.Error_FD then
         return Loaded_ELF;
      end if;

      Loaded_ELF := ELF.Load_ELF (FD, Map, Base);
      FS.File.Close (FD);
      return Loaded_ELF;
   end Open_And_Load_ELF;

   --  Get the linker path string from a given interpreter program header.
   function Get_Linker
      (File_D : FS.File.FD;
       Header : Program_Header) return access String is
      Discard : Natural;
   begin
      return Ret : access String := new String (1 .. Header.File_Size_Bytes)
      do
         FS.File.Set_Index (File_D, Natural (Header.Offset));
         Discard := FS.File.Read
            (File_D, Header.File_Size_Bytes, Ret.all'Address);
      end return;
   end Get_Linker;

   --  Load and map a loadable program header to memory.
   function Load_Header
      (File_D : FS.File.FD;
       Header : Program_Header;
       Map    : in out Memory.Virtual.Page_Map;
       Base   : Unsigned_64) return Boolean
   is
      MisAlign : constant Unsigned_64 :=
         Header.Virt_Address and (Memory.Virtual.Page_Size - 1);
      Load_Size : constant Unsigned_64 := MisAlign + Header.Mem_Size_Bytes;
      Load : array (1 .. Load_Size) of Unsigned_8
         with Address => To_Address (Memory.Physical.Alloc (Size (Load_Size)));
      Load_Addr : constant System.Address := Load'Address +
         Storage_Offset (MisAlign);
      ELF_Virtual : constant Virtual_Address :=
         Virtual_Address (Base + Header.Virt_Address);
      Flags : constant Memory.Virtual.Page_Flags :=
         (Present         => True,
          Read_Write      => True,
          User_Supervisor => True,
          Write_Through   => False,
          Cache_Disable   => False,
          Accessed        => False,
          Dirty           => False,
          PAT             => False,
          Global          => False);
   begin
      Memory.Virtual.Map_Range
         (Map         => Map,
          Virtual     => ELF_Virtual,
          Physical    => To_Integer (Load'Address) - Memory.Memory_Offset,
          Length      => Load_Size,
          Flags       => Flags,
          Not_Execute => False);
      FS.File.Set_Index (File_D, Natural (Header.Offset));
      return FS.File.Read (File_D, Header.File_Size_Bytes, Load_Addr) =
             Header.File_Size_Bytes;
   end Load_Header;
end Userland.ELF;
