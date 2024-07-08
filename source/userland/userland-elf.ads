--  userland-elf.ads: ELF loading library.
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

with System;
with Interfaces; use Interfaces;
with VFS;
with Arch.MMU;

package Userland.ELF is
   type String_Acc is access String;

   --  Auxval values.
   Auxval_Null            : constant := 0;
   Auxval_Program_Headers : constant := 3;
   Auxval_Header_Size     : constant := 4;
   Auxval_Header_Count    : constant := 5;
   Auxval_Entrypoint      : constant := 9;
   type Auxval is record
      Entrypoint           : Unsigned_64;
      Program_Headers      : Unsigned_64;
      Program_Header_Count : Unsigned_64;
      Program_Header_Size  : Unsigned_64;
   end record;

   type Parsed_ELF is record
      Was_Loaded  : Boolean;
      Entrypoint  : System.Address;
      Linker_Path : String_Acc;
      Vector      : Auxval;
      Exec_Stack  : Boolean;
   end record;

   --  Load an ELF from a file into memory with the passed base, and map it
   --  into the passed map. Return parsed info about the ELF.
   function Load_ELF
      (FS             : VFS.FS_Handle;
       Ino            : VFS.File_Inode_Number;
       Map            : Arch.MMU.Page_Table_Acc;
       Requested_Base : Unsigned_64) return Parsed_ELF;

private

   --  ELF types.
   ET_DYN : constant := 3;

   --  Segment types.
   Program_Loadable_Segment     : constant := 1;
   Program_Dynamic_Segment      : constant := 2;
   Program_Interpreter_Segment  : constant := 3;
   Program_Header_Table_Segment : constant := 6;
   Program_GNU_Stack            : constant := 16#6474E551#;

   --  Masks for the flags field.
   Flags_Executable : constant := 1;
   Flags_Write      : constant := 2;
   Flags_Read       : constant := 4;
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

   --  Get the linker path string from a given interpreter program header.
   function Get_Linker
      (FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Header : Program_Header) return String_Acc;

   --  Load and map a loadable program header to memory.
   function Load_Header
      (FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Header : Program_Header;
       Map    : Arch.MMU.Page_Table_Acc;
       Base   : Unsigned_64) return Boolean;
end Userland.ELF;
