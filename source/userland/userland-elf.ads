--  userland-elf.ads: Specification of the ELF loading library.
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

with System;
with Interfaces; use Interfaces;
with FS.File;
with Memory.Virtual;

package Userland.ELF is
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
      Linker_Path : access String;
      Vector      : Auxval;
   end record;

   --  Load an ELF from a file into memory with the passed base, and map it
   --  into the passed map. Return parsed info about the ELF.
   function Load_ELF
      (File_D : FS.File.FD;
       Map    : Memory.Virtual.Page_Map_Acc;
       Base   : Unsigned_64) return Parsed_ELF;

   --  Do the same as the one above but opens and closes the file for you.
   function Open_And_Load_ELF
      (Path : String;
       Map  : Memory.Virtual.Page_Map_Acc;
       Base : Unsigned_64) return Parsed_ELF;

private

   Program_Loadable_Segment     : constant := 1;
   Program_Dynamic_Segment      : constant := 2;
   Program_Interpreter_Segment  : constant := 3;
   Program_Header_Table_Segment : constant := 6;
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

   function Get_Linker
      (File_D : FS.File.FD;
       Header : Program_Header) return access String;

   function Load_Header
      (File_D : FS.File.FD;
       Header : Program_Header;
       Map    : Memory.Virtual.Page_Map_Acc;
       Base   : Unsigned_64) return Boolean;
end Userland.ELF;
