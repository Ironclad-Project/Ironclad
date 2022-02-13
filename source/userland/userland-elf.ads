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
   --  Load an ELF from a file into memory with the passed base, and map it
   --  into the passed map. Return parsed info about the ELF.
   type Parsed_ELF is record
      Was_Loaded  : Boolean;
      Entrypoint  : System.Address;
      Linker_Path : access String;
   end record;
   function Load_ELF
      (File_D : FS.File.FD;
       Map    : in out Memory.Virtual.Page_Map;
       Base   : Unsigned_64) return Parsed_ELF;
end Userland.ELF;
