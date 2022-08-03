--  arch.adb: Architecture-specific package.
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

with System.Address_To_Access_Conversions;
with Arch.Stivale2; use Arch.Stivale2;
with Arch.Entrypoint;
pragma Unreferenced (Arch.Entrypoint);

package body Arch with SPARK_Mode => Off is
   --  Cache parsing.
   Did_Cache   : Boolean := False;
   Cached_Info : Boot_Information;

   function Get_Info return Boot_Information is
      package C is new System.Address_To_Access_Conversions (Memmap_Tag);

      Mmap : access Memmap_Tag;
   begin
      if Did_Cache then
         return Cached_Info;
      end if;

      Mmap := C.To_Pointer (To_Address (Get_Tag (Stivale_Tag, Memmap_ID)));

      for I in Mmap.Entries'Range loop
         exit when I > Cached_Info.Memmap'Length;

         Cached_Info.Memmap (I) := (
            Start  => To_Address (Integer_Address (Mmap.Entries (I).Base)),
            Length => Storage_Count (Mmap.Entries (I).Length),
            others => <>
         );

         case Mmap.Entries (I).EntryType is
            when Memmap_Entry_Usable =>
               Cached_Info.Memmap (I).MemType := Memory_Free;
            when Memmap_Entry_Kernel_And_Modules =>
               Cached_Info.Memmap (I).MemType := Memory_Kernel;
            when others =>
               Cached_Info.Memmap (I).MemType := Memory_Reserved;
         end case;

         Cached_Info.Memmap_Len := I;
      end loop;

      --  No module or cmdline support.
      Cached_Info.Cmdline_Len   := 0;
      Cached_Info.RAM_Files_Len := 0;

      Did_Cache := True;
      return Cached_Info;
   end Get_Info;
end Arch;
