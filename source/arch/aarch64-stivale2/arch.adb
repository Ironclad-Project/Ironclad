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
with Arch.Stivale2;
with Arch.Entrypoint;
pragma Unreferenced (Arch.Entrypoint);

package body Arch with SPARK_Mode => Off is
   function Get_Info return Boot_Information is
      package ST renames Stivale2;
      package C is new System.Address_To_Access_Conversions (ST.Memmap_Tag);

      Memmap : constant access ST.Memmap_Tag :=
         C.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Memmap_ID)));

      Ret : Boot_Information;
   begin
      Ret.Memmap_Len := 0;
      for I in Memmap.Entries'First .. Memmap.Entries'Last loop
         exit when I > Ret.Memmap'Length;

         Ret.Memmap (Ret.Memmap_Len + 1) := (
            Start   => To_Address (Integer_Address (Memmap.Entries (I).Base)),
            Length  => Storage_Count (Memmap.Entries (I).Length),
            others  => <>
         );

         case Memmap.Entries (I).EntryType is
            when ST.Memmap_Entry_Usable |
                 ST.Memmap_Entry_Bootloader_Reclaimable =>
               Ret.Memmap (Ret.Memmap_Len + 1).MemType := Memory_Free;
            when ST.Memmap_Entry_Kernel_And_Modules =>
               Ret.Memmap (Ret.Memmap_Len + 1).MemType := Memory_Kernel;
            when others =>
               Ret.Memmap (Ret.Memmap_Len + 1).MemType := Memory_Reserved;
         end case;

         Ret.Memmap_Len := Ret.Memmap_Len + 1;
      end loop;

      --  No module or cmdline support.
      Ret.Cmdline_Len   := 0;
      Ret.RAM_Files_Len := 0;

      return Ret;
   end Get_Info;
end Arch;
