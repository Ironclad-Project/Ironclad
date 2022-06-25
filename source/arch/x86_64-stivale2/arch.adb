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
with Lib;
with Interfaces; use Interfaces;

package body Arch is
   function Get_Info return Boot_Information is
      package ST renames Stivale2;
      package C1 is new System.Address_To_Access_Conversions (ST.Cmdline_Tag);
      package C2 is new System.Address_To_Access_Conversions (ST.Modules_Tag);
      package C3 is new System.Address_To_Access_Conversions (ST.Memmap_Tag);

      Cmdline : constant access ST.Cmdline_Tag :=
      C1.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Cmdline_ID)));
      Modules : constant access ST.Modules_Tag :=
      C2.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Modules_ID)));
      Memmap : constant access ST.Memmap_Tag :=
      C3.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Memmap_ID)));

      Cmd_Len : constant Natural := Lib.C_String_Length (Cmdline.Inner);
      Cmd_Str : String (1 .. Cmd_Len) with Address => Cmdline.Inner;

      Ret : Boot_Information;
   begin
      Ret.Cmdline (1 .. Cmd_Len) := Cmd_Str;
      Ret.Cmdline_Len            := Cmd_Len;

      Ret.Memmap_Len := 0;
      for I in Memmap.Entries'First .. Memmap.Entries'Last loop
         exit when I > Ret.Memmap'Length;
         Ret.Memmap (Ret.Memmap_Len + 1) := (
            Start   => To_Address (Integer_Address (Memmap.Entries (I).Base)),
            Length  => Storage_Count (Memmap.Entries (I).Length),
            Is_Free => Memmap.Entries (I).EntryType = ST.Memmap_Entry_Usable
         );
         Ret.Memmap_Len := Ret.Memmap_Len + 1;
      end loop;

      Ret.RAM_Files_Len := 0;
      for I in Modules.Entries'First .. Modules.Entries'Last loop
         exit when I > Ret.RAM_Files'Length;
         Ret.RAM_Files (Ret.RAM_Files_Len + 1) := (
            Start  => Modules.Entries (I).Begin_Address,
            Length => Storage_Count (
                        To_Integer (Modules.Entries (I).End_Address) -
                        To_Integer (Modules.Entries (I).Begin_Address)
                      )
         );
         Ret.RAM_Files_Len := Ret.RAM_Files_Len + 1;
      end loop;

      return Ret;
   end Get_Info;
end Arch;
