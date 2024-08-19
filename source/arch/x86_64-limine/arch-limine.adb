--  arch-limine.adb: Limine utilities.
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

with System; use System;
with Lib;
with Lib.Messages;

package body Arch.Limine is
   function Get_Physical_Address return System.Address is
      PhysPonse : Kernel_Address_Response
         with Import, Address => Address_Request.Response;
   begin
      return PhysPonse.Phys_Addr;
   end Get_Physical_Address;

   procedure Translate_Proto is
      CmdPonse : Kernel_File_Response
         with Import, Address => Kernel_File_Request.Response;
      Cmdline_Addr : constant System.Address := CmdPonse.Kernel_File.Cmdline;
      Cmdline_Len  : constant Natural := Lib.C_String_Length (Cmdline_Addr);
      Cmdline : String (1 .. Cmdline_Len) with Import, Address => Cmdline_Addr;

      InfoPonse : Bootloader_Info_Response
         with Import, Address => Bootloader_Info_Request.Response;
      Name_Addr : constant System.Address := InfoPonse.Name_Addr;
      Name_Len  : constant        Natural := Lib.C_String_Length (Name_Addr);
      Boot_Name : String (1 .. Name_Len) with Import, Address => Name_Addr;
      Vers_Addr : constant System.Address := InfoPonse.Version_Addr;
      Vers_Len  : constant        Natural := Lib.C_String_Length (Vers_Addr);
      Boot_Vers : String (1 .. Vers_Len) with Import, Address => Vers_Addr;

      MemPonse : Memmap_Response
         with Import, Address => Memmap_Request.Response;
      Inner_MMap : constant Memmap_Entry_Arr (1 .. MemPonse.Count)
         with Import, Address => MemPonse.Entries;
      Type_Entry : Boot_Memory_Type;
   begin
      Lib.Messages.Put_Line ("Booted by " & Boot_Name & " " & Boot_Vers);

      Global_Info.Cmdline (1 .. Cmdline_Len) := Cmdline;
      Global_Info.Cmdline_Len := Cmdline_Len;
      Global_Info.RAM_Files_Len := 0;

      for Ent of Inner_MMap loop
         if Ent.EntryType = LIMINE_MEMMAP_USABLE then
            Type_Entry := Memory_Free;
         elsif Ent.EntryType = LIMINE_MEMMAP_KERNEL_AND_MODS then
            Type_Entry := Memory_Kernel;
         else
            Type_Entry := Memory_Reserved;
         end if;

         Global_Info.Memmap_Len := Global_Info.Memmap_Len + 1;
         Global_Info.Memmap (Global_Info.Memmap_Len) :=
            (Start   => To_Address (Integer_Address (Ent.Base)),
             Length  => Storage_Count (Ent.Length),
             MemType => Type_Entry);
      end loop;

      for I in 1 .. Global_Info.Memmap_Len loop
         if Global_Info.Memmap (I).MemType = Memory_Free            and
            To_Integer (Global_Info.Memmap (I).Start) <= 16#100000# and
            Global_Info.Memmap (I).Length > Max_Sub_1MiB_Size
         then
            Sub_1MiB_Region := Global_Info.Memmap (I).Start;
            Global_Info.Memmap (I).Start  :=
               Global_Info.Memmap (I).Start + Max_Sub_1MiB_Size;
            Global_Info.Memmap (I).Length :=
               Global_Info.Memmap (I).Length - Max_Sub_1MiB_Size;
         end if;
      end loop;
   end Translate_Proto;
end Arch.Limine;
