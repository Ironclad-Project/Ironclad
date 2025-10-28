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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Messages;
with Panic;

package body Arch.Limine is
   Base_Request : Limine.Base_Revision :=
      (ID_1     => 16#f9562b2d5c95a6c8#,
       ID_2     => 16#6a7b384944536bdc#,
       Revision => 4)
      with Export;

   Bootloader_Info_Request : Request :=
      (ID       => Bootloader_Info_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export;

   Kernel_File_Request : Request :=
      (ID       => Kernel_File_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export;

   procedure Translate_Proto is
      InfoPonse : Bootloader_Info_Response
         with Import, Address => Bootloader_Info_Request.Response;
      Name_Addr : constant System.Address := InfoPonse.Name_Addr;
      Name_Len  : constant Natural := Strlen (Name_Addr);
      Boot_Name : String (1 .. Name_Len) with Import, Address => Name_Addr;
      Vers_Addr : constant System.Address := InfoPonse.Version_Addr;
      Vers_Len  : constant Natural := Strlen (Vers_Addr);
      Boot_Vers : String (1 .. Vers_Len) with Import, Address => Vers_Addr;
      Revision  : constant Unsigned_64 := Base_Request.Revision;
   begin
      Messages.Put_Line ("Booted by " & Boot_Name & " " & Boot_Vers);
      if Revision /= 0 then
         Panic.Hard_Panic ("Revision unsupported");
      end if;

      declare
         CmdPonse : Kernel_File_Response
            with Import, Address => Kernel_File_Request.Response;
         Cmdline_Addr : constant System.Address :=
            CmdPonse.Kernel_File.Cmdline;
         Cmdline_Len : constant Natural := Strlen (Cmdline_Addr);
         Cmdline : String (1 .. Cmdline_Len)
            with Import, Address => Cmdline_Addr;
      begin
         Arch.Cmdline_Len := Cmdline_Len;
         Arch.Cmdline (1 .. Cmdline_Len) := Cmdline;
      end;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception encountered translating limine");
   end Translate_Proto;
end Arch.Limine;
