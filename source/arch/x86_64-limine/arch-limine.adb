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
   --  Delimiters of the request section, so that the bootloader does not have
   --  to scan the whole executable in search of requests.
   type Start_Marker is array (1 .. 4) of Unsigned_64;
   type End_Marker   is array (1 .. 2) of Unsigned_64;

   Requests_Start : constant Start_Marker :=
      [16#f6b8f4b39de7d1ae#, 16#fab91a6940fcb9cf#,
       16#785c6ed015d3e316#, 16#181e920a7852b9d9#]
      with Linker_Section => ".limine_requests_start";
   pragma Machine_Attribute (Requests_Start, "used");

   Requests_End : constant End_Marker :=
      [16#adc0e0531bb10d03#, 16#9572709f31764c62#]
      with Linker_Section => ".limine_requests_end";
   pragma Machine_Attribute (Requests_End, "used");

   Base_Request : Limine.Base_Revision :=
      (ID_1     => 16#f9562b2d5c95a6c8#,
       ID_2     => 16#6a7b384944536bdc#,
       Revision => 6)
      with Export, Linker_Section => ".limine_requests";

   Bootloader_Info_Request : Request :=
      (ID       => Bootloader_Info_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Linker_Section => ".limine_requests";

   Kernel_File_Request : Request :=
      (ID       => Kernel_File_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Linker_Section => ".limine_requests";

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
