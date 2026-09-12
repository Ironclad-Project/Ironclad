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

   Kernel_Address_Request : Request :=
      (ID       => Kernel_Address_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Linker_Section => ".limine_requests";

   --  Pieces of the ELF64 format needed to walk the loadable segments of the
   --  kernel executable. The file header only carries the fields that are
   --  consulted, with the representation clause skipping over the rest.
   ELF_Magic : constant Unsigned_32 := 16#464C457F#;
   PT_LOAD   : constant Unsigned_32 := 1;
   PF_X      : constant Unsigned_32 := 1;
   PF_W      : constant Unsigned_32 := 2;

   type ELF_Header is record
      Magic                : Unsigned_32;
      Program_Header_List  : Unsigned_64;
      Program_Header_Size  : Unsigned_16;
      Program_Header_Count : Unsigned_16;
   end record;
   for ELF_Header use record
      Magic                at  0 range 0 .. 31;
      Program_Header_List  at 32 range 0 .. 63;
      Program_Header_Size  at 54 range 0 .. 15;
      Program_Header_Count at 56 range 0 .. 15;
   end record;
   for ELF_Header'Size use 512;

   type Program_Header is record
      Segment_Type : Unsigned_32;
      Flags        : Unsigned_32;
      Offset       : Unsigned_64;
      Virt_Address : Unsigned_64;
      Phys_Address : Unsigned_64;
      File_Size    : Unsigned_64;
      Mem_Size     : Unsigned_64;
      Alignment    : Unsigned_64;
   end record;
   for Program_Header use record
      Segment_Type at  0 range 0 .. 31;
      Flags        at  4 range 0 .. 31;
      Offset       at  8 range 0 .. 63;
      Virt_Address at 16 range 0 .. 63;
      Phys_Address at 24 range 0 .. 63;
      File_Size    at 32 range 0 .. 63;
      Mem_Size     at 40 range 0 .. 63;
      Alignment    at 48 range 0 .. 63;
   end record;
   for Program_Header'Size use 448;

   type Program_Header_Arr is array (Natural range <>) of Program_Header;
   for Program_Header_Arr'Component_Size use 448;

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

   procedure Get_Kernel_Segments
      (Segments : out Boot_Kernel_Segments;
       Count    : out Natural;
       Success  : out Boolean)
   is
      AddrPonse : Kernel_Address_Response
         with Import, Address => Kernel_Address_Request.Response;
      FilePonse : Kernel_File_Response
         with Import, Address => Kernel_File_Request.Response;
   begin
      Segments :=
         [others =>
            (Physical_Start => System.Null_Address,
             Virtual_Start  => System.Null_Address,
             Length         => 0,
             Can_Write      => False,
             Can_Execute    => False)];
      Count   := 0;
      Success := False;

      if Kernel_Address_Request.Response = System.Null_Address or else
         Kernel_File_Request.Response = System.Null_Address
      then
         return;
      end if;

      declare
         --  The executable is loaded contiguously, so a single offset takes
         --  any of its virtual addresses to the matching physical one.
         Phys : constant Integer_Address := To_Integer (AddrPonse.Phys_Addr);
         Virt : constant Integer_Address := To_Integer (AddrPonse.Virt_Addr);
         File : constant System.Address  := FilePonse.Kernel_File.Address;
         Head : ELF_Header with Import, Address => File;
      begin
         if Head.Magic /= ELF_Magic or else
            Head.Program_Header_Size /= Program_Header'Size / 8
         then
            return;
         end if;

         declare
            PHDR_Count : constant Natural :=
               Natural (Head.Program_Header_Count);
            PHDRs : constant Program_Header_Arr (1 .. PHDR_Count)
               with Import, Address =>
                  File + Storage_Offset (Head.Program_Header_List);
         begin
            for PHDR of PHDRs loop
               if PHDR.Segment_Type = PT_LOAD and then PHDR.Mem_Size /= 0 then
                  if Count = Segments'Length then
                     Count := 0;
                     return;
                  end if;

                  Count := Count + 1;
                  Segments (Segments'First + Count - 1) :=
                     (Physical_Start => To_Address
                        (Integer_Address (PHDR.Virt_Address) - Virt + Phys),
                      Virtual_Start  => To_Address
                        (Integer_Address (PHDR.Virt_Address)),
                      Length         => Storage_Count (PHDR.Mem_Size),
                      Can_Write      => (PHDR.Flags and PF_W) /= 0,
                      Can_Execute    => (PHDR.Flags and PF_X) /= 0);
               end if;
            end loop;
         end;
      end;

      Success := Count /= 0;
   exception
      when Constraint_Error =>
         Count   := 0;
         Success := False;
   end Get_Kernel_Segments;
end Arch.Limine;
