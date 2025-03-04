--  userland-elf.adb: ELF loading.
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

with System.Storage_Elements; use System.Storage_Elements;
with Memory; use Memory;
with Lib.Alignment;
with Devices;
with Userland.Syscall;

package body Userland.ELF is
   procedure Load_ELF
      (FS             : VFS.FS_Handle;
       Ino            : VFS.File_Inode_Number;
       Map            : Arch.MMU.Page_Table_Acc;
       Requested_Base : Unsigned_64;
       Result         : out Parsed_ELF)
   is
      use VFS;

      Base         : Unsigned_64 := Requested_Base;
      Header       : ELF_Header;
      Header_Bytes : constant Natural := ELF_Header'Size / 8;
      Header_Data  : Devices.Operation_Data (1 .. Header_Bytes)
         with Import, Address => Header'Address;

      Ret_Count : Natural;
      Pos       : Unsigned_64 := 0;
      Success   : FS_Status;
      Success2  : Boolean;
   begin
      Result :=
         (Was_Loaded  => False,
          Entrypoint  => System.Null_Address,
          Linker_Path => null,
          Vector =>
            (Entrypoint => 0,
             Program_Headers => 0,
             Program_Header_Count => 0,
             Program_Header_Size => 0),
         Exec_Stack => True);

      --  Read and check the header.
      VFS.Read (FS, Ino, Pos, Header_Data, Ret_Count, True, Success);
      Pos := Pos + Unsigned_64 (Ret_Count);
      if Success /= FS_Success or Ret_Count /= Header_Bytes or
         Header.Identifier (1 .. 4) /= ELF_Signature
      then
         return;
      end if;

      --  If we have a dynamic ELF, we may run into the issue of it specifying
      --  to load itself at 0, which can result on odd behaviour on
      --  NULL-dereferences. For ease of debugging and predictable behaviour,
      --  we will add a small 1 page slide. Since they are dynamic to begin
      --  with, it wont hurt.
      if Header.ELF_Type = ET_DYN and Base = 0 then
         Base := Arch.MMU.Page_Size;
      end if;

      --  Assign the data we already know.
      Result.Entrypoint := Header.Entrypoint + Storage_Offset (Base);
      Result.Vector.Entrypoint := Unsigned_64 (To_Integer (Result.Entrypoint));
      Result.Vector.Program_Header_Size  := Program_Header'Size / 8;
      Result.Vector.Program_Header_Count :=
         Unsigned_64 (Header.Program_Header_Count);

      --  Loop the program headers and either load them, or get info.
      declare
         PHDRs : array (1 .. Header.Program_Header_Count) of Program_Header;
         HSize : constant Unsigned_64 :=
            Unsigned_64 (Header.Program_Header_Size);
         RSize : constant Unsigned_64 := HSize * PHDRs'Length;
         PHDRs_Data : Devices.Operation_Data (1 .. Natural (RSize))
            with Import, Address => PHDRs'Address;
      begin
         if HSize = 0 or PHDRs'Length = 0 then
            return;
         end if;

         Pos := Header.Program_Header_List;
         VFS.Read (FS, Ino, Pos, PHDRs_Data, Ret_Count, True, Success);
         Pos := Pos + Unsigned_64 (Ret_Count);
         if Success /= FS_Success or Ret_Count /= Natural (RSize) then
            return;
         end if;

         for HDR of PHDRs loop
            case HDR.Segment_Type is
               when Program_Loadable_Segment =>
                  Load_Header (FS, Ino, HDR, Map, Base, Success2);
                  if not Success2 then
                     return;
                  end if;
               when Program_Header_Table_Segment =>
                  Result.Vector.Program_Headers := Base + HDR.Virt_Address;
               when Program_Interpreter_Segment =>
                  Get_Linker (FS, Ino, HDR, Result.Linker_Path);
               when Program_GNU_Stack =>
                  Result.Exec_Stack := (HDR.Flags and Flags_Executable) /= 0;
               when others =>
                  null;
            end case;
         end loop;

         --  Return success.
         Result.Was_Loaded := True;
      end;
   exception
      when Constraint_Error =>
         Result.Was_Loaded := False;
   end Load_ELF;
   ----------------------------------------------------------------------------
   procedure Get_Linker
      (FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Header : Program_Header;
       Linker : out String_Acc)
   is
      use VFS;

      Discard   : Unsigned_64;
      Ret       : String_Acc;
      Ret_Count : Natural;
      Discard2  : Boolean;
      Success   : FS_Status;
   begin
      Ret := new String'[1 .. Header.File_Size_Bytes => ' '];
      declare
         Ret_Data : Devices.Operation_Data (1 .. Header.File_Size_Bytes)
            with Import, Address => Ret (1)'Address;
      begin
         VFS.Read (FS, Ino, Header.Offset, Ret_Data, Ret_Count, True, Success);
      end;
      if Success = FS_Success and Ret_Count = Header.File_Size_Bytes then
         Linker := Ret;
      else
         Linker := null;
      end if;
   exception
      when Constraint_Error =>
         Linker := null;
   end Get_Linker;

   procedure Load_Header
      (FS      : VFS.FS_Handle;
       Ino     : VFS.File_Inode_Number;
       Header  : Program_Header;
       Map     : Arch.MMU.Page_Table_Acc;
       Base    : Unsigned_64;
       Success : out Boolean)
   is
      use VFS;

      package A is new Lib.Alignment (Integer_Address);

      MisAlign, Load_Size : Unsigned_64;
      ELF_Virtual : Virtual_Address;
      Flags : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accesible => True,
          Can_Read          => (Header.Flags and Flags_Read)       /= 0,
          Can_Write         => (Header.Flags and Flags_Write)      /= 0,
          Can_Execute       => (Header.Flags and Flags_Executable) /= 0,
          Is_Global         => False);
      Ret_Count  : Natural;
      Success2   : FS_Status;
      Result     : System.Address;
      Ali_V, Ali_L : Integer_Address;
   begin
      ELF_Virtual := Virtual_Address (Base + Header.Virt_Address);
      MisAlign    :=  Header.Virt_Address mod Arch.MMU.Page_Size;
      Load_Size   := MisAlign + Header.Mem_Size_Bytes;
      Ali_V       := ELF_Virtual;
      Ali_L       := Integer_Address (Load_Size);

      if (Flags.Can_Execute and Flags.Can_Write) or
         Header.Alignment = 0                    or
         (Header.Alignment mod Arch.MMU.Page_Size) /= 0
      then
         Success := False;
         return;
      end if;

      A.Align_Memory_Range (Ali_V, Ali_L, Integer_Address (Header.Alignment));
      Syscall.Check_Userland_Mappability
         (Map, Ali_V, Unsigned_64 (Ali_L), Success);
      if not Success then
         return;
      end if;

      Arch.MMU.Map_Allocated_Range
         (Map            => Map,
          Virtual_Start  => To_Address (Ali_V),
          Length         => Storage_Count (Ali_L),
          Permissions    => Flags,
          Physical_Start => Result,
          Success        => Success);
      if not Success then
         return;
      end if;

      declare
         Load2 : Devices.Operation_Data (1 .. Header.File_Size_Bytes)
            with Import, Address => Result + Storage_Offset (MisAlign);
      begin
         VFS.Read (FS, Ino, Header.Offset, Load2, Ret_Count, True, Success2);
         Success := Success2 = FS_Success and
                    Ret_Count = Header.File_Size_Bytes;
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Load_Header;
end Userland.ELF;
