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
with Alignment;
with Devices;
with Userland.Memory_Locations;
with Userland.Syscall;
with Arch.MMU;

package body Userland.ELF is
   procedure Load_ELF
      (FS             : VFS.FS_Handle;
       Ino            : VFS.File_Inode_Number;
       Map            : Memory.MMU.Page_Table_Acc;
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
      Desired_ISA : ELF_ISA;
   begin
      Result :=
         (Was_Loaded  => False,
          Entrypoint  => System.Null_Address,
          Linker_Path => [others => ' '],
          Linker_Len  => 0,
          Vector =>
            (Entrypoint => 0,
             Program_Headers => 0,
             Program_Header_Count => 0,
             Program_Header_Size => 0),
         Exec_Stack => True);

      --  Read and check the header.
      VFS.Read (FS, Ino, Pos, Header_Data, Ret_Count, True, Success);
      Pos := Pos + Unsigned_64 (Ret_Count);
      if Success /= FS_Success or else Ret_Count /= Header_Bytes then
         return;
      end if;

      #if ArchName = """x86_64-limine""" then
         Desired_ISA := ELF_x86_64;
      #elsif ArchName = """riscv64-limine""" then
         Desired_ISA := ELF_RISCV;
      #end if;

      if Header.Magic_Number /= ELF_Signature or else
         Header.Bits         /= ELF_64bits    or else
         Header.Machine      /= Desired_ISA
      then
         return;
      end if;

      --  If we dont have a dynamic ELF, we cannot really use the kernel-passed
      --  ASLR offset, we have to use 0 instead and let the headers figure it
      --  out.
      if Header.ELF_Type /= ET_DYN then
         Base := 0;
      end if;

      --  Assign the data we already know.
      Result.Entrypoint := Header.Entrypoint + Storage_Offset (Base);
      Result.Vector.Entrypoint := Unsigned_64 (To_Integer (Result.Entrypoint));
      Result.Vector.Program_Header_Size  := Program_Header'Size / 8;
      Result.Vector.Program_Header_Count :=
         Unsigned_64 (Header.Program_Header_Count);

      --  Loop the program headers and either load them, or get info.
      declare
         Hdr_Cnt : constant Unsigned_16 := Header.Program_Header_Count;
         PHDRs : array (1 .. Hdr_Cnt) of Program_Header;
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
                  Get_Linker (FS, Ino, HDR, Result.Linker_Path,
                     Result.Linker_Len);
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
       Linker : out Linker_Str;
       Len    : out Natural)
   is
      pragma SPARK_Mode (Off);
      use VFS;

      Discard   : Unsigned_64;
      Ret       : String_Acc;
      Ret_Count : Natural;
      Discard2  : Boolean;
      Success   : FS_Status;
   begin
      if Header.File_Size_Bytes > Linker.Length then
         Len := 0;
         return;
      end if;

      declare
         Ret_Data : Devices.Operation_Data (1 .. Header.File_Size_Bytes)
            with Import, Address => Linker'Address;
      begin
         VFS.Read (FS, Ino, Header.Offset, Ret_Data, Ret_Count, True, Success);
      end;
      if Success = FS_Success and Ret_Count = Header.File_Size_Bytes then
         Len := Header.File_Size_Bytes;
      else
         Len := 0;
      end if;
   exception
      when Constraint_Error =>
         Len := 0;
   end Get_Linker;

   procedure Load_Header
      (FS      : VFS.FS_Handle;
       Ino     : VFS.File_Inode_Number;
       Header  : Program_Header;
       Map     : Memory.MMU.Page_Table_Acc;
       Base    : Unsigned_64;
       Success : out Boolean)
   is
      use VFS;

      package A is new Alignment (Integer_Address);

      Misalign, Load_Size : Unsigned_64;
      ELF_Virtual : Virtual_Address;
      Flags : constant Arch.MMU.Page_Permissions :=
         (Is_User_Accessible => True,
          Can_Read          => (Header.Flags and Flags_Read)       /= 0,
          Can_Write         => (Header.Flags and Flags_Write)      /= 0,
          Can_Execute       => (Header.Flags and Flags_Executable) /= 0,
          Is_Global         => False);
      Ret_Count    : Natural;
      FS_Suc       : FS_Status;
      Curr_Map     : System.Address;
      Ali_V, Ali_L : Integer_Address;
   begin
      ELF_Virtual := Virtual_Address (Base + Header.Virt_Address);
      Misalign    := Header.Virt_Address mod Memory.MMU.Page_Size;
      Load_Size   := Misalign + Header.Mem_Size_Bytes;
      Ali_V       := ELF_Virtual;
      Ali_L       := Integer_Address (Load_Size);

      if (Flags.Can_Execute and Flags.Can_Write) or
         Header.Alignment = 0                    or
         (Header.Alignment mod Memory.MMU.Page_Size) /= 0
      then
         Success := False;
         return;
      end if;

      --  If the base is 0, which means we are loading a static executable, we
      --  will disallow loading below the minimum offset, as a security measure
      --  for making sure NULL remains unmapped on load.
      A.Align_Memory_Range (Ali_V, Ali_L, Integer_Address (Header.Alignment));
      if (Base = 0) and (Ali_V < Memory_Locations.Min_Memory_Offset) then
         Success := False;
         return;
      end if;

      Syscall.Check_Userland_Mappability
         (Map, Ali_V, Unsigned_64 (Ali_L), Success);
      if not Success then
         return;
      end if;

      --  Set the stack map so we can access the allocated range.
      Curr_Map := Memory.MMU.Get_Curr_Table_Addr;
      Success  := Memory.MMU.Make_Active (Map);
      if not Success then
         return;
      end if;

      Memory.MMU.Map_Allocated_Range
         (Map           => Map,
          Virtual_Start => To_Address (Ali_V),
          Length        => Storage_Count (Ali_L),
          Success       => Success,
          Permissions   =>
            (Is_User_Accessible => False,
             Can_Read           => True,
             Can_Write          => True,
             Can_Execute        => False,
             Is_Global          => False));
      if Success then
         declare
            Load2 : Devices.Operation_Data (1 .. Header.File_Size_Bytes)
               with Import,
                    Address => To_Address (Ali_V) + Storage_Offset (Misalign);
         begin
            VFS.Read (FS, Ino, Header.Offset, Load2, Ret_Count, True, FS_Suc);
            Success := FS_Suc = FS_Success and
                       Ret_Count = Header.File_Size_Bytes;
         end;

         Memory.MMU.Remap_Range
            (Map           => Map,
             Virtual_Start => To_Address (Ali_V),
             Length        => Storage_Count (Ali_L),
             Permissions   => Flags,
             Success       => Success);
      end if;

      Memory.MMU.Set_Table_Addr (Curr_Map);
   exception
      when Constraint_Error =>
         Success := False;
   end Load_Header;
end Userland.ELF;
