--  devices-bootmfb.adb: Boot-time memory framebuffer driver.
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

with System; use System;
with VFS.Device;
with VFS;
with Memory; use Memory;
with Memory.Virtual;
with System.Storage_Elements; use System.Storage_Elements;

package body Devices.BootFB is
   type FB_Arr is array (Unsigned_32 range <>) of Unsigned_32;

   function Init (Fb : access Arch.Stivale2.Framebuffer_Tag) return Boolean is
      Dev  : VFS.Device.Device_Data;
      Addr : constant Integer_Address := To_Integer (Fb.Address);
      Fb_Flags : constant Memory.Virtual.Page_Flags := (
         Present         => True,
         Read_Write      => True,
         User_Supervisor => False,
         Write_Through   => True,
         Cache_Disable   => False,
         Accessed        => False,
         Dirty           => False,
         PAT             => True,
         Global          => True
      );
   begin
      --  Map the framebuffer write-combining in the higher-half
      --  (also in the lower half because stivale2 might use it).
      --  We only have to map it in the kernel map lower half because we
      --  always switch to it when calling the terminal (PERFORMANCE died).
      Memory.Virtual.Remap_Range (
         Map         => Memory.Virtual.Kernel_Map,
         Virtual     => Memory.Virtual_Address (Addr),
         Length      => Unsigned_64 (Fb.Height) * 4 * Unsigned_64 (Fb.Pitch),
         Flags       => Fb_Flags,
         Not_Execute => False
      );
      Memory.Virtual.Remap_Range (
         Map         => Memory.Virtual.Kernel_Map,
         Virtual     => Memory.Virtual_Address (Addr - Memory.Memory_Offset),
         Length      => Unsigned_64 (Fb.Height) * 4 * Unsigned_64 (Fb.Pitch),
         Flags       => Fb_Flags,
         Not_Execute => False
      );

      --  Register the device.
      Dev.Name (1 .. 6)       := "bootfb";
      Dev.Name_Len            := 6;
      Dev.Data                := Fb.all'Address;
      Dev.Stat.Type_Of_File   := VFS.File_Character_Device;
      Dev.Stat.Mode           := 8#660#;
      Dev.Stat.Byte_Size      := 0;
      Dev.Stat.IO_Block_Size  := 4096;
      Dev.Stat.IO_Block_Count := 0;
      Dev.Read                := Read'Access;
      Dev.Write               := Write'Access;
      Dev.IO_Control          := IO_Control'Access;
      return VFS.Device.Register (Dev);
   end Init;

   function Read
      (Data    : System.Address;
       Offset  : Unsigned_64;
       Count   : Unsigned_64;
       To_Read : System.Address) return Unsigned_64
   is
      Dev_Data : Arch.Stivale2.Framebuffer_Tag with Address => Data;
      Dev_Y    : constant Unsigned_32     := Unsigned_32 (Dev_Data.Height);
      Dev_X    : constant Unsigned_32     := Unsigned_32 (Dev_Data.Pitch) / 4;
      Offset2  : constant Unsigned_32     := Unsigned_32 (Offset);
      Count2   : constant Unsigned_32     := Unsigned_32 (Count);

      Read_Data : FB_Arr (1 .. Count2)        with Address => To_Read;
      Window    : FB_Arr (1 .. Dev_X * Dev_Y) with Address => Dev_Data.Address;
   begin
      Read_Data := Window (Offset2 + 1 .. Offset2 + Count2);
      return Count;
   end Read;

   function Write
      (Data     : System.Address;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      Dev_Data : Arch.Stivale2.Framebuffer_Tag with Address => Data;
      Dev_Y    : constant Unsigned_32     := Unsigned_32 (Dev_Data.Height);
      Dev_X    : constant Unsigned_32     := Unsigned_32 (Dev_Data.Pitch) / 4;
      Offset2  : constant Unsigned_32     := Unsigned_32 (Offset);
      Count2   : constant Unsigned_32     := Unsigned_32 (Count);

      Write_Data : FB_Arr (1 .. Count2)       with Address => To_Write;
      Window    : FB_Arr (1 .. Dev_X * Dev_Y) with Address => Dev_Data.Address;
   begin
      Window (Offset2 + 1 .. Offset2 + Count2) := Write_Data;
      return Count;
   end Write;

   IO_Control_Report_Dimensions : constant := 1;
   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      type Dimensions is record
         Width            : Unsigned_16;
         Height           : Unsigned_16;
         Pitch            : Unsigned_16;
         BPP              : Unsigned_16;
         Red_Mask_Size    : Unsigned_8;
         Red_Mask_Shift   : Unsigned_8;
         Green_Mask_Size  : Unsigned_8;
         Green_Mask_Shift : Unsigned_8;
         Blue_Mask_Size   : Unsigned_8;
         Blue_Mask_Shift  : Unsigned_8;
      end record;

      Dev_Data : Arch.Stivale2.Framebuffer_Tag with Address => Data;
   begin
      case Request is
         when IO_Control_Report_Dimensions =>
            declare
               Requested_Data : Dimensions with Address => Argument;
            begin
               Requested_Data := (
                  Width            => Dev_Data.Width,
                  Height           => Dev_Data.Height,
                  Pitch            => Dev_Data.Pitch,
                  BPP              => Dev_Data.BPP,
                  Red_Mask_Size    => Dev_Data.Red_Mask_Size,
                  Red_Mask_Shift   => Dev_Data.Red_Mask_Shift,
                  Green_Mask_Size  => Dev_Data.Green_Mask_Size,
                  Green_Mask_Shift => Dev_Data.Green_Mask_Shift,
                  Blue_Mask_Size   => Dev_Data.Blue_Mask_Size,
                  Blue_Mask_Shift  => Dev_Data.Blue_Mask_Shift
               );
               return True;
            end;
         when others =>
            return False;
      end case;
   end IO_Control;
end Devices.BootFB;
