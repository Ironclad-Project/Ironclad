--  devices-fb.adb: Boot-time memory framebuffer driver.
--  Copyright (C) 2023 streaksu
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with System; use System;
with Memory; use Memory;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.MMU;
with Arch.CPU;
with Arch.Multiboot2; use Arch.Multiboot2;
with Userland.Process;

package body Devices.FB with SPARK_Mode => Off is
   --  Structures used by fbdev.
   FB_TYPE_PACKED_PIXELS  : constant := 0;
   FB_VISUAL_TRUECOLOR    : constant := 2;
   FB_ACTIVATE_NOW        : constant := 0;
   FB_VMODE_NONINTERLACED : constant := 0;
   type FB_Bitfield is record
      Offset    : Unsigned_32;
      Length    : Unsigned_32;
      MSB_Right : Unsigned_32;
   end record;
   type FB_Var_ScreenInfo is record
      X_Res          : Unsigned_32;
      Y_Res          : Unsigned_32;
      X_Res_Virtual  : Unsigned_32;
      Y_Res_Virtual  : Unsigned_32;
      X_Offset       : Unsigned_32;
      Y_Offset       : Unsigned_32;
      Bits_Per_Pixel : Unsigned_32;
      Grayscale      : Unsigned_32;
      Red            : FB_Bitfield;
      Green          : FB_Bitfield;
      Blue           : FB_Bitfield;
      Transp         : FB_Bitfield;
      Non_Std        : Unsigned_32;
      Activate       : Unsigned_32;
      Height         : Unsigned_32;
      Width          : Unsigned_32;
      Accel_Flags    : Unsigned_32;
      PixClock       : Unsigned_32;
      Left_Margin    : Unsigned_32;
      Right_Margin   : Unsigned_32;
      Upper_Margin   : Unsigned_32;
      Lower_Margin   : Unsigned_32;
      HSync_Len      : Unsigned_32;
      VSync_Len      : Unsigned_32;
      Sync           : Unsigned_32;
      VMode          : Unsigned_32;
      Rotate         : Unsigned_32;
      Colorspace     : Unsigned_32;
   end record;
   type FB_Fix_ScreenInfo is record
      ID           : String (1 .. 16);
      SMem_Start   : Unsigned_64;
      SMem_Length  : Unsigned_32;
      FB_Type      : Unsigned_32;
      Type_Aux     : Unsigned_32;
      Visual       : Unsigned_32;
      X_Pan_Step   : Unsigned_16;
      Y_Pan_Step   : Unsigned_16;
      Y_Wrap_Step  : Unsigned_16;
      Line_Length  : Unsigned_32;
      MMIO_Start   : Unsigned_64;
      MMIO_Length  : Unsigned_32;
      Accel        : Unsigned_32;
      Capabilities : Unsigned_16;
   end record;

   --  Data for storing device data.
   type Internal_FB_Data is record
      Multiboot_Data : Framebuffer_Tag;
      Fixed_Info     : FB_Fix_ScreenInfo;
      Variable_Info  : FB_Var_ScreenInfo;
   end record;
   type Internal_FB_Data_Acc is access Internal_FB_Data;

   function Init return Boolean is
      Device  : Resource;
      Data    : constant Internal_FB_Data_Acc := new Internal_FB_Data;
      Fb      : constant Framebuffer_Tag      := Get_Framebuffer;
      Success : Boolean;
   begin
      --  Translate the multiboot information into fbdev info and register.
      Data.all :=
         (Multiboot_Data => Fb,
          Fixed_Info =>
            (ID           => "Multiboot 2" & (12 .. 16 => NUL),
             SMem_Start   => 0,
             SMem_Length  => Fb.Pitch * Fb.Height,
             FB_Type      => FB_TYPE_PACKED_PIXELS,
             Type_Aux     => 0,
             Visual       => FB_VISUAL_TRUECOLOR,
             X_Pan_Step   => 0,
             Y_Pan_Step   => 0,
             Y_Wrap_Step  => 0,
             Line_Length  => 0,
             MMIO_Start   => 0,
             MMIO_Length  => 0,
             Accel        => 0,
             Capabilities => 0),
         Variable_Info =>
            (X_Res          => Fb.Width,
             Y_Res          => Fb.Height,
             X_Res_Virtual  => Fb.Width,
             Y_Res_Virtual  => Fb.Height,
             Bits_Per_Pixel => Unsigned_32 (Fb.BPP),
             Activate       => FB_ACTIVATE_NOW,
             VMode          => FB_VMODE_NONINTERLACED,
             Width          => Unsigned_32'Last,
             Height         => Unsigned_32'Last,
             Red            => (16, 8, 0), --  TODO: These 4 are hardcoded.
             Green          => (08, 8, 0), --  They can be fetched from
             Blue           => (00, 8, 0), --  multiboot2 instead, which is
             Transp         => (24, 8, 0), --  a bit painful in all honesty.
             others         => 0));

      Device :=
         (Data        => Data.all'Address,
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Sync        => null,
          Sync_Range  => null,
          Read        => null,
          Write       => null,
          IO_Control  => IO_Control'Access,
          Mmap        => Mmap'Access,
          Poll        => null);

      Register (Device, "fb0", Success);
      return Success;
   end Init;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      --  fbdev ioctl requests.
      FBIOGET_VSCREENINFO : constant := 16#4600#;
      FBIOPUT_VSCREENINFO : constant := 16#4601#;
      FBIOGET_FSCREENINFO : constant := 16#4602#;

      Dev_Data : Internal_FB_Data  with Import, Address => Data;
      Var_Req  : FB_Var_ScreenInfo with Import, Address => Argument;
      Fix_Req  : FB_Fix_ScreenInfo with Import, Address => Argument;
   begin
      case Request is
         when FBIOGET_VSCREENINFO => Var_Req := Dev_Data.Variable_Info;
         when FBIOPUT_VSCREENINFO => Dev_Data.Variable_Info := Var_Req;
         when FBIOGET_FSCREENINFO => Fix_Req := Dev_Data.Fixed_Info;
         when others => return False;
      end case;
      return True;
   end IO_Control;

   function Mmap
      (Data    : System.Address;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions) return Boolean
   is
      Dev_Data : Internal_FB_Data with Import, Address => Data;
   begin
      return Arch.MMU.Map_Range
         (Map              => Userland.Process.Get_Common_Map
                              (Arch.CPU.Get_Local.Current_Process),
          Virtual_Start    => To_Address (Address),
          Physical_Start   => Dev_Data.Multiboot_Data.Address,
          Length           => Storage_Count (Length),
          Permissions      => Flags);
   end Mmap;
end Devices.FB;
