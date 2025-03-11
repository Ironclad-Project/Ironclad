--  arch-flanterm.adb: Flanterm management.
--  Copyright (C) 2025 streaksu
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
with Interfaces; use Interfaces;
with Devices.FB;

package body Arch.Flanterm is
   function FB_Init
      (Malloc            : System.Address;
       Free              : System.Address;
       Fb                : System.Address;
       Width             : Unsigned_64;
       Height            : Unsigned_64;
       Pitch             : Unsigned_64;
       Red_Mask_Size     : Unsigned_8;
       Red_Mask_Shift    : Unsigned_8;
       Green_Mask_Size   : Unsigned_8;
       Green_Mask_Shift  : Unsigned_8;
       Blue_Mask_Size    : Unsigned_8;
       Blue_Mask_Shift   : Unsigned_8;
       Canvas            : System.Address;
       ANSI_Colours      : System.Address;
       ANSI_Brights      : System.Address;
       Default_BG        : System.Address;
       Default_FG        : System.Address;
       Default_BG_Bright : System.Address;
       Default_FG_Bright : System.Address;
       Font              : System.Address;
       Font_Width        : Unsigned_64;
       Font_Height       : Unsigned_64;
       Font_Spacing      : Unsigned_64;
       Font_Scale_X      : Unsigned_64;
       Font_Scale_Y      : Unsigned_64;
       Margin            : Unsigned_64) return System.Address
      with Import, Convention => C, External_Name => "flanterm_fb_init";

   procedure Term_Write (Ctx, Buf : System.Address; Len : Unsigned_64)
      with Import, Convention => C, External_Name => "flanterm_write";

   Is_Enabled :        Boolean := False;
   Ctx        : System.Address := System.Null_Address;

   procedure Init is
      Addr : System.Address;
      Width, Height, Pitch         : Unsigned_64;
      RSz, RSh, GSz, GSh, BSz, BSh : Unsigned_8;
   begin
      Devices.FB.Get_Early_Framebuffer
         (Addr             => Addr,
          Width            => Width,
          Height           => Height,
          Pitch            => Pitch,
          Red_Mask_Size    => RSz,
          Red_Mask_Shift   => RSh,
          Green_Mask_Size  => GSz,
          Green_Mask_Shift => GSh,
          Blue_Mask_Size   => BSz,
          Blue_Mask_Shift  => BSh);
      if Addr = System.Null_Address then
         return;
      end if;

      Ctx := FB_Init
         (Malloc            => System.Null_Address,
          Free              => System.Null_Address,
          Fb                => Addr,
          Width             => Width,
          Height            => Height,
          Pitch             => Pitch,
          Red_Mask_Size     => RSz,
          Red_Mask_Shift    => RSh,
          Green_Mask_Size   => GSz,
          Green_Mask_Shift  => GSh,
          Blue_Mask_Size    => BSz,
          Blue_Mask_Shift   => BSh,
          Canvas            => System.Null_Address,
          ANSI_Colours      => System.Null_Address,
          ANSI_Brights      => System.Null_Address,
          Default_BG        => System.Null_Address,
          Default_FG        => System.Null_Address,
          Default_BG_Bright => System.Null_Address,
          Default_FG_Bright => System.Null_Address,
          Font              => System.Null_Address,
          Font_Width        => 0,
          Font_Height       => 0,
          Font_Spacing      => 1,
          Font_Scale_X      => 1,
          Font_Scale_Y      => 1,
          Margin            => 0);
      Is_Enabled := Ctx /= System.Null_Address;
   end Init;

   procedure Disable is
   begin
      Is_Enabled := False;
   end Disable;

   procedure Put (C : Character) is
   begin
      if not Is_Enabled then
         return;
      end if;

      Term_Write (Ctx, C'Address, 1);
   end Put;

   procedure Put (Line : String) is
   begin
      if not Is_Enabled then
         return;
      end if;

      Term_Write (Ctx, Line (Line'First)'Address, Line'Length);
   exception
      when Constraint_Error =>
         null;
   end Put;
end Arch.Flanterm;
