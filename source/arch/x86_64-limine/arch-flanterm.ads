--  arch-flanterm.ads: Flanterm management.
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

package Arch.Flanterm is
   procedure Init;
   procedure Disable;
   procedure Put (C : Character);
   procedure Put (Line : String);

private

   subtype Flanterm_Ctx is System.Address;

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
       Margin            : Unsigned_64) return Flanterm_Ctx
      with Import, Convention => C, External_Name => "flanterm_fb_init";

   procedure Ctx_Deinit
      (Ctx  : Flanterm_Ctx;
       Free : System.Address)
      with Import, Convention => C, External_Name => "flanterm_fb_init";

   procedure Term_Write
      (Ctx : Flanterm_Ctx;
       Buf : System.Address;
       Len : Unsigned_64)
      with Import, Convention => C, External_Name => "flanterm_write";
end Arch.Flanterm;
