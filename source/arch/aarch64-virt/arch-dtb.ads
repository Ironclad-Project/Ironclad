--  arch-dtb.ads: Linux's DTB parser.
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

with System;
with Interfaces; use Interfaces;

package Arch.DTB is
   Magic_Value : constant := 16#D00DFEED#;
   type Header is record
      Magic                     : Unsigned_32;
      Total_Size                : Unsigned_32;
      Structure_Block_Offset    : Unsigned_32;
      Strings_Block_Offset      : Unsigned_32;
      Memory_Reservation_Offset : Unsigned_32;
      Version                   : Unsigned_32;
      Last_Compatible_Version   : Unsigned_32;
      Physical_ID_Boot_CPU      : Unsigned_32;
      Size_Structure_Block      : Unsigned_32;
      Size_Strings_Block        : Unsigned_32;
   end record;
   for Header use record
      Magic                     at  0 range 0 .. 31;
      Total_Size                at  4 range 0 .. 31;
      Structure_Block_Offset    at  8 range 0 .. 31;
      Strings_Block_Offset      at 12 range 0 .. 31;
      Memory_Reservation_Offset at 16 range 0 .. 31;
      Version                   at 20 range 0 .. 31;
      Last_Compatible_Version   at 24 range 0 .. 31;
      Physical_ID_Boot_CPU      at 28 range 0 .. 31;
      Size_Structure_Block      at 32 range 0 .. 31;
      Size_Strings_Block        at 36 range 0 .. 31;
   end record;
   for Header'Bit_Order            use System.High_Order_First;
   for Header'Scalar_Storage_Order use System.High_Order_First;

   --  Sometimes the pointer is null, we tell Ada it will never be null to
   --  avoid the checks.
   type Header_Acc is not null access Header;

   --  Validate whether a header is valid for the parser.
   function Is_Valid (Hdr : Header_Acc) return Boolean;

   --  The memory reservation block is a list of areas of physical memory
   --  reserved for the hardware. Its a list of this entries followed by an
   --  entry zero'd out.
   type Reservation is record
      Address : Unsigned_64;
      Size    : Unsigned_64;
   end record;
   type Memory_Reservation_Block is array (Natural range <>) of Reservation;
   function Get_Memory_Reservation (Hdr : Header_Acc) return System.Address;
end Arch.DTB;
