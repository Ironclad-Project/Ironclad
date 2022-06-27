--  arch-dtb.adb: Linux's DTB parser.
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

with System.Address_To_Access_Conversions;

package body Arch.DTB is
   function Is_Valid (Hdr : Header_Acc) return Boolean is
   begin
      --  We implement version 17, which says to be last compatible with 16 but
      --  no earlier.
      return Hdr.Magic = Magic_Value and Hdr.Last_Compatible_Version = 16;
   end Is_Valid;

   function Get_Memory_Reservation (Hdr : Header_Acc) return System.Address is
      package C is new System.Address_To_Access_Conversions (Header);

      Converted_Ptr : constant C.Object_Pointer := C.Object_Pointer (Hdr);
      Start_Address : constant System.Address := C.To_Address (Converted_Ptr);
   begin
      return Start_Address + Storage_Count (Hdr.Memory_Reservation_Offset);
   end Get_Memory_Reservation;
end Arch.DTB;
