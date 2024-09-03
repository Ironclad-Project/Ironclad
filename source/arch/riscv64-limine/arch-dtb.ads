--  arch-dtb.ads: Device-tree blob parsing.
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

with Interfaces; use Interfaces;
with Arch.Limine;

package Arch.DTB with SPARK_Mode => Off is
   function Init return Boolean;

private

   DTB_Request : Arch.Limine.Request :=
      (ID       => Arch.Limine.DTB_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  It is DOODFEED, but that is big endian, this is little endian.
   FDT_Magic : constant := 16#EDFE0DD0#;

   type FDT_Header is record
      Magic                : Unsigned_32;
      Size                 : Unsigned_32;
      Offset_DT_Struct     : Unsigned_32;
      Offset_DT_Strings    : Unsigned_32;
      Offset_Reserved_Flag : Unsigned_32;
      Version              : Unsigned_32;
      Last_Compatible_Vers : Unsigned_32;
      Boot_CPU_Physical_ID : Unsigned_32;
      Size_DT_Stings       : Unsigned_32;
      Size_DT_Struct       : Unsigned_32;
   end record with Pack;
end Arch.DTB;
