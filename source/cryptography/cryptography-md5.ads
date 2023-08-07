--  cryptography-md5.ads: MD5 digest.
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

with Interfaces; use Interfaces;

package Cryptography.MD5 is
   --  MD5 manages 512-bit blocks.
   type MD5_Hash   is array (1 .. 4) of Unsigned_32;
   type MD5_Block  is array (Unsigned_32 range 0 .. 15) of Unsigned_32;
   type MD5_Blocks is array (Natural range <>) of MD5_Block;

   --  Digest a string of binary data into a 128-bit hash.
   --  Data is taken already padded according to what MD5 mandates.
   function Digest (Data : MD5_Blocks) return MD5_Hash;

   subtype MD5_String is String (1 .. 32);
   function To_String (Hash : MD5_Hash) return MD5_String;

private

   function BSwap32 (V : Unsigned_32) return Unsigned_32;
   pragma Import (Intrinsic, BSwap32, "__builtin_bswap32");
end Cryptography.MD5;
