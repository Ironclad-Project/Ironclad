--  cryptography-chacha20.ads: Chacha20 implementation.
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

package Cryptography.Chacha20 is
   --  Implementation of Chacha20 per the book.
   --  Source: https://en.wikipedia.org/wiki/Salsa20

   --  Chacha20 generates keys operating on a user key, nonce, and block
   --  number. This key, which is in the format of a 512-bit block, is then
   --  XORd with the message for a successful encrytion.
   --  For decrypting, the same key must be XORd again.

   type Block is array (0 .. 15) of Unsigned_32;
   type Key is record
      Value1, Value2, Value3, Value4 : Unsigned_32;
      Value5, Value6, Value7, Value8 : Unsigned_32;
   end record with Size => 256;

   --  Get a generated key for a combination of key, nonce, and block number.
   function Gen_Key (K : Key; Nonce, Block_ID : Unsigned_64) return Block;

private

   procedure Quarter_Round (A, B, C, D : in out Unsigned_32);
end Cryptography.Chacha20;
