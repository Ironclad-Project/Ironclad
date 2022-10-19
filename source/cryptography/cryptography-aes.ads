--  cryptography-aes.ads: Hardware-accelerated AES implementation.
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
with Arch.Snippets;

package Cryptography.AES is
   --  Implementation of AES using hardware acceleration.
   --  Only available when Arch.Snippets.Supports_AES_Accel is True.

   --  AES manages 128 bit blocks.
   type AES_Data is array (Natural range <>) of Unsigned_128;

   --  Encrypt and decrypt a block with a key using AES-128 and ECB.
   procedure Encrypt_ECB (Key : Unsigned_128; Data : in out AES_Data);
   procedure Decrypt_ECB (Key : Unsigned_128; Data : in out AES_Data);

private

   function Encrypt_128
      (Data, Original_Key : Unsigned_128;
       Key : Arch.Snippets.Expanded_AES_Key) return Unsigned_128;
   function Decrypt_128
      (Data, Original_Key : Unsigned_128;
       Key : Arch.Snippets.Expanded_AES_Key) return Unsigned_128;
end Cryptography.AES;
