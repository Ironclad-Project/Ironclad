--  cryptography-random.ads: The random number generator of the kernel.
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

package Cryptography.Random is
   --  Fill a region in memory with random data.
   type Crypto_Data is array (Natural range <>) of Unsigned_32;
   procedure Fill_Data (Data : out Crypto_Data);

private

   function Get_Seed return Unsigned_128 with Global => null;
end Cryptography.Random;
