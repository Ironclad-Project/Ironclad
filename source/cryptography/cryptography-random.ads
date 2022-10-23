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

   --  Get random integers with optional ranges.
   function Get_Integer return Unsigned_64;

   --  FIXME: Fix this annotation.
   function Get_Integer (Min, Max : Unsigned_64) return Unsigned_64
      with Pre  => Max >= Min and Max <= Unsigned_64'Last - 1,
           Post => Min <= Get_Integer'Result and Get_Integer'Result <= Max;
   pragma Annotate (GNATprove, False_Positive, "postcondition might fail",
                    "Counterexample works, could be a gnatprove bug?");
private

   function Get_Seed return Unsigned_128 with Global => null;
end Cryptography.Random;
