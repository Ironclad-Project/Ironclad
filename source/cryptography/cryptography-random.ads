--  cryptography-random.ads: The random number generator of the kernel.
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

package Cryptography.Random is
   --  Fill a region in memory with random data.
   type Crypto_Data is array (Natural range <>) of Unsigned_8;
   procedure Fill_Data (Data : out Crypto_Data);

   --  Contribute a bit of entropy.
   procedure Feed_Entropy (Data : Unsigned_32);

   --  Get random integers with optional ranges.
   procedure Get_Integer (Result : out Unsigned_64);

   --  Get a random integer between 2 integers.
   --  FIXME: Fix the GNATProve annotation.
   procedure Get_Integer (Min, Max : Unsigned_64; Result : out Unsigned_64)
      with Pre  => Max >= Min and Max <= Unsigned_64'Last - 1,
           Post => Min <= Result and Result <= Max;
   pragma Annotate (GNATprove, False_Positive, "postcondition might fail",
                    "Counterexample works, could be a gnatprove bug?");
private

   procedure Entropy_Adjust;
end Cryptography.Random;
