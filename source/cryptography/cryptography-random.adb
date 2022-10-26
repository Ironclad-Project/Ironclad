--  cryptography-random.adb: The random number generator of the kernel.
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

with Ada.Unchecked_Conversion;
with Memory.Physical; use Memory.Physical;
with Arch.Snippets;
with Memory; use Memory;
with Cryptography.Chacha20;

package body Cryptography.Random is
   procedure Fill_Data (Data : out Crypto_Data) is
      type Seed is record
         Seed1 : Unsigned_128;
         Seed2 : Unsigned_128;
      end record with Object_Size => 256;
      function To_Seed is new Ada.Unchecked_Conversion (Seed, Chacha20.Key);

      --  Seed every time a new chain of random numbers is requested.
      S : constant Seed := (
         Seed1 => Get_Seed,
         Seed2 => Get_Seed
      );

      Cha_Block   : Chacha20.Block := (others => 0);
      Index       : Natural        := Cha_Block'Last + 1;
      Block_Index : Unsigned_64    := 0;
   begin
      --  The core of our RNG is Chacha20-based. We just create keys with a
      --  random key and incremental block ID a-la CTR mode and use them as
      --  random data. For some increased security once could XOR
      --  low quality disposable entropy.
      for Value of Data loop
         if Index > Cha_Block'Last then
            Cha_Block   := Chacha20.Gen_Key (To_Seed (S), 1, Block_Index);
            Index       := Cha_Block'First;
            Block_Index := Block_Index + 1;
         end if;

         Value := Cha_Block (Index);
         Index := Index + 1;
      end loop;
   end Fill_Data;

   function Get_Integer return Unsigned_64 is
      Data : Crypto_Data (1 .. 2);
   begin
      Fill_Data (Data);
      return Shift_Left (Unsigned_64 (Data (1)), 31) or Unsigned_64 (Data (2));
   end Get_Integer;

   function Get_Integer (Min, Max : Unsigned_64) return Unsigned_64 is
   begin
      return (Get_Integer mod (Max + 1 - Min)) + Min;
   end Get_Integer;

   --  TODO: More entropy, please.
   function Get_Seed return Unsigned_128 is
      pragma SPARK_Mode (Off);

      --  Seeds for mixing.
      S1 : constant Unsigned_64 := Arch.Snippets.Read_Cycles;
      S2 : constant Unsigned_64 := Unsigned_64 (Get_Statistics.Used_Memory);

      --  Hash them, ideally this should be MD5 or something stronger.
      --  This is totally homegrown and loosely based on boost::hash_combine.
      M1 : constant Unsigned_64 := S2 xor (S1 + 16#9e3779b9# + S2);
      M2 : constant Unsigned_64 := M1 xor (S1 + Shift_Left (S2, 6)) xor S2;
   begin
      return Shift_Left (Unsigned_128 (M1), 64) or Unsigned_128 (M2);
   end Get_Seed;
end Cryptography.Random;
