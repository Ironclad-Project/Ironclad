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

package body Cryptography.Random is
   procedure Fill_Data (Data : out Crypto_Data) is
      --  We will reseed at the start of every request, no matter the size, and
      --  when reaching this limit.
      Reseed_Limit : constant := 1024;

      type Seed is record
         Seed1 : Unsigned_32;
         Seed2 : Unsigned_32;
         Seed3 : Unsigned_32;
         Seed4 : Unsigned_32;
      end record with Object_Size => 128;
      function To_Seed is new Ada.Unchecked_Conversion (Unsigned_128, Seed);

      S     : Seed;
      Index : Unsigned_64 := 0;
      Inter : Unsigned_32;
   begin
      S := To_Seed (Get_Seed);
      for Val of Data loop
         --  Mix our seeds using LFSR113.
         Inter := Shift_Right (Shift_Left (S.Seed1, 6) xor S.Seed1, 13);
         S.Seed1 := Shift_Left (S.Seed1 and 16#FFFFFFFE#, 18) xor Inter;
         Inter := Shift_Right (Shift_Left (S.Seed2, 2) xor S.Seed2, 27);
         S.Seed2 := Shift_Left (S.Seed2 and 16#FFFFFFFE#, 2) xor Inter;
         Inter := Shift_Right (Shift_Left (S.Seed3, 13) xor S.Seed3, 21);
         S.Seed3 := Shift_Left (S.Seed3 and 16#FFFFFFFE#, 7) xor Inter;
         Inter := Shift_Right (Shift_Left (S.Seed4, 3) xor S.Seed4, 12);
         S.Seed4 := Shift_Left (S.Seed4 and 16#FFFFFFFE#, 13) xor Inter;
         Val := S.Seed1 xor S.Seed2 xor S.Seed3 xor S.Seed4;

         Index := Index + 1;
         if Index >= Reseed_Limit then
            S := To_Seed (Get_Seed);
            Index := 0;
         end if;
      end loop;
   end Fill_Data;

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
      return Shift_Left (Unsigned_128 (M1), 63) or Unsigned_128 (M2);
   end Get_Seed;
end Cryptography.Random;
