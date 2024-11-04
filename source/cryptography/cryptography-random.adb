--  cryptography-random.adb: The random number generator of the kernel.
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

with Ada.Unchecked_Conversion;
with Memory.Physical; use Memory.Physical;
with Arch.Clocks;
with Memory; use Memory;
with Cryptography.Chacha20;
with Cryptography.MD5;
with Lib.Synchronization; use Lib.Synchronization;

package body Cryptography.Random is
   --  We maintain a pool of entropy that we shift and shuffle with new data
   --  every time data is requested to be given. Then, we hash the data with
   --  MD5 (but any cryptographic hash would work), and using it to seed a
   --  chacha20 keystream.

   pragma Suppress (All_Checks); --  Unit passes GNATprove AoRTE.

   Accumulator_Mutex   : aliased Binary_Semaphore := Unlocked_Semaphore;
   Entropy_Accumulator : MD5.MD5_Blocks (1 .. 1) := (1 => (others => 16#33#));

   procedure Fill_Data (Data : out Crypto_Data) is
      type Seed is record
         Entropy_Hash : MD5.MD5_Hash;
         Mono_Sec     : Unsigned_64;
         Mono_NSec    : Unsigned_64;
      end record with Size => 256;
      function To_Seed is new Ada.Unchecked_Conversion (Seed, Chacha20.Key);

      S           : Seed;
      Nonce       : Unsigned_64;
      Cha_Block   : Chacha20.Block := (others => 0);
      Index       : Natural     := Cha_Block'Last + 1;
      Mini_Index  : Natural     := 0;
      Block_Index : Unsigned_64 := 0;
      Temp        : Unsigned_32 := 0;
   begin
      Seize (Accumulator_Mutex);

      Entropy_Adjust;

      S.Entropy_Hash := MD5.Digest (Entropy_Accumulator);
      Arch.Clocks.Get_Monotonic_Time (S.Mono_Sec, S.Mono_NSec);
      Nonce := S.Mono_NSec;

      for Value of Data loop
         if Index > Cha_Block'Last then
            Cha_Block   := Chacha20.Gen_Key (To_Seed (S), Nonce, Block_Index);
            Index       := Cha_Block'First;
            Block_Index := Block_Index + 1;
         end if;

         case Mini_Index is
            when 0 => Temp := Shift_Right (Cha_Block (Index), 0);
            when 1 => Temp := Shift_Right (Cha_Block (Index), 8);
            when 2 => Temp := Shift_Right (Cha_Block (Index), 16);
            when 3 => Temp := Shift_Right (Cha_Block (Index), 24);
            when others => null;
         end case;
         Value := Unsigned_8 (Temp and 16#FF#);

         Mini_Index := Mini_Index + 1;
         if Mini_Index = 4 then
            Index      := Index + 1;
            Mini_Index := 0;
         end if;
      end loop;

      Release (Accumulator_Mutex);
   end Fill_Data;

   procedure Feed_Entropy (Data : Unsigned_32) is
   begin
      Seize (Accumulator_Mutex);
      Entropy_Accumulator (1) (0) := Entropy_Accumulator (1) (3) xor Data;
      Release (Accumulator_Mutex);
   end Feed_Entropy;

   procedure Get_Integer (Result : out Unsigned_64) is
      Data : Crypto_Data (1 .. 8);
   begin
      Fill_Data (Data);
      Result := Shift_Left (Unsigned_64 (Data (1)), 56) or
                Shift_Left (Unsigned_64 (Data (2)), 48) or
                Shift_Left (Unsigned_64 (Data (3)), 40) or
                Shift_Left (Unsigned_64 (Data (4)), 32) or
                Shift_Left (Unsigned_64 (Data (5)), 24) or
                Shift_Left (Unsigned_64 (Data (6)), 16) or
                Shift_Left (Unsigned_64 (Data (7)),  8) or
                Shift_Left (Unsigned_64 (Data (8)),  0);
   end Get_Integer;

   procedure Get_Integer (Min, Max : Unsigned_64; Result : out Unsigned_64) is
   begin
      Get_Integer (Result);
      Result := (Result mod (Max + 1 - Min)) + Min;
   end Get_Integer;
   ----------------------------------------------------------------------------
   procedure Entropy_Adjust is
      RSec, RNSec, MSec, MNSec, Avail, Free : Unsigned_64;
      S      : Memory.Physical.Statistics;
      Hashed : constant MD5.MD5_Hash := MD5.Digest (Entropy_Accumulator);
   begin
      Get_Statistics (S);
      Arch.Clocks.Get_Real_Time (RSec, RNSec);
      Arch.Clocks.Get_Monotonic_Time (MSec, MNSec);

      Avail := Unsigned_64 (S.Available);
      Free  := Unsigned_64 (S.Free);

      Entropy_Accumulator (1) :=
         (0  => Unsigned_32 (Shift_Right (MSec, 32) and 16#FFFFFFFF#),
          1  => Unsigned_32 (MSec and 16#FFFFFFFF#),
          2  => Unsigned_32 (Shift_Right (Avail, 32) and 16#FFFFFFFF#),
          3  => Unsigned_32 (Avail and 16#FFFFFFFF#),
          4  => Unsigned_32 (Shift_Right (RSec, 32) and 16#FFFFFFFF#),
          5  => Unsigned_32 (RSec and 16#FFFFFFFF#),
          6  => Unsigned_32 (Shift_Right (RNSec, 32) and 16#FFFFFFFF#),
          7  => Unsigned_32 (RNSec and 16#FFFFFFFF#),
          8  => Hashed (1),
          9  => Hashed (2),
          10 => Hashed (3),
          11 => Hashed (4),
          12 => Unsigned_32 (Shift_Right (Free, 32) and 16#FFFFFFFF#),
          13 => Unsigned_32 (Free and 16#FFFFFFFF#),
          14 => Unsigned_32 (MNSec and 16#FFFFFFFF#),
          15 => 128);
   end Entropy_Adjust;
end Cryptography.Random;
