--  cryptography-random.adb: The random number generator of the kernel.
--  Copyright (C) 2025 streaksu
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
with Cryptography.Chacha20;
with Cryptography.MD5; use Cryptography.MD5;
with Synchronization; use Synchronization;
with Arch.Clocks;
with Memory.Physical;
with Memory.MMU;
with Time;

package body Cryptography.Random is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   --  We implement a fortuna-like PRNG as the kernel's random source.
   --  https://en.wikipedia.org/wiki/Fortuna_(PRNG)

   --  We have 6 pools, each the size of an MD5 block, since that makes it
   --  easier for us to use MD5 to hash our fortuna pools.
   --  Fortuna calls for 32, we use 16, I feel that is enough.
   Pool_Count : constant := 16;
   subtype Entropy_Pool_Idx is Natural range 1 .. Pool_Count;

   --  Fortuna recommends we reseed every request but no faster than 10 times
   --  a second, we will do no faster than 5 for extra speed.
   Nanoseconds_Between_Reseed : constant := 200_000_000;

   --  Data we use to store the current seed.
   type Seed is record
      Hash1, Hash2 : MD5.MD5_Hash;
   end record with Size => 256, Universal_Aliasing;
   function To_Seed is new Ada.Unchecked_Conversion (Seed, Chacha20.Key);


   subtype U64_Data is Crypto_Data (1 .. 8);
   function Conv is new Ada.Unchecked_Conversion (Unsigned_64, U64_Data);
   function Conv is new Ada.Unchecked_Conversion (U64_Data, Unsigned_64);

   --  Globals to keep track of RNG state.
   Accumulator_Mutex  :      aliased Mutex := Unlocked_Mutex;
   Reseeding_Count    :        Unsigned_64 := 0;
   Last_Reseed_Sec    :        Unsigned_64 := 0;
   Last_Reseed_NSec   :        Unsigned_64 := 0;
   Entropy_Feed_Idx   :   Entropy_Pool_Idx := 1;
   Current_Seed_Block :        Unsigned_64 := 0;
   Entropy_Pool       : MD5.MD5_Blocks_Acc := null;
   Current_Seed       :               Seed := ([others => 0], [others => 0]);

   procedure Init is
      RT_Time, Mono_Time : Time.Timestamp;
      S : Memory.Physical.Statistics;
      V : Memory.MMU.Virtual_Statistics;
      Data1, Data2, Data3, Data4, Data5, Data6, Data7 : U64_Data;
      Data8, Data9 : U64_Data;
   begin
      --  Initialize.
      Seize (Accumulator_Mutex);
      Entropy_Pool := new MD5.MD5_Blocks'(1 .. Pool_Count => [others => 0]);
      Release (Accumulator_Mutex);

      --  Fill some preliminary data. This is just meant to have a baseline
      --  that isnt just zeros, but the entropy here will be VERY VERY
      --  anemic.
      Memory.Physical.Get_Statistics (S);
      Memory.MMU.Get_Statistics (V);
      Arch.Clocks.Get_Real_Time (RT_Time);
      Arch.Clocks.Get_Monotonic_Time (Mono_Time);

      --  Translate to feedable data.
      Data1 := Conv (RT_Time.Seconds);
      Data2 := Conv (RT_Time.Nanoseconds);
      Data3 := Conv (Mono_Time.Seconds);
      Data4 := Conv (Mono_Time.Nanoseconds);
      Data5 := Conv (Unsigned_64 (S.Available));
      Data6 := Conv (Unsigned_64 (S.Free));
      Data7 := Conv (Unsigned_64 (S.Total));
      Data8 := Conv (Unsigned_64 (V.Kernel_Usage));
      Data9 := Conv (Unsigned_64 (V.Table_Usage));

      --  Feed.
      Feed_Entropy (Data1); Feed_Entropy (Data2); Feed_Entropy (Data3);
      Feed_Entropy (Data4); Feed_Entropy (Data5); Feed_Entropy (Data6);
      Feed_Entropy (Data7); Feed_Entropy (Data8); Feed_Entropy (Data9);
   end Init;

   procedure Fill_Data (Data : out Crypto_Data) is
      Mono_Time   : Time.Timestamp;
      Nonce       : Unsigned_64;
      Cha_Block   : Chacha20.Block := [others => 0];
      Index       : Natural     := Cha_Block'Last + 1;
      Mini_Index  : Natural     := 0;
      Temp        : Unsigned_32 := 0;
   begin
      Seize (Accumulator_Mutex);

      --  Check whether we need to reseed at all, take the chance to take a
      --  nonce as well.
      Arch.Clocks.Get_Monotonic_Time (Mono_Time);
      Nonce := Mono_Time.Nanoseconds;
      if Mono_Time.Seconds > Last_Reseed_Sec or else
         Nonce - Last_Reseed_NSec >= Nanoseconds_Between_Reseed
      then
         Last_Reseed_Sec  := Mono_Time.Seconds;
         Last_Reseed_NSec := Nonce;

         --  Choose which pools to seed with. We are doing this with a
         --  fortuna-like approach, attempting for the higher pools to be used
         --  less than the earlier ones.
         for I in reverse 1 .. Pool_Count loop
            if (I mod 2 ** (I - 1)) = 0 then
               Current_Seed.Hash1 := MD5.Digest (Entropy_Pool (I .. I));
               Current_Seed.Hash2 := Current_Seed.Hash1;
               Current_Seed_Block := 0;

               if Reseeding_Count = Unsigned_64'Last then
                  Reseeding_Count := 0;
               else
                  Reseeding_Count := Reseeding_Count + 1;
               end if;

               exit;
            end if;
         end loop;
      end if;
      Nonce := Nonce xor Reseeding_Count; --  Just for some extra razzmatazz.

      --  Do the block cypher thing.
      for Value of Data loop
         if Index > Cha_Block'Last then
            Cha_Block := Chacha20.Gen_Key
               (To_Seed (Current_Seed), Nonce, Current_Seed_Block);
            Index := Cha_Block'First;
            if Current_Seed_Block /= Unsigned_64'Last then
               Current_Seed_Block := Current_Seed_Block + 1;
            else
               Current_Seed_Block := 0;
            end if;
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

   procedure Feed_Entropy (Data : Crypto_Data) is
      E_Idx : Natural renames Entropy_Feed_Idx;
      Idx   : Unsigned_32 := 0;
   begin
      Seize (Accumulator_Mutex);

      --  This function might be called before initialization.
      if Entropy_Pool = null or Data'Length = 0 then
         return;
      end if;

      --  Actually reseed.
      for C of Data loop
         Entropy_Pool (E_Idx) (Idx) :=
            Entropy_Pool (E_Idx) (Unsigned_32 (C) mod MD5_Block'Length) xor
            Entropy_Pool (E_Idx) (Idx) xor Shift_Left (Unsigned_32 (C), 16);
         Idx := (if Idx = Entropy_Pool (E_Idx)'Last then 0 else Idx + 1);
      end loop;

      --  Adjust for next reseed.
      E_Idx := (if E_Idx = Pool_Count then 1 else E_Idx + 1);

      Release (Accumulator_Mutex);
   end Feed_Entropy;

   procedure Get_Integer (Result : out Unsigned_64) is
      Data : U64_Data;
   begin
      Fill_Data (Data);
      Result := Conv (Data);
   end Get_Integer;

   procedure Get_Integer (Min, Max : Unsigned_64; Result : out Unsigned_64) is
   begin
      Get_Integer (Result);
      Result := (Result mod (Max + 1 - Min)) + Min;
   end Get_Integer;
end Cryptography.Random;
