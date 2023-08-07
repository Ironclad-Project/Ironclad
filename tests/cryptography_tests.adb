--  cryptography_tests.adb: Cryptography-related unit tests.
--  Copyright (C) 2023 streaksu
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

with Cryptography.MD5;      use Cryptography.MD5;
with Cryptography.Chacha20; use Cryptography.Chacha20;
with Ada.Unchecked_Conversion;

package body Cryptography_Tests is
   procedure Run_Chacha20_Tests is
      Test_Key   : Cryptography.Chacha20.Key;
      Returned   : Cryptography.Chacha20.Block;
      Test_Block : Cryptography.Chacha20.Block;
   begin
      Test_Key   := (0, 0, 0, 0, 0, 0, 0, 0);
      Returned   := Cryptography.Chacha20.Gen_Key (Test_Key, 0, 0);
      Test_Block := (16#7CB7FD84#, 16#0BD21656#, 16#81E7ABD2#, 16#8420B8D0#,
                     16#B3B60989#, 16#2D54A092#, 16#AD7A6043#, 16#F8DED59A#,
                     16#F099991B#, 16#DAAA95FD#, 16#CCC6A0B1#, 16#C755D901#,
                     16#433E0355#, 16#FA53AB82#, 16#006964FE#, 16#560E3B68#);
      if Returned /= Test_Block then
         raise Crypto_Exception with "Chacha20 test failed";
      end if;
   end Run_Chacha20_Tests;

   procedure Run_MD5_Tests is
      type MD5_Exact_Str is new String (1 .. 64) with Size => 512;
      function Conv is new Ada.Unchecked_Conversion
         (Source => MD5_Exact_Str, Target => Cryptography.MD5.MD5_Block);

      --  MD5 test array.
      type MD5_Test is record
         Data     : access MD5_Blocks;
         Expected : MD5_String;
      end record;
      type MD5_Test_Arr is array (Positive range <>) of MD5_Test;

      MD5_Test1 : constant MD5_Exact_Str :=
         "VIHP4l285gJ4IenB0EeLk9QLChfBx35QCJR11LY90XIsiyfW4qgSyESgtw2idle4";
      MD5_Test_1 : constant access MD5_Blocks :=
         new MD5_Blocks'((0 => Conv (MD5_Test1),
                       1 => (0 => 16#80#, 1 .. 13 => 0, 14 => 512, 15 => 0)));

      --  Empty string.
      MD5_Test_2 : constant access MD5_Blocks :=
         new MD5_Blocks'(0 => (0 => 16#80#, 1 .. 15 => 0));
      MD5_Tests : constant MD5_Test_Arr (1 .. 2) := (
         1 => (Data     => MD5_Test_1,
               Expected => "90adb0735901070d47c9d32cc10b975c"),
         2 => (Data     => MD5_Test_2,
               Expected => "d41d8cd98f00b204e9800998ecf8427e")
      );
   begin
      for I in MD5_Tests'Range loop
         if MD5_Tests (I).Expected /=
            To_String (Digest (MD5_Tests (I).Data.all))
         then
            raise Crypto_Exception with Integer'Image (I) & " did not match";
         end if;
      end loop;
   end Run_MD5_Tests;
end Cryptography_Tests;
