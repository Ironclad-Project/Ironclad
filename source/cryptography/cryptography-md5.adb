--  cryptography-md5.adb: MD5 digest.
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

package body Cryptography.MD5 is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   --  Algorithm adapted from Wikipedia's MD5 pseudocode.
   --  https://en.wikipedia.org/wiki/MD5#Pseudocode
   --  Code in Wikipedia is CC-BY-SA 3.0, relicensed to BY-SA 4.0 under 4(b),
   --  which is GPLv3-Or-Later compatible.

   S : constant array (Unsigned_32 range 0 .. 63) of Natural :=
      (7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
       5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
       4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
       6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21);

   K : constant array (Unsigned_32 range 0 .. 63) of Unsigned_32 :=
      (16#d76aa478#, 16#e8c7b756#, 16#242070db#, 16#c1bdceee#,
       16#f57c0faf#, 16#4787c62a#, 16#a8304613#, 16#fd469501#,
       16#698098d8#, 16#8b44f7af#, 16#ffff5bb1#, 16#895cd7be#,
       16#6b901122#, 16#fd987193#, 16#a679438e#, 16#49b40821#,
       16#f61e2562#, 16#c040b340#, 16#265e5a51#, 16#e9b6c7aa#,
       16#d62f105d#, 16#02441453#, 16#d8a1e681#, 16#e7d3fbc8#,
       16#21e1cde6#, 16#c33707d6#, 16#f4d50d87#, 16#455a14ed#,
       16#a9e3e905#, 16#fcefa3f8#, 16#676f02d9#, 16#8d2a4c8a#,
       16#fffa3942#, 16#8771f681#, 16#6d9d6122#, 16#fde5380c#,
       16#a4beea44#, 16#4bdecfa9#, 16#f6bb4b60#, 16#bebfbc70#,
       16#289b7ec6#, 16#eaa127fa#, 16#d4ef3085#, 16#04881d05#,
       16#d9d4d039#, 16#e6db99e5#, 16#1fa27cf8#, 16#c4ac5665#,
       16#f4292244#, 16#432aff97#, 16#ab9423a7#, 16#fc93a039#,
       16#655b59c3#, 16#8f0ccc92#, 16#ffeff47d#, 16#85845dd1#,
       16#6fa87e4f#, 16#fe2ce6e0#, 16#a3014314#, 16#4e0811a1#,
       16#f7537e82#, 16#bd3af235#, 16#2ad7d2bb#, 16#eb86d391#);

   function Digest (Data : MD5_Blocks) return MD5_Hash is
      Total_A : Unsigned_32 := 16#67452301#;
      Total_B : Unsigned_32 := 16#efcdab89#;
      Total_C : Unsigned_32 := 16#98badcfe#;
      Total_D : Unsigned_32 := 16#10325476#;
      Round_A, Round_B, Round_C, Round_D : Unsigned_32;
      F, G : Unsigned_32;
   begin
      for Block of Data loop
         Round_A := Total_A;
         Round_B := Total_B;
         Round_C := Total_C;
         Round_D := Total_D;

         for I in K'Range loop
            if I <= 15 then
               F := (Round_B and Round_C) or ((not Round_B) and Round_D);
               G := I;
            elsif I <= 31 then
               F := (Round_D and Round_B) or ((not Round_D) and Round_C);
               G := (5 * I + 1) mod 16;
            elsif I <= 47 then
               F := Round_B xor Round_C xor Round_D;
               G := (3 * I + 5) mod 16;
            else
               F := Round_C xor (Round_B or (not Round_D));
               G := (7 * I) mod 16;
            end if;
            pragma Loop_Invariant (G <= 15);

            F := F + Round_A + K (I) + Block (G);
            Round_A := Round_D;
            Round_D := Round_C;
            Round_C := Round_B;
            Round_B := Round_B + Rotate_Left (F, S (I));
         end loop;

         Total_A := Total_A + Round_A;
         Total_B := Total_B + Round_B;
         Total_C := Total_C + Round_C;
         Total_D := Total_D + Round_D;
      end loop;

      return (Total_A, Total_B, Total_C, Total_D);
   end Digest;

   function To_String (Hash : MD5_Hash) return MD5_String is
      Hex_Chars : constant array (0 .. 15) of Character :=
         ('0', '1', '2', '3', '4', '5', '6', '7',
          '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
      Item     : MD5_Hash;
      Result   : MD5_String := (others => '0');
      Temp     : Unsigned_32;
      Position : Integer range Result'First - 1 .. Result'Last := Result'Last;
   begin
      for I in Item'Range loop
         Item (I) := BSwap32 (Hash (I));
      end loop;

      for Part in reverse Item'Range loop
         Temp := Item (Part);
         while Position > Result'Last - (5 - Part) * 8 loop
            Result (Position) := Hex_Chars (Natural (Temp mod 16));
            Position          := Position - 1;
            Temp              := Temp / 16;
         end loop;
      end loop;
      return Result;
   end To_String;
end Cryptography.MD5;
