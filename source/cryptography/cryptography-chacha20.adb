--  cryptography-chacha20.adb: Chacha20 implementation.
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

package body Cryptography.Chacha20 is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   function Gen_Key (K : Key; Nonce, Block_ID : Unsigned_64) return Block is
      UB : constant Unsigned_64 := Shift_Right (Block_ID, 32) and 16#FFFFFFFF#;
      LB : constant Unsigned_64 := Shift_Right (Block_ID, 0)  and 16#FFFFFFFF#;
      UN : constant Unsigned_64 := Shift_Right (Nonce, 32)    and 16#FFFFFFFF#;
      LN : constant Unsigned_64 := Shift_Right (Nonce, 0)     and 16#FFFFFFFF#;

      Initial_Block : constant Block := (
         --  Hardcoded values of "expa", "nd 3", "2-by", "te k"
             16#65787061#,     16#6E642065#,     16#322D6279#,    16#7465206B#,
                 K.Value1,         K.Value2,         K.Value3,        K.Value4,
                 K.Value5,         K.Value6,         K.Value7,        K.Value8,
         Unsigned_32 (UB), Unsigned_32 (LB), Unsigned_32 (UN), Unsigned_32 (LN)
      );
      B : Block := Initial_Block;
   begin
      --  Do 2 iterations at once.
      for I in 1 .. 20 / 2 loop
         --  Odd iteration.
         Quarter_Round (B (0), B (4), B  (8), B (12));
         Quarter_Round (B (1), B (5), B  (9), B (13));
         Quarter_Round (B (2), B (6), B (10), B (14));
         Quarter_Round (B (3), B (7), B (11), B (15));

         --  Even iteration.
         Quarter_Round (B (0), B (5), B (10), B (15));
         Quarter_Round (B (1), B (6), B (11), B (12));
         Quarter_Round (B (2), B (7), B  (8), B (13));
         Quarter_Round (B (3), B (4), B  (9), B (14));
      end loop;

      --  Sum the initial and final.
      for I in B'Range loop
         B (I) := B (I) + Initial_Block (I);
      end loop;
      return B;
   end Gen_Key;

   procedure Quarter_Round (A, B, C, D : in out Unsigned_32) is
   begin
      A := A + B; D := D xor A; D := Rotate_Left (D, 16);
      C := C + D; B := B xor C; B := Rotate_Left (B, 12);
      A := A + B; D := D xor A; D := Rotate_Left (D, 8);
      C := C + D; B := B xor C; B := Rotate_Left (B, 7);
   end Quarter_Round;
end Cryptography.Chacha20;
