--  cryptography-aes.adb: Hardware-accelerated AES implementation.
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

package body Cryptography.AES is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Encrypt_ECB (Key : Unsigned_128; Data : in out AES_Data) is
      Expanded_Key : constant Expanded_AES_Key := AES_Expand_Key (Key);
   begin
      for I in Data'Range loop
         Data (I) := Encrypt_128 (Data (I), Key, Expanded_Key);
      end loop;
   end Encrypt_ECB;

   procedure Decrypt_ECB (Key : Unsigned_128; Data : in out AES_Data) is
      Expanded_Key : constant Expanded_AES_Key := AES_Expand_Inv_Key (Key);
   begin
      for I in Data'Range loop
         Data (I) := Decrypt_128 (Data (I), Key, Expanded_Key);
      end loop;
   end Decrypt_ECB;

   procedure Encrypt_CBC (Key, IV : Unsigned_128; Data : in out AES_Data) is
      Expanded_Key : constant Expanded_AES_Key := AES_Expand_Key (Key);
      Curr_IV : Unsigned_128 := IV;
   begin
      for I in Data'Range loop
         Data (I) := Encrypt_128 (Data (I) xor Curr_IV, Key, Expanded_Key);
         Curr_IV  := Data (I);
      end loop;
   end Encrypt_CBC;

   procedure Decrypt_CBC (Key, IV : Unsigned_128; Data : in out AES_Data) is
      Expanded_Key : constant Expanded_AES_Key := AES_Expand_Inv_Key (Key);
      Curr_IV : Unsigned_128 := IV;
      Next_IV : Unsigned_128;
   begin
      for I in Data'Range loop
         Next_IV  := Data (I);
         Data (I) := Decrypt_128 (Data (I), Key, Expanded_Key) xor Curr_IV;
         Curr_IV  := Next_IV;
      end loop;
   end Decrypt_CBC;

   procedure Crypt_CTR (Key, IV : Unsigned_128; Data : in out AES_Data) is
      Expanded_Key : constant Expanded_AES_Key := AES_Expand_Key (Key);
      Curr_IV : Unsigned_128 := Shift_Left (IV, 31) and
         Unsigned_128 (not Unsigned_32'Last);
   begin
      for I in Data'Range loop
         Data (I) := Encrypt_128 (Curr_IV, Key, Expanded_Key) xor Data (I);
         Curr_IV  := Curr_IV + 1;
      end loop;
   end Crypt_CTR;

   function Encrypt_128
      (Data, Original_Key : Unsigned_128;
       Key : Expanded_AES_Key) return Unsigned_128
   is
      Encrypted : Unsigned_128 := Data;
   begin
      Encrypted := Encrypted xor Original_Key;
      for I in 1 .. 9 loop
         Encrypted := Arch.Snippets.AES_Encrypt_One (Encrypted, Key (I));
      end loop;
      Encrypted := Arch.Snippets.AES_Encrypt_Last (Encrypted, Key (10));
      return Encrypted;
   end Encrypt_128;

   function Decrypt_128
      (Data, Original_Key : Unsigned_128;
       Key : Expanded_AES_Key) return Unsigned_128
   is
      Decrypted : Unsigned_128 := Data;
   begin
      Decrypted := Decrypted xor Key (10);
      for I in reverse 1 .. 9 loop
         Decrypted := Arch.Snippets.AES_Decrypt_One (Decrypted, Key (I));
      end loop;
      Decrypted := Arch.Snippets.AES_Decrypt_Last (Decrypted, Original_Key);
      return Decrypted;
   end Decrypt_128;
end Cryptography.AES;
