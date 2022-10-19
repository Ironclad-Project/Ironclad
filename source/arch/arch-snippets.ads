--  arch-snippets.ads: Architecture-specific bits.
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

package Arch.Snippets is
   --  All of these are always inlined because they usually are 3 or 4
   --  instructions tops, and used in hot execution paths.
   --  Inline instead of Always_Inline seems to never inline at all.

   --  Drive the execution thread to an irrecoverable state.
   --  (Halt and Catch Fire).
   procedure HCF with Inline_Always, No_Return;

   --  Enable and disable external interrupts.
   procedure Enable_Interrupts with Inline_Always;
   procedure Disable_Interrupts with Inline_Always;

   --  Processor hint for waiting for interrupts in an energy-efficient state.
   procedure Wait_For_Interrupt with Inline_Always;

   --  Processor hint for optimizing spinlocks and another cache-intensitive
   --  situations.
   procedure Pause with Inline_Always;

   --  Get a rough value of the number of cycles the system has gone thru.
   --  The value is not guaranteed to be linear, or even changing, it is only
   --  to be used for statistical or entropy purposes.
   function Read_Cycles return Unsigned_64 with Inline_Always;
   ----------------------------------------------------------------------------
   --  Check whether the CPU supports AES acceleration.
   --  If false, the AES functions below will not work.
   function Supports_AES_Accel return Boolean;

   --  Expand a 128-bit key into a 176 byte array.
   type Expanded_AES_Key is array (1 .. 10) of Unsigned_128;
   function AES_Expand_Key (Key : Unsigned_128) return Expanded_AES_Key;
   function AES_Expand_Inv_Key (Key : Unsigned_128) return Expanded_AES_Key;

   --  Perform one round of AES encryption, using 128-bit data and a 128-bit
   --  round key.
   function AES_Encrypt_One (Data, Key : Unsigned_128) return Unsigned_128;
   function AES_Encrypt_Last (Data, Key : Unsigned_128) return Unsigned_128;

   --  Perform one round of AES decryption, using 128-bit data and a 128-bit
   --  round key.
   function AES_Decrypt_One (Data, Key : Unsigned_128) return Unsigned_128;
   function AES_Decrypt_Last (Data, Key : Unsigned_128) return Unsigned_128;
end Arch.Snippets;
