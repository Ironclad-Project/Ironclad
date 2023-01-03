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
with Memory;     use Memory;

package Arch.Snippets is
   --  Lots of these are inlined because they usually are 3 or 4 instructions
   --  tops, and used in hot execution paths.

   --  Drive the execution thread to an irrecoverable state.
   --  (Halt and Catch Fire).
   procedure HCF with Inline, No_Return;

   --  Enable and disable external interrupts.
   procedure Enable_Interrupts with Inline;
   procedure Disable_Interrupts with Inline;

   --  Processor hint for waiting for interrupts in an energy-efficient state.
   procedure Wait_For_Interrupt with Inline;

   --  Processor hint for optimizing spinlocks and another cache-intensitive
   --  situations.
   procedure Pause with Inline;

   --  Get a rough value of the number of cycles the system has gone thru.
   --  The value is not guaranteed to be linear, or even changing, it is only
   --  to be used for statistical or entropy purposes.
   function Read_Cycles return Unsigned_64 with Inline;
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
   ----------------------------------------------------------------------------
   --  Architecture-specific snippets.
   --  FIXME: We use gnatprep for setting up architecture-specific snppets.
   --  There are probably more idiomatic ways to do this, but for this small
   --  little type changes, it is probably fine, else we would have to put
   --  more files on each port, and that would segregate fundamentally
   --  equivalent code for no reason.
   #if ArchName = """aarch64-stivale2"""
   #elsif ArchName = """sparc-leon3"""
   #elsif ArchName = """x86_64-multiboot2"""
      procedure Port_Out (Port : Unsigned_16; Value : Unsigned_8) with Inline;
      function Port_In (Port : Unsigned_16) return Unsigned_8 with Inline;
      procedure Port_Out16 (Port, Value : Unsigned_16) with Inline;
      function Port_In16 (Port : Unsigned_16) return Unsigned_16 with Inline;
      procedure Port_Out32 (Port : Unsigned_16; Value : Unsigned_32)
         with Inline;
      function Port_In32 (Port : Unsigned_16) return Unsigned_32 with Inline;
      procedure Invalidate_Page (Value : Virtual_Address) with Inline;
      function Read_MSR (MSR : Unsigned_32) return Unsigned_64 with Inline;
      procedure Write_MSR (MSR : Unsigned_32; Value : Unsigned_64) with Inline;
      function Read_CR0 return Unsigned_64 with Inline;
      procedure Write_CR0 (Value : Unsigned_64) with Inline;
      function Read_CR2 return Unsigned_64 with Inline;
      function Read_CR3 return Unsigned_64 with Inline;
      procedure Write_CR3 (Value : Unsigned_64) with Inline;
      function Read_CR4 return Unsigned_64 with Inline;
      procedure Write_CR4 (Value : Unsigned_64) with Inline;
      function Read_FS return Unsigned_64 with Inline;
      procedure Write_FS (Value : Unsigned_64) with Inline;
      function Read_GS return Unsigned_64 with Inline;
      procedure Write_GS (Value : Unsigned_64) with Inline;
      function Read_Kernel_GS return Unsigned_64 with Inline;
      procedure Write_Kernel_GS (Value : Unsigned_64) with Inline;
      procedure Swap_GS with Inline;
      procedure Get_CPUID
         (Leaf, Subleaf : Unsigned_32;
          EAX, EBX, ECX, EDX : out Unsigned_32);
   #end if;
end Arch.Snippets;
