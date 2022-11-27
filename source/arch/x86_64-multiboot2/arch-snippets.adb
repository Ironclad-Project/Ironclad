--  arch-snippets.adb: Architecture-specific bits.
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

with System.Machine_Code;     use System.Machine_Code;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Arch.Wrappers;

package body Arch.Snippets with SPARK_Mode => Off is
   procedure HCF is
   begin
      --  Interrupts ought to be disabled every iteration and not only once
      --  because of spurious interrupts.
      loop
         Disable_Interrupts;
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      Asm ("sti", Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      Asm ("cli", Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      Asm ("hlt", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      Asm ("pause", Volatile => True);
   end Pause;

   function Read_Cycles return Unsigned_64 is
      High : Unsigned_32;
      Low  : Unsigned_32;
   begin
      Asm ("rdtsc",
          Outputs => (Unsigned_32'Asm_Output ("=d", High),
                      Unsigned_32'Asm_Output ("=a", Low)),
          Volatile => True);
      return Unsigned_64 (Shift_Left (High, 32) or Low);
   end Read_Cycles;
   ----------------------------------------------------------------------------
   function Supports_AES_Accel return Boolean is
      EAX, EBX, ECX, EDX : Unsigned_32;
   begin
      Wrappers.Get_CPUID (1, 0, EAX, EBX, ECX, EDX);
      return (EDX and Shift_Left (1, 25)) /= 0;
   end Supports_AES_Accel;

   function AES_Expand_Key (Key : Unsigned_128) return Expanded_AES_Key is
      Result : Expanded_AES_Key;
   begin
      Asm ("mov             %%rsp, %%rax"          & LF & HT &
           "sub             $48, %%rax"            & LF & HT &
           "and             $~15, %%rax"           & LF & HT &
           "movdqa          %%xmm0, (%%rax)"       & LF & HT &
           "movdqa          %%xmm1, 16(%%rax)"     & LF & HT &
           "movdqa          %%xmm2, 32(%%rax)"     & LF & HT &
           "movdqu          %10, %%xmm0"           & LF & HT &
           "aeskeygenassist $1, %%xmm0, %%xmm1"    & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %0"            & LF & HT &
           "aeskeygenassist $2, %%xmm0, %%xmm1"    & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %1"            & LF & HT &
           "aeskeygenassist $4, %%xmm0, %%xmm1"    & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %2"            & LF & HT &
           "aeskeygenassist $8, %%xmm0, %%xmm1"    & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %3"            & LF & HT &
           "aeskeygenassist $16, %%xmm0, %%xmm1"   & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %4"            & LF & HT &
           "aeskeygenassist $32, %%xmm0, %%xmm1"   & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %5"            & LF & HT &
           "aeskeygenassist $64, %%xmm0, %%xmm1"   & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %6"            & LF & HT &
           "aeskeygenassist $128, %%xmm0, %%xmm1"  & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %7"            & LF & HT &
           "aeskeygenassist $27, %%xmm0, %%xmm1"   & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %8"            & LF & HT &
           "aeskeygenassist $54, %%xmm0, %%xmm1"   & LF & HT &
           "call            2f"                    & LF & HT &
           "movdqu          %%xmm0, %9"            & LF & HT &
           "movdqa          (%%rax),   %%xmm0"     & LF & HT &
           "movdqa          16(%%rax), %%xmm1"     & LF & HT &
           "movdqa          32(%%rax), %%xmm2"     & LF & HT &
           "jmp 1f"                                & LF & HT &
        "2:"                                       & LF & HT &
           "pshufd          $0xff, %%xmm1, %%xmm1" & LF & HT &
           "movaps          %%xmm0, %%xmm2"        & LF & HT &
           "pslldq          $0x4,  %%xmm2"         & LF & HT &
           "pxor            %%xmm2, %%xmm0"        & LF & HT &
           "movaps          %%xmm0, %%xmm2"        & LF & HT &
           "pslldq          $0x4,  %%xmm2"         & LF & HT &
           "pxor            %%xmm2, %%xmm0"        & LF & HT &
           "pxor            %%xmm1, %%xmm0"        & LF & HT &
           "ret"                                   & LF & HT &
        "1:",
           Outputs  => (Unsigned_128'Asm_Output ("=m", Result (1)),
                        Unsigned_128'Asm_Output ("=m", Result (2)),
                        Unsigned_128'Asm_Output ("=m", Result (3)),
                        Unsigned_128'Asm_Output ("=m", Result (4)),
                        Unsigned_128'Asm_Output ("=m", Result (5)),
                        Unsigned_128'Asm_Output ("=m", Result (6)),
                        Unsigned_128'Asm_Output ("=m", Result (7)),
                        Unsigned_128'Asm_Output ("=m", Result (8)),
                        Unsigned_128'Asm_Output ("=m", Result (9)),
                        Unsigned_128'Asm_Output ("=m", Result (10))),
           Inputs   => Unsigned_128'Asm_Input ("m", Key),
           Clobber  => "memory,rax",
           Volatile => True);
      return Result;
   end AES_Expand_Key;

   function AES_Expand_Inv_Key (Key : Unsigned_128) return Expanded_AES_Key is
      Result : Expanded_AES_Key := AES_Expand_Key (Key);
   begin
      for I in Result'First .. (Result'Last - 1) loop
         Asm ("mov    %%rsp, %%rax"      & LF & HT &
              "sub    $16, %%rax"        & LF & HT &
              "and    $~15, %%rax"       & LF & HT &
              "movdqa %%xmm0, (%%rax)"   & LF & HT &
              "movdqu %0, %%xmm0"        & LF & HT &
              "aesimc %%xmm0, %%xmm0"    & LF & HT &
              "movdqu %%xmm0, %0"        & LF & HT &
              "movdqa (%%rax), %%xmm0",
              Outputs  => Unsigned_128'Asm_Output ("+m", Result (I)),
              Clobber  => "memory,rax",
              Volatile => True);
      end loop;
      return Result;
   end AES_Expand_Inv_Key;

   function AES_Encrypt_One (Data, Key : Unsigned_128) return Unsigned_128 is
      Result : Unsigned_128 := Data;
   begin
      Asm ("mov    %%rsp, %%rax"      & LF & HT &
           "sub    $32, %%rax"        & LF & HT &
           "and    $~15, %%rax"       & LF & HT &
           "movdqa %%xmm0, (%%rax)"   & LF & HT &
           "movdqa %%xmm1, 16(%%rax)" & LF & HT &
           "movdqu %0, %%xmm0"        & LF & HT &
           "movdqu %1, %%xmm1"        & LF & HT &
           "aesenc %%xmm1, %%xmm0"    & LF & HT &
           "movdqu %%xmm0, %0"        & LF & HT &
           "movdqa (%%rax), %%xmm0"   & LF & HT &
           "movdqa 16(%%rax), %%xmm1",
           Outputs  => Unsigned_128'Asm_Output ("+m", Result),
           Inputs   => Unsigned_128'Asm_Input  ("m",  Key),
           Clobber  => "memory,rax",
           Volatile => True);
      return Result;
   end AES_Encrypt_One;

   function AES_Encrypt_Last (Data, Key : Unsigned_128) return Unsigned_128 is
      Result : Unsigned_128 := Data;
   begin
      Asm ("mov        %%rsp, %%rax"      & LF & HT &
           "sub        $32, %%rax"        & LF & HT &
           "and        $~15, %%rax"       & LF & HT &
           "movdqa     %%xmm0, (%%rax)"   & LF & HT &
           "movdqa     %%xmm1, 16(%%rax)" & LF & HT &
           "movdqu     %0, %%xmm0"        & LF & HT &
           "movdqu     %1, %%xmm1"        & LF & HT &
           "aesenclast %%xmm1, %%xmm0"    & LF & HT &
           "movdqu     %%xmm0, %0"        & LF & HT &
           "movdqa     (%%rax), %%xmm0"   & LF & HT &
           "movdqa     16(%%rax), %%xmm1",
           Outputs  => Unsigned_128'Asm_Output ("+m", Result),
           Inputs   => Unsigned_128'Asm_Input  ("m",  Key),
           Clobber  => "memory,rax",
           Volatile => True);
      return Result;
   end AES_Encrypt_Last;

   function AES_Decrypt_One (Data, Key : Unsigned_128) return Unsigned_128 is
      Result : Unsigned_128 := Data;
   begin
      Asm ("mov    %%rsp, %%rax"      & LF & HT &
           "sub    $32, %%rax"        & LF & HT &
           "and    $~15, %%rax"       & LF & HT &
           "movdqa %%xmm0, (%%rax)"   & LF & HT &
           "movdqa %%xmm1, 16(%%rax)" & LF & HT &
           "movdqu %0, %%xmm0"        & LF & HT &
           "movdqu %1, %%xmm1"        & LF & HT &
           "aesdec %%xmm1, %%xmm0"    & LF & HT &
           "movdqu %%xmm0, %0"        & LF & HT &
           "movdqa (%%rax), %%xmm0"   & LF & HT &
           "movdqa 16(%%rax), %%xmm1",
           Outputs  => Unsigned_128'Asm_Output ("+m", Result),
           Inputs   => Unsigned_128'Asm_Input  ("m",  Key),
           Clobber  => "memory,rax",
           Volatile => True);
      return Result;
   end AES_Decrypt_One;

   function AES_Decrypt_Last (Data, Key : Unsigned_128) return Unsigned_128 is
      Result : Unsigned_128 := Data;
   begin
      Asm ("mov        %%rsp, %%rax"      & LF & HT &
           "sub        $32, %%rax"        & LF & HT &
           "and        $~15, %%rax"       & LF & HT &
           "movdqa     %%xmm0, (%%rax)"   & LF & HT &
           "movdqa     %%xmm1, 16(%%rax)" & LF & HT &
           "movdqu     %0, %%xmm0"        & LF & HT &
           "movdqu     %1, %%xmm1"        & LF & HT &
           "aesdeclast %%xmm1, %%xmm0"    & LF & HT &
           "movdqu     %%xmm0, %0"        & LF & HT &
           "movdqa     (%%rax), %%xmm0"   & LF & HT &
           "movdqa     16(%%rax), %%xmm1",
           Outputs  => Unsigned_128'Asm_Output ("+m", Result),
           Inputs   => Unsigned_128'Asm_Input  ("m",  Key),
           Clobber  => "memory,rax",
           Volatile => True);
      return Result;
   end AES_Decrypt_Last;
end Arch.Snippets;
