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

with System.Machine_Code;    use System.Machine_Code;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

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
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Read_Cycles;
   ----------------------------------------------------------------------------
   function Supports_AES_Accel return Boolean is
      EAX, EBX, ECX, EDX : Unsigned_32;
   begin
      Get_CPUID (1, 0, EAX, EBX, ECX, EDX);
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
   ----------------------------------------------------------------------------
   procedure Port_Out (Port : Unsigned_16; Value : Unsigned_8) is
   begin
      Asm ("outb %0, %1",
           Inputs   => (Unsigned_8'Asm_Input  ("a",  Value),
                        Unsigned_16'Asm_Input ("Nd", Port)),
           Clobber  => "memory",
           Volatile => True);
   end Port_Out;

   function Port_In (Port : Unsigned_16) return Unsigned_8 is
      Value : Unsigned_8;
   begin
      Asm ("inb %1, %0",
           Outputs  => Unsigned_8'Asm_Output ("=a", Value),
           Inputs   => Unsigned_16'Asm_Input ("Nd", Port),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Port_In;

   procedure Port_Out16 (Port, Value : Unsigned_16) is
   begin
      Asm ("outw %0, %1",
           Inputs   => (Unsigned_16'Asm_Input ("a",  Value),
                        Unsigned_16'Asm_Input ("Nd", Port)),
           Clobber  => "memory",
           Volatile => True);
   end Port_Out16;

   function Port_In16 (Port : Unsigned_16) return Unsigned_16 is
      Value : Unsigned_16;
   begin
      Asm ("inw %1, %0",
           Outputs  => Unsigned_16'Asm_Output ("=a", Value),
           Inputs   => Unsigned_16'Asm_Input  ("Nd", Port),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Port_In16;

   procedure Port_Out32 (Port : Unsigned_16; Value : Unsigned_32) is
   begin
      Asm ("out %0, %1",
           Inputs   => (Unsigned_32'Asm_Input ("a",  Value),
                        Unsigned_16'Asm_Input ("Nd", Port)),
           Clobber  => "memory",
           Volatile => True);
   end Port_Out32;

   function Port_In32 (Port : Unsigned_16) return Unsigned_32 is
      Value : Unsigned_32;
   begin
      Asm ("in %1, %0",
           Outputs  => Unsigned_32'Asm_Output ("=a", Value),
           Inputs   => Unsigned_16'Asm_Input  ("Nd", Port),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Port_In32;

   procedure Invalidate_Page (Value : Virtual_Address) is
   begin
      Asm ("invlpg (%0)",
           Inputs   => Virtual_Address'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Invalidate_Page;

   function Read_MSR (MSR : Unsigned_32) return Unsigned_64 is
      Res_High : Unsigned_32;
      Res_Low  : Unsigned_32;
   begin
      Asm ("rdmsr",
           Outputs  => (Unsigned_32'Asm_Output ("=a", Res_Low),
                        Unsigned_32'Asm_Output ("=d", Res_High)),
           Inputs   => Unsigned_32'Asm_Input  ("c", MSR),
           Clobber  => "memory",
           Volatile => True);
      return Shift_Left (Unsigned_64 (Res_High), 32) or Unsigned_64 (Res_Low);
   end Read_MSR;

   procedure Write_MSR (MSR : Unsigned_32; Value : Unsigned_64) is
      Value_Hi : constant Unsigned_32 := Unsigned_32 (Shift_Right (Value, 32));
      Value_Lo : constant Unsigned_32 := Unsigned_32 (Value and 16#FFFFFFFF#);
   begin
      Asm ("wrmsr",
           Inputs   => (Unsigned_32'Asm_Input ("a", Value_Lo),
                        Unsigned_32'Asm_Input ("d", Value_Hi),
                        Unsigned_32'Asm_Input ("c", MSR)),
           Clobber  => "memory",
           Volatile => True);
   end Write_MSR;

   function Read_CR0 return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("mov %%cr0, %0",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_CR0;

   procedure Write_CR0 (Value : Unsigned_64) is
   begin
      Asm ("mov %0, %%cr0",
           Inputs   => Unsigned_64'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Write_CR0;

   function Read_CR2 return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("mov %%cr2, %0",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_CR2;

   function Read_CR3 return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("mov %%cr3, %0",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_CR3;

   procedure Write_CR3 (Value : Unsigned_64) is
   begin
      Asm ("mov %0, %%cr3",
           Inputs   => Unsigned_64'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Write_CR3;

   function Read_CR4 return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("mov %%cr4, %0",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_CR4;

   procedure Write_CR4 (Value : Unsigned_64) is
   begin
      Asm ("mov %0, %%cr4",
           Inputs   => Unsigned_64'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Write_CR4;

   FS_MSR        : constant := 16#C0000100#;
   GS_MSR        : constant := 16#C0000101#;
   Kernel_GS_MSR : constant := 16#C0000102#;

   function Read_FS return Unsigned_64 is
   begin
      return Read_MSR (FS_MSR);
   end Read_FS;

   procedure Write_FS (Value : Unsigned_64) is
   begin
      Write_MSR (FS_MSR, Value);
   end Write_FS;

   function Read_GS return Unsigned_64 is
   begin
      return Read_MSR (GS_MSR);
   end Read_GS;

   procedure Write_GS (Value : Unsigned_64) is
   begin
      Write_MSR (GS_MSR, Value);
   end Write_GS;

   function Read_Kernel_GS return Unsigned_64 is
   begin
      return Read_MSR (Kernel_GS_MSR);
   end Read_Kernel_GS;

   procedure Write_Kernel_GS (Value : Unsigned_64) is
   begin
      Write_MSR (Kernel_GS_MSR, Value);
   end Write_Kernel_GS;

   procedure Swap_GS is
   begin
      Asm ("swapgs", Volatile => True);
   end Swap_GS;

   procedure Get_CPUID
      (Leaf, Subleaf : Unsigned_32;
       EAX, EBX, ECX, EDX : out Unsigned_32)
   is
   begin
      Asm ("cpuid",
           Outputs  => (Unsigned_32'Asm_Output ("=a", EAX),
                        Unsigned_32'Asm_Output ("=b", EBX),
                        Unsigned_32'Asm_Output ("=c", ECX),
                        Unsigned_32'Asm_Output ("=d", EDX)),
           Inputs   => (Unsigned_32'Asm_Input ("a", Leaf),
                        Unsigned_32'Asm_Input ("c", Subleaf)),
           Clobber  => "memory",
           Volatile => True);
   end Get_CPUID;
end Arch.Snippets;
