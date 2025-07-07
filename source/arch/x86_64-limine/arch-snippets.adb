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

with System.Machine_Code; use System.Machine_Code;
with Arch.Entrypoint;
with Arch.CPU;
pragma Unreferenced (Arch.Entrypoint);

package body Arch.Snippets is
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

   function Interrupts_Enabled return Boolean is
      Flags : Unsigned_64;
   begin
      Asm ("pushfq; popq %0",
           Outputs  => Unsigned_64'Asm_Output ("=r", Flags),
           Volatile => True);
      return (Flags and 16#200#) /= 0;
   end Interrupts_Enabled;

   procedure Pause is
   begin
      Asm ("pause", Volatile => True);
   end Pause;

   procedure Enable_Userland_Memory_Access is
   begin
      if CPU.Global_Use_SMAP then
         Asm ("stac", Clobber => "cc", Volatile => True);
      end if;
   end Enable_Userland_Memory_Access;

   procedure Disable_Userland_Memory_Access is
   begin
      if CPU.Global_Use_SMAP then
         Asm ("clac", Clobber => "cc", Volatile => True);
      end if;
   end Disable_Userland_Memory_Access;
   ----------------------------------------------------------------------------
   procedure Port_Out (Port : Unsigned_16; Value : Unsigned_8) is
   begin
      Asm ("outb %0, %1",
           Inputs   => [Unsigned_8'Asm_Input  ("a",  Value),
                        Unsigned_16'Asm_Input ("Nd", Port)],
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
           Inputs   => [Unsigned_16'Asm_Input ("a",  Value),
                        Unsigned_16'Asm_Input ("Nd", Port)],
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
           Inputs   => [Unsigned_32'Asm_Input ("a",  Value),
                        Unsigned_16'Asm_Input ("Nd", Port)],
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
           Outputs  => [Unsigned_32'Asm_Output ("=a", Res_Low),
                        Unsigned_32'Asm_Output ("=d", Res_High)],
           Inputs   => Unsigned_32'Asm_Input  ("c", MSR),
           Clobber  => "memory",
           Volatile => True);
      return Shift_Left (Unsigned_64 (Res_High), 32) or Unsigned_64 (Res_Low);
   end Read_MSR;

   procedure Write_MSR (MSR : Unsigned_32; Value : Unsigned_64) is
      Value_Hi, Value_Lo : Unsigned_32;
   begin
      Value_Hi := Unsigned_32 (Shift_Right (Value, 32));
      Value_Lo := Unsigned_32 (Value and 16#FFFFFFFF#);
      Asm ("wrmsr",
           Inputs   => [Unsigned_32'Asm_Input ("a", Value_Lo),
                        Unsigned_32'Asm_Input ("d", Value_Hi),
                        Unsigned_32'Asm_Input ("c", MSR)],
           Clobber  => "memory",
           Volatile => True);
   exception
      when Constraint_Error =>
         null;
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

   procedure Write_XCR (Register : Unsigned_32; Value : Unsigned_64) is
      Lo_32, Hi_32 : Unsigned_64;
   begin
      Lo_32 := Value and 16#FFFFFFFF#;
      Hi_32 := Shift_Right (Value, 32) and 16#FFFFFFFF#;
      Asm ("xsetbv",
           Inputs   => [Unsigned_32'Asm_Input ("a", Unsigned_32 (Lo_32)),
                        Unsigned_32'Asm_Input ("d", Unsigned_32 (Hi_32)),
                        Unsigned_32'Asm_Input ("c", Register)],
           Clobber  => "memory",
           Volatile => True);
   exception
      when Constraint_Error =>
         null;
   end Write_XCR;

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

   procedure Invalidate_Caches is
   begin
      Asm ("wbinvd", Volatile => True);
   end Invalidate_Caches;

   procedure Get_CPUID
      (Leaf    : Unsigned_32;
       Subleaf : Unsigned_32;
       EAX     : out Unsigned_32;
       EBX     : out Unsigned_32;
       ECX     : out Unsigned_32;
       EDX     : out Unsigned_32;
       Success : out Boolean)
   is
      CPUID_Max : Unsigned_32;
   begin
      Asm ("cpuid",
           Outputs  => Unsigned_32'Asm_Output ("=a", CPUID_Max),
           Inputs   => Unsigned_32'Asm_Input ("a", Leaf and 16#80000000#),
           Clobber  => "memory,ebx,ecx,edx",
           Volatile => True);

      Success := Leaf <= CPUID_Max;
      if Success then
         Asm ("cpuid",
              Outputs  => [Unsigned_32'Asm_Output ("=a", EAX),
                           Unsigned_32'Asm_Output ("=b", EBX),
                           Unsigned_32'Asm_Output ("=c", ECX),
                           Unsigned_32'Asm_Output ("=d", EDX)],
              Inputs   => [Unsigned_32'Asm_Input ("a", Leaf),
                           Unsigned_32'Asm_Input ("c", Subleaf)],
              Clobber  => "memory",
              Volatile => True);
      else
         EAX := 0;
         EBX := 0;
         ECX := 0;
         EDX := 0;
      end if;
   end Get_CPUID;

   function Read_TSC return Unsigned_64 is
      Low32, High32 : Unsigned_32;
   begin
      --  Quoting Linux (arch/x86/include/asm/msr.h):
      --
      --  The RDTSC instruction is not ordered relative to memory
      --  access.  The Intel SDM and the AMD APM are both vague on this
      --  point, but empirically an RDTSC instruction can be
      --  speculatively executed before prior loads.  An RDTSC
      --  immediately after an appropriate barrier appears to be
      --  ordered as a normal load, that is, it provides the same
      --  ordering guarantees as reading from a global memory location
      --  that some other imaginary CPU is updating continuously with a
      --  time stamp. So we LFENCE.
      Asm ("lfence; rdtsc",
           Outputs  =>
            [Unsigned_32'Asm_Output ("=a", Low32),
             Unsigned_32'Asm_Output ("=d", High32)],
           Clobber  => "memory",
           Volatile => True);
      return Shift_Left (Unsigned_64 (High32), 32) or Unsigned_64 (Low32);
   end Read_TSC;
end Arch.Snippets;
