--  arch-context.adb: Architecture-specific context switching.
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with System.Machine_Code;    use System.Machine_Code;
with Arch.GDT;
with Arch.CPU;
with Memory.Physical;
with Interfaces.C;

package body Arch.Context is
   procedure Init_GP_Context
      (Ctx        : out GP_Context;
       Stack      : System.Address;
       Start_Addr : System.Address)
   is
   begin
      Ctx := (
         CS     => GDT.User_Code64_Segment or 3,
         SS     => GDT.User_Data64_Segment or 3,
         RFLAGS => 16#202#,
         RIP    => Unsigned_64 (To_Integer (Start_Addr)),
         RSP    => Unsigned_64 (To_Integer (Stack)),
         RBP    => 0,
         others => <>
      );
   end Init_GP_Context;

   procedure Load_GP_Context (Ctx : GP_Context) is
   begin
      Asm (
         "mov %0, %%rsp"   & LF & HT &
         "pop %%rax"       & LF & HT &
         "pop %%rbx"       & LF & HT &
         "pop %%rcx"       & LF & HT &
         "pop %%rdx"       & LF & HT &
         "pop %%rsi"       & LF & HT &
         "pop %%rdi"       & LF & HT &
         "pop %%rbp"       & LF & HT &
         "pop %%r8"        & LF & HT &
         "pop %%r9"        & LF & HT &
         "pop %%r10"       & LF & HT &
         "pop %%r11"       & LF & HT &
         "pop %%r12"       & LF & HT &
         "pop %%r13"       & LF & HT &
         "pop %%r14"       & LF & HT &
         "pop %%r15"       & LF & HT &
         "add $8, %%rsp"   & LF & HT &
         "swapgs"          & LF & HT &
         "iretq",
         Inputs   => System.Address'Asm_Input ("rm", Ctx'Address),
         Clobber  => "memory",
         Volatile => True
      );
      loop null; end loop;
   end Load_GP_Context;

   procedure Save_Core_Context (Ctx : out Core_Context) is
   begin
      Ctx := Arch.CPU.Get_Local.User_Stack;
   end Save_Core_Context;

   procedure Success_Fork_Result (Ctx : in out GP_Context) is
   begin
      Ctx.RAX := 0;
   end Success_Fork_Result;

   procedure Init_FP_Context (Ctx : out FP_Context) is
      FPU_Word : constant Unsigned_32 := 2#1100111111#;
      MXCSR    : constant Unsigned_64 := 2#1111110000000#;
   begin
      --  Set up FPU control word and MXCSR as defined by SysV.
      Asm ("fldcw %0",
           Inputs   => Unsigned_32'Asm_Input ("m", FPU_Word),
           Clobber  => "memory",
           Volatile => True);
      Asm ("ldmxcsr %0",
           Inputs   => Unsigned_64'Asm_Input ("m", MXCSR),
           Clobber  => "memory",
           Volatile => True);

      --  Allocate a block as big as needed and just set the context to that.
      Ctx := To_Address (Memory.Physical.Alloc
         (Interfaces.C.size_t (FPU_Area_Size)));

      --  Save the current context with the control words and all.
      Save_FP_Context (Ctx);
   end Init_FP_Context;

   procedure Save_FP_Context (Ctx : in out FP_Context) is
   begin
      Save_Access (Ctx);
   end Save_FP_Context;

   procedure Load_FP_Context (Ctx : FP_Context) is
   begin
      Rstor_Access (Ctx);
   end Load_FP_Context;

   procedure Destroy_FP_Context (Ctx : in out FP_Context) is
   begin
      Memory.Physical.Free (Interfaces.C.size_t (To_Integer (Ctx)));
      Ctx := System.Null_Address;
   end Destroy_FP_Context;
   ----------------------------------------------------------------------------
   procedure Setup_XSAVE (Use_XSAVE : Boolean; Area_Size : Unsigned_32) is
   begin
      FPU_Area_Size := Area_Size;
      if Use_XSAVE then
         Save_Access  := Save_XSAVE'Access;
         Rstor_Access := Load_XRSTOR'Access;
      else
         Save_Access  := Save_FXSAVE'Access;
         Rstor_Access := Load_FXSTOR'Access;
      end if;
   end Setup_XSAVE;

   procedure Save_FXSAVE (Ctx : in out System.Address) is
   begin
      Asm ("fxsave (%0)",
           Inputs   => System.Address'Asm_Input ("r", Ctx),
           Clobber  => "memory",
           Volatile => True);
   end Save_FXSAVE;

   procedure Save_XSAVE  (Ctx : in out System.Address) is
   begin
      Asm ("xsave (%0)",
           Inputs   => (System.Address'Asm_Input ("r", Ctx),
                        Unsigned_32'Asm_Input ("a", 16#FFFFFFFF#),
                        Unsigned_32'Asm_Input ("d", 16#FFFFFFFF#)),
           Clobber  => "memory",
           Volatile => True);
   end Save_XSAVE;

   procedure Load_FXSTOR (Ctx : System.Address) is
   begin
      Asm ("fxrstor (%0)",
           Inputs   => System.Address'Asm_Input ("r", Ctx),
           Clobber  => "memory",
           Volatile => True);
   end Load_FXSTOR;

   procedure Load_XRSTOR (Ctx : System.Address) is
   begin
      Asm ("xrstor (%0)",
           Inputs   => (System.Address'Asm_Input ("r", Ctx),
                        Unsigned_32'Asm_Input ("a", 16#FFFFFFFF#),
                        Unsigned_32'Asm_Input ("d", 16#FFFFFFFF#)),
           Clobber  => "memory",
           Volatile => True);
   end Load_XRSTOR;
end Arch.Context;
