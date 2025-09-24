--  arch-context.ads: Architecture-specific context switching.
--  Copyright (C) 2024 streaksu
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

with System.Machine_Code;

package body Arch.Context with SPARK_Mode => Off is
   procedure Init_GP_Context
      (Ctx        : out GP_Context;
       Stack      : System.Address;
       Start_Addr : System.Address;
       Argument_1 : Unsigned_64 := 0;
       Argument_2 : Unsigned_64 := 0;
       Argument_3 : Unsigned_64 := 0)
   is
      Val : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("csrr %0, sstatus",
          Outputs  => Unsigned_64'Asm_Output ("=r", Val),
          Clobber  => "memory",
          Volatile => True);
      Ctx :=
         (X2      => Unsigned_64 (To_Integer (Stack)),
          X10     => Argument_1,
          X11     => Argument_2,
          X12     => Argument_3,
          SEPC    => Unsigned_64 (To_Integer (Start_Addr)),
          --  Clear SPP to switch to user and force enable interrupts.
          SSTATUS => (Val and not Shift_Left (1, 8)) or 2,
          others  => 0);
   end Init_GP_Context;

   procedure Success_Fork_Result (Ctx : in out GP_Context) is
   begin
      Ctx.X10 := 0;
   end Success_Fork_Result;

   procedure Init_FP_Context (Ctx : out FP_Context) is
   begin
      Ctx := (others => 0);
   end Init_FP_Context;

   procedure Save_FP_Context (Ctx : in out FP_Context) is
   begin
      System.Machine_Code.Asm
         (".option push;"         &
          ".option arch, +d;"     &
          "fsd      f1, 0(%0);"   &
          "fsd      f3, 16(%0);"  &
          "fsd      f4, 24(%0);"  &
          "fsd      f5, 32(%0);"  &
          "fsd      f6, 40(%0);"  &
          "fsd      f7, 48(%0);"  &
          "fsd      f8, 56(%0);"  &
          "fsd      f9, 64(%0);"  &
          "fsd     f10, 72(%0);"  &
          "fsd     f11, 80(%0);"  &
          "fsd     f12, 88(%0);"  &
          "fsd     f13, 96(%0);"  &
          "fsd     f14, 104(%0);" &
          "fsd     f15, 112(%0);" &
          "fsd     f16, 120(%0);" &
          "fsd     f17, 128(%0);" &
          "fsd     f18, 136(%0);" &
          "fsd     f19, 144(%0);" &
          "fsd     f20, 152(%0);" &
          "fsd     f21, 160(%0);" &
          "fsd     f22, 168(%0);" &
          "fsd     f23, 176(%0);" &
          "fsd     f24, 184(%0);" &
          "fsd     f25, 192(%0);" &
          "fsd     f26, 200(%0);" &
          "fsd     f27, 208(%0);" &
          "fsd     f28, 216(%0);" &
          "fsd     f29, 224(%0);" &
          "fsd     f30, 232(%0);" &
          "fsd     f31, 240(%0);" &
          "frcsr   a0;"           &
          "sd      a0, 248(%0);"  &
          ".option pop;",
          Inputs   => System.Address'Asm_Input ("r", Ctx'Address),
          Clobber  => "memory,a0",
          Volatile => True);
   end Save_FP_Context;

   procedure Load_FP_Context (Ctx : FP_Context) is
   begin
      System.Machine_Code.Asm
         (".option push;"         &
          ".option arch, +d;"     &
          "fld      f1, 0(%0);"   &
          "fld      f3, 16(%0);"  &
          "fld      f4, 24(%0);"  &
          "fld      f5, 32(%0);"  &
          "fld      f6, 40(%0);"  &
          "fld      f7, 48(%0);"  &
          "fld      f8, 56(%0);"  &
          "fld      f9, 64(%0);"  &
          "fld     f10, 72(%0);"  &
          "fld     f11, 80(%0);"  &
          "fld     f12, 88(%0);"  &
          "fld     f13, 96(%0);"  &
          "fld     f14, 104(%0);" &
          "fld     f15, 112(%0);" &
          "fld     f16, 120(%0);" &
          "fld     f17, 128(%0);" &
          "fld     f18, 136(%0);" &
          "fld     f19, 144(%0);" &
          "fld     f20, 152(%0);" &
          "fld     f21, 160(%0);" &
          "fld     f22, 168(%0);" &
          "fld     f23, 176(%0);" &
          "fld     f24, 184(%0);" &
          "fld     f25, 192(%0);" &
          "fld     f26, 200(%0);" &
          "fld     f27, 208(%0);" &
          "fld     f28, 216(%0);" &
          "fld     f29, 224(%0);" &
          "fld     f30, 232(%0);" &
          "fld     f31, 240(%0);" &
          "ld       a0, 248(%0);" &
          "fscsr    a0;"          &
          ".option pop;",
          Inputs   => System.Address'Asm_Input ("r", Ctx'Address),
          Clobber  => "memory,a0",
          Volatile => True);
   end Load_FP_Context;

   procedure Destroy_FP_Context (Ctx : in out FP_Context) is
      pragma Unreferenced (Ctx);
   begin
      null;
   end Destroy_FP_Context;
end Arch.Context;
