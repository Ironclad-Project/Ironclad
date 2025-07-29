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

package body Arch.Context is
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

   procedure Load_GP_Context (Ctx : GP_Context) is
   begin
      System.Machine_Code.Asm
         ("mv      sp, %0;" &

          "ld      t0, 248(sp);" &
          "csrw    sstatus, t0;" &
          "ld      t0, 256(sp);" &
          "csrw    sepc, t0;"    &

          "ld      x1, 0(sp);"   &
          "ld      x3, 16(sp);"  &
          "ld      x4, 24(sp);"  &
          "ld      x5, 32(sp);"  &
          "ld      x6, 40(sp);"  &
          "ld      x7, 48(sp);"  &
          "ld      x8, 56(sp);"  &
          "ld      x9, 64(sp);"  &
          "ld     x10, 72(sp);"  &
          "ld     x11, 80(sp);"  &
          "ld     x12, 88(sp);"  &
          "ld     x13, 96(sp);"  &
          "ld     x14, 104(sp);" &
          "ld     x15, 112(sp);" &
          "ld     x16, 120(sp);" &
          "ld     x17, 128(sp);" &
          "ld     x18, 136(sp);" &
          "ld     x19, 144(sp);" &
          "ld     x20, 152(sp);" &
          "ld     x21, 160(sp);" &
          "ld     x22, 168(sp);" &
          "ld     x23, 176(sp);" &
          "ld     x24, 184(sp);" &
          "ld     x25, 192(sp);" &
          "ld     x26, 200(sp);" &
          "ld     x27, 208(sp);" &
          "ld     x28, 216(sp);" &
          "ld     x29, 224(sp);" &
          "ld     x30, 232(sp);" &
          "ld     x31, 240(sp);" &

          "ld      x2, 8(sp);"  &
          "sret",
          Inputs   => System.Address'Asm_Input ("r", Ctx'Address),
          Clobber  => "memory",
          Volatile => True);
      loop null; end loop;
   end Load_GP_Context;

   procedure Save_Core_Context (Ctx : out Core_Context) is
   begin
      Ctx := 0;
   end Save_Core_Context;

   procedure Success_Fork_Result (Ctx : in out GP_Context) is
   begin
      Ctx.X10 := 0;
   end Success_Fork_Result;

   procedure Init_FP_Context (Ctx : out FP_Context) is
   begin
      Ctx := [others => 0];
   end Init_FP_Context;

   procedure Save_FP_Context (Ctx : in out FP_Context) is
   begin
      Ctx := [others => 0];
   end Save_FP_Context;

   procedure Load_FP_Context (Ctx : FP_Context) is
      pragma Unreferenced (Ctx);
   begin
      null;
   end Load_FP_Context;

   procedure Destroy_FP_Context (Ctx : in out FP_Context) is
      pragma Unreferenced (Ctx);
   begin
      null;
   end Destroy_FP_Context;
end Arch.Context;
