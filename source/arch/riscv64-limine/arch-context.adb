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

package body Arch.Context is
   procedure Init_GP_Context
      (Ctx        : out GP_Context;
       Stack      : System.Address;
       Start_Addr : System.Address)
   is
      pragma Unreferenced (Stack);
      pragma Unreferenced (Start_Addr);
   begin
      Ctx := (others => 0);
   end Init_GP_Context;

   procedure Load_GP_Context (Ctx : GP_Context) is
      pragma Unreferenced (Ctx);
   begin
      loop
         null;
      end loop;
   end Load_GP_Context;

   procedure Save_Core_Context (Ctx : out Core_Context) is
   begin
      Ctx := 0;
   end Save_Core_Context;

   procedure Success_Fork_Result (Ctx : in out GP_Context) is
   begin
      Ctx := (others => 0);
   end Success_Fork_Result;

   procedure Init_FP_Context (Ctx : out FP_Context) is
   begin
      Ctx := (others => 0);
   end Init_FP_Context;

   procedure Save_FP_Context (Ctx : in out FP_Context) is
   begin
      Ctx := (others => 0);
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
