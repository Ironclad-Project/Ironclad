--  arch-context.ads: Architecture-specific context switching.
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
with Arch.Interrupts;
package Arch.Context with SPARK_Mode => Off is
   --  General-purpose context switching.
   subtype GP_Context is Arch.Interrupts.ISR_GPRs;
   type GP_Context_Acc is access all GP_Context;
   procedure Init_GP_Context
      (Ctx        : GP_Context_Acc;
       Stack      : System.Address;
       Start_Addr : System.Address);
   procedure Load_GP_Context (Ctx : GP_Context_Acc) with No_Return;

   --  Save and restore floating-point context.
   type FP_Context is array (1 .. 512) of Unsigned_8 with Alignment => 16;
   type FP_Context_Acc is access all FP_Context;
   procedure Init_FP_Context (Ctx : FP_Context_Acc);
   procedure Save_FP_Context (Ctx : FP_Context_Acc);
   procedure Load_FP_Context (Ctx : FP_Context_Acc);
end Arch.Context;
