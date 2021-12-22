--  arch-interrupts.ads: Specification of interrupt utilities.
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

package Arch.Interrupts is
   --  Handlers for exceptions.
   procedure DE_Handler;
   procedure DB_Handler;
   procedure BP_Handler;
   procedure OF_Handler;
   procedure BR_Handler;
   procedure UD_Handler;
   procedure NM_Handler;
   procedure DF_Handler;
   procedure TS_Handler;
   procedure NP_Handler;
   procedure SS_Handler;
   procedure GP_Handler;
   procedure PF_Handler;
   procedure MF_Handler;
   procedure AC_Handler;
   procedure MC_Handler;
   procedure XM_Handler;
   procedure VE_Handler;
   procedure CP_Handler;
   procedure HV_Handler;
   procedure VC_Handler;
   procedure SX_Handler;

   --  Enable or disable interrupts for the callee core.
   procedure Set_Interrupt_Flag (Enable : Boolean);
end Arch.Interrupts;
