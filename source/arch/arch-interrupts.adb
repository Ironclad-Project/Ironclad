--  arch-interrupts.adb: Setup and management of interrupts.
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

with System.Machine_Code;
with Lib.Messages;

package body Arch.Interrupts is
   procedure DE_Handler is begin Lib.Messages.Panic ("#DE"); end DE_Handler;
   procedure DB_Handler is begin Lib.Messages.Panic ("#DB"); end DB_Handler;
   procedure BP_Handler is begin Lib.Messages.Panic ("#BP"); end BP_Handler;
   procedure OF_Handler is begin Lib.Messages.Panic ("#OF"); end OF_Handler;
   procedure BR_Handler is begin Lib.Messages.Panic ("#BR"); end BR_Handler;
   procedure UD_Handler is begin Lib.Messages.Panic ("#UD"); end UD_Handler;
   procedure NM_Handler is begin Lib.Messages.Panic ("#NM"); end NM_Handler;
   procedure DF_Handler is begin Lib.Messages.Panic ("#DF"); end DF_Handler;
   procedure TS_Handler is begin Lib.Messages.Panic ("#TS"); end TS_Handler;
   procedure NP_Handler is begin Lib.Messages.Panic ("#NP"); end NP_Handler;
   procedure SS_Handler is begin Lib.Messages.Panic ("#SS"); end SS_Handler;
   procedure GP_Handler is begin Lib.Messages.Panic ("#GP"); end GP_Handler;
   procedure PF_Handler is begin Lib.Messages.Panic ("#PF"); end PF_Handler;
   procedure MF_Handler is begin Lib.Messages.Panic ("#MF"); end MF_Handler;
   procedure AC_Handler is begin Lib.Messages.Panic ("#AC"); end AC_Handler;
   procedure MC_Handler is begin Lib.Messages.Panic ("#MC"); end MC_Handler;
   procedure XM_Handler is begin Lib.Messages.Panic ("#XM"); end XM_Handler;
   procedure VE_Handler is begin Lib.Messages.Panic ("#VE"); end VE_Handler;
   procedure CP_Handler is begin Lib.Messages.Panic ("#CP"); end CP_Handler;
   procedure HV_Handler is begin Lib.Messages.Panic ("#HV"); end HV_Handler;
   procedure VC_Handler is begin Lib.Messages.Panic ("#VC"); end VC_Handler;
   procedure SX_Handler is begin Lib.Messages.Panic ("#SX"); end SX_Handler;

   procedure Set_Interrupt_Flag (Enable : Boolean) is
   begin
      if Enable then
         System.Machine_Code.Asm ("sti", Volatile => True);
      else
         System.Machine_Code.Asm ("cli", Volatile => True);
      end if;
   end Set_Interrupt_Flag;
end Arch.Interrupts;
