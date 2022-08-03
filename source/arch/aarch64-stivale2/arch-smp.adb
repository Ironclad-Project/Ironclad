--  arch-smp.adb: SMP initialization.
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
with System.Machine_Code;
with Arch.MMU;
with Arch.InnerMMU;
with Lib.Panic;

package body Arch.SMP with SPARK_Mode => Off is
   procedure Init_Cores (SMP_Info : access Stivale2.SMP_Tag) is
      type Stack is array (1 .. 32768) of Unsigned_8;
      type Stack_Acc is access Stack;
      New_Sk : Stack_Acc;
   begin
      --  Check whether we have several CPUs to init.
      if SMP_Info = null then
         Core_Count  := 1;
         Core_Locals := new Core_Local_Arr (1 .. 1);
         Init_Common (1);
      else
         Core_Count  := SMP_Info.Entries'Length;
         Core_Locals := new Core_Local_Arr (SMP_Info.Entries'Range);
         Init_Common (1);
         for I in SMP_Info.Entries'Range loop
            New_Sk := new Stack;
            SMP_Info.Entries (I).Extra_Argument := Unsigned_64 (I);
            SMP_Info.Entries (I).Target_Stack := New_Sk (New_Sk'Last)'Address;
            SMP_Info.Entries (I).Goto_Address := Init_Core'Address;
         end loop;
      end if;
   end Init_Cores;

   function Get_Local return not null Core_Local_Acc is
      Ret : Core_Local_Acc;
   begin
      System.Machine_Code.Asm
         ("mrs %0, tpidr_el1",
          Outputs  => Core_Local_Acc'Asm_Output ("=r", Ret),
          Volatile => True);
      return Ret;
   end Get_Local;

   procedure Init_Core (Core_Info : access Arch.Stivale2.SMP_Core) is
   begin
      --  Initialize MMU.
      InnerMMU.Set_MMU_State;
      if not MMU.Make_Active (MMU.Kernel_Table) then
         Lib.Panic.Soft_Panic ("Could not set core map active");
      end if;

      Init_Common (Positive (Core_Info.Extra_Argument));
      Scheduler.Idle_Core;
   end Init_Core;

   procedure Init_Common (Core_Number : Positive) is
      Addr  : constant System.Address := Core_Locals (Core_Number)'Address;
   begin
      --  Initialize the FPU by setting the instructions to not trap.
      System.Machine_Code.Asm
         ("msr cpacr_el1, %0",
          Inputs   => Unsigned_64'Asm_Input ("r", Shift_Left (2#11#, 20)),
          Volatile => True);

      --  Prepare the core local structure and set it to TLS.
      Core_Locals (Core_Number).Self   := Core_Locals (Core_Number)'Access;
      Core_Locals (Core_Number).Number := Core_Number;
      System.Machine_Code.Asm
         ("msr tpidr_el1, %0",
          Inputs   => System.Address'Asm_Input ("r", Addr),
          Volatile => True);
   end Init_Common;
end Arch.SMP;
