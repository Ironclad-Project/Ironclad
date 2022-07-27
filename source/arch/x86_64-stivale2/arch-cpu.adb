--  arch-cpu.adb: CPU management routines.
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
with Arch.APIC;
with Arch.IDT;
with Arch.Wrappers;
with Memory.Virtual;

package body Arch.CPU with SPARK_Mode => Off is
   procedure Init_Cores (SMP_Info : access Arch.Stivale2.SMP_Tag) is
      type Stack is array (1 .. 32768) of Unsigned_8;
      type Stack_Acc is access Stack;
      New_Stk : Stack_Acc;
   begin
      --  Initialize the locals list, and fill the BSP fields.
      Core_Count  := SMP_Info.Entries'Length;
      Core_Locals := new Core_Local_Arr (SMP_Info.Entries'Range);
      Init_Common (1, SMP_Info.BSP_LAPIC_ID);

      --  Initialize the rest of cores.
      --  Stivale2 guarantees that all the cores in its tag are alive, so
      --  we dont need to check which are actually answering.
      for I in SMP_Info.Entries'Range loop
         New_Stk := new Stack;
         SMP_Info.Entries (I).Extra_Argument := Unsigned_64 (I);
         SMP_Info.Entries (I).Target_Stack   := New_Stk (New_Stk'Last)'Address;
         SMP_Info.Entries (I).Goto_Address   := Init_Core'Address;
      end loop;
   end Init_Cores;

   function Get_Local return not null Core_Local_Acc is
      Locals : Core_Local_Acc;
   begin
      --  XXX: We are making the guarantee this can never be null, which it
      --  can if the scheduler does not swap gs correctly.
      Asm ("mov %%gs:0, %0",
           Outputs  => Core_Local_Acc'Asm_Output ("=a", Locals),
           Volatile => True);
      return Locals;
   end Get_Local;

   procedure Init_Core (Core_Info : access Arch.Stivale2.SMP_Core) is
      Discard : Boolean;
   begin
      --  Load the global GDT, IDT, mappings, and LAPIC.
      GDT.Load_GDT;
      IDT.Load_IDT;
      Discard := Memory.Virtual.Make_Active (Memory.Virtual.Kernel_Map);
      APIC.Init_LAPIC;

      --  Load several goodies.
      Init_Common (Positive (Core_Info.Extra_Argument), Core_Info.LAPIC_ID);

      --  Send the core to idle, waiting for the scheduler to tell it to do
      --  something, from here, we lose control. Fairwell, core.
      Scheduler.Idle_Core;
   end Init_Core;

   --  The BSP already has some facilities initialized, so we have to take
   --  that into account when compared with other cores.
   --  This function enables things common to BSP and other cores.
   procedure Init_Common (Core_Number : Positive; LAPIC : Unsigned_32) is
      PAT_MSR : constant := 16#277#;

      CR0 : Unsigned_64 := Wrappers.Read_CR0;
      CR4 : Unsigned_64 := Wrappers.Read_CR4;
      PAT : Unsigned_64 := Wrappers.Read_MSR (PAT_MSR);

      Locals_Addr : constant Unsigned_64 :=
         Unsigned_64 (To_Integer (Core_Locals (Core_Number)'Address));
   begin
      --  Enable SSE/2.
      CR0 := (CR0 and (not Shift_Left (1, 2))) or Shift_Left (1, 1);
      CR4 := CR4 or Shift_Left (3, 9);
      Wrappers.Write_CR0 (CR0);
      Wrappers.Write_CR4 (CR4);

      --  Prepare the core local structure and set it in GS.
      Core_Locals (Core_Number).Self     := Core_Locals (Core_Number)'Access;
      Core_Locals (Core_Number).Number   := Core_Number;
      Core_Locals (Core_Number).LAPIC_ID := LAPIC;
      Core_Locals (Core_Number).LAPIC_Timer_Hz :=
         Arch.APIC.LAPIC_Timer_Calibrate;
      Wrappers.Write_GS        (Locals_Addr);
      Wrappers.Write_Kernel_GS (Locals_Addr);

      --  Initialise the PAT (write-protect / write-combining).
      PAT := PAT and (16#FFFFFFFF#);
      PAT := PAT or  Shift_Left (Unsigned_64 (16#0105#), 32);
      Wrappers.Write_MSR (PAT_MSR, PAT);

      --  Load the TSS.
      GDT.Load_TSS (Core_Locals (Core_Number).Core_TSS'Address);
   end Init_Common;
end Arch.CPU;
