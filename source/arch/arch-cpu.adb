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


with Arch.APIC;
with Arch.GDT;
with Arch.IDT;
with Arch.Wrappers;
with Memory.Virtual;
with Scheduler;
with System.Storage_Elements; use System.Storage_Elements;

package body Arch.CPU is
   procedure Init_Cores (SMP_Info : access Arch.Stivale2.SMP_Tag) is
   begin
      --  Initialize the LAPIC list, and fill the BSP fields.
      Core_Count  := SMP_Info.Entries'Length;
      Core_LAPICs := new LAPIC_Array (SMP_Info.Entries'Range);
      Core_LAPICs (1) := SMP_Info.BSP_LAPIC_ID;

      Init_Common;
      Arch.Wrappers.Write_GS        (1);
      Arch.Wrappers.Write_Kernel_GS (1);

      --  Initialize the rest of cores.
      --  Stivale2 guarantees that all the cores in its tag are alive, so
      --  we dont need to check which are actually answering.
      for I in SMP_Info.Entries'Range loop
         declare
            Stack_Size : constant := 32768;
            type Stack is array (1 .. Stack_Size) of Unsigned_8;
            type Stack_Acc is access Stack;
            New_Stack : constant Stack_Acc := new Stack;
         begin
            SMP_Info.Entries (I).Extra_Argument := Unsigned_64 (I);
            SMP_Info.Entries (I).Target_Stack :=
               To_Integer (New_Stack.all'Address);
            SMP_Info.Entries (I).Goto_Address :=
               To_Integer (Init_Core'Address);
         end;
      end loop;
   end Init_Cores;

   function Get_Core_Number return Positive is
   begin
      --  We store the core number on kernel space in GS.
      return Positive (Arch.Wrappers.Read_GS);
   end Get_Core_Number;

   procedure Init_Core (Core_Info : access Arch.Stivale2.SMP_Core) is
   begin
      --  Load the global GDT, IDT, mappings, and LAPIC.
      Arch.GDT.Load_GDT;
      Arch.IDT.Load_IDT;
      Memory.Virtual.Make_Active (Memory.Virtual.Kernel_Map);
      Arch.APIC.Init_LAPIC;

      --  Load several goodies.
      Init_Common;

      --  Initialize things depending on the core number.
      Arch.Wrappers.Write_GS        (Core_Info.Extra_Argument);
      Arch.Wrappers.Write_Kernel_GS (Core_Info.Extra_Argument);
      Core_LAPICs (Integer (Core_Info.Extra_Argument)) := Core_Info.LAPIC_ID;

      --  Send the core to idle, waiting for the scheduler to tell it to do
      --  something, from here, we lose control. Fairwell, core.
      Scheduler.Idle_Core;
   end Init_Core;

   --  The BSP already has some facilities initialized, so we have to take
   --  that into account when compared with other cores.
   --  This function enables things common to BSP and other cores.
   procedure Init_Common is
      CR0 : Unsigned_64 := Arch.Wrappers.Read_CR0;
      CR4 : Unsigned_64 := Arch.Wrappers.Read_CR4;
   begin
      --  Enable SSE/2.
      CR0 := (CR0 and (not Shift_Left (1, 2))) or Shift_Left (1, 1);
      CR4 := CR4 or Shift_Left (3, 9);
      Arch.Wrappers.Write_CR0 (CR0);
      Arch.Wrappers.Write_CR4 (CR4);
   end Init_Common;
end Arch.CPU;
