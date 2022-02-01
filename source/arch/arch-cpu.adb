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

with Interfaces;
with Arch.APIC;
with Arch.GDT;
with Arch.IDT;
with Arch.Wrappers;
with Memory.Virtual;
with System.Storage_Elements; use System.Storage_Elements;

package body Arch.CPU is
   Stack_Size : constant := 32768;

   procedure Init_BSP is
   begin
      --  The BSP already has some facilities initialized, so we have to take
      --  that into account when compared with other cores.
      Arch.Wrappers.Write_GS (1);
   end Init_BSP;

   procedure Init_Cores (SMP_Info : access Arch.Stivale2.SMP_Tag) is
   begin
      --  Take into account the BSP is already there.
      Core_Count := 1;

      --  Initialize the cores.
      for I in SMP_Info.Entries'Range loop
         declare
            type Stack is array (1 .. Stack_Size) of Interfaces.Unsigned_8;
            type Stack_Acc is access Stack;
            New_Stack : constant Stack_Acc := new Stack;
         begin
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
      Memory.Virtual.Make_Active (Memory.Virtual.Kernel_Map.all);
      Arch.APIC.Init_LAPIC;

      --  Greet this core with a core number.
      Core_Count := Core_Count + 1;
      Arch.Wrappers.Write_GS (Interfaces.Unsigned_64 (Core_Count));

      loop null; end loop;
   end Init_Core;
end Arch.CPU;
