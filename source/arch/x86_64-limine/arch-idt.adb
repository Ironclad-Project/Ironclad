--  arch-idt.adb: IDT driver.
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

with Interfaces;              use Interfaces;
with System;                  use System;
with System.Machine_Code;     use System.Machine_Code;
with Arch.APIC;
with Arch.GDT;
with Arch.Interrupts;

package body Arch.IDT is
   --  Records for the GDT structure and its entries.
   Gate_Type_Interrupt : constant := 16#E#;
   Gate_Type_Trap      : constant := 16#F#;

   type Gate_Value is mod 2 ** 4;
   type DPL_Value  is mod 2 ** 2;

   type IDT_Entry is record
      Offset_Low  : Unsigned_16;
      Selector    : Unsigned_16;
      IST         : IST_Index;
      Gate_Type   : Gate_Value;
      Zero        : Boolean;
      DPL         : DPL_Value;
      Present     : Boolean;
      Offset_Mid  : Unsigned_16;
      Offset_High : Unsigned_32;
      Reserved_2  : Unsigned_32;
   end record;
   for IDT_Entry use record
      Offset_Low  at 0 range  0 ..  15;
      Selector    at 0 range 16 ..  31;
      IST         at 0 range 32 ..  39;
      Gate_Type   at 0 range 40 ..  43;
      Zero        at 0 range 44 ..  44;
      DPL         at 0 range 45 ..  46;
      Present     at 0 range 47 ..  47;
      Offset_Mid  at 0 range 48 ..  63;
      Offset_High at 0 range 64 ..  95;
      Reserved_2  at 0 range 96 .. 127;
   end record;
   for IDT_Entry'Size use 128;

   type IDT_Pointer is record
      Size    : Unsigned_16;
      Address : System.Address;
   end record;
   for IDT_Pointer use record
      Size    at 0 range  0 .. 15;
      Address at 0 range 16 .. 79;
   end record;
   for IDT_Pointer'Size use 80;

   --  Global variables for the user IDT and its pointer.
   Global_IDT     : array (IDT_Index) of IDT_Entry;
   Global_Pointer : IDT_Pointer;

   --  Thunk list and the isr table, the thunk list containts a list of thunks
   --  to be loaded to the idt that directly call the elements in isr_table.
   type Addr_List is array (IDT_Index) of System.Address with Pack;
   Thunk_List : Addr_List with Import, External_Name => "interrupt_thunk_list";
   ISR_Table  : Addr_List with Export, External_Name => "isr_table";

   procedure Init is
   begin
      --  Load the thunks into the actual IDT and default handlers in the
      --  ISR list.
      for I in IDT_Index loop
         Load_IDT_ISR (I, Thunk_List (I));
         Load_ISR (I, Interrupts.Default_ISR_Handler'Address);
      end loop;

      --  Load exceptions.
      for I in IDT_Index (1) .. IDT_Index (31) loop
         Load_ISR (I, Interrupts.Exception_Handler'Address);
      end loop;

      --  Some special entries for several hardcoded hardware and syscalls.
      Load_ISR (Interrupts.Scheduler_Interrupt,
                Interrupts.Scheduler_Handler'Address);
      Load_ISR (APIC.LAPIC_Spurious_Entry,
                Interrupts.Spurious_Handler'Address);
      Load_ISR (Interrupts.Panic_Interrupt,
                Interrupts.Panic_Handler'Address);
      Load_ISR (Interrupts.Invalidate_Interrupt,
                Interrupts.Invalidate_Handler'Address);

      --  Prepare the pointer and load the IDT.
      Global_Pointer := ((Global_IDT'Size / 8) - 1, Global_IDT'Address);
      Load_IDT;
   end Init;

   procedure Load_IDT is
   begin
      Asm ("lidt %0",
           Inputs   => IDT_Pointer'Asm_Input ("m", Global_Pointer),
           Clobber  => "memory",
           Volatile => True);
   end Load_IDT;

   procedure Load_ISR
      (Index      : IDT_Index;
       Address    : System.Address;
       Gate_Type  : Gate := Gate_Interrupt;
       Allow_User : Boolean := False) is
   begin
      ISR_Table (Index) := Address;

      case Gate_Type is
         when Gate_Trap =>
            Global_IDT (Index).Gate_Type := Gate_Type_Trap;
         when Gate_Interrupt =>
            Global_IDT (Index).Gate_Type := Gate_Type_Interrupt;
      end case;

      if Allow_User then
         Global_IDT (Index).DPL := 3;
      else
         Global_IDT (Index).DPL := 0;
      end if;
   end Load_ISR;

   procedure Load_ISR
      (Address    : System.Address;
       Index      : out IRQ_Index;
       Success    : out Boolean;
       Gate_Type  : Gate := Gate_Interrupt;
       Allow_User : Boolean := False)
   is
   begin
      --  Allocate an interrupt in the IRQ region.
      for I in IRQ_Index loop
         if ISR_Table (I) = Interrupts.Default_ISR_Handler'Address then
            Index := I;
            Load_ISR (I, Address, Gate_Type, Allow_User);
            Success := True;
            return;
         end if;
      end loop;

      Index   := IRQ_Index'First;
      Success := False;
   end Load_ISR;

   procedure Unload_ISR (Index : IDT_Index) is
   begin
      ISR_Table (Index) := Interrupts.Default_ISR_Handler'Address;
   end Unload_ISR;

   procedure Load_IDT_ISR (Index : IDT_Index; Address : System.Address) is
      Addr   : constant Unsigned_64 := Unsigned_64 (To_Integer (Address));
      Low16  : constant Unsigned_64 := Addr                   and 16#FFFF#;
      Mid16  : constant Unsigned_64 := Shift_Right (Addr, 16) and 16#FFFF#;
      High32 : constant Unsigned_64 := Shift_Right (Addr, 32) and 16#FFFFFFFF#;
   begin
      Global_IDT (Index) := (
         Offset_Low  => Unsigned_16 (Low16),
         Selector    => GDT.Kernel_Code64_Segment,
         IST         => 0,
         Gate_Type   => Gate_Type_Interrupt,
         Zero        => False,
         DPL         => 0,
         Present     => True,
         Offset_Mid  => Unsigned_16 (Mid16),
         Offset_High => Unsigned_32 (High32),
         Reserved_2  => 0
      );
   end Load_IDT_ISR;
end Arch.IDT;
