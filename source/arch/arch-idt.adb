--  arch-idt.adb: Management of the IDT, and registering of callbacks.
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

with Interfaces;              use Interfaces;
with System;                  use System;
with System.Machine_Code;     use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.APIC;
with Arch.GDT;
with Arch.Interrupts;
with Lib.Panic;

package body Arch.IDT is
   --  Records for the GDT structure and its entries.
   Gate_Type_Interrupt : constant := 16#E#;

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
   Thunk_List : array (IDT_Index) of System.Address
      with Import, External_Name => "interrupt_thunk_list";
   ISR_Table : array (IDT_Index) of System.Address
      with Export, External_Name => "isr_table";

   procedure Init is
   begin
      --  Load the thunks into the actual IDT and default handlers in the
      --  ISR list.
      for I in IDT_Index loop
         Load_IDT_ISR (I, Thunk_List (I));
         Load_ISR (I, Interrupts.Default_ISR_Handler'Address);
      end loop;

      --  Load exceptions.
      Load_ISR  (1, Interrupts.DE_Handler'Address);
      Load_ISR  (2, Interrupts.DB_Handler'Address);
      Load_ISR  (4, Interrupts.BP_Handler'Address);
      Load_ISR  (5, Interrupts.OF_Handler'Address);
      Load_ISR  (6, Interrupts.BR_Handler'Address);
      Load_ISR  (7, Interrupts.UD_Handler'Address);
      Load_ISR  (8, Interrupts.NM_Handler'Address);
      Load_ISR  (9, Interrupts.DF_Handler'Address);
      Load_ISR (11, Interrupts.TS_Handler'Address);
      Load_ISR (12, Interrupts.NP_Handler'Address);
      Load_ISR (13, Interrupts.SS_Handler'Address);
      Load_ISR (14, Interrupts.GP_Handler'Address);
      Load_ISR (15, Interrupts.PF_Handler'Address);
      Load_ISR (17, Interrupts.MF_Handler'Address);
      Load_ISR (18, Interrupts.AC_Handler'Address);
      Load_ISR (19, Interrupts.MC_Handler'Address);
      Load_ISR (20, Interrupts.XM_Handler'Address);
      Load_ISR (21, Interrupts.VE_Handler'Address);
      Load_ISR (22, Interrupts.CP_Handler'Address);
      Load_ISR (29, Interrupts.HV_Handler'Address);
      Load_ISR (30, Interrupts.VC_Handler'Address);
      Load_ISR (31, Interrupts.SX_Handler'Address);

      --  Some special entries for several hardcoded hardware.
      Load_ISR (APIC.LAPIC_Spurious_Entry,
                Interrupts.Spurious_Handler'Address);

      Load_ISR (Lib.Panic.Panic_Vector,
                Lib.Panic.Panic_Handler'Address);

      --  Prepare the pointer and load the IDT.
      Global_Pointer := (Global_IDT'Size - 1, Global_IDT'Address);
      Load_IDT;
   end Init;

   procedure Load_IDT is
   begin
      Asm ("lidt %0",
           Inputs   => IDT_Pointer'Asm_Input ("m",  Global_Pointer),
           Clobber  => "memory",
           Volatile => True);
   end Load_IDT;

   procedure Load_ISR (Index : IDT_Index; Address : System.Address) is
   begin
      ISR_Table (Index) := Address;
   end Load_ISR;

   function Load_ISR (Address : System.Address;
                      Index : out IRQ_Index) return Boolean is
   begin
      --  Allocate an interrupt in the IRQ region.
      for I in IRQ_Index loop
         if ISR_Table (I) = Interrupts.Default_ISR_Handler'Address then
            Index := I;
            Load_ISR (I, Address);
            return True;
         end if;
      end loop;

      Index := IRQ_Index'First;
      return False;
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
