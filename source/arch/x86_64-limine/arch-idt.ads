--  arch-idt.ads: IDT driver.
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

with System;

package Arch.IDT is
   --  Initialize the global IDT and load it on the callee core.
   procedure Init;

   --  Load the IDT in the callee core.
   procedure Load_IDT;

   --  Load and unload an ISR into the IDT, either statically or dynamically.
   type    IST_Index is           range  0 ..   7;
   type    IDT_Index is           range  1 .. 256;
   subtype IRQ_Index is IDT_Index range 33 .. 256;
   type Gate is (Gate_Interrupt, Gate_Trap);

   procedure Load_ISR
      (Index      : IDT_Index;
       Address    : System.Address;
       Gate_Type  : Gate := Gate_Interrupt;
       Allow_User : Boolean := False);
   procedure Load_ISR
      (Address    : System.Address;
       Index      : out IRQ_Index;
       Success    : out Boolean;
       Gate_Type  : Gate := Gate_Interrupt;
       Allow_User : Boolean := False);
   procedure Unload_ISR (Index : IDT_Index);

private

   procedure Load_IDT_ISR (Index : IDT_Index; Address : System.Address);
end Arch.IDT;
