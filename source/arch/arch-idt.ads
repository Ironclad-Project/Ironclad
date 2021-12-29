--  arch-idt.ads: Specification of the IDT package.
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

with System;

package Arch.IDT is
   --  Initialize the global IDT and load it on the callee core.
   procedure Init;

   --  Load the IDT in the callee core.
   procedure Load_IDT;

   --  Load an ISR into the IDT.
   type IST_Index is range 0 .. 7;
   procedure Load_ISR
      (Index   : Integer;
       Address : System.Address;
       IST     : IST_Index);
end Arch.IDT;
