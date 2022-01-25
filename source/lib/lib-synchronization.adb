--  lib-synchronization.adb: Synchronization primitives and such.
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
with System.Machine_Code; use System.Machine_Code;

package body Lib.Synchronization is
   Semaphore_Locked : constant Integer := 1;
   Semaphore_Free   : constant Integer := 0;

   procedure Seize (Semaphore : access Binary_Semaphore) is
   begin
      loop
         exit when Try_Seize (Semaphore);
      end loop;
   end Seize;

   procedure Release (Semaphore : access Binary_Semaphore) is
      Write_This : Integer                 := Semaphore_Free;
      In_Here    : constant System.Address := Semaphore.all'Address;
   begin
      Asm ("lock; xchg (%1), %0",
           Outputs  => Integer'Asm_Output       ("+r", Write_This),
           Inputs   => System.Address'Asm_Input ("r", In_Here),
           Clobber  => "memory",
           Volatile => True);
   end Release;

   function Try_Seize (Semaphore : access Binary_Semaphore) return Boolean is
      In_Here    : constant System.Address := Semaphore.all'Address;
      If_This    : Integer                 := Semaphore_Free;
      Write_This : constant Integer        := Semaphore_Locked;
      Did_Change : Boolean                 := False;
   begin
      Asm ("lock; cmpxchg %2, (%3)",
           Outputs  => (Integer'Asm_Output       ("+a",    If_This),
                        Boolean'Asm_Output       ("=@ccz", Did_Change)),
           Inputs   => (Integer'Asm_Input        ("r",     Write_This),
                        System.Address'Asm_Input ("r",     In_Here)),
           Clobber  => "memory",
           Volatile => True);
      return Did_Change;
   end Try_Seize;
end Lib.Synchronization;
