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
   Semaphore_Locked : constant Binary_Semaphore := True;
   Semaphore_Free   : constant Binary_Semaphore := False;

   procedure Seize (Semaphore : access Binary_Semaphore) is
   begin
      loop
         exit when Try_Seize (Semaphore);
      end loop;
   end Seize;

   procedure Release (Semaphore : access Binary_Semaphore) is
      Write_This : Binary_Semaphore := Semaphore_Free;
   begin
      Asm ("lock; xchg %1, %0",
           Outputs  => Binary_Semaphore'Asm_Output ("+r", Write_This),
           Inputs   => Binary_Semaphore'Asm_Input ("m", Semaphore.all),
           Clobber  => "memory",
           Volatile => True);
   end Release;

   function Try_Seize (Semaphore : access Binary_Semaphore) return Boolean is
      If_This    : Binary_Semaphore          := Semaphore_Free;
      Write_This : constant Binary_Semaphore := Semaphore_Locked;
      Did_Change : Boolean                   := False;
   begin
      Asm ("lock; cmpxchg %3, %1; setz %2",
           Outputs  => (Binary_Semaphore'Asm_Output ("+a", If_This),
                        Binary_Semaphore'Asm_Output ("+m", Semaphore.all),
                        Boolean'Asm_Output          ("=r", Did_Change)),
           Inputs   => Binary_Semaphore'Asm_Input ("r",  Write_This),
           Clobber  => "memory, cc",
           Volatile => True);
      return Did_Change;
   end Try_Seize;
end Lib.Synchronization;
