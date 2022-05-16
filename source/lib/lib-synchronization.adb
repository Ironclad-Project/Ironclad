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
with Lib.Messages;
with Lib.Panic;
with Arch.Wrappers;

package body Lib.Synchronization is
   Semaphore_Locked : constant Boolean := True;
   Semaphore_Free   : constant Boolean := False;

   procedure Seize (Semaphore : access Binary_Semaphore) is
   begin
      for I in 1 .. 50000000 loop
         if Try_Seize (Semaphore) then
            Semaphore.Caller := Get_Caller_Address (0);
            return;
         end if;
         Arch.Wrappers.Pause;
      end loop;

      Lib.Messages.Put ("Deadlock at address: ");
      Lib.Messages.Put (Semaphore.Caller);
      Lib.Messages.Put_Line ("");
      Lib.Panic.Hard_Panic ("Deadlocked!");
   end Seize;

   procedure Release (Semaphore : access Binary_Semaphore) is
      Write_This : Boolean := Semaphore_Free;
   begin
      Asm ("lock; xchg %1, %0",
           Outputs  => Boolean'Asm_Output ("+r", Write_This),
           Inputs   => Boolean'Asm_Input ("m", Semaphore.Is_Locked),
           Clobber  => "memory",
           Volatile => True);
   end Release;

   function Try_Seize (Semaphore : access Binary_Semaphore) return Boolean is
      If_This    : Boolean          := Semaphore_Free;
      Write_This : constant Boolean := Semaphore_Locked;
      Did_Change : Boolean          := False;
   begin
      Asm ("lock; cmpxchg %3, %1; setz %2",
           Outputs  => (Boolean'Asm_Output ("+a", If_This),
                        Boolean'Asm_Output ("+m", Semaphore.Is_Locked),
                        Boolean'Asm_Output ("=r", Did_Change)),
           Inputs   => Boolean'Asm_Input ("r",  Write_This),
           Clobber  => "memory, cc",
           Volatile => True);
      if Did_Change then
         Semaphore.Caller := Get_Caller_Address (0);
      end if;
      return Did_Change;
   end Try_Seize;
end Lib.Synchronization;
