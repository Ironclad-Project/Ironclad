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

with Lib.Atomic; use Lib.Atomic;
with Arch;
with Arch.Snippets;

package body Lib.Synchronization with SPARK_Mode => Off is
   --  Both locks do a rough wait until the lock is free for cache-locality.
   --  https://en.wikipedia.org/wiki/Test_and_test-and-set

   procedure Seize (Semaphore : aliased in out Binary_Semaphore) is
   begin
      loop
         if not Atomic_Test_And_Set (Semaphore.Is_Locked'Address, Mem_Acquire)
         then
            return;
         end if;
         Poll_Until_Zero (Semaphore.Is_Locked'Address);
      end loop;
   end Seize;

   procedure Release (Semaphore : aliased in out Binary_Semaphore) is
   begin
      Atomic_Clear (Semaphore.Is_Locked'Address, Mem_Release);
   end Release;
   ----------------------------------------------------------------------------
   procedure Seize (Lock : aliased in out Critical_Lock) is
   begin
      loop
         Arch.Snippets.Disable_Interrupts;
         if not Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire)
         then
            return;
         end if;
         Arch.Snippets.Enable_Interrupts;
         Poll_Until_Zero (Lock.Is_Locked'Address);
      end loop;
   end Seize;

   procedure Release
     (Lock        : aliased in out Critical_Lock;
      Do_Not_Lift : Boolean := False)
   is
   begin
      Atomic_Clear (Lock.Is_Locked'Address, Mem_Release);
      if not Do_Not_Lift then
         Arch.Snippets.Enable_Interrupts;
      end if;
   end Release;
   ----------------------------------------------------------------------------
   procedure Poll_Until_Zero (Address : System.Address) is
   begin
      while Atomic_Load_8 (Address, Mem_Relaxed) /= 0 loop
         Arch.Snippets.Pause;
      end loop;
   end Poll_Until_Zero;
end Lib.Synchronization;
