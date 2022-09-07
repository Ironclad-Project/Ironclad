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
   function Is_Locked (Semaphore : aliased Binary_Semaphore) return Boolean is
   begin
      return Atomic_Load_8 (Semaphore.Is_Locked'Address, Mem_Seq_Cst) /= 0;
   end Is_Locked;

   procedure Seize (Semaphore : aliased in out Binary_Semaphore) is
   begin
<<Retry_Lock>>
      if not Atomic_Test_And_Set (Semaphore.Is_Locked'Address, Mem_Acquire)
      then
         return;
      end if;

      --  Do a rough wait until the lock is free for cache-locality.
      --  https://en.wikipedia.org/wiki/Test_and_test-and-set
      loop
         if Atomic_Load_8 (Semaphore.Is_Locked'Address, Mem_Relaxed) = 0 then
            goto Retry_Lock;
         end if;
         Arch.Snippets.Pause;
      end loop;
   end Seize;

   procedure Release (Semaphore : aliased in out Binary_Semaphore) is
   begin
      Atomic_Clear (Semaphore.Is_Locked'Address, Mem_Release);
   end Release;

   procedure Try_Seize
      (Semaphore : aliased in out Binary_Semaphore;
       Did_Lock  : out Boolean)
   is
   begin
      Did_Lock := not Atomic_Test_And_Set
         (Semaphore.Is_Locked'Address, Mem_Acquire);
   end Try_Seize;
end Lib.Synchronization;
