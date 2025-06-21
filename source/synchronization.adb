--  synchronization.adb: Synchronization primitives and such.
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

with Atomic; use Atomic;
with Arch;
with Arch.Snippets;
with Panic;
with Scheduler;

package body Synchronization with SPARK_Mode => Off is
   pragma Suppress (All_Checks); --  Checks are too expensive in this paths.

   --  Both locks do a rough wait until the lock is free for cache-locality.
   --  https://en.wikipedia.org/wiki/Test_and_test-and-set

   procedure Seize
      (Lock : aliased in out Binary_Semaphore;
       Do_Not_Disable_Interrupts : Boolean := False)
   is
      Ints : Boolean := Arch.Snippets.Interrupts_Enabled;
   begin
      Ints := Ints and not Do_Not_Disable_Interrupts;

      if Ints then
         Arch.Snippets.Disable_Interrupts;
      end if;

   <<RETEST>>
      if not Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire) then
         Lock.Were_Interrupts_Enabled := Ints;
         return;
      end if;

      for I in 1 .. 50_000_000 loop
         if Atomic_Load_8 (Lock.Is_Locked'Address, Mem_Relaxed) = 0 then
            goto RETEST;
         else
            Arch.Snippets.Pause;
         end if;
      end loop;

      Panic.Hard_Panic ("Deadlock at " & Caller_Address (0)'Image);
   end Seize;

   procedure Release (Lock : aliased in out Binary_Semaphore) is
      Reenable_Ints : constant Boolean := Lock.Were_Interrupts_Enabled;
   begin
      Atomic_Clear (Lock.Is_Locked'Address, Mem_Release);
      if Reenable_Ints then
         Arch.Snippets.Enable_Interrupts;
      end if;
   end Release;
   ----------------------------------------------------------------------------
   procedure Seize (Lock : aliased in out Mutex) is
   begin
      loop
      <<RETEST>>
         if not Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire) then
            return;
         end if;

         for I in 1 .. 1_000_000 loop
            if Atomic_Load_8 (Lock.Is_Locked'Address, Mem_Relaxed) = 0 then
               goto RETEST;
            end if;
         end loop;

         Scheduler.Yield_If_Able;
      end loop;
   end Seize;

   procedure Release (Lock : aliased in out Mutex) is
   begin
      Atomic_Clear (Lock.Is_Locked'Address, Mem_Release);
   end Release;
   ----------------------------------------------------------------------------
   procedure Seize_Reader (Lock : aliased in out Readers_Writer_Lock) is
   begin
      Seize (Lock.Semaphore_1);
      Lock.Readers := Lock.Readers + 1;
      if Lock.Readers = 1 then
         Seize (Lock.Semaphore_2);
      end if;
      Release (Lock.Semaphore_1);
   end Seize_Reader;

   procedure Seize_Writer (Lock : aliased in out Readers_Writer_Lock) is
   begin
      Seize (Lock.Semaphore_2);
   end Seize_Writer;

   procedure Release_Reader (Lock : aliased in out Readers_Writer_Lock) is
   begin
      Seize (Lock.Semaphore_1);
      Lock.Readers := Lock.Readers - 1;
      if Lock.Readers = 0 then
         Release (Lock.Semaphore_2);
      end if;
      Release (Lock.Semaphore_1);
   end Release_Reader;

   procedure Release_Writer (Lock : aliased in out Readers_Writer_Lock) is
   begin
      Release (Lock.Semaphore_2);
   end Release_Writer;
end Synchronization;
