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

with System; use System;
with System.Atomic_Operations; use System.Atomic_Operations;
with Arch;
with Arch.Local;
with Arch.Snippets;
with Scheduler;

package body Synchronization with SPARK_Mode => Off is
   pragma Suppress (All_Checks); --  Checks are too expensive in this paths.

   procedure Try_Seize
      (Lock : aliased in out Binary_Semaphore;
       Success : out Boolean)
   is
      Ints : constant Boolean := Arch.Snippets.Interrupts_Enabled;
   begin
      Arch.Snippets.Disable_Interrupts;
      Success := not Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire);
      if Success then
         Lock.Were_Interrupts_Enabled := Ints;
      elsif Ints then
         Arch.Snippets.Enable_Interrupts;
      end if;
   end Try_Seize;

   procedure Seize (Lock : aliased in out Binary_Semaphore) is
      Ints : constant Boolean := Arch.Snippets.Interrupts_Enabled;
   begin
      --  Whether we have ints on or off, we always have to have them off.
      Arch.Snippets.Disable_Interrupts;

      --  We do a rough wait until the lock is free for cache-locality.
      --  https://en.wikipedia.org/wiki/Test_and_test-and-set
      loop
         if not Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire) then
            Lock.Were_Interrupts_Enabled := Ints;
            exit;
         end if;

         while Atomic_Load_8 (Lock.Is_Locked'Address, Mem_Relaxed) /= 0 loop
            Arch.Snippets.Pause;
         end loop;
      end loop;
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
      Th : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      Synchronization.Seize (Lock.Pool_Mutex);
      if Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire) then
         for ID of Lock.Thread_Pool loop
            if ID = 0 then
               ID := Scheduler.Convert (Th);
               Scheduler.Mark_Suspend;
               exit;
            end if;
         end loop;

         Synchronization.Release (Lock.Pool_Mutex);
         while Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire) loop
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Release (Lock.Pool_Mutex);
      end if;
   end Seize;

   procedure Release (Lock : aliased in out Mutex) is
   begin
      Synchronization.Seize (Lock.Pool_Mutex);
      for ID of Lock.Thread_Pool loop
         if ID /= 0 then
            Scheduler.Lift_Suspension (Scheduler.Convert (ID));
            ID := 0;
         end if;
      end loop;
      Atomic_Clear (Lock.Is_Locked'Address, Mem_Release);
      Synchronization.Release (Lock.Pool_Mutex);
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
