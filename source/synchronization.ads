--  synchronization.ads: Specification of the synchronization library.
--  Copyright (C) 2023 streaksu
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

with Interfaces; use Interfaces;
with System;     use System;

package Synchronization is
   --  A simple binary semaphore for critical sections only.
   --
   --  Interrupt control is implemented with it, as to improve responsiveness,
   --  having an interrupt happen while holding a lock could add massive
   --  amounts of latency.
   type Binary_Semaphore is private;

   --  Value to initialize semaphores with.
   Unlocked_Semaphore : constant Binary_Semaphore;

   --  Lock a semaphore.
   --  When entering this routine, if interrupts are enabled, they will be
   --  disabled.
   --  @param Lock Semaphore to lock.
   procedure Seize (Lock : aliased in out Binary_Semaphore);

   --  Release a semaphore unconditionally. If interrupts were disabled, they
   --  will be reenabled.
   --  @param Lock Semaphore to release.
   procedure Release (Lock : aliased in out Binary_Semaphore);
   ----------------------------------------------------------------------------
   --  A more complex synchronization mechanism for more generic uses.
   --
   --  Mutexes will use the scheduler if available and other utilities to
   --  more effectively use waiting time. Use this to guard resources where
   --  having a bit of latency at the time of entering the critical section
   --  is fine, or where the held resource may take a long time to get out
   --  of the section, so you dont want other threads to wait too much doing
   --  nothing.
   type Mutex is private;

   --  Value to initialize mutexes with.
   Unlocked_Mutex : constant Mutex;

   --  Lock a mutex, and not return until locked.
   --  @param Lock  Mutex to lock.
   procedure Seize (Lock : aliased in out Mutex);

   --  Release a mutex unconditionally.
   --  @param Lock  Mutex to lock.
   procedure Release (Lock : aliased in out Mutex);
   ----------------------------------------------------------------------------
   --  A lock for several readers and one writer.
   --
   --  Behaves the same as a Mutex in regards to interrupts, while
   --  implementing the readers-writer lock semantics.
   --  https://en.wikipedia.org/wiki/Readers%E2%80%93writer_lock
   type Readers_Writer_Lock is private;

   --  Value to initialize RW locks with.
   Unlocked_RW_Lock : constant Readers_Writer_Lock;

   --  Lock as a reader.
   --  @param Lock Lock to lock lockingly.
   procedure Seize_Reader (Lock : aliased in out Readers_Writer_Lock);

   --  Lock as a writer.
   --  @param Lock Lock to lock lockingly.
   procedure Seize_Writer (Lock : aliased in out Readers_Writer_Lock);

   --  Release a reader lock unconditionally.
   --  @param Lock Lock to unlock lockingly.
   procedure Release_Reader (Lock : aliased in out Readers_Writer_Lock);

   --  Release a writer lock unconditionally.
   --  @param Lock Lock to unlock lockingly.
   procedure Release_Writer (Lock : aliased in out Readers_Writer_Lock);

private

   type Binary_Semaphore is record
      Is_Locked               : Unsigned_8;
      Were_Interrupts_Enabled : Boolean;
   end record;
   Unlocked_Semaphore : constant Binary_Semaphore := (0, False);
   ----------------------------------------------------------------------------
   type Mutex is record
      Is_Locked : Unsigned_8;
   end record;
   Unlocked_Mutex : constant Mutex := (Is_Locked => 0);
   ----------------------------------------------------------------------------
   type Readers_Writer_Lock is record
      Readers     : Natural;
      Semaphore_1 : aliased Mutex;
      Semaphore_2 : aliased Mutex;
   end record;
   Unlocked_RW_Lock : constant Readers_Writer_Lock :=
      (Readers     => 0,
       Semaphore_1 => Unlocked_Mutex,
       Semaphore_2 => Unlocked_Mutex);
   ----------------------------------------------------------------------------
   function Caller_Address (Depth : Natural) return System.Address;
   pragma Import (Intrinsic, Caller_Address, "__builtin_return_address");
end Synchronization;
