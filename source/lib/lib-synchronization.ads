--  lib-synchronization.ads: Specification of the synchronization library.
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
with System;

package Lib.Synchronization is
   --  A simple binary semaphore that will busy loop with no complications.
   type Binary_Semaphore is private;
   Unlocked_Semaphore : constant Binary_Semaphore;

   --  Lock a semaphore, and not return until locked.
   --  @param Semaphore Semaphore to lock.
   procedure Seize (Semaphore : aliased in out Binary_Semaphore);

   --  Release a semaphore unconditionally.
   --  @param Semaphore Semaphore to release.
   procedure Release (Semaphore : aliased in out Binary_Semaphore);
   ----------------------------------------------------------------------------
   --  Sometimes you may want a simple binary semaphore, but contention is
   --  fatal. For that critical locks will disable interrupts along the usual
   --  binary semaphore business.
   --  If you are considering using them, be careful! Improper use will
   --  potentially obliterate the system, or tank performance, or both!
   type Critical_Lock is private;
   Unlocked_Critical : constant Critical_Lock;

   --  Lock a critical section, and not return until locked, disabling
   --  interrupts.
   --  @param Lock Lock to use.
   procedure Seize (Lock : aliased in out Critical_Lock);

   --  Release a critical section unconditionally
   --  @param Lock Lock to use.
   --  @param Do_Not_Lift If True, interrupts wont be enabled, else, they will.
   procedure Release
     (Lock        : aliased in out Critical_Lock;
      Do_Not_Lift : Boolean := False);

private

   type Binary_Semaphore is record
      Is_Locked : Unsigned_8;
   end record;
   Unlocked_Semaphore : constant Binary_Semaphore := (others => 0);
   ----------------------------------------------------------------------------
   type Critical_Lock is record
      Is_Locked : Unsigned_8;
   end record;
   Unlocked_Critical : constant Critical_Lock := (others => 0);
   ----------------------------------------------------------------------------
   procedure Poll_Until_Zero (Address : System.Address);
end Lib.Synchronization;
