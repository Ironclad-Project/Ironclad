--  lib-synchronization.ads: Specification of the synchronization library.
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

package Lib.Synchronization is
   --  A simple binary semaphore.
   type Binary_Semaphore is record
      Is_Locked : Unsigned_8;
   end record;
   Unlocked_Semaphore : constant Binary_Semaphore := (others => 0);

   --  Check if a lock is available or locked.
   --  @param Semaphore Semaphore to test.
   --  @return True if locked, False if not locked.
   function Is_Locked (Semaphore : aliased Binary_Semaphore) return Boolean;

   --  Lock a semaphore, and not return until locked.
   --  @param Semaphore Semaphore to lock.
   procedure Seize (Semaphore : aliased in out Binary_Semaphore)
      with Post => Is_Locked (Semaphore);

   --  Release a semaphore unconditionally.
   --  @param Semaphore Semaphore to release.
   procedure Release (Semaphore : aliased in out Binary_Semaphore)
      with Post => not Is_Locked (Semaphore);

   --  Attempt to lock the passed semaphore.
   --  @param Semaphore Semaphore to try to lock.
   --  @param Did_Lock True if the attempt succeeded.
   procedure Try_Seize
      (Semaphore : aliased in out Binary_Semaphore;
       Did_Lock  : out Boolean);
end Lib.Synchronization;
