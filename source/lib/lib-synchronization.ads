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

with System;
with Interfaces; use Interfaces;

package Lib.Synchronization is
   --  A simple binary semaphore.
   type Binary_Semaphore is private;

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

   procedure Try_Seize
      (Semaphore : aliased in out Binary_Semaphore;
       Did_Lock  : out Boolean);

private

   type Binary_Semaphore is record
      Caller    : System.Address;
      Is_Locked : Unsigned_32;
   end record;

   function Get_Caller_Address (Depth : Natural) return System.Address;
   pragma Import (Intrinsic, Get_Caller_Address, "__builtin_return_address");
end Lib.Synchronization;
