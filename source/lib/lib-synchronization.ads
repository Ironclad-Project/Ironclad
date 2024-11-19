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

package Lib.Synchronization is
   --  A simple binary semaphore that will busy loop with no complications.
   type Binary_Semaphore is private;
   Unlocked_Semaphore : constant Binary_Semaphore;

   --  Lock a semaphore, and not return until locked.
   --  @param Semaphore Semaphore to lock.
   procedure Seize
      (Lock : aliased in out Binary_Semaphore;
       Do_Not_Disable_Interrupts : Boolean := False);

   --  Release a semaphore unconditionally.
   --  @param Semaphore Semaphore to release.
   procedure Release (Lock : aliased in out Binary_Semaphore);

private

   type Binary_Semaphore is record
      Is_Locked               : Unsigned_8;
      Were_Interrupts_Enabled : Boolean;
   end record;
   Unlocked_Semaphore : constant Binary_Semaphore := (0, False);

   function Caller_Address (Depth : Natural) return System.Address;
   pragma Import (Intrinsic, Caller_Address, "__builtin_return_address");
end Lib.Synchronization;
