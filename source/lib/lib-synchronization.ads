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

package Lib.Synchronization is
   --  A simple binary semaphore.
   type Binary_Semaphore is record
      Is_Locked : Boolean;
      Caller    : System.Address;
   end record;

   procedure Seize (Semaphore : access Binary_Semaphore);
   procedure Release (Semaphore : access Binary_Semaphore);
   function Try_Seize (Semaphore : access Binary_Semaphore) return Boolean;

private

   function Get_Caller_Address (Depth : Unsigned_32) return System.Address;
   pragma Import (Intrinsic, Get_Caller_Address, "__builtin_return_address");
end Lib.Synchronization;
