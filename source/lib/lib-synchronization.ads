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
      Caller    : System.Address;
      Is_Locked : Unsigned_8;
   end record;

   procedure Seize (Semaphore : not null access Binary_Semaphore);
   procedure Release (Semaphore : not null access Binary_Semaphore);
   function Try_Seize
      (Semaphore : not null access Binary_Semaphore) return Boolean;

private

   --  Values from libgnat/s-atopri.ads for memory ordering.
   Mem_Relaxed : constant := 0; --  Implies no ordering constraints.
   Mem_Consume : constant := 1; --  Implemented right now as acquire.
   Mem_Acquire : constant := 2; --  Happens before constraint from the release.
   Mem_Release : constant := 3; --  Happens before constraint to acquire.
   Mem_Acq_Rel : constant := 4; --  Both acquire and release.
   Mem_Seq_Cst : constant := 5; --  Enforces all ordering.

   subtype Mem_Model is Integer range Mem_Relaxed .. Mem_Seq_Cst;

   procedure Pause;
   pragma Import (Intrinsic, Pause, "__builtin_ia32_pause");

   function Atomic_Load
     (Pointer : System.Address;
      Model   : Mem_Model := Mem_Seq_Cst) return Unsigned_8;
   pragma Import (Intrinsic, Atomic_Load, "__atomic_load_n");

   function Atomic_Test_And_Set
     (Pointer : System.Address;
      Model   : Mem_Model := Mem_Seq_Cst) return Boolean;
   pragma Import (Intrinsic, Atomic_Test_And_Set, "__atomic_test_and_set");

   procedure Atomic_Clear
     (Pointer : System.Address;
      Model   : Mem_Model := Mem_Seq_Cst);
   pragma Import (Intrinsic, Atomic_Clear, "__atomic_clear");

   function Get_Caller_Address (Depth : Natural) return System.Address;
   pragma Import (Intrinsic, Get_Caller_Address, "__builtin_return_address");
end Lib.Synchronization;
