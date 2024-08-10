--  lib-atomic.ads: Atomic operations.
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

package Lib.Atomic is
   --  Values from libgnat/s-atopri.ads for memory ordering.
   Mem_Relaxed : constant := 0; --  Implies no ordering constraints.
   Mem_Consume : constant := 1; --  Implemented right now as acquire.
   Mem_Acquire : constant := 2; --  Happens before constraint from the release.
   Mem_Release : constant := 3; --  Happens before constraint to acquire.
   Mem_Acq_Rel : constant := 4; --  Both acquire and release.
   Mem_Seq_Cst : constant := 5; --  Enforces all ordering.

   subtype Mem_Model is Integer range Mem_Relaxed .. Mem_Seq_Cst;

   generic
      type Atomic_Type is mod <>;
   function Atomic_Load
     (Ptr   : System.Address;
      Model : Mem_Model := Mem_Seq_Cst) return Atomic_Type;
   pragma Import (Intrinsic, Atomic_Load, "__atomic_load_n");

   function Atomic_Load_8  is new Atomic_Load (Unsigned_8);
   function Atomic_Load_16 is new Atomic_Load (Unsigned_16);
   function Atomic_Load_32 is new Atomic_Load (Unsigned_32);
   function Atomic_Load_64 is new Atomic_Load (Unsigned_64);

   generic
      type Atomic_Type2 is mod <>;
   function Atomic_Fetch_Add
     (Ptr   : System.Address;
      Val   : Atomic_Type2;
      Model : Mem_Model := Mem_Seq_Cst) return Atomic_Type2;
   pragma Import (Intrinsic, Atomic_Fetch_Add, "__atomic_fetch_add");

   generic
      type Atomic_Type3 is mod <>;
   function Atomic_Fetch_Sub
     (Ptr   : System.Address;
      Val   : Atomic_Type3;
      Model : Mem_Model := Mem_Seq_Cst) return Atomic_Type3;
   pragma Import (Intrinsic, Atomic_Fetch_Sub, "__atomic_fetch_sub");

   function Atomic_Test_And_Set
     (Pointer : System.Address;
      Model   : Mem_Model := Mem_Seq_Cst) return Boolean;
   pragma Import (Intrinsic, Atomic_Test_And_Set, "__atomic_test_and_set");

   procedure Atomic_Clear
     (Pointer : System.Address;
      Model   : Mem_Model := Mem_Seq_Cst);
   pragma Import (Intrinsic, Atomic_Clear, "__atomic_clear");
end Lib.Atomic;
