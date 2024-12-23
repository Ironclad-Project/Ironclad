--  lib-kasan.ads: Kernel Address SANitizer.
--  Copyright (C) 2024 streaksu
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
with Interfaces.C; use Interfaces.C;

package Lib.KASAN is
   --  Initialize the address registries.
   procedure Init;

private

   --  Functions called to check for load and stores.
   procedure Load1 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_load1_noabort";

   procedure Load2 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_load2_noabort";

   procedure Load4 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_load4_noabort";

   procedure Load8 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_load8_noabort";

   procedure Load16 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_load16_noabort";

   procedure Load_N (Addr : System.Address; Size : size_t)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_loadN_noabort";

   procedure Store1 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_store1_noabort";

   procedure Store2 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_store2_noabort";

   procedure Store4 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_store4_noabort";

   procedure Store8 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_store8_noabort";

   procedure Store16 (Addr : System.Address)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_store16_noabort";

   procedure Store_N (Addr : System.Address; Size : size_t)
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_storeN_noabort";
   ----------------------------------------------------------------------------
   --  Honestly I am not sure what this is for, but it is needed.
   procedure Handle_No_Return
      with Export, Convention => C, Obsolescent,
           External_Name => "__asan_handle_no_return";
   ----------------------------------------------------------------------------
   procedure Check_Memory
      (Caller, Fault_Addr : System.Address;
       Size               : size_t;
       Was_Load           : Boolean);

   procedure Report_Event
      (Caller, Fault_Addr : System.Address;
       Size               : size_t;
       Was_Load           : Boolean);

   function Caller_Address (Depth : Natural) return System.Address
      with Import, Convention => Intrinsic,
           External_Name => "__builtin_return_address";
end Lib.KASAN;
