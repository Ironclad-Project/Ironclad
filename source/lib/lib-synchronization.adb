--  lib-synchronization.adb: Synchronization primitives and such.
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

with Lib.Atomic; use Lib.Atomic;
with System.Storage_Elements; use System.Storage_Elements;
with Arch;
with Arch.Snippets;
with Lib.Messages;
with Lib.Panic;

package body Lib.Synchronization with SPARK_Mode => Off is
   --  Both locks do a rough wait until the lock is free for cache-locality.
   --  https://en.wikipedia.org/wiki/Test_and_test-and-set

   procedure Seize
      (Lock : aliased in out Binary_Semaphore;
       Do_Not_Disable_Interrupts : Boolean := False)
   is
      Stp  : Lib.Messages.Translated_String;
      Len  : Natural;
      Ints : constant Boolean := Arch.Snippets.Interrupts_Enabled;
   begin
      if not Do_Not_Disable_Interrupts then
         Arch.Snippets.Disable_Interrupts;
      end if;

      loop
      <<RETEST>>
         if not Atomic_Test_And_Set (Lock.Is_Locked'Address, Mem_Acquire) then
            Lock.Were_Interrupts_Enabled := Ints;
            return;
         end if;

         for I in 1 .. 50000000 loop
            if Atomic_Load_8 (Lock.Is_Locked'Address, Mem_Relaxed) = 0 then
               goto RETEST;
            else
               Arch.Snippets.Pause;
            end if;
         end loop;

         Lib.Messages.Image
            (Unsigned_64 (To_Integer (Caller_Address (0))), Stp, Len, True);
         Lib.Panic.Hard_Panic
            ("Deadlock at " & Stp (Stp'Last - Len + 1 .. Stp'Last));
      end loop;
   end Seize;

   procedure Release (Lock : aliased in out Binary_Semaphore) is
   begin
      Atomic_Clear (Lock.Is_Locked'Address, Mem_Release);
      if Lock.Were_Interrupts_Enabled then
         Arch.Snippets.Enable_Interrupts;
      end if;
   end Release;
end Lib.Synchronization;
