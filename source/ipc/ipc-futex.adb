--  ipc-futex.adb: Fast userland mutex.
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

with Scheduler;
with Lib.Synchronization; use Lib.Synchronization;
with Arch.Clocks;
with Lib.Time;

package body IPC.Futex is
   type Futex_Inner is record
      Key         : access Unsigned_32;
      Wakey_Wakey : Boolean;
      Waiters     : Unsigned_32;
   end record;
   type Futex_Arr is array (1 .. 20) of Futex_Inner;

   Registry_Mutex : aliased Binary_Semaphore := Unlocked_Semaphore;
   Registry       :                Futex_Arr := (others => (null, False, 0));

   function Wait
      (Keys        : Element_Arr;
       Max_Seconds : Unsigned_64;
       Max_Nanos   : Unsigned_64) return Boolean
   is
      Curr_Sec, Curr_NSec, Final_Sec, Final_NSec : Unsigned_64;
      Idx : array (1 .. Keys'Length) of Natural;
   begin
      --  Find and/or allocate indexes for the passed mutexes.
      for I in Keys'Range loop
         if Keys (I).Key.all /= Keys (I).Expected then
            return False;
         end if;

         Lib.Synchronization.Seize (Registry_Mutex);
         for J in Registry'Range loop
            if Registry (J).Key = Keys (I).Key then
               Idx (I) := J;
               Registry (J).Waiters := Registry (J).Waiters + 1;
               goto End_Of_Iter;
            end if;
         end loop;
         for J in Registry'Range loop
            if Registry (J).Key = null then
               Idx (I) := J;
               Registry (J).Key         := Keys (I).Key;
               Registry (J).Wakey_Wakey := False;
               Registry (J).Waiters     := 1;
               goto End_Of_Iter;
            end if;
         end loop;

         Lib.Synchronization.Release (Registry_Mutex);
         return False;
      <<End_Of_Iter>>
         Lib.Synchronization.Release (Registry_Mutex);
      end loop;

      --  Now that we have a built list of indexes to wait, we wait.
      Arch.Clocks.Get_Monotonic_Time (Final_Sec, Final_NSec);
      Lib.Time.Increment (Final_Sec, Final_NSec, Max_Seconds, Max_Nanos);

      for I of Idx loop
         Lib.Synchronization.Seize (Registry_Mutex);
         if Registry (I).Wakey_Wakey then
            Registry (I).Waiters := Registry (I).Waiters - 1;
            if Registry (I).Waiters = 0 then
               Registry (I).Key := null;
            end if;
            Lib.Synchronization.Release (Registry_Mutex);
            exit;
         end if;
         Lib.Synchronization.Release (Registry_Mutex);

         Arch.Clocks.Get_Monotonic_Time (Curr_Sec, Curr_NSec);
         exit when Lib.Time.Is_Greater_Equal
            (Curr_Sec, Curr_NSec, Final_Sec, Final_NSec);

         Scheduler.Yield_If_Able;
      end loop;

      return True;
   end Wait;

   procedure Wake (Keys : Element_Arr) is
   begin
      for K of Keys loop
         Lib.Synchronization.Seize (Registry_Mutex);
         for I in Registry'Range loop
            if Registry (I).Key = K.Key then
               Registry (I).Wakey_Wakey := True;
            end if;
         end loop;
         Lib.Synchronization.Release (Registry_Mutex);
      end loop;
   end Wake;
end IPC.Futex;
