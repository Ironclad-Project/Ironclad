--  ipc-futex.adb: Fast userland mutex.
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

with System; use System;
with Scheduler;
with Synchronization; use Synchronization;
with Arch.Clocks;
with Time; use Time;
with Memory.Userland_Transfer;

package body IPC.Futex is
   package Trans is new Memory.Userland_Transfer (Unsigned_32);

   Empty_Futex : constant System.Address := Null_Address;

   --  Wakes counts the wakes an address had, so that a wait is woken by the
   --  ones after it registered, and never by one that came before.
   type Futex_Inner is record
      Key_Addr : System.Address;
      Wakes    : Unsigned_64;
      Waiters  : Unsigned_32;
   end record;
   type Futex_Arr is array (1 .. 300) of Futex_Inner;

   Registry_Mutex : aliased Mutex := Unlocked_Mutex;
   Registry : Futex_Arr := [others => (System.Null_Address, 0, 0)];

   --  Key the waiters of a registry entry wait on.
   function Entry_Key (Index : Natural) return System.Address
      with Global => null;

   function Entry_Key (Index : Natural) return System.Address is
      pragma SPARK_Mode (Off);
   begin
      return Registry (Index)'Address;
   exception
      when Constraint_Error =>
         return System.Null_Address;
   end Entry_Key;

   procedure Wait
      (Map         : Memory.MMU.Page_Table_Acc;
       Keys        : Element_Arr;
       Max_Seconds : Unsigned_64;
       Max_Nanos   : Unsigned_64;
       Success     : out Wait_Status)
   is
      Curr, Final : Time.Timestamp;
      Value : Unsigned_32;
      Success2 : Boolean;
      Registered : Boolean := True;
   begin
      if Keys'Length = 0 then
         Success := Wait_Success;
         return;
      end if;

      declare
         Idx   : array (1 .. Keys'Length) of Natural     := [others => 0];
         Seen  : array (1 .. Keys'Length) of Unsigned_64 := [others => 0];
         Count : Natural := 0;

         --  Give back the keys registered so far, with the lock held.
         procedure Leave;
         procedure Leave is
         begin
            for I in 1 .. Count loop
               Registry (Idx (I)).Waiters := Registry (Idx (I)).Waiters - 1;
               if Registry (Idx (I)).Waiters = 0 then
                  Registry (Idx (I)).Key_Addr := Empty_Futex;
               end if;
            end loop;
         exception
            when Constraint_Error =>
               null;
         end Leave;
      begin
         --  The values are compared under the lock Wake takes as well, so a
         --  wake between a comparison and the registration cannot be lost.
         Synchronization.Seize (Registry_Mutex);
         for K of Keys loop
            Trans.Take_From_Userland (Map, Value, K.Key_Addr, Success2);
            if not Success2 or else Value /= K.Expected then
               Leave;
               Synchronization.Release (Registry_Mutex);
               Success := Wait_Try_Again;
               return;
            end if;

            for J in Registry'Range loop
               if Registry (J).Key_Addr = K.Key_Addr then
                  Idx (Count + 1) := J;
                  goto Register;
               end if;
            end loop;
            for J in Registry'Range loop
               if Registry (J).Key_Addr = Empty_Futex then
                  Idx (Count + 1) := J;
                  Registry (J).Key_Addr := K.Key_Addr;
                  goto Register;
               end if;
            end loop;

            Leave;
            Synchronization.Release (Registry_Mutex);
            Success := Wait_No_Space;
            return;

         <<Register>>
            Count := Count + 1;
            Seen (Count) := Registry (Idx (Count)).Wakes;
            Registry (Idx (Count)).Waiters :=
               Registry (Idx (Count)).Waiters + 1;
         end loop;
         Synchronization.Release (Registry_Mutex);

         --  Now that we have a built list of indexes to wait, we wait.
         Arch.Clocks.Get_Monotonic_Time (Final);
         Final := Final + (Max_Seconds, Max_Nanos);

         Scheduler.Begin_Wait;
         for I in 1 .. Count loop
            Scheduler.Add_Wait_Key (Entry_Key (Idx (I)), Success2);
            Registered := Registered and Success2;
         end loop;

         Success := Wait_Try_Again;
         loop
            Scheduler.Clear_Wake;
            Synchronization.Seize (Registry_Mutex);
            for I in 1 .. Count loop
               if Registry (Idx (I)).Wakes /= Seen (I) then
                  Success := Wait_Success;
               end if;
            end loop;

            Arch.Clocks.Get_Monotonic_Time (Curr);
            if Success = Wait_Success or Curr >= Final then
               Leave;
               Synchronization.Release (Registry_Mutex);
               Scheduler.End_Wait;
               return;
            end if;
            Synchronization.Release (Registry_Mutex);
            Scheduler.Wait_Event
               (Final,
                (if Registered then Scheduler.Woken_Sleep_Micros
                 else Scheduler.Polled_Sleep_Micros));
         end loop;
      end;
   exception
      when Constraint_Error =>
         Success := Wait_Try_Again;
   end Wait;

   procedure Wake (Keys : Element_Arr; Awoken_Count : out Natural) is
   begin
      Awoken_Count := 0;
      for K of Keys loop
         Synchronization.Seize (Registry_Mutex);
         for I in Registry'Range loop
            if Registry (I).Key_Addr /= Empty_Futex and then
               Registry (I).Key_Addr = K.Key_Addr
            then
               Registry (I).Wakes := Registry (I).Wakes + 1;
               Awoken_Count       := Awoken_Count + 1;
               Scheduler.Wake_Event (Entry_Key (I));
            end if;
         end loop;
         Synchronization.Release (Registry_Mutex);
      end loop;
   exception
      when Constraint_Error =>
         Synchronization.Release (Registry_Mutex);
   end Wake;
end IPC.Futex;
