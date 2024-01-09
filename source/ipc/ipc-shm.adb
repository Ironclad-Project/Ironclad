--  ipc-shm.adb: SysV shared memory segments.
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

with Lib.Synchronization; use Lib.Synchronization;
with Memory;
with Memory.Physical; use Memory.Physical;
with Interfaces.C; use Interfaces.C;
with System.Storage_Elements; use System.Storage_Elements;

package body IPC.SHM with SPARK_Mode => Off is
   type Segment_Inner is record
      Is_Present       : Boolean;
      Key              : Unsigned_32;
      Physical_Address : Memory.Physical_Address;
      Size             : Memory.Size;
   end record;
   type Segment_Arr is array (Segment_ID range 1 .. 20) of Segment_Inner;

   Registry_Mutex : aliased Binary_Semaphore := Unlocked_Semaphore;
   Registry : Segment_Arr := (others => (False, 0, 0, 0));

   function Create_Segment
      (Wanted_Key  : Unsigned_32;
       Wanted_Size : Unsigned_64;
       Mode        : Unsigned_64) return Segment_ID
   is
      Returned : Segment_ID := Error_ID;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and Registry (I).Key = Wanted_Key then
            goto Cleanup;
         end if;
         if not Registry (I).Is_Present and Returned = Error_ID then
            Returned := I;
         end if;
      end loop;

      Registry (Returned).Is_Present := True;
      Registry (Returned).Key := Wanted_Key;
      Registry (Returned).Physical_Address :=
         Alloc (size_t (Wanted_Size)) - Memory.Memory_Offset;
      Registry (Returned).Size := Memory.Size (Wanted_Size);

   <<Cleanup>>
      Lib.Synchronization.Release (Registry_Mutex);
      return Returned;
   end Create_Segment;

   function Get_Segment (Key : Unsigned_32) return Segment_ID is
      Returned : Segment_ID := Error_ID;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and Registry (I).Key = Key then
            Returned := I;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
      return Returned;
   end Get_Segment;

   procedure Get_Address
      (ID      : Segment_ID;
       Address : out Unsigned_64;
       Size    : out Unsigned_64)
   is
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Address := Unsigned_64 (Registry (ID).Physical_Address);
         Size := Unsigned_64 (Registry (ID).Size);
      else
         Address := 0;
         Size := 0;
      end if;
      Lib.Synchronization.Release (Registry_Mutex);
   end Get_Address;
end IPC.SHM;
