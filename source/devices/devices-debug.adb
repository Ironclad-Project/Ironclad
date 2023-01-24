--  devices-debugdev.adb: Driver for debug utilities.
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

with Arch.Debug;
with Lib.Synchronization;

package body Devices.Debug with SPARK_Mode => Off is
   function Init return Boolean is
      Device  : Resource;
      Success : Boolean;
   begin
      Device := (
         Data        => System.Null_Address,
         Mutex       => Lib.Synchronization.Unlocked_Semaphore,
         Is_Block    => False,
         Block_Size  => 4096,
         Block_Count => 0,
         Sync        => null,
         Read        => null,
         Write       => Write'Access,
         IO_Control  => null,
         Mmap        => null,
         Munmap      => null
      );
      Register (Device, "debug", Success);
      return Success;
   end Init;

   function Write
      (Data     : Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Buff : String (1 .. Natural (Count)) with Address => To_Write;
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      Arch.Debug.Print (Buff);
      Lib.Synchronization.Release (Data.Mutex);
      return Count;
   end Write;
end Devices.Debug;
