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

package body Devices.Debug is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

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
         Safe_Read   => null,
         Safe_Write  => Write'Access,
         Sync        => null,
         Read        => null,
         Write       => null,
         IO_Control  => null,
         Mmap        => null,
         Munmap      => null
      );
      Register (Device, "debug", Success);
      return Success;
   end Init;

   procedure Write
      (Key       : Resource_Acc;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Offset);
   begin
      Lib.Synchronization.Seize (Key.Mutex);
      for C of Data loop
         Arch.Debug.Print (Character'Val (C));
      end loop;
      Lib.Synchronization.Release (Key.Mutex);
      Ret_Count := Data'Length;
      Success   := True;
   end Write;
end Devices.Debug;
