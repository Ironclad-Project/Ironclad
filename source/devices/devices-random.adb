--  devices-random.adb: Random devices.
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

with Lib.Synchronization;
with Cryptography.Random;

package body Devices.Random is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   function Init return Boolean is
      Random_Res : constant Resource := (
         Data        => System.Null_Address,
         Mutex       => Lib.Synchronization.Unlocked_Semaphore,
         Is_Block    => False,
         Block_Size  => 4096,
         Block_Count => 0,
         Safe_Read   => Read'Access,
         Safe_Write  => null,
         Sync        => null,
         Read        => null,
         Write       => null,
         IO_Control  => null,
         Mmap        => null,
         Munmap      => null
      );
      Success_1, Success_2 : Boolean;
   begin
      Register (Random_Res, "random",  Success_1);
      Register (Random_Res, "urandom", Success_2);
      return Success_1 and Success_2;
   end Init;

   procedure Read
      (Key       : Resource_Acc;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      Len  : constant Natural := Data'Length / 4;
      Temp : Cryptography.Random.Crypto_Data (1 .. Len)
         with Import, Address => Data (Data'First)'Address;
   begin
      Cryptography.Random.Fill_Data (Temp);
      Ret_Count := Len;
      Success   := True;
   end Read;
end Devices.Random;
