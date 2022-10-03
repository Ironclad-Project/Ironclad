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
with Cryptography.Random; use Cryptography.Random;

package body Devices.Random with SPARK_Mode => Off is
   function Init return Boolean is
      Random_Res : VFS.Resource := (
         Data       => System.Null_Address,
         Mutex      => <>,
         Stat       => (
            Unique_Identifier => 0,
            Type_Of_File      => VFS.File_Character_Device,
            Mode              => 8#660#,
            Hard_Link_Count   => 1,
            Byte_Size         => 0,
            IO_Block_Size     => 4096,
            IO_Block_Count    => 0
         ),
         Sync       => null,
         Read       => Read'Access,
         Write      => null,
         IO_Control => null,
         Mmap       => null,
         Munmap     => null
      );
   begin
      Lib.Synchronization.Release (Random_Res.Mutex);
      return VFS.Register (Random_Res, "random") and
             VFS.Register (Random_Res, "urandom");
   end Init;

   function Read
      (Data   : VFS.Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);

      Element_Size : constant Natural := Unsigned_32'Size / 8;
      Buffer : Crypto_Data (1 .. Natural (Count) / (Element_Size))
         with Address => Desto, Import;
   begin
      Fill_Data (Buffer);
      return Buffer'Size / 8;
   end Read;
end Devices.Random;
