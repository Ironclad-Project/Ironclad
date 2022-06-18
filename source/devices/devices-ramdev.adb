--  devices-ramdev.adb: RAM devices.
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

with System; use System;
with Memory; use Memory;

package body Devices.Ramdev is
   --  Ramdev data.
   type Ramdev_Data is record
      Start_Address : System.Address;
      Size          : Virtual_Address;
   end record;
   type Ramdev_Data_Acc is access Ramdev_Data;

   function Init_Module (Module : Arch.Boot_RAM_File) return VFS.Resource is
      Stat   : VFS.File_Stat;
      Device : VFS.Resource;
      Data   : constant Ramdev_Data_Acc := new Ramdev_Data'(
         Start_Address => Module.Start,
         Size          => Virtual_Address (Module.Length)
      );
   begin
      Stat := (
         Unique_Identifier => 0,
         Type_Of_File      => VFS.File_Block_Device,
         Mode              => 8#660#,
         Hard_Link_Count   => 1,
         Byte_Size         => Unsigned_64 (Data.Size),
         IO_Block_Size     => 4096,
         IO_Block_Count    => (Unsigned_64 (Data.Size) + 4096 - 1) / 4096
      );

      Device := (
         Data       => Data.all'Address,
         Mutex      => (others => <>),
         Stat       => Stat,
         Sync       => null,
         Read       => Read'Access,
         Write      => null,
         IO_Control => null,
         Mmap       => null,
         Munmap     => null
      );

      return Device;
   end Init_Module;

   function Read
      (Data   : VFS.Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      Data2     : Ramdev_Data with Address => Data.Data;
      Data_Addr : constant System.Address  := Data2.Start_Address;
      Data_Sz   : constant Natural         := Natural (Data2.Size);
      Result    : array (1 .. Natural (Count)) of Unsigned_8
         with Address => Desto;
      Real_Data : array (1 .. Data_Sz) of Unsigned_8 with Address => Data_Addr;
      Offset2   : constant Natural := Natural (Offset) + Natural (Count);
      To_Write  : Natural := Natural (Count);
   begin
      if Offset2 > Data_Sz then
         To_Write := To_Write - (Offset2 - Data_Sz);
      end if;

      for I in 1 .. To_Write loop
         Result (I) := Real_Data (Natural (Offset) + I);
      end loop;

      return Unsigned_64 (To_Write);
   end Read;
end Devices.Ramdev;
