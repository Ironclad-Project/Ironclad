--  devices-pci-virtiorng.ads: VirtIO RNG devices.
--  Copyright (C) 2025 no95
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

with Devices.PCI.Virtio;
with Synchronization;

package Devices.PCI.VirtioRNG with SPARK_Mode => Off is
   pragma Warnings (Off, "may call Last_Chance_Handler");
   type Rng_Data is record
      Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
      Mutex : aliased Synchronization.Mutex;
   end record;
   type Rng_Data_Acc is not null access all Rng_Data;
   pragma Warnings (On, "may call Last_Chance_Handler");

   procedure Init (Success : out Boolean);

   procedure Read
      (Key : System.Address;
       Offset : Unsigned_64;
       Data : out Operation_Data;
       Ret_Count : out Natural;
       Success : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Write
      (Key : System.Address;
       Offset : Unsigned_64;
       Data : Operation_Data;
       Ret_Count : out Natural;
       Success : out Dev_Status;
       Is_Blocking : Boolean);

   function Issue_Command
      (Device : Rng_Data_Acc;
       Data_Addr : Unsigned_64;
       Data_Length : Unsigned_32) return Natural;
end Devices.PCI.VirtioRNG;
