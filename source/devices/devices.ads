--  devices.ads: Device management library specification.
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

with System;
with Interfaces; use Interfaces;
with Memory;
with Lib.Synchronization;

package Devices with SPARK_Mode => Off is
   type Resource;
   type Resource_Acc is access all Resource;
   type Resource is record
      Mutex       : aliased Lib.Synchronization.Binary_Semaphore; --  Driver.
      Data        : System.Address;
      Is_Block    : Boolean; --  True for block dev, false for character dev.
      Block_Size  : Unsigned_64;
      Block_Count : Unsigned_64;
      Unique_Identifier : Natural;

      Sync : access procedure (Data : Resource_Acc);
      Read : access function
         (Data   : Resource_Acc;
          Offset : Unsigned_64;
          Count  : Unsigned_64;
          Desto  : System.Address) return Unsigned_64;
      Write : access function
         (Data     : Resource_Acc;
          Offset   : Unsigned_64;
          Count    : Unsigned_64;
          To_Write : System.Address) return Unsigned_64;
      IO_Control : access function
         (Data     : Resource_Acc;
          Request  : Unsigned_64;
          Argument : System.Address) return Boolean;
      Mmap : access function
         (Data        : Resource_Acc;
          Address     : Memory.Virtual_Address;
          Length      : Unsigned_64;
          Map_Read    : Boolean;
          Map_Write   : Boolean;
          Map_Execute : Boolean) return Boolean;
      Munmap : access function
         (Data    : Resource_Acc;
          Address : Memory.Virtual_Address;
          Length  : Unsigned_64) return Boolean;
   end record;

   --  Initialize the device registry and devices.
   procedure Init;

   --  Register and fetch devices, identified by a unique name.
   function Register (Dev : Resource; Name : String) return Boolean;
   function Fetch (Name : String) return Resource_Acc;
end Devices;
