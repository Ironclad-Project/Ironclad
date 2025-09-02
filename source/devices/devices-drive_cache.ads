--  devices-drive_cache.ads: Drive-independent cache.
--  Copyright (C) 2025 streaksu
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

with Synchronization;

generic
   --  Sector size in bytes.
   Sector_Size : Natural;
package Devices.Drive_Cache is
   --  This module implements a small cache for drives to speed up a bit the
   --  fetch and write of sectors by using fixed sections of memory.
   --
   --  This is not a dynamic page cache! Instead, this is meant to be used by
   --  the driver itself, with page caches depending on block devices and not
   --  this cache.

   --  Type to represent the registry.
   type Cache_Registry is private;

   --  Type used to represent a sector.
   subtype Sector_Data is Operation_Data (1 .. Sector_Size);

   --  Functions used to interface with the driver which are passed as an
   --  address to the init function.
   type Read_Sector_Acc is access procedure
      (Drive       : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : out Sector_Data;
       Success     : out Boolean);
   type Write_Sector_Acc is access procedure
      (Drive       : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : Sector_Data;
       Success     : out Boolean);

   procedure Init
      (Drive_Arg : System.Address;
       Read      : not null Read_Sector_Acc;
       Write     : not null Write_Sector_Acc;
       Registry  : aliased out Cache_Registry);

   procedure Read
      (Registry  : aliased in out Cache_Registry;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Dev_Status);

   procedure Write
      (Registry  : aliased in out Cache_Registry;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Dev_Status);

   procedure Sync
      (Registry : aliased in out Cache_Registry;
       Success  : out Boolean);

   procedure Sync_Range
      (Registry : aliased in out Cache_Registry;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       Success  : out Boolean);

private

   --  We use a flat array for sector caches.
   --  To calculate where a sector cache goes, we take the LBA, divide it by
   --  the total amount of cached items, and the modulo is our index.
   Max_Caching_Len : constant := 10_000;

   type Sector_Cache is record
      Mutex      : aliased Synchronization.Mutex;
      Is_Used    : Boolean;
      LBA_Offset : Unsigned_64;
      Data       : Sector_Data;
      Is_Dirty   : Boolean;
   end record;
   type Sector_Caches is array (Unsigned_64 range <>) of Sector_Cache;

   type Cache_Registry is record
      Drive_Arg  : System.Address;
      Read_Proc  : Read_Sector_Acc := null;
      Write_Proc : Write_Sector_Acc := null;
      Caches     : Sector_Caches (1 .. Max_Caching_Len);
   end record;

   procedure Get_Cache_Index
      (Registry : aliased in out Cache_Registry;
       LBA      : Unsigned_64;
       Idx      : out Unsigned_64;
       Success  : out Boolean);

   function Get_Cache_Index (LBA : Unsigned_64) return Unsigned_64;
end Devices.Drive_Cache;
