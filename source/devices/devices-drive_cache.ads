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
   --  Type to represent the registry.
   type Cache_Registry is private;

   --  Type used to represent a sector.
   subtype Sector_Data is Operation_Data (1 .. Sector_Size);

   --  Functions used to interface with the driver which are passed as an
   --  address to the init function.
   type Read_Sector is access procedure
      (Drive       : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : out Sector_Data;
       Success     : out Boolean);
   type Write_Sector is access procedure
      (Drive       : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : Sector_Data;
       Success     : out Boolean);

   procedure Init
      (Drive_Arg : System.Address;
       Read      : System.Address;
       Write     : System.Address;
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
   --  the total amount of cached items, and the modulo is our start index.
   --  If that spot is already taken, go forward a maximum number of times. If
   --  all are taken, just deallocate the modulo of the LBA and the maximum
   --  times, and take its place.
   --  This maximum number is vibes based, the current number strikes a balance
   --  in my experience.
   Max_Caching_Step : constant := 200;

   type Sector_Cache is record
      Mutex      : aliased Synchronization.Mutex;
      Is_Used    : Boolean;
      LBA_Offset : Unsigned_64;
      Data       : Sector_Data;
      Is_Dirty   : Boolean;
   end record with Alignment => 16;
   type Sector_Caches is array (Natural range <>) of Sector_Cache;

   type Cache_Registry is record
      Drive_Arg  : System.Address;
      Read_Proc  : System.Address;
      Write_Proc : System.Address;
      Caches     : Sector_Caches (1 .. 250_000);
   end record;

   procedure Get_Cache_Index
      (Registry : aliased in out Cache_Registry;
       LBA      : Unsigned_64;
       Idx      : out Natural;
       Success  : out Boolean);
end Devices.Drive_Cache;
