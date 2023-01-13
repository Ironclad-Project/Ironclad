--  vfs.ads: FS and register dispatching.
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

with Interfaces; use Interfaces;
with System;
with Devices; use Devices;

package VFS with SPARK_Mode => Off is
   --  Stat structure of a file, which describes the qualities of a file.
   type File_Type is (
      File_Regular,
      File_Directory,
      File_Symbolic_Link,
      File_Character_Device,
      File_Block_Device
   );
   type File_Stat is record
      Unique_Identifier : Unsigned_64;
      Type_Of_File      : File_Type;
      Mode              : Unsigned_32;
      Hard_Link_Count   : Positive;
      Byte_Size         : Unsigned_64;
      IO_Block_Size     : Natural;
      IO_Block_Count    : Unsigned_64;
   end record;
   ----------------------------------------------------------------------------
   --  Initialize the internal VFS registries.
   procedure Init;

   --  Types of supported FS by the VFS.
   type FS_Type is (FS_USTAR);

   --  Mount the passed device name into the passed path.
   --  @param Name Name of the device (/dev/<name>).
   --  @param Path Absolute path for mounting.
   --  @param FS FS Type to mount as.
   function Mount (Name, Path : String; FS : FS_Type) return Boolean;

   --  Unmount a mount, syncing when possible.
   --  @param Path Path of the mount to unmount.
   procedure Unmount (Path : String);

   --  Get a mount mounted exactly in the passed path.
   --  @param Path Path to search a mount for.
   --  @return Key to use to refer to the mount, or 0 if not found.
   --  TODO: Make this do a closest instead of exact match in order to support
   --  mounts better, along with file dispatching in vfs-file.adb.
   function Get_Mount (Path : String) return Natural;

   --  Check if a key is valid.
   --  @param Key Key to check.
   --  @return True if valid, False if not.
   function Is_Valid (Key : Positive) return Boolean;

   --  Get the backing FS type.
   --  @param Key Key to use to fetch the info.
   --  @return The FS type, will be a placeholder if the key is not valid.
   function Get_Backing_FS (Key : Positive) return FS_Type
      with Pre => Is_Valid (Key);

   --  Get the backing data of the FS.
   --  @param Key Key to use to fetch the info.
   --  @return The FS data, or System.Null_Address if not a valid key.
   function Get_Backing_FS_Data (Key : Positive) return System.Address
      with Pre => Is_Valid (Key);

   --  Get the backing device of a mount.
   --  @param Key Key to use to fetch the info.
   --  @return The backing device.
   function Get_Backing_Device (Key : Positive) return Resource_Acc
      with Pre => Is_Valid (Key);

   --  Read from the mount itself, this function is needed as mount registering
   --  involves some caching by the registry for use by FSs.
   function Read
      (Key    : Positive;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   with Pre => Is_Valid (Key);

   --  Write to the mount, this function is needed as mount registering
   --  involves some caching by the registry for use by FSs.
   function Write
      (Key      : Positive;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   with Pre => Is_Valid (Key);

   --  Flush the aforementioned caches.
   --  @param Key Mount to flush the caches of.
   procedure Flush_Caches (Key : Positive) with Pre => Is_Valid (Key);

   --  Flush the aforementioned caches for all mounts.
   procedure Flush_Caches;
   ----------------------------------------------------------------------------
   --  Check whether a path is absolute.
   --  @param Path to check.
   --  @return True if absolute, False if not.
   function Is_Absolute (Path : String) return Boolean;

   --  Check whether a path is canonical, that is, whether the path is the
   --  shortest form it could be, symlinks are not checked.
   --  @param Path Path to check.
   --  @return True if canonical, False if not.
   function Is_Canonical (Path : String) return Boolean;

private

   --  Read and write individual sectors, return true in success.
   function Read_Sector
      (Dev   : Devices.Resource_Acc;
       LBA   : Unsigned_64;
       Desto : System.Address) return Boolean;
   function Write_Sector
      (Dev  : Devices.Resource_Acc;
       LBA  : Unsigned_64;
       Data : System.Address) return Boolean;

   --  Data kept for each sector as cache.
   type Sector_Data is array (Unsigned_64 range <>) of Unsigned_8;
   type Sector_Cache (Size : Unsigned_64) is record
      LBA_Offset : Unsigned_64;
      Is_Dirty   : Boolean;
      Data       : Sector_Data (1 .. Size);
   end record;
   type Sector_Cache_Acc is access Sector_Cache;

   --  Evict the passed information, and replace it.
   function Evict_Sector
      (Dev     : Devices.Resource_Acc;
       Sector  : Sector_Cache_Acc;
       New_LBA : Unsigned_64) return Boolean;
end VFS;
