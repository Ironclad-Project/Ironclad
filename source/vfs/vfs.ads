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

with System;
with Interfaces; use Interfaces;
with Memory;
with Lib.Synchronization;

package VFS is
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

   type Resource;
   type Resource_Acc is access all Resource;
   type Resource is record
      Mutex : aliased Lib.Synchronization.Binary_Semaphore; -- Driver-owned.
      Data  : System.Address;
      Stat  : VFS.File_Stat;

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

   --  Initialize the device registry.
   procedure Init;

   --  Register and fetch devices, identified by a unique name.
   function Register (Dev : Resource; Name : String) return Boolean;
   function Fetch (Name : String) return Resource_Acc;

   --  Mount or unmount an FS, along with getting devices based on their FS.
   type FS_Type is (FS_USTAR);
   function Mount
      (Name : String;
       Path : String;
       FS   : FS_Type) return Boolean;
   function Get_Mount
      (Path : String;
       FS   : out FS_Type;
       Dev  : out Resource_Acc) return System.Address;
   procedure Unmount (Path : String);
end VFS;
