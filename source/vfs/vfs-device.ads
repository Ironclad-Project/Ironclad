--  vfs-device.ads: Device management.
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
with Memory;

package VFS.Device is
   --  Devices are nodes held by the system with some basic operations.
   --  The user sees them as inside a dir in the root called /dev, but this is
   --  just a facade, there is no devfs.
   --  All devices have 7-character names.
   type Device_Data is record
      Name     : String (1 .. 64);
      Name_Len : Natural;
      Data     : System.Address;
      Stat     : VFS.File_Stat;

      Sync : access procedure (Data : System.Address);
      Read : access function
         (Data   : System.Address;
          Offset : Unsigned_64;
          Count  : Unsigned_64;
          Desto  : System.Address) return Unsigned_64;
      Write : access function
         (Data     : System.Address;
          Offset   : Unsigned_64;
          Count    : Unsigned_64;
          To_Write : System.Address) return Unsigned_64;
      IO_Control : access function
         (Data     : System.Address;
          Request  : Unsigned_64;
          Argument : System.Address) return Boolean;
      Mmap : access function
         (Data        : System.Address;
          Address     : Memory.Virtual_Address;
          Length      : Unsigned_64;
          Map_Read    : Boolean;
          Map_Write   : Boolean;
          Map_Execute : Boolean) return Boolean;
   end record;

   --  Initialize the device registry.
   procedure Init_Registry;

   --  Register and fetch devices, identified by a unique name.
   function Register (Dev : Device_Data) return Boolean;
   function Fetch (Name : String; Dev : out Device_Data) return Boolean;

   --  Mount or unmount an FS, along with getting devices based on their FS.
   type FS_Type is (FS_USTAR);
   function Mount
      (Name : String;
       Path : String;
       FS   : FS_Type) return Boolean;
   function Get_Mount (Path : String; FS : out FS_Type) return System.Address;
   procedure Unmount (Path : String);
end VFS.Device;
