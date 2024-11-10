--  devices.ads: Device management library specification.
--  Copyright (C) 2024 streaksu
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
with Arch.MMU;

package Devices is
   --  Data to operate with read-write.
   --  This data type imposes a hard limit on operation length of
   --  Natural. Linux shares this limitation funnily enough.
   --  We additionally force a start of 1 because else an array
   --  (0 .. Natural'Last) actually contains Natural'Last + 1 entries.
   subtype Operation_Count is Natural range 1 .. Natural'Last;
   type Operation_Data     is array (Operation_Count range <>) of Unsigned_8;
   type Operation_Data_Acc is access Operation_Data;

   --  Devices might have a UUID. If all components are 0, they have no UUID.
   --  Example for the string version: 123e4567-e89b-12d3-a456-426614174000.
   type UUID is array (1 .. 16) of Unsigned_8;
   subtype UUID_String is String (1 .. 36);
   Zero_UUID : constant UUID := (others => 0);

   --  Data that defines a device.
   type Resource is record
      Data        : System.Address;
      ID          : UUID;
      Is_Block    : Boolean; --  True for block dev, false for character dev.
      Block_Size  : Natural;
      Block_Count : Unsigned_64;

      Read : access procedure
         (Key         : System.Address;
          Offset      : Unsigned_64;
          Data        : out Operation_Data;
          Ret_Count   : out Natural;
          Success     : out Boolean;
          Is_Blocking : Boolean);
      Write : access procedure
         (Key         : System.Address;
          Offset      : Unsigned_64;
          Data        : Operation_Data;
          Ret_Count   : out Natural;
          Success     : out Boolean;
          Is_Blocking : Boolean);
      Sync : access function (Key : System.Address) return Boolean;
      Sync_Range : access function
         (Key    : System.Address;
          Offset : Unsigned_64;
          Count  : Unsigned_64) return Boolean;
      IO_Control : access function
         (Key      : System.Address;
          Request  : Unsigned_64;
          Argument : System.Address) return Boolean;
      Mmap : access function
         (Key     : System.Address;
          Map     : Arch.MMU.Page_Table_Acc;
          Address : Memory.Virtual_Address;
          Length  : Unsigned_64;
          Flags   : Arch.MMU.Page_Permissions) return Boolean;
      Poll : access procedure
         (Handle    : System.Address;
          Can_Read  : out Boolean;
          Can_Write : out Boolean;
          Is_Error  : out Boolean);
   end record;

   --  Handle for interfacing with devices, and device conditions.
   type Device_Handle is private;
   type Device_List   is array (Natural range <>) of Device_Handle;
   Error_Handle : constant Device_Handle;

   --  Device names have a maximum fixed length.
   Max_Name_Length : constant Natural;

   --  Initialize the device registry and register some devices.
   --  @return True on success, False if some devices could not be registered.
   procedure Init with Post => Is_Initialized = True;

   --  Register a device with a resource description and matching name.
   --  @param Dev  Device description to register.
   --  @param Name Name to register the device with, must be unique.
   --  @return True on success, False on failure.
   procedure Register (Dev : Resource; Name : String; Success : out Boolean)
      with Pre => ((Is_Initialized = True) and Name'Length <= Max_Name_Length),
           Post => Is_Initialized = True;

   --  Fetch a device by name.
   --  @param Name Name to search.
   --  @return A handle on success, or Error_Handle on failure.
   function Fetch (Name : String) return Device_Handle
      with Pre => ((Is_Initialized = True) and Name'Length <= Max_Name_Length);

   --  Fetch a device by numeric UUID.
   --  @param ID UUID to search.
   --  @return A handle on success, or Error_Handle on failure.
   function Fetch (ID : UUID) return Device_Handle
      with Pre => Is_Initialized;

   --  Fetch a device by string representation of a numeric UUID.
   --  @param ID UUID to search.
   --  @return A handle on success, or Error_Handle on failure.
   function Fetch_UUID (ID : UUID_String) return Device_Handle
      with Pre => (Is_Initialized = True);

   --  Fetch the numeric UUID of a device.
   --  @param Dev Device to report the UUID of.
   --  @return The UUID on success, or the Zero UUID if the device has no UUID.
   function Fetch (Dev : Device_Handle) return UUID
      with Pre => Is_Initialized;

   --  Write the name associated to a device handle to the passed buffer.
   --  @param Handle Handle to fetch the name of.
   --  @param Name   Buffer to write the name.
   --  @param Count  Count of characters written to the buffer.
   procedure Fetch_Name
      (Handle : Device_Handle;
       Name   : out String;
       Length : out Natural)
      with Pre  => ((Is_Initialized = True) and (Handle /= Error_Handle) and
                    (Name'Length = Max_Name_Length)),
           Post => (Length >= 0 and Length <= Max_Name_Length);

   --  List all devices registered.
   --  @param Buffer Buffer to write the devices.
   --  @param Total  Returned count of devices, even if it does not fit.
   procedure List (Buffer : out Device_List; Total : out Natural)
      with Pre => Is_Initialized;

   --  Ghost function for checking whether the device handling is initialized.
   function Is_Initialized return Boolean with Ghost;
   ----------------------------------------------------------------------------
   --  Fetch generic properties of a device handle.
   --  @param Handle Handle to fetch, must be valid, as checking is not done.
   --  @return The requested data.
   function Is_Block_Device (Handle : Device_Handle) return Boolean
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));
   function Get_Block_Count (Handle : Device_Handle) return Unsigned_64
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));
   function Get_Block_Size  (Handle : Device_Handle) return Natural
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));
   function Is_Read_Only (Handle : Device_Handle) return Boolean
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Get a unique ID for the device. This does not have anything to do with
   --  device UUIDs, and is always supported for a valid handle.
   function Get_Unique_ID (Handle : Device_Handle) return Natural
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Get a device handle from a unique ID.
   function From_Unique_ID (ID : Natural) return Device_Handle
      with Pre => (Is_Initialized = True);

   --  Synchronize internal device state, in order to ensure coherency.
   --  @param Handle Handle to synchronize if supported, must be valid.
   --  @return True on success or sync not supported. False on device failure.
   function Synchronize (Handle : Device_Handle) return Boolean
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Synchronize a data range of a device, in order to ensure coherency.
   --  @param Handle Handle to synchronize if supported, must be valid.
   --  @param Offset Offset to start synchronizing.
   --  @param Count  Count of bytes to synchronize.
   --  @return True on success or sync not supported. False on device failure.
   function Synchronize
      (Handle : Device_Handle;
       Offset : Unsigned_64;
       Count  : Unsigned_64) return Boolean
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Read from a device.
   --  @param Handle    Handle to read if supported, must be valid.
   --  @param Offset    Byte offset to start reading from, for block devices.
   --  @param Count     Count of bytes to read.
   --  @param Desto     Destination address where to write the read data.
   --  @param Ret_Count Count of bytes actually read, < count if EOF or error.
   --  @param Success   True on success, False on non-supported/failure.
   --  @param Is_Blocking Whether to do the operation blocking or non-blocking.
   procedure Read
      (Handle      : Device_Handle;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean := True)
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Write to a device.
   --  @param Handle    Handle to read if supported, must be valid.
   --  @param Offset    Byte offset to start writing to, for block devices.
   --  @param Count     Count of bytes to write.
   --  @param To_Write  Source address for the data to write.
   --  @param Ret_Count Count of bytes actually written, < count if EOF/error.
   --  @param Success   True on success, False on non-supported/failure.
   --  @param Is_Blocking Whether to do the operation blocking or non-blocking.
   procedure Write
      (Handle      : Device_Handle;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean := True)
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Do a device-specific IO control request.
   --  @param Handle   Handle to operate on, must be valid.
   --  @param Request  Device-specific request.
   --  @param Argument Device-specific argument address.
   --  @result True in success, False if not supported or failed.
   function IO_Control
      (Handle   : Device_Handle;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Do a device-specific memory map request.
   --  @param Handle   Handle to operate on, must be valid.
   --  @param Map      Map to map the device to.
   --  @param Address  Virtual address to map device memory to.
   --  @param Length   Length in bytes of the mapping.
   --  @result True in success, False if not supported or failed.
   function Mmap
      (Handle  : Device_Handle;
       Map     : Arch.MMU.Page_Table_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions) return Boolean
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

   --  Do a device-specific polling for status.
   --  @param Handle    Handle to operate on, must be valid.
   --  @param Can_Read  True if the device can read right now.
   --  @param Can_Write True if the device can write right now.
   --  @param Is_Error  True if the device is in an error state.
   procedure Poll
      (Handle    : Device_Handle;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
      with Pre => ((Is_Initialized = True) and (Handle /= Error_Handle));

private

   type Device_Handle is new Natural range 0 .. 30;
   Error_Handle    : constant Device_Handle := 0;
   Max_Name_Length : constant Natural       := 64;
   type Device is record
      Is_Present : Boolean;
      Name       : String (1 .. Max_Name_Length);
      Name_Len   : Natural range 0 .. Max_Name_Length;
      Contents   : Resource;
   end record;
   type Device_Arr     is array (Device_Handle range 1 .. 30) of Device;
   type Device_Arr_Acc is access Device_Arr;

   Devices_Data : Device_Arr_Acc;

   function Is_Initialized return Boolean is (Devices_Data /= null);
   ----------------------------------------------------------------------------
   type UUID_Fragment is array (Natural range <>) of Unsigned_8;

   function To_Integer (C : Character) return Unsigned_8;
   procedure Convert_LE (Frag : out UUID_Fragment; Val : String);
   procedure Convert_BE (Frag : out UUID_Fragment; Val : String);
end Devices;
