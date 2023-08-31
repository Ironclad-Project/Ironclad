--  devices.adb: Device management.
--  Copyright (C) 2023 streaksu
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

with Devices.Random;
with Devices.Streams;
with Devices.Debug;
with Lib.Panic;
with Arch.Hooks;

package body Devices is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Init is
      pragma SPARK_Mode (Off); --  Some devices here are not verified.
      Success : Boolean;
   begin
      Devices_Data := new Device_Arr;
      for Dev of Devices_Data.all loop
         Dev.Is_Present := False;
      end loop;

      Random.Init (Success);
      if not Success then goto Panic_Error; end if;
      Streams.Init (Success);
      if not Success then goto Panic_Error; end if;
      Debug.Init (Success);
      if not Success or else not Arch.Hooks.Devices_Hook then
         goto Panic_Error;
      end if;

      return;

   <<Panic_Error>>
      Lib.Panic.Hard_Panic ("Some devices could not be added");
   end Init;

   procedure Register (Dev : Resource; Name : String; Success : out Boolean) is
   begin
      --  Search if the name is already taken.
      Success := False;
      for E of Devices_Data.all loop
         if E.Is_Present and then E.Name (1 .. E.Name_Len) = Name then
            return;
         end if;
      end loop;

      --  Allocate.
      for I in Devices_Data'Range loop
         if not Devices_Data (I).Is_Present then
            Devices_Data (I).Is_Present              := True;
            Devices_Data (I).Name (1 .. Name'Length) := Name;
            Devices_Data (I).Name_Len                := Name'Length;
            Devices_Data (I).Contents                := Dev;
            Success := True;
            exit;
         end if;
      end loop;
   end Register;

   function Fetch (Name : String) return Device_Handle is
   begin
      for I in Devices_Data'Range loop
         if Devices_Data (I).Is_Present and
            Devices_Data (I).Name (1 .. Devices_Data (I).Name_Len) = Name
         then
            return I;
         end if;
      end loop;
      return Error_Handle;
   end Fetch;

   procedure Fetch_Name
      (Handle : Device_Handle;
       Name   : out String;
       Length : out Natural)
   is
   begin
      Name   := Devices_Data (Handle).Name;
      Length := Devices_Data (Handle).Name_Len;
   end Fetch_Name;

   function Is_Block_Device (Handle : Device_Handle) return Boolean is
   begin
      return Devices_Data (Handle).Contents.Is_Block;
   end Is_Block_Device;

   function Get_Block_Count (Handle : Device_Handle) return Unsigned_64 is
   begin
      return Devices_Data (Handle).Contents.Block_Count;
   end Get_Block_Count;

   function Get_Block_Size (Handle : Device_Handle) return Natural is
   begin
      return Devices_Data (Handle).Contents.Block_Size;
   end Get_Block_Size;

   function Get_Unique_ID (Handle : Device_Handle) return Natural is
   begin
      --  Since handles are unique, reuse them as unique ID.
      return Natural (Handle);
   end Get_Unique_ID;

   function Is_Read_Only (Handle : Device_Handle) return Boolean is
   begin
      return Devices_Data (Handle).Contents.Write = null;
   end Is_Read_Only;

   function Synchronize (Handle : Device_Handle) return Boolean is
      Success : Boolean := True;
   begin
      if Devices_Data (Handle).Contents.Sync /= null then
         Success := Devices_Data (Handle).Contents.Sync
            (Devices_Data (Handle).Contents.Data);
      end if;
      return Success;
   end Synchronize;

   function Synchronize
      (Handle : Device_Handle;
       Offset : Unsigned_64;
       Count  : Unsigned_64) return Boolean
   is
      Success : Boolean := True;
   begin
      if Devices_Data (Handle).Contents.Sync_Range /= null then
         Success := Devices_Data (Handle).Contents.Sync_Range
            (Devices_Data (Handle).Contents.Data, Offset, Count);
      elsif Devices_Data (Handle).Contents.Sync /= null then
         Success := Devices_Data (Handle).Contents.Sync
            (Devices_Data (Handle).Contents.Data);
      end if;
      return Success;
   end Synchronize;

   procedure Read
      (Handle    : Device_Handle;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
   begin
      if Devices_Data (Handle).Contents.Read /= null then
         Devices_Data (Handle).Contents.Read
            (Key       => Devices_Data (Handle).Contents.Data,
             Offset    => Offset,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Data      := (others => 0);
         Ret_Count := 0;
         Success   := False;
      end if;
   end Read;

   procedure Write
      (Handle    : Device_Handle;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
   begin
      if Devices_Data (Handle).Contents.Write /= null then
         Devices_Data (Handle).Contents.Write
            (Key       => Devices_Data (Handle).Contents.Data,
             Offset    => Offset,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Success);
      else
         Ret_Count := 0;
         Success   := False;
      end if;
   end Write;

   function IO_Control
      (Handle   : Device_Handle;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
   begin
      if Devices_Data (Handle).Contents.IO_Control /= null then
         return Devices_Data (Handle).Contents.IO_Control
            (Devices_Data (Handle).Contents.Data, Request, Argument);
      else
         return False;
      end if;
   end IO_Control;

   function Mmap
      (Handle  : Device_Handle;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions) return Boolean
   is
   begin
      if Devices_Data (Handle).Contents.Mmap /= null then
         return Devices_Data (Handle).Contents.Mmap
            (Devices_Data (Handle).Contents.Data, Address, Length, Flags);
      else
         return False;
      end if;
   end Mmap;

   procedure Poll
      (Handle    : Device_Handle;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
   begin
      if Devices_Data (Handle).Contents.Poll /= null then
         Devices_Data (Handle).Contents.Poll
            (Devices_Data (Handle).Contents.Data, Can_Read, Can_Write,
             Is_Error);
      else
         Can_Read  := False;
         Can_Write := False;
         Is_Error  := True;
      end if;
   end Poll;
end Devices;
