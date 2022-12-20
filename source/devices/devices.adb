--  devices.adb: Device management.
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

with Devices.Random;
with Devices.Streams;
with Devices.Debug;
with Lib.Panic;
with Arch.Hooks;
with Devices.PTY;
with Config;

package body Devices with SPARK_Mode => Off is
   type Device_Container is record
      Is_Present : Boolean;
      Name       : String (1 .. 64);
      Name_Len   : Natural;
      Contents   : aliased Resource;
   end record;
   type Device_Container_Arr is array (1 .. 20) of Device_Container;
   Devices : access Device_Container_Arr;

   procedure Init is
   begin
      Devices := new Device_Container_Arr;

      --  Initialize architectural devices.
      Arch.Hooks.Devices_Hook;

      --  Initialize config-driven devices.
      if Config.Support_Device_Streams then
         if not Streams.Init then
            goto Failure;
         end if;
      end if;
      if Config.Support_Device_RNG then
         if not Random.Init then
            goto Failure;
         end if;
      end if;
      if Config.Support_Device_PTY then
         if not PTY.Init then
            goto Failure;
         end if;
      end if;

      --  Initialize unconditional devices.
      if not Debug.Init then
         goto Failure;
      end if;
      return;

   <<Failure>>
      Lib.Panic.Hard_Panic ("Could not start arch-independent devices");
   end Init;

   function Register (Dev : Resource; Name : String) return Boolean is
   begin
      --  Search if the name is already taken.
      for E of Devices.all loop
         if E.Is_Present and then E.Name (1 .. E.Name_Len) = Name then
            return False;
         end if;
      end loop;

      --  Allocate.
      for I in Devices'Range loop
         if not Devices (I).Is_Present then
            Devices (I).Is_Present                 := True;
            Devices (I).Name (1 .. Name'Length)    := Name;
            Devices (I).Name_Len                   := Name'Length;
            Devices (I).Contents                   := Dev;
            Devices (I).Contents.Unique_Identifier := I;
            return True;
         end if;
      end loop;

      return False;
   end Register;

   function Fetch (Name : String) return Resource_Acc is
   begin
      for E of Devices.all loop
         if E.Is_Present and then E.Name (1 .. E.Name_Len) = Name then
            return E.Contents'Access;
         end if;
      end loop;
      return null;
   end Fetch;
end Devices;
