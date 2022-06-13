--  vfs.adb: FS and register dispatching.
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

with System; use System;
with VFS.USTAR;

package body VFS is
   type Device_Container is record
      Is_Present  : Boolean;
      Name        : String (1 .. 64);
      Name_Len    : Natural;
      Contents    : aliased Resource;
      Is_Mounted  : Boolean;
      Mounted_FS  : FS_Type;
      FS_Data     : System.Address;
      Path_Length : Natural;
      Path_Buffer : String (1 .. 100);
   end record;
   type Device_Container_Arr is array (1 .. 20) of Device_Container;
   Devices : access Device_Container_Arr;

   procedure Init is
   begin
      Devices := new Device_Container_Arr;
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
            Devices (I).Is_Present                      := True;
            Devices (I).Name (1 .. Name'Length)         := Name;
            Devices (I).Name_Len                        := Name'Length;
            Devices (I).Contents                        := Dev;
            Devices (I).Contents.Stat.Unique_Identifier := Unsigned_64 (I);
            Devices (I).Is_Mounted                      := False;
            Devices (I).FS_Data                         := System.Null_Address;
            Devices (I).Path_Length                     := 0;
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

   function Mount
      (Name : String;
       Path : String;
       FS : FS_Type) return Boolean
   is
      FS_Data : System.Address := System.Null_Address;
   begin
      for E of Devices.all loop
         if E.Is_Present and then E.Name (1 .. E.Name_Len) = Name then
            case FS is
               when FS_USTAR =>
                  FS_Data := USTAR.Probe (E.Contents'Access);
                  if FS_Data /= System.Null_Address then
                     E.Is_Mounted  := True;
                     E.Mounted_FS  := FS;
                     E.FS_Data     := FS_Data;
                     E.Path_Length := Path'Length;
                     E.Path_Buffer (1 .. Path'Length) := Path;
                     return True;
                  end if;
                  exit;
            end case;
         end if;
      end loop;

      return False;
   end Mount;

   function Get_Mount
      (Path : String;
       FS   : out FS_Type;
       Dev  : out Resource_Acc) return System.Address
   is
   begin
      for E of Devices.all loop
         if E.Is_Present and E.Is_Mounted then
            if Path = E.Path_Buffer (1 .. E.Path_Length) then
               FS  := E.Mounted_FS;
               Dev := E.Contents'Access;
               return E.FS_Data;
            end if;
         end if;
      end loop;
      FS := FS_USTAR;
      return System.Null_Address;
   end Get_Mount;

   procedure Unmount (Path : String) is
   begin
      for E of Devices.all loop
         if E.Is_Present and E.Is_Mounted then
            if Path = E.Path_Buffer (1 .. E.Path_Length) then
               if E.Contents.Sync /= null then
                  E.Contents.Sync.all (E.Contents'Access);
               end if;
               E.Is_Mounted := False;
            end if;
         end if;
      end loop;
   end Unmount;
end VFS;
