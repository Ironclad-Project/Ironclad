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

with VFS.USTAR;
with System; use System;

package body VFS with SPARK_Mode => Off is
   type Mount_Container is record
      Mounted_Dev : Devices.Resource_Acc;
      Mounted_FS  : FS_Type;
      FS_Data     : System.Address;
      Path_Length : Natural;
      Path_Buffer : String (1 .. 100);
   end record;
   type Mount_Container_Arr is array (1 .. 5) of Mount_Container;
   Mounts : access Mount_Container_Arr;

   procedure Init is
   begin
      Mounts := new Mount_Container_Arr;
      for Mount of Mounts.all loop
         Mount.Mounted_Dev := null;
      end loop;
   end Init;

   function Mount
      (Name : String;
       Path : String;
       FS : FS_Type) return Boolean
   is
      Free_I  : Natural := 0;
      Dev     : constant Devices.Resource_Acc := Devices.Fetch (Name);
      FS_Data : System.Address := System.Null_Address;
   begin
      --  Check whether we found the dev, or whether it is already mounted.
      if Dev = null then
         return False;
      end if;
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev = Dev then
            return False;
         elsif Mounts (I).Mounted_Dev = null then
            Free_I := I;
         end if;
      end loop;

      --  Register if found.
      if Free_I /= 0 then
         case FS is
            when FS_USTAR =>
               FS_Data := VFS.USTAR.Probe (Dev);
               if FS_Data /= System.Null_Address then
                  Mounts (Free_I).Mounted_Dev                    := Dev;
                  Mounts (Free_I).FS_Data                        := FS_Data;
                  Mounts (Free_I).Path_Length                   := Path'Length;
                  Mounts (Free_I).Path_Buffer (1 .. Path'Length) := Path;
               end if;
         end case;
         return True;
      end if;
      return False;
   end Mount;

   function Get_Mount
      (Path : String;
       FS   : out FS_Type;
       Dev  : out Devices.Resource_Acc) return System.Address
   is
   begin
      for Mount of Mounts.all loop
         if Mount.Mounted_Dev /= null and then
            Mount.Path_Buffer (1 .. Mount.Path_Length) = Path
         then
            FS  := Mount.Mounted_FS;
            Dev := Mount.Mounted_Dev;
            return Mount.FS_Data;
         end if;
      end loop;
      FS := FS_USTAR;
      return System.Null_Address;
   end Get_Mount;

   procedure Unmount (Path : String) is
   begin
      for Mount of Mounts.all loop
         if Mount.Mounted_Dev /= null and then
            Mount.Path_Buffer (1 .. Mount.Path_Length) = Path
         then
            if Mount.Mounted_Dev.Sync /= null then
               Mount.Mounted_Dev.Sync.all (Mount.Mounted_Dev);
            end if;
            Mount.Mounted_Dev := null;
         end if;
      end loop;
   end Unmount;
   ----------------------------------------------------------------------------
   function Is_Absolute (Path : String) return Boolean is
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute;

   function Is_Canonical (Path : String) return Boolean is
      Previous_Char : Character := ' ';
   begin
      if not Is_Absolute (Path) or else Path (Path'Last) = '/' then
         return False;
      end if;

      for C of Path loop
         if (Previous_Char = '/' and C = '/') or
            (Previous_Char = '.' and C = '.')
         then
            return False;
         end if;
         Previous_Char := C;
      end loop;

      return True;
   end Is_Canonical;
end VFS;
