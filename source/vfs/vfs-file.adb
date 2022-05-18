--  vfs-file.adb: File creation and management.
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
with VFS.Path;
with VFS.USTAR;

package body VFS.File is
   function Open (Path : String; Access_Flags : Access_Mode) return File_Acc is
      Fetched_Dev  : VFS.Device.Device_Data;
      Fetched_Type : VFS.Device.FS_Type := VFS.Device.FS_USTAR;
      Fetched_FS   : System.Address     := System.Null_Address;
      Fetched_File : System.Address     := System.Null_Address;
   begin
      if Path'Length >= 12 and then
         Path (Path'First .. Path'First + 4) = "/dev/"
      then
         if not VFS.Device.Fetch
            (Path (Path'First + 5 .. Path'First + 11), Fetched_Dev)
         then
            return null;
         end if;
      elsif VFS.Path.Is_Absolute (Path) then
         Fetched_FS := VFS.Device.Get_Mount ("/", Fetched_Type);
         if Fetched_FS = Null_Address then
            return null;
         end if;

         case Fetched_Type is
            when VFS.Device.FS_USTAR =>
               Fetched_File :=
                  USTAR.Open (Fetched_FS, Path (Path'First + 1 .. Path'Last));
               if Fetched_File = Null_Address then
                  return null;
               end if;
         end case;
      else
         --  TODO: Do relative opening, at all.
         return null;
      end if;

      --  Return the created file.
      return new File'(
         Dev_Data  => Fetched_Dev,
         FS_Data   => Fetched_FS,
         File_Data => Fetched_File,
         FS_Type   => Fetched_Type,
         Index     => 0,
         Flags     => Access_Flags
      );
   end Open;

   procedure Close (To_Close : File_Acc) is
   begin
      if To_Close /= null and then To_Close.FS_Data /= System.Null_Address then
         USTAR.Close (To_Close.FS_Data, To_Close.File_Data);
      end if;
   end Close;

   function Read
      (To_Read     : File_Acc;
       Count       : Natural;
       Destination : System.Address) return Natural
   is
      Read_Count : Natural;
   begin
      if To_Read = null or else To_Read.Flags = Access_W then
         return 0;
      end if;

      if To_Read.FS_Data /= System.Null_Address
         and To_Read.File_Data /= System.Null_Address
      then
         Read_Count := Integer (USTAR.Read (
            To_Read.FS_Data,
            To_Read.File_Data,
            Unsigned_64 (To_Read.Index),
            Unsigned_64 (Count),
            Destination
         ));
         To_Read.Index := To_Read.Index + Read_Count;
         return Read_Count;
      elsif To_Read.Dev_Data.Read /= null then
         Read_Count := Integer (To_Read.Dev_Data.Read (
            To_Read.Dev_Data.Data,
            Unsigned_64 (To_Read.Index),
            Unsigned_64 (Count),
            Destination
         ));
         To_Read.Index := To_Read.Index + Read_Count;
         return Read_Count;
      else
         return 0;
      end if;
   end Read;

   function Write
      (To_Write : File_Acc;
       Count    : Natural;
       Data     : System.Address) return Natural
   is
      Write_Count : Natural;
   begin
      if To_Write = null or else To_Write.Flags = Access_R then
         return 0;
      end if;
      if To_Write.FS_Data /= System.Null_Address and
         To_Write.File_Data /= System.Null_Address
      then
         return 0;
      else
         Write_Count := Integer (To_Write.Dev_Data.Write (
            To_Write.Dev_Data.Data,
            Unsigned_64 (To_Write.Index),
            Unsigned_64 (Count),
            Data
         ));
         To_Write.Index := To_Write.Index + Write_Count;
         return Write_Count;
      end if;
   end Write;

   function Stat (F : File_Acc; S : out VFS.File_Stat) return Boolean is
   begin
      if F = null then
         return False;
      end if;
      if F.FS_Data /= System.Null_Address and
         F.File_Data /= System.Null_Address
      then
         return USTAR.Stat (F.FS_Data, F.File_Data, S);
      else
         S := F.Dev_Data.Stat;
         return True;
      end if;
   end Stat;

   function IO_Control
      (F        : File_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
   begin
      if F = null then
         return False;
      end if;
      if F.FS_Data /= System.Null_Address and
         F.File_Data /= System.Null_Address
      then
         --  Support USTAR IOCTL.
         return False;
      elsif F.Dev_Data.IO_Control /= null then
         return F.Dev_Data.IO_Control (
            F.Dev_Data.Data,
            Request,
            Argument
         );
      else
         return False;
      end if;
   end IO_Control;
end VFS.File;
