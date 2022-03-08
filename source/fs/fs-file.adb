--  fs-file.adb: File creation and management.
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
with FS;

package body FS.File is
   function Open (Path : String; Access_Flags : Access_Mode) return File_Acc is
      Root_Path : constant String := Path (Path'First + 1 .. Path'First + 7);
      Rest_Path : constant String := Path (Path'First + 9 .. Path'Last);
      Fetched_Root : FS.Root;
      Fetched_Obj  : FS.Object := System.Null_Address;
   begin
      --  Fetch the root and object.
      if not FS.Get_Root (Root_Path, Fetched_Root) then
         return null;
      end if;
      if Rest_Path'Length /= 0 then
         Fetched_Obj := Fetched_Root.Open.all (Fetched_Root.Data, Rest_Path);
         if Fetched_Obj = System.Null_Address then
            return null;
         end if;
      end if;

      --  Return the created file.
      return new File'(
         Root   => Fetched_Root,
         Object => Fetched_Obj,
         Index  => 0,
         Flags  => Access_Flags
      );
   end Open;

   procedure Close (To_Close : File_Acc) is
   begin
      if To_Close /= null then
         To_Close.Root.Close.all (
            To_Close.Root.Data,
            To_Close.Object
         );
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
      else
         Read_Count := To_Read.Root.Read.all (
            To_Read.Root.Data,
            To_Read.Object,
            System'To_Address (To_Read.Index),
            Count,
            Destination
         );
         To_Read.Index := To_Read.Index + Read_Count;
         return Read_Count;
      end if;
   end Read;

   function Write
      (To_Write : File_Acc;
       Count    : Natural;
       Data     : System.Address) return Natural
   is
      Written_Count : Natural;
   begin
      if To_Write = null or else To_Write.Flags = Access_W then
         return 0;
      else
         Written_Count := To_Write.Root.Write.all (
            To_Write.Root.Data,
            To_Write.Object,
            System'To_Address (To_Write.Index),
            Count,
            Data
         );
         To_Write.Index := To_Write.Index + Written_Count;
         return Written_Count;
      end if;
   end Write;

   function Get_Size (F : File_Acc) return Natural is
   begin
      if F /= null then
         return F.Root.Get_Size.all (
            F.Root.Data,
            F.Object
         );
      end if;
      return 0;
   end Get_Size;
end FS.File;
