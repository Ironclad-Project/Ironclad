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
with Lib.Synchronization;

package body FS.File is
   type File_Data is record
      Is_Present    : Boolean;
      Root_Data     : FS.Root;
      Root_Object   : FS.Object;
      Current_Index : Natural;
      References    : Positive;
      Is_Error      : Boolean;
      Mode          : Access_Mode;
   end record;
   type File_Data_Arr is array (FD range 1 .. 256) of File_Data;
   type File_Data_Acc is access File_Data_Arr;
   File_Info_Lock : aliased Lib.Synchronization.Binary_Semaphore;
   File_Info      : File_Data_Acc;

   procedure Init is
   begin
      --  Initialize the registry.
      File_Info := new File_Data_Arr;
      Lib.Synchronization.Release (File_Info_Lock'Access);
   end Init;

   function Open (Name : String; Flags : Access_Mode) return FD is
      Returned_FD  : FD := Error_FD;
      Fetched_Root : FS.Root;
      Fetched_Obj  : FS.Object := System.Null_Address;
   begin
      --  Find an available file.
      Lib.Synchronization.Seize (File_Info_Lock'Access);
      for I in File_Info'First .. File_Info'Last loop
         if not File_Info (I).Is_Present then
            Returned_FD := I;
            goto Found_File;
         end if;
      end loop;
      goto Returning;

   <<Found_File>>
      declare
         Root_Nam : constant String := Name (Name'First + 1 .. Name'First + 7);
         Real_Nam : constant String := Name (Name'First + 9 .. Name'Last);
      begin
         --  Fetch the root and object.
         if not FS.Get_Root (Root_Nam, Fetched_Root) then
            Returned_FD := Error_FD;
            goto Returning;
         end if;
         if Real_Nam'Length /= 0 then
            Fetched_Obj := Fetched_Root.Open.all (Fetched_Root.Data, Real_Nam);
            if Fetched_Obj = System.Null_Address then
               Returned_FD := Error_FD;
               goto Returning;
            end if;
         end if;
      end;

      --  Fill the file.
      File_Info (Returned_FD) := (
         Is_Present    => True,
         Root_Data     => Fetched_Root,
         Root_Object   => Fetched_Obj,
         Current_Index => 0,
         References    => 1,
         Is_Error      => False,
         Mode          => Flags
      );

   <<Returning>>
      Lib.Synchronization.Release (File_Info_Lock'Access);
      return Returned_FD;
   end Open;

   procedure Close (ID : FD) is
   begin
      if ID = Error_FD then
         return;
      end if;

      Lib.Synchronization.Seize (File_Info_Lock'Access);

      if File_Info (ID).References = 1 then
         File_Info (ID).Root_Data.Close.all (
            File_Info (ID).Root_Data.Data,
            File_Info (ID).Root_Object
         );
         File_Info (ID).Is_Present := False;
      else
         File_Info (ID).References := File_Info (ID).References - 1;
      end if;

      Lib.Synchronization.Release (File_Info_Lock'Access);
   end Close;

   function Read
      (ID    : FD;
       Count : Integer;
       Desto : System.Address) return Natural
   is
      Ret_Count : Natural;
   begin
      if ID = Error_FD or else File_Info (ID).Mode = Access_W then
         return 0;
      end if;

      Ret_Count := File_Info (ID).Root_Data.Read.all (
         File_Info (ID).Root_Data.Data,
         File_Info (ID).Root_Object,
         System'To_Address (File_Info (ID).Current_Index),
         Count,
         Desto
      );

      File_Info (ID).Current_Index := File_Info (ID).Current_Index + Ret_Count;

      return Ret_Count;
   end Read;

   function Write
      (ID    : FD;
       Count : Integer;
       Data  : System.Address) return Natural
   is
      Ret_Count : Natural;
   begin
      if ID = Error_FD or else File_Info (ID).Mode = Access_R then
         return 0;
      end if;

      Ret_Count := File_Info (ID).Root_Data.Write.all (
         File_Info (ID).Root_Data.Data,
         File_Info (ID).Root_Object,
         System'To_Address (File_Info (ID).Current_Index),
         Count,
         Data
      );

      File_Info (ID).Current_Index := File_Info (ID).Current_Index + Ret_Count;
      return Ret_Count;
   end Write;

   function Get_Size (ID : FD) return Natural is
      Ret_Size : Natural;
   begin
      if ID = Error_FD then
         return 0;
      end if;

      Ret_Size := File_Info (ID).Root_Data.Get_Size.all (
         File_Info (ID).Root_Data.Data,
         File_Info (ID).Root_Object
      );

      return Ret_Size;
   end Get_Size;

   procedure Set_Index (ID : FD; Index : Natural) is
   begin
      File_Info (ID).Current_Index := Index;
   end Set_Index;

   procedure Reset (ID : FD) is
   begin
      File_Info (ID).Current_Index := 0;
      File_Info (ID).Is_Error := False;
   end Reset;
end FS.File;
