--  devices-ramdev.adb: RAM devices.
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

with System; use System;
with Lib.Alignment;

package body Devices.Ramdev is
   --  Ramdev data.
   type Ramdev_Data is record
      Start_Address : System.Address;
      Size          : Unsigned_64;
   end record;
   type Ramdev_Data_Acc is access Ramdev_Data;

   function Init (Modules : Arch.Boot_RAM_Files) return Boolean is
      Success  : Boolean;
      Dev_Res  : Resource;
      Dev_Name : String := "ramdev0";
   begin
      for I in 1 .. Modules'Length loop
         Dev_Name (Dev_Name'Last) := Character'Val (I + Character'Pos ('0'));
         Dev_Res                  := Init_Module (Modules (I));
         Devices.Register (Dev_Res, Dev_Name, Success);
         if not Success then
            return False;
         end if;
      end loop;
      return True;
   end Init;

   function Init_Module (Module : Arch.Boot_RAM_File) return Resource is
      package A is new Lib.Alignment (Unsigned_64);
      Data   : constant Ramdev_Data_Acc := new Ramdev_Data'
         (Start_Address => Module.Start,
          Size          => Unsigned_64 (Module.Length));
   begin
      return
         (Data        => Data.all'Address,
          Is_Block    => False,
          ID          => (others => 0),
          Block_Size  => 4096,
          Block_Count => A.Divide_Round_Up (Data.Size, 4096),
          Read        => Read'Access,
          Write       => Write'Access,
          Sync        => null,
          Sync_Range  => null,
          IO_Control  => null,
          Mmap        => null,
          Poll        => null);
   end Init_Module;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
      Dev      : constant Ramdev_Data with Import, Address => Key;
      Dev_Size : constant        Natural := Natural (Dev.Size);
      Dev_Data : constant Operation_Data (1 .. Dev_Size)
         with Import, Address => Dev.Start_Address;

      Final_Loc : constant Natural := Natural (Offset) + Data'Length;
      To_Write  :          Natural := Data'Length;
   begin
      if Final_Loc > Dev_Size then
         To_Write := To_Write - (Final_Loc - Dev_Size);
      end if;

      Data (Data'First .. Data'First + To_Write - 1) :=
         Dev_Data (Natural (Offset) + 1 .. Natural (Offset) + To_Write);

      Ret_Count := To_Write;
      Success   := True;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
      Dev      : constant Ramdev_Data with Import, Address => Key;
      Dev_Size : constant        Natural := Natural (Dev.Size);
      Dev_Data : Operation_Data (1 .. Dev_Size)
         with Import, Address => Dev.Start_Address;

      Final_Loc : constant Natural := Natural (Offset) + Data'Length;
      To_Write  :          Natural := Data'Length;
   begin
      if Final_Loc > Dev_Size then
         To_Write := To_Write - (Final_Loc - Dev_Size);
      end if;

      Dev_Data (Natural (Offset) + 1 .. Natural (Offset) + To_Write) :=
         Data (Data'First .. Data'First + To_Write - 1);

      Ret_Count := To_Write;
      Success   := True;
   end Write;
end Devices.Ramdev;
