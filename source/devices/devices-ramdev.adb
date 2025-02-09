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
with Lib.Synchronization;

package body Devices.Ramdev is
   --  Ramdev data.
   type Ramdev_Data is record
      Mutex         : aliased Lib.Synchronization.Readers_Writer_Lock;
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
         (Mutex         => Lib.Synchronization.Unlocked_RW_Lock,
          Start_Address => Module.Start,
          Size          => Unsigned_64 (Module.Length));
   begin
      return
         (Data        => Data.all'Address,
          Is_Block    => False,
          ID          => [others => 0],
          Block_Size  => 4096,
          Block_Count => A.Divide_Round_Up (Data.Size, 4096),
          Read        => Read'Access,
          Write       => Write'Access,
          Sync        => null,
          Sync_Range  => null,
          IO_Control  => null,
          Mmap        => null,
          Poll        => null,
          Remove      => null);
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

      Dev      : Ramdev_Data with Import, Address => Key;
      Dev_Data : constant array (1 .. Dev.Size) of Unsigned_8
         with Import, Address => Dev.Start_Address;

      Final_Loc : constant Unsigned_64 := Offset + Unsigned_64 (Data'Length);
      To_Read   :              Natural := Data'Length;
   begin
      if Offset > Dev.Size then
         Data      := [others => 0];
         Ret_Count := 0;
         Success   := True;
         return;
      elsif Final_Loc >= Dev.Size then
         To_Read := Natural (Dev.Size - Offset);
      end if;

      Lib.Synchronization.Seize_Reader (Dev.Mutex);
      for I in 1 .. To_Read loop
         Data (Data'First + I - 1) := Dev_Data (Offset + Unsigned_64 (I));
      end loop;
      Lib.Synchronization.Release_Reader (Dev.Mutex);

      Ret_Count := To_Read;
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

      Dev      : Ramdev_Data with Import, Address => Key;
      Dev_Data : array (1 .. Dev.Size) of Unsigned_8
         with Import, Address => Dev.Start_Address;

      Final_Loc : constant Unsigned_64 := Offset + Unsigned_64 (Data'Length);
      To_Write  :              Natural := Data'Length;
   begin
      if Offset > Dev.Size then
         Ret_Count := 0;
         Success   := True;
         return;
      elsif Final_Loc >= Dev.Size then
         To_Write := Natural (Dev.Size - Offset);
      end if;

      Lib.Synchronization.Seize_Writer (Dev.Mutex);
      for I in 1 .. To_Write loop
         Dev_Data (Offset + Unsigned_64 (I)) := Data (Data'First + I - 1);
      end loop;
      Lib.Synchronization.Release_Writer (Dev.Mutex);

      Ret_Count := To_Write;
      Success   := True;
   end Write;
end Devices.Ramdev;
