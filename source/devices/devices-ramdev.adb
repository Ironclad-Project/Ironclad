--  devices-ramdev.adb: RAM devices.
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

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Memory.Physical;
with Memory; use Memory;

package body Devices.Ramdev is
   --  TODO: Dynamically detect if its a USTAR, and allow reading it instead
   --  of 'as a raw binary'.

   type Ramdev_Data is record
      Start_Address : System.Address;
      Size          : Virtual_Address;
   end record;
   type Ramdev_Data_Acc is access Ramdev_Data;

   function Init_Module
      (Module : Arch.Stivale2.Module;
       Name   : Root_Name) return Root is
      Start : constant Virtual_Address := To_Integer (Module.Begin_Address);
      End2  : constant Virtual_Address := To_Integer (Module.End_Address);
      Data : constant Ramdev_Data_Acc := new Ramdev_Data'(
         Start_Address => To_Address (Start + Memory_Offset),
         Size          => End2 - Start
      );
   begin
      return (
         Name   => Name,
         Data   => Data.all'Address,
         Init   => Ramdev_Init'Access,
         Unload => Ramdev_Unload'Access,
         Sync   => null,
         Create => null,
         Open   => null,
         Close  => null,
         Read   => Ramdev_Read'Access,
         Write  => null
      );
   end Init_Module;

   function Ramdev_Init (Data : Root_Data) return Root_Data is
   begin
      return Data;
   end Ramdev_Init;

   procedure Ramdev_Unload (Data : Root_Data) is
   begin
      Memory.Physical.Free (To_Integer (Data));
   end Ramdev_Unload;

   function Ramdev_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : System.Address;
       Count  : Positive;
       Desto  : System.Address) return Natural is
      Data2     : Ramdev_Data with Address => Data;
      Data_Addr : constant System.Address  := Data2.Start_Address;
      Data_Sz   : constant Natural         := Natural (Data2.Size);
      Result    : array (1 .. Count)   of Unsigned_8 with Address => Desto;
      Real_Data : array (1 .. Data_Sz) of Unsigned_8 with Address => Data_Addr;
      Offset2   : constant Natural := Natural (To_Integer (Offset)) + Count;
      To_Write  : Natural := Count;
      pragma Unreferenced (Obj);
   begin
      if Offset2 > Data_Sz then
         To_Write := To_Write - (Offset2 - Data_Sz);
      end if;

      for I in 1 .. To_Write loop
         Result (I) := Real_Data (Natural (To_Integer (Offset)) + I);
      end loop;

      return To_Write;
   end Ramdev_Read;
end Devices.Ramdev;
