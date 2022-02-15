--  devices-streams.adb: Virtual stream devices.
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

package body Devices.Streams is
   function Init return Boolean is
      Nulldev : constant Root := (
         Name   => "nulldev",
         Data   => System.Null_Address,
         Init   => null,
         Unload => null,
         Sync   => null,
         Create => null,
         Open   => null,
         Close  => null,
         Read   => Nulldev_Read'Access,
         Write  => Nulldev_Write'Access
      );
      Zerodev : constant Root := (
         Name   => "zerodev",
         Data   => System.Null_Address,
         Init   => null,
         Unload => null,
         Sync   => null,
         Create => null,
         Open   => null,
         Close  => null,
         Read   => Zerodev_Read'Access,
         Write  => Zerodev_Write'Access
      );
   begin
      if Register_Root (Nulldev) = False then return False; end if;
      if Register_Root (Zerodev) = False then return False; end if;
      return True;
   end Init;
   ----------------------------------------------------------------------------
   function Nulldev_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : System.Address;
       Count  : Positive;
       Desto  : System.Address) return Natural
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Count);
      pragma Unreferenced (Desto);
   begin
      --  Return that there is nothing to read, end of file.
      return 0;
   end Nulldev_Read;

   function Nulldev_Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : System.Address;
       Count    : Positive;
       To_Write : System.Address) return Natural
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
      pragma Unreferenced (To_Write);
   begin
      --  Return that everything was written successfully
      return Count;
   end Nulldev_Write;
   ----------------------------------------------------------------------------
   function Zerodev_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : System.Address;
       Count  : Positive;
       Desto  : System.Address) return Natural
   is
      Result : array (1 .. Count) of Unsigned_8 with Address => Desto;
      pragma Unreferenced (Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
   begin
      for I of Result loop
         I := 0;
      end loop;
      return Count;
   end Zerodev_Read;

   function Zerodev_Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : System.Address;
       Count    : Positive;
       To_Write : System.Address) return Natural
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Obj);
      pragma Unreferenced (Offset);
      pragma Unreferenced (To_Write);
   begin
      --  Return that everything was written successfully
      return Count;
   end Zerodev_Write;
end Devices.Streams;
