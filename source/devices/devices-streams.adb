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

package body Devices.Streams is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Init (Success : out Boolean) is
      Nulldev : Resource;
      Zerodev : Resource;
      Success_1, Success_2 : Boolean;
   begin
      Nulldev := (
         Data        => System.Null_Address,
         Is_Block    => False,
         Block_Size  => 4096,
         Block_Count => 0,
         Read        => Null_Read'Access,
         Write       => Null_Write'Access,
         Sync        => null,
         Sync_Range  => null,
         IO_Control  => null,
         Mmap        => null,
         Munmap      => null
      );

      Zerodev := (
         Data        => System.Null_Address,
         Is_Block    => False,
         Block_Size  => 4096,
         Block_Count => 0,
         Read        => Zero_Read'Access,
         Write       => Zero_Write'Access,
         Sync        => null,
         Sync_Range  => null,
         IO_Control  => null,
         Mmap        => null,
         Munmap      => null
      );

      Register (Nulldev, "null", Success_1);
      Register (Zerodev, "zero", Success_2);
      Success := Success_1 and Success_2;
   end Init;
   ----------------------------------------------------------------------------
   procedure Null_Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Data);
   begin
      Data      := (others => 0);
      Ret_Count := 0;
      Success   := True;
   end Null_Read;

   procedure Null_Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
   begin
      Ret_Count := Data'Length;
      Success   := True;
   end Null_Write;
   ----------------------------------------------------------------------------
   procedure Zero_Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
   begin
      Data      := (others => 0);
      Ret_Count := Data'Length;
      Success   := True;
   end Zero_Read;

   procedure Zero_Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
   begin
      Ret_Count := Data'Length;
      Success   := True;
   end Zero_Write;
end Devices.Streams;
