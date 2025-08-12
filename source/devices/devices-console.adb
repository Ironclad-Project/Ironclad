--  devices-console.adb: Driver for debug output.
--  Copyright (C) 2023 streaksu
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

with Arch.Debug;

package body Devices.Console with SPARK_Mode => Off is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Init (Success : out Boolean) is
      Device : Resource;
   begin
      Device :=
         (Data        => System.Null_Address,
          ID          => [others => 0],
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Read        => Read'Access,
          Write       => Write'Access,
          Sync        => null,
          Sync_Range  => null,
          IO_Control  => null,
          Mmap        => null,
          Poll        => null,
          Remove      => null);
      Register (Device, "console", Success);
   end Init;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      if Arch.Debug.Supports_Read then
         Arch.Debug.Read (Data);
         Ret_Count := Data'Length;
         Success   := Dev_Success;
      else
         Ret_Count := 0;
         Success   := Dev_Not_Supported;
      end if;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      Arch.Debug.Print (Data);
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   end Write;
end Devices.Console;
