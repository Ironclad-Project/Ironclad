--  devices-streams.adb: Virtual stream devices.
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

with Cryptography.Random;

package body Devices.Streams is
   pragma Suppress (All_Checks); --  Unit passes GNATprove AoRTE..

   procedure Init (Success : out Boolean) is
      Success_1, Success_2, Success_3, Success_4 : Boolean;
   begin
      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Null_Read'Access,
           Write       => Null_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null), "null", Success_1);
      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Zero_Read'Access,
           Write       => Zero_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null), "zero", Success_2);
      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Random_Read'Access,
           Write       => Random_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null), "random", Success_3);
      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Random_Read'Access,
           Write       => Random_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null), "urandom", Success_4);
      Success := Success_1 and Success_2 and Success_3 and Success_4;
   end Init;
   ----------------------------------------------------------------------------
   procedure Null_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Data);
      pragma Unreferenced (Is_Blocking);
   begin
      Data      := (others => 0);
      Ret_Count := 0;
      Success   := True;
   end Null_Read;

   procedure Null_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
   begin
      Ret_Count := Data'Length;
      Success   := True;
   end Null_Write;
   ----------------------------------------------------------------------------
   procedure Zero_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
   begin
      Data      := (others => 0);
      Ret_Count := Data'Length;
      Success   := True;
   end Zero_Read;

   procedure Zero_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
   begin
      Ret_Count := Data'Length;
      Success   := True;
   end Zero_Write;
   ----------------------------------------------------------------------------
   procedure Random_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
   begin
      Cryptography.Random.Fill_Data (Cryptography.Random.Crypto_Data (Data));
      Ret_Count := Data'Length;
      Success   := True;
   end Random_Read;

   procedure Random_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
   begin
      if Data'Length >= 4 then
         Cryptography.Random.Feed_Entropy
            (Shift_Left (Unsigned_32 (Data (Data'First + 3)), 24) or
             Shift_Left (Unsigned_32 (Data (Data'First + 2)), 16) or
             Shift_Left (Unsigned_32 (Data (Data'First + 1)),  8) or
             Shift_Left (Unsigned_32 (Data (Data'First + 0)),  0));
      end if;
      Ret_Count := Data'Length;
      Success   := True;
   end Random_Write;
end Devices.Streams;
