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

with Cryptography.Random; use Cryptography.Random;

package body Devices.Streams is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Init (Success : out Boolean) is
      pragma SPARK_Mode (Off); --  Access to procedures is not SPARK friendly.
      Success_1, Success_2, Success_3, Success_4, Success_5 : Boolean;
   begin
      Register
         ((Data        => System.Null_Address,
           ID          => Zero_UUID,
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Null_Read'Access,
           Write       => Null_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null,
           Remove      => null), "null", Success_1);
      Register
         ((Data        => System.Null_Address,
           ID          => Zero_UUID,
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Zero_Read'Access,
           Write       => Zero_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null,
           Remove      => null), "zero", Success_2);
      Register
         ((Data        => System.Null_Address,
           ID          => Zero_UUID,
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Full_Read'Access,
           Write       => Full_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null,
           Remove      => null), "full", Success_3);
      Register
         ((Data        => System.Null_Address,
           ID          => Zero_UUID,
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Random_Read'Access,
           Write       => Random_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null,
           Remove      => null), "random", Success_4);
      Register
         ((Data        => System.Null_Address,
           ID          => Zero_UUID,
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Random_Read'Access,
           Write       => Random_Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null,
           Remove      => null), "urandom", Success_5);
      Success := Success_1 and Success_2 and Success_3 and Success_4 and
                 Success_5;
   end Init;
   ----------------------------------------------------------------------------
   procedure Null_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Data, Is_Blocking);
   begin
      Data      := [others => 0];
      Ret_Count := 0;
      Success   := Dev_Success;
   end Null_Read;

   procedure Null_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   end Null_Write;
   ----------------------------------------------------------------------------
   procedure Zero_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      Data      := [others => 0];
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   end Zero_Read;

   procedure Zero_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   end Zero_Write;
   ----------------------------------------------------------------------------
   procedure Full_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      Data      := [others => 0];
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   end Full_Read;

   procedure Full_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Data, Is_Blocking);
   begin
      Ret_Count := 0;
      Success   := Dev_Full;
   end Full_Write;
   ----------------------------------------------------------------------------
   procedure Random_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      Cryptography.Random.Fill_Data (Cryptography.Random.Crypto_Data (Data));
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   end Random_Read;

   procedure Random_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Is_Blocking);
   begin
      Feed_Entropy (Crypto_Data (Data));
      Ret_Count := Data'Length;
      Success   := Dev_Success;
   end Random_Write;
end Devices.Streams;
