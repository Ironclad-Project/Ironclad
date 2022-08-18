--  devices-random.adb: Random devices.
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

with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Lib.Synchronization;
with Memory.Physical;
with Memory; use Memory;
with Arch.Snippets;

package body Devices.Random with SPARK_Mode => Off is
   Reseed_Limit : constant := 4096;
   type Random_Data is record
      Since_Reseed : Unsigned_64;
      Seed1        : Unsigned_32;
      Seed2        : Unsigned_32;
      Seed3        : Unsigned_32;
      Seed4        : Unsigned_32;
   end record;
   type Random_Data_Acc is access all Random_Data;

   package Conv is new System.Address_To_Access_Conversions (Random_Data);

   function Init return Boolean is
      Stat : constant VFS.File_Stat := (
         Unique_Identifier => 0,
         Type_Of_File      => VFS.File_Character_Device,
         Mode              => 8#660#,
         Hard_Link_Count   => 1,
         Byte_Size         => 0,
         IO_Block_Size     => 4096,
         IO_Block_Count    => 0
      );
      Random_Dat : constant Random_Data_Acc := new Random_Data'(
         Since_Reseed => 0,
         others       => <>
      );
      Random_Res : VFS.Resource := (
         Data       => Conv.To_Address (Conv.Object_Pointer (Random_Dat)),
         Mutex      => <>,
         Stat       => Stat,
         Sync       => null,
         Read       => Random_Read'Access,
         Write      => null,
         IO_Control => null,
         Mmap       => null,
         Munmap     => null
      );
   begin
      Reseed (
         Random_Dat.Seed1,
         Random_Dat.Seed2,
         Random_Dat.Seed3,
         Random_Dat.Seed4
      );
      Lib.Synchronization.Release (Random_Res.Mutex);
      return VFS.Register (Random_Res, "random");
   end Init;

   function Random_Read
      (Data   : VFS.Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Buff : array (1 .. Count / 4) of Unsigned_32
         with Address => Desto, Import;
      Dat : constant Random_Data_Acc :=
         Random_Data_Acc (Conv.To_Pointer (Data.Data));
      Inter : Unsigned_32;
   begin
      if Buff'Length = 0 then
         return 0;
      end if;

      Lib.Synchronization.Seize (Data.Mutex);

      --  Adjust reseeds and bytes since reseed.
      if Dat.Since_Reseed >= Reseed_Limit then
         Dat.Since_Reseed := 0;
         Reseed (Dat.Seed1, Dat.Seed2, Dat.Seed3, Dat.Seed4);
      end if;
      Dat.Since_Reseed := Dat.Since_Reseed + Count;

      for Val of Buff loop
         --  Mix our seeds using LFSR113.
         Inter := Shift_Right (Shift_Left (Dat.Seed1, 6) xor Dat.Seed1, 13);
         Dat.Seed1 := Shift_Left (Dat.Seed1 and 16#FFFFFFFE#, 18) xor Inter;
         Inter := Shift_Right (Shift_Left (Dat.Seed2, 2) xor Dat.Seed2, 27);
         Dat.Seed2 := Shift_Left (Dat.Seed2 and 16#FFFFFFFE#, 2) xor Inter;
         Inter := Shift_Right (Shift_Left (Dat.Seed3, 13) xor Dat.Seed3, 21);
         Dat.Seed3 := Shift_Left (Dat.Seed3 and 16#FFFFFFFE#, 7) xor Inter;
         Inter := Shift_Right (Shift_Left (Dat.Seed4, 3) xor Dat.Seed4, 12);
         Dat.Seed4 := Shift_Left (Dat.Seed4 and 16#FFFFFFFE#, 13) xor Inter;
         Val := Dat.Seed1 xor Dat.Seed2 xor Dat.Seed3 xor Dat.Seed4;
      end loop;

      Lib.Synchronization.Release (Data.Mutex);
      return Count;
   end Random_Read;

   procedure Reseed (Seed1, Seed2, Seed3, Seed4 : out Unsigned_32) is
      --  Fetch some system data for reseeding.
      --  - Currently allocated memory (weak runtime entropy).
      --  - Address of a variable in the stack (weak runtime entropy).
      --  - Cycles the system has gone thru till now (??).
      S1 : constant Memory.Size := Memory.Physical.Get_Statistics.Used_Memory;
      S2 : constant Unsigned_64 := Unsigned_64 (To_Integer (S1'Address));
      S3 : constant Unsigned_64 := Arch.Snippets.Read_Cycles;
   begin
      Seed1 := Unsigned_32 (S1 and 16#FFFFFFFF#);
      Seed2 := Unsigned_32 (S2 and 16#FFFFFFFF#);
      Seed3 := Unsigned_32 (S3 and 16#FFFFFFFF#);
      Seed4 := 4315; --  Plug here other entropy sources, like clock drift.
   end Reseed;
end Devices.Random;
