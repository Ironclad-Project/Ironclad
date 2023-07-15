--  devices-random.adb: Random devices.
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

with Cryptography.Random;

package body Devices.Random is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Init (Success : out Boolean) is
      Random_Res : constant Resource :=
         (Data        => System.Null_Address,
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Read        => Read'Access,
          Write       => null,
          Sync        => null,
          Sync_Range  => null,
          IO_Control  => null,
          Mmap        => null,
          Poll        => null);
      Success_1, Success_2 : Boolean;
   begin
      Register (Random_Res, "random",  Success_1);
      Register (Random_Res, "urandom", Success_2);
      Success := Success_1 and Success_2;
   end Init;

   procedure Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      DLen : constant Natural := Data'Length;
      Len  : constant Natural := (if DLen > 4096 then 4096 else DLen / 4);
      Temp : Cryptography.Random.Crypto_Data (1 .. Len);
      Idx  : Natural;
   begin
      Data := (others => 0);

      if Len = 0 then
         Ret_Count := 0;
         Success   := True;
         return;
      end if;

      Idx := Data'First;
      Cryptography.Random.Fill_Data (Temp);
      for V of Temp loop
         pragma Loop_Invariant (Idx >= Data'First and Idx <= Data'Last - 3);
         Data (Idx + 0) := Unsigned_8 (Shift_Right (V and 16#FF000000#, 24));
         Data (Idx + 1) := Unsigned_8 (Shift_Right (V and 16#00FF0000#, 16));
         Data (Idx + 2) := Unsigned_8 (Shift_Right (V and 16#0000FF00#,  8));
         Data (Idx + 3) := Unsigned_8 (Shift_Right (V and 16#000000FF#,  0));
         if Idx < Natural'Last - 4 then
            Idx := Idx + 4;
         end if;
      end loop;

      Ret_Count := Len;
      Success   := True;
   end Read;
end Devices.Random;
