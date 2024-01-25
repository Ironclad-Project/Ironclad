--  devices-lpt.adb: Parallel port driver.
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

with System.Address_To_Access_Conversions;
with Arch.Snippets;

package body Devices.LPT is
   package Conv is new System.Address_To_Access_Conversions (LP_Data);

   function Init return Boolean is
      Success : Boolean;
      Data    : LP_Data_Acc;
   begin
      for I in LPT_Ports'Range loop
         Data := new LP_Data'(Port => LPT_Ports (I));
         Register
            ((Data        => Conv.To_Address (Conv.Object_Pointer (Data)),
              ID          => (others => 0),
              Is_Block    => True,
              Block_Size  => 4096,
              Block_Count => 0,
              Read        => null,
              Write       => Write'Access,
              Sync        => null,
              Sync_Range  => null,
              IO_Control  => null,
              Mmap        => null,
              Poll        => null),
             "lpt" & (Character'Val (I + Character'Pos ('0'))), Success);
      end loop;
      return Success;
   end Init;
   ----------------------------------------------------------------------------
   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
      LPT    : LP_Data with Import, Address => Key;
      Strobe : Unsigned_8;
   begin
      for Val of Data loop
         loop
            exit when (Arch.Snippets.Port_In (LPT.Port + 1) and 16#80#) /= 0;
         end loop;

         Arch.Snippets.Port_Out (LPT.Port, Val);

         Strobe := Arch.Snippets.Port_In (LPT.Port + 2);
         Arch.Snippets.Port_Out (LPT.Port + 2, Strobe or 1);
         Arch.Snippets.Port_Out (LPT.Port + 2, Strobe);
      end loop;
      Ret_Count := Data'Length;
      Success   := True;
   end Write;
end Devices.LPT;
