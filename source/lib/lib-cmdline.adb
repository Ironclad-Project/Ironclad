--  lib-messages.ads: Parsing command line options.
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

with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;

package body Lib.Cmdline is
   function Get_Parameter
      (Cmdline_Addr : System.Address;
       Key          : String) return access String is
      Cmdline_Length : Natural := 0;
   begin
      --  Get the length of the C Cmdline.
      loop
         declare
            C : Character
               with Address => Cmdline_Addr + Storage_Offset (Cmdline_Length);
         begin
            exit when C = Ada.Characters.Latin_1.NUL;
            Cmdline_Length := Cmdline_Length + 1;
         end;
      end loop;

      --  Turn the C array to Ada and search for our key.
      declare
         Cmdline : String (1 .. Cmdline_Length) with Address => Cmdline_Addr;
         Value_Start  : Natural := 0;
         Value_Length : Natural := 0;
      begin
         --  TODO: For some reason using keys longer than 4, or using Key'Last,
         --  makes it not match, I have absolutely no idea why.
         for I in 1 .. Cmdline_Length loop
            exit when Key'Length + I > Cmdline_Length;
            if Key = Cmdline (I .. Key'Last + I - 1) then
               Value_Start := I + Key'Length + 1;
               for X in Value_Start .. Cmdline_Length loop
                  exit when Cmdline (X) = ' ';
                  Value_Length := Value_Length + 1;
               end loop;
               goto Found_Value;
            end if;
         end loop;
         return null;
      <<Found_Value>>
         return Ret : constant access String := new String (1 .. Value_Length)
         do
            for I in 1 .. Value_Length loop
               Ret.all (I) := Cmdline (Value_Start + I - 1);
            end loop;
         end return;
      end;
   end Get_Parameter;
end Lib.Cmdline;
