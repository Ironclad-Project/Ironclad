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

package body Lib.Cmdline is
   function Get_Parameter
      (Address : System.Address;
       Key     : String) return String_Acc
   is
      Value_Start  : Natural := 0;
      Value_Length : Natural := 0;
      Cmdline_Length : constant Natural := Lib.C_String_Length (Address);
      Cmdline        : String (1 .. Cmdline_Length) with Address => Address;
   begin
      for I in 1 .. Cmdline'Last loop
         exit when Key'Length - 1 + I > Cmdline'Last;
         if Key = Cmdline (I .. Key'Last + I - 1) then
            Value_Start := I + Key'Length + 1;
            for X in Value_Start .. Cmdline'Last loop
               exit when Cmdline (X) = ' ';
               Value_Length := Value_Length + 1;
            end loop;
            goto Found_Value;
         end if;
      end loop;
      return null;

   <<Found_Value>>
      return Ret : constant String_Acc := new String (1 .. Value_Length)
      do
         for I in 1 .. Value_Length loop
            Ret.all (I) := Cmdline (Value_Start + I - 1);
         end loop;
      end return;
   end Get_Parameter;

   function Is_Key_Present
      (Address : System.Address;
       Key     : String) return Boolean
   is
      Cmdline_Length : constant Natural := Lib.C_String_Length (Address);
      Cmdline        : String (1 .. Cmdline_Length) with Address => Address;
   begin
      for I in 1 .. Cmdline'Last loop
         exit when Key'Length - 1 + I > Cmdline'Last;
         if Key = Cmdline (I .. Key'Last + I - 1) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Key_Present;
end Lib.Cmdline;
