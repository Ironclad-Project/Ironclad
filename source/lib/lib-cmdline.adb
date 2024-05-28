--  lib-cmdline.adb: Parsing command line options.
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

package body Lib.Cmdline is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Get_Key_Value
      (Cmdline, Key : String;
       Returned     : out String;
       Found        : out Boolean;
       Length       : out Natural)
   is
      Last_Index      : Integer;
      Do_Until_Quotes : Boolean;
      Returned_Idx    : Natural := Returned'First;
   begin
      Returned := (others => ' ');

      Find_Key (Cmdline, Key, Found, Last_Index);
      if not Found then
         goto Not_Found;
      end if;

      Last_Index := Last_Index + 1;
      if Cmdline'Last < Last_Index or else Cmdline (Last_Index) /= '=' then
         goto Found_But_No_Value;
      end if;

      Last_Index := Last_Index + 1;
      if Cmdline'Last > Last_Index then
         Do_Until_Quotes := Cmdline (Last_Index) = '"';
         if Do_Until_Quotes then
            Last_Index := Last_Index + 1;
         end if;
      else
         Do_Until_Quotes := False;
      end if;

      Length := 0;

      while Cmdline'Last >= Last_Index loop
         if Returned'Last < Returned_Idx then
            goto Found_But_No_Value;
         end if;

         if Cmdline (Last_Index) = ' ' then
            if not Do_Until_Quotes then
               return;
            end if;
         elsif Cmdline (Last_Index) = '"' then
            if Do_Until_Quotes then
               return;
            end if;
         end if;

         Returned (Returned_Idx) := Cmdline (Last_Index);
         Returned_Idx := Returned_Idx + 1;
         Length       := Length       + 1;
         Last_Index   := Last_Index   + 1;
      end loop;

      return;

   <<Not_Found>>
      Found  := False;
      Length := 0;
      return;

   <<Found_But_No_Value>>
      Found  := True;
      Length := 0;
   end Get_Key_Value;

   function Is_Key_Present (Cmdline, Key : String) return Boolean is
      Found   : Boolean;
      Discard : Natural;
   begin
      Find_Key (Cmdline, Key, Found, Discard);
      return Found;
   end Is_Key_Present;
   ----------------------------------------------------------------------------
   procedure Find_Key
      (Cmdline, Key : String;
       Found        : out Boolean;
       End_Index    : out Natural)
   is
      Curr_Index : Integer;
   begin
      for I in 1 .. Cmdline'Length - Key'Length + 1 loop
         Curr_Index := Cmdline'First + (I - 1);
         if Key = Cmdline (Curr_Index .. Curr_Index + (Key'Length - 1)) then
            Found     := True;
            End_Index := Curr_Index + (Key'Length - 1);
            return;
         end if;
      end loop;

      Found     := False;
      End_Index := 0;
   end Find_Key;
end Lib.Cmdline;
