--  arch-stivale2.adb: Stivale2 utilities.
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

package body Arch.Stivale2 is
   function Get_Tag
     (Proto     : access Header;
     Identifier : Unsigned_64) return Virtual_Address
   is
      package Convert is new System.Address_To_Access_Conversions (Tag);

      Search_Address : Virtual_Address := Proto.Tags;
      Search_Tag     : access Tag;
   begin
      while Search_Address /= Null_Address loop
         Search_Tag := Convert.To_Pointer (To_Address (Search_Address));
         if Search_Tag.Identifier = Identifier then
            return Search_Address;
         end if;
         Search_Address := Search_Tag.Next;
      end loop;
      return Null_Address;
   end Get_Tag;
end Arch.Stivale2;
