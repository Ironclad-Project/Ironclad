--  networking.ads: Specification of networking library.
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

package Networking is
   --  Maximum length of a hostname
   Hostname_Max_Len : constant := 255;
   subtype Hostname_Len is Integer range 0 .. Hostname_Max_Len;

   --  Fetch the system's hostname.
   --  @param Name Hostname.
   --  @param Length Length of the hostname.
   procedure Get_Hostname (Name : out String; Length : out Hostname_Len)
      with Pre => Name'Last <= Integer'Last - Hostname_Max_Len and
                  Name'Length <= Hostname_Max_Len;

   --  Set the system's hostname.
   --  @param Name Hostname to set;
   --  @return True on success, False on failure.
   procedure Set_Hostname (Name : String; Success : out Boolean);
end Networking;
