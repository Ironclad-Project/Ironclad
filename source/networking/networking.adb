--  networking.adb: Networking library.
--  Copyright (C) 2025 streaksu
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

package body Networking is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Get_Hostname (Name : out String; Length : out Natural) is
   begin
      Name := [others => Ada.Characters.Latin_1.NUL];

      if Name'Length < Hostname_Length then
         Length := Name'Length;
      else
         Length := Hostname_Length;
      end if;

      Seize (Names_Lock);
      Name (Name'First .. Name'First + Length - 1) := Hostname (1 .. Length);
      Release (Names_Lock);
   end Get_Hostname;

   procedure Set_Hostname (Name : String; Success : out Boolean) is
   begin
      if Name'Length = 0 or Name'Length > Hostname'Length then
         Success := False;
         return;
      end if;

      Seize (Names_Lock);
      Hostname_Length := Name'Length;
      Hostname (1 .. Name'Length) := Name;
      Release (Names_Lock);
      Success := True;
   end Set_Hostname;

   procedure Get_Domain_Name (Name : out String; Length : out Natural) is
   begin
      Name := [others => Ada.Characters.Latin_1.NUL];

      if Name'Length < Domain_Length then
         Length := Name'Length;
      else
         Length := Domain_Length;
      end if;

      Seize (Names_Lock);
      Name (Name'First .. Name'First + Length - 1) := Domain (1 .. Length);
      Release (Names_Lock);
   end Get_Domain_Name;

   procedure Set_Domain_Name (Name : String; Success : out Boolean) is
   begin
      if Name'Length = 0 or Name'Length > Domain'Length then
         Success := False;
         return;
      end if;

      Seize (Names_Lock);
      Domain_Length := Name'Length;
      Domain (1 .. Name'Length) := Name;
      Release (Names_Lock);
      Success := True;
   end Set_Domain_Name;
end Networking;

