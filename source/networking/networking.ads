--  networking.ads: Networking library.
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

with Interfaces; use Interfaces;
with Lib.Synchronization; use Lib.Synchronization;

package Networking is
   --  Networking types.
   type MAC_Address  is array (1 ..  6) of Unsigned_8;
   type IPv4_Address is array (1 ..  4) of Unsigned_8;
   type IPv6_Address is array (1 .. 16) of Unsigned_8;
   type IPv4_Port is new Unsigned_16;
   type IPv6_Port is new Unsigned_16;

   --  Standard submasks.
   IPv4_8_Submask   : constant IPv4_Address := (255, 0, 0, 0);
   IPv6_128_Submask : constant IPv6_Address := (others => 16#FF#);
   ----------------------------------------------------------------------------
   --  Maximum size of a hostname.
   Hostname_Max_Len : constant Natural;

   --  Fetch the system's hostname.
   --  @param Name   Buffer to write the hostname.
   --  @param Length Length of the hostname, even if it doesn't fit.
   procedure Get_Hostname (Name : out String; Length : out Natural)
      with Pre => Name'Length /= 0 and then
                  Name'Last <= Natural'Last - Hostname_Max_Len;

   --  Set the system's hostname.
   --  @param Name Hostname to set;
   --  @return True on success, False on failure.
   procedure Set_Hostname (Name : String; Success : out Boolean);

private

   Hostname_Max_Len : constant Natural := 255;

   Hostname_Lock   : aliased Binary_Semaphore := Unlocked_Semaphore;
   Hostname_Length : Natural range 0 .. Hostname_Max_Len := 4;
   Hostname : String (1 .. Hostname_Max_Len) := "none" & (1 .. 251 => ' ');

end Networking;
