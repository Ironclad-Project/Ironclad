--  userland-mac.ads: Mandatory access control.
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

package Userland.MAC is
   --  MAC (Mandatory Access Control) is configured by a bitmap of some
   --  broad permissions.
   type Permissions is record
      Can_Exit_Itself       : Boolean;
      Can_Create_Others     : Boolean;
      Can_Change_Scheduling : Boolean;
      Can_Access_Entropy    : Boolean;
      Can_Allocate_Memory   : Boolean;
      Can_Deallocate_Memory : Boolean;
      Can_Manage_Networking : Boolean;
   end record with Pack, Size => 7;

   --  Default permissions are none (PoLP).
   Default_Permissions : constant Permissions := (others => False);
end Userland.MAC;
