--  arch-virtualization.ads: Architecture-specific virtualization code.
--  Copyright (C) 2024 mintsuki
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

package Arch.Virtualization is
   --  Checks whether virtualization in any form is supported.
   function Is_Supported return Boolean;

   --  List of MSRs indexes supported by the virtualization.
   type MSR_List is array (Natural range <>) of Unsigned_32;

   --  List all MSRs supported by the virtualized architecture.
   --  @param List  List to write the MSRs to.
   --  @param Count Count of MSRs supported, might be bigger than List.
   procedure Get_MSR_List (List : out MSR_List; Count : out Natural);
end Arch.Virtualization;
