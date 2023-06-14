--  arch.adb: Architecture-specific package.
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

with Arch.Entrypoint;
pragma Unreferenced (Arch.Entrypoint);

package body Arch with SPARK_Mode => Off is
   function Get_Info return Boot_Information is
      End_Val : Natural with Import, External_Name => "__end";
      Addr    : constant System.Address := End_Val'Address;
      SAddr   : constant  Storage_Count := Storage_Count (To_Integer (Addr));
   begin
      return
         (Cmdline       => (others => <>),
          Cmdline_Len   => 0,
          Memmap        =>
            (1      => (To_Address (16#0000#), 16#8000#, Memory_Reserved),
             2      => (To_Address (16#8000#), SAddr, Memory_Kernel),
             3      => (Addr, 16#3F000000#, Memory_Free),
             others => <>),
          Memmap_Len    => 3,
          RAM_Files     => (others => <>),
          RAM_Files_Len => 0);
   end Get_Info;
end Arch;
