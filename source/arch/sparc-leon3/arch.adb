--  arch.adb: Architecture-specific package.
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

with Arch.Entrypoint;
pragma Unreferenced (Arch.Entrypoint);

package body Arch with SPARK_Mode => Off is
   function Get_Info return Boot_Information is
      --  LEON3 memmap structure of relevant structures.
      PROM_Start : constant := 16#00000000#;
      RAM_Start  : constant := 16#40000000#;

      --  Calculated offsets between sections.
      --  We cannot detect memory, so we hardcode 16MiB RAM.
      PROM_Len  : constant := RAM_Start - PROM_Start;
      RAM_Len   : constant := 16#0FFFFFF#;
      End_Start : constant := RAM_Start + RAM_Len * 1;

      Info : Boot_Information;
   begin
      Info.Memmap (1) := (To_Address (PROM_Start), PROM_Len, Memory_Reserved);
      Info.Memmap (2) := (To_Address (RAM_Start),  RAM_Len,  Memory_Free);
      Info.Memmap (3) := (To_Address (End_Start),  1000000,  Memory_Reserved);
      Info.Memmap_Len := 3;
      return Info;
   end Get_Info;
end Arch;
