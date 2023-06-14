--  arch-entrypoint.adb: First architecture-specific Ada entrypoint.
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

with Lib.Panic;
with Memory.Physical;
with Memory.Virtual;
with Memory; use Memory;
with Main;
with Devices.UART;

package body Arch.Entrypoint with SPARK_Mode => Off is
   procedure Bootstrap_Main is
      Info : constant Boot_Information := Get_Info;
   begin
      Devices.UART.Configure;

      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      if not Memory.Virtual.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("Could not start the VMM");
      end if;

      Main;
   end Bootstrap_Main;
end Arch.Entrypoint;
