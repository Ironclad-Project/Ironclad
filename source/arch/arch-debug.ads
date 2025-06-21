--  arch-debug.ads: Architecture-specific debug channels.
--  Copyright (C) 2024 streaksu
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

with Devices;

package Arch.Debug is
   --  This package implements read / write access to a target-specific debug
   --  channel, which could take the shape of a serial port or debug monitor.

   --  Whether the abstracted channel supports write only or read / write.
   Supports_Read : constant Boolean;

   --  Read byte device arrays atomically.
   --  @param Message Array to print.
   procedure Read (Message : out Devices.Operation_Data);

   --  Print a character message atomically.
   --  @param Message Character to print.
   procedure Print (Message : Character);

   --  Print a string message atomically.
   --  @param Message String to print.
   procedure Print (Message : String);

   --  Print byte device arrays atomically.
   --  @param Message Array to print.
   procedure Print (Message : Devices.Operation_Data);

private

   #if ArchName = """riscv64-limine"""
      Supports_Read : constant Boolean := False;
   #elsif ArchName = """x86_64-limine"""
      Supports_Read : constant Boolean := True;
   #end if;
end Arch.Debug;
