--  entrypoint.adb: Specification of the main function's package.
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
with Arch.SMP;
with Lib.Messages;
with Lib.Panic;
with Config;
with Memory.Physical;
with Memory.Virtual;
with Memory; use Memory;
with Main;

package body Arch.Entrypoint with SPARK_Mode => Off is
   procedure Bootstrap_Main (Protocol : access Arch.Stivale2.Header) is
      package ST renames Arch.Stivale2;
      package C is new System.Address_To_Access_Conversions (ST.SMP_Tag);

      SMP_Addr : Integer_Address := ST.Get_Tag (Protocol, ST.SMP_ID);
      Info     : Boot_Information;
   begin
      Lib.Messages.Put (Config.Name & " " & Config.Version & " booted by ");
      Lib.Messages.Put (Protocol.BootloaderBrand & " ");
      Lib.Messages.Put_Line (Protocol.BootloaderVersion);
      Lib.Messages.Put ("Please report errors and issues to ");
      Lib.Messages.Put_Line (Config.Bug_Site);

      --  Initialize memory allocators and virtual mappings.
      Arch.Stivale2.Stivale_Tag := Protocol;
      Info := Get_Info;
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      if not Memory.Virtual.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("Could not start the VMM");
      end if;

      --  Initialize the rest of cores and core locals.
      if SMP_Addr /= 0 then
         SMP_Addr := Memory_Offset + SMP_Addr;
      end if;
      Arch.SMP.Init_Cores (C.To_Pointer (To_Address (SMP_Addr)));

      --  Jump to main.
      Main;
   end Bootstrap_Main;
end Arch.Entrypoint;
