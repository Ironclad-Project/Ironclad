--  arch-entrypoint.adb: Limine plops us here.
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

with Interfaces; use Interfaces;
with Kernel_Main;
with Devices.UART;
with Arch.Limine;
with Lib.Messages; use Lib.Messages;
with Memory.Physical;
with Arch.MMU;
with Lib.Panic;
with Arch.DTB;
with Arch.CPU;

package body Arch.Entrypoint is
   procedure Bootstrap_Main is
      Info     : Boot_Information renames Limine.Global_Info;
      St1, St2 : Lib.Messages.Translated_String;
      Stp_Len  : Natural;
   begin
      --  Initialize architectural state first.
      Devices.UART.Init_UART0;

      --  Translate the limine protocol into arch-agnostic structures.
      Limine.Translate_Proto;

      --  Initialize device discovery facilities.
      if not Arch.DTB.Init then
         Lib.Panic.Hard_Panic ("No DTB was found!");
      end if;

      --  Print the memory map, it is useful at times.
      Lib.Messages.Put_Line ("Physical memory map:");
      for E of Info.Memmap (1 .. Info.Memmap_Len) loop
         Image (Unsigned_64 (To_Integer (E.Start)), St1, Stp_Len, True);
         Image (Unsigned_64 (E.Length), St2, Stp_Len, True);
         Lib.Messages.Put_Line (St1 & " + " & St2 & " " &
            Boot_Memory_Type'Image (E.MemType));
      end loop;

      --  Initialize the allocators and MMU.
      Lib.Messages.Put_Line ("Initializing allocators");
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      if not Arch.MMU.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("The VMM could not be initialized");
      end if;

      --  Initialize the other cores of the system.
      Arch.CPU.Init_Cores;

      --  Go to main kernel.
      Kernel_Main.Entrypoint (Info.Cmdline (1 .. Info.Cmdline_Len));
   end Bootstrap_Main;
end Arch.Entrypoint;
