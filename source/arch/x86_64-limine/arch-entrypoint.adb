--  entrypoint.adb: Main function and its closest utilities.
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

with Arch.APIC;
with Arch.GDT;
with Arch.HPET;
with Arch.IDT;
with Arch.PIC;
with Arch.CPU;
with Lib.Panic;
with Lib.Messages; use Lib.Messages;
with Memory.Physical;
with Arch.MMU;
with Devices.Serial;
with Arch.Limine;
with Main;

#if KASAN
   with Lib.KASAN;
#end if;

package body Arch.Entrypoint is
   procedure Bootstrap_Main is
      Info : Boot_Information renames Limine.Global_Info;
      Addr : System.Address;
   begin
      --  Initialize architectural state first.
      Devices.Serial.Init_COM1;
      GDT.Init;
      IDT.Init;

      --  Translate the limine protocol into arch-agnostic structures.
      Limine.Translate_Proto;

      --  Initialize the allocators and MMU.
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      if not Arch.MMU.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("The VMM could not be initialized");
      end if;

      --  Enable dmesg buffers and KASAN if wanted.
      Lib.Messages.Enable_Logging;
      #if KASAN
         Lib.KASAN.Init;
      #end if;

      --  Print the memory map, it is useful at times.
      Lib.Messages.Put_Line ("Physical memory map:");
      for E of Info.Memmap (1 .. Info.Memmap_Len) loop
         Addr := E.Start + E.Length;
         Lib.Messages.Put_Line
            ("[" & E.Start'Image & " - " & Addr'Image & "] " &
             Boot_Memory_Type'Image (E.MemType));
      end loop;

      --  Initialize the core's LAPIC and system's IOAPIC, essential for
      --  handling interrupts and IPIs.
      PIC.Mask_All;
      APIC.Init_LAPIC;
      APIC.Init_Core_LAPIC;
      if not Arch.APIC.Init_IOAPIC then
         Lib.Panic.Hard_Panic ("Could not start IOAPIC");
      end if;

      --  Initialize some system timers for interval counting.
      if not Arch.HPET.Init then
         Lib.Panic.Hard_Panic ("Could not start HPET");
      end if;

      --  Initialize other cores, and then jump to the freestanding main.
      Arch.CPU.Init_Cores;

      Arch.Cmdline_Len := Info.Cmdline_Len;
      Arch.Cmdline (1 .. Info.Cmdline_Len) :=
         Info.Cmdline (1 .. Info.Cmdline_Len);

      Main;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Uncatched exception on the arch entrypoint");
   end Bootstrap_Main;
end Arch.Entrypoint;
