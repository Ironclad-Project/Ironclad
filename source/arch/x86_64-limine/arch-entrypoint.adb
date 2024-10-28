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

with Interfaces; use Interfaces;
with Arch.APIC;
with Arch.ACPI;
with Arch.GDT;
with Arch.HPET;
with Arch.IDT;
with Arch.PIC;
with Arch.PCI;
with Arch.PIT;
with Arch.CPU;
with Lib.Panic;
with Lib.Messages; use Lib.Messages;
with Memory.Physical;
with Kernel_Main;
with Arch.MMU;
with Devices.Serial;
with Arch.Limine;

package body Arch.Entrypoint is
   procedure Bootstrap_Main is
      Info     : Boot_Information renames Limine.Global_Info;
      St1, St2 : Lib.Messages.Translated_String;
      Stp_Len  : Natural;
   begin
      --  Initialize architectural state first.
      Devices.Serial.Init_COM1;
      GDT.Init;
      IDT.Init;

      --  Translate the limine protocol into arch-agnostic structures.
      Limine.Translate_Proto;

      --  Print the memory map, it is useful at times.
      Lib.Messages.Put_Line ("Physical memory map:");
      for E of Info.Memmap (1 .. Info.Memmap_Len) loop
         Image (Unsigned_64 (To_Integer (E.Start)), St1, Stp_Len, True);
         Image (Unsigned_64 (E.Length), St2, Stp_Len, True);
         Lib.Messages.Put_Line (St1 & " + " & St2 & " " &
            Boot_Memory_Type'Image (E.MemType));
      end loop;

      --  Initialize the allocators and MMU.
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      if not Arch.MMU.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("The VMM could not be initialized");
      end if;

      --  Scan the system's ACPI tables, which are needed for devices like
      --  the HPET or IOAPIC.
      if not Arch.ACPI.ScanTables then
         Lib.Panic.Hard_Panic ("ACPI tables not found");
      end if;

      --  Initialize the core's LAPIC and system's IOAPIC, essential for
      --  handling interrupts and IPIs.
      PIC.Mask_All;
      APIC.Init_LAPIC;
      APIC.Init_Core_LAPIC;
      if not Arch.APIC.Init_IOAPIC then
         Lib.Panic.Hard_Panic ("Could not start IOAPIC");
      end if;

      --  Initialize some system timers for interval counting.
      if not Arch.PIT.Init then
         Lib.Panic.Hard_Panic ("Could not start PIT");
      end if;
      Arch.HPET.Init;

      --  Scan all PCI devices.
      PCI.Scan_PCI;

      --  Initialize other cores, and then jump to the freestanding main.
      Arch.CPU.Init_Cores;
      Kernel_Main.Entrypoint (Info.Cmdline (1 .. Info.Cmdline_Len));
   end Bootstrap_Main;
end Arch.Entrypoint;
