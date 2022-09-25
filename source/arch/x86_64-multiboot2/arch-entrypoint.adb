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
with Arch.ACPI;
with Arch.GDT;
with Arch.HPET;
with Arch.IDT;
with Arch.PIC;
with Arch.PIT;
with Arch.CPU;
with Lib.Panic;
with Memory.Physical;
with Main;
with Memory.Virtual;

package body Arch.Entrypoint with SPARK_Mode => Off is
   procedure Bootstrap_Main (Proto : Multiboot2.Header_Acc) is
      Info : constant Boot_Information := Multiboot2.Translate_Proto (Proto);
   begin
      --  Initialize architectural state first.
      GDT.Init;
      IDT.Init;

      --  Initialize the allocators.
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      if not Memory.Virtual.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
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
      if not Arch.APIC.Init_IOAPIC then
         Lib.Panic.Hard_Panic ("Could not start IOAPIC");
      end if;

      --  Initialize some system timers for interval counting.
      if not Arch.PIT.Init then
         Lib.Panic.Hard_Panic ("Could not start PIT");
      end if;
      Arch.HPET.Init;

      --  Initialize other cores, and then jump to the freestanding main.
      Arch.CPU.Init_Cores;
      Main;
   end Bootstrap_Main;
end Arch.Entrypoint;
