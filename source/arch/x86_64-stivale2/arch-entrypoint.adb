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

with System; use System;
with System.Address_To_Access_Conversions;
with Arch.ACPI;
with Arch.APIC;
with Arch.CPU;
with Arch.GDT;
with Arch.HPET;
with Arch.IDT;
with Arch.PIT;
with Lib.Messages;
with Lib.Panic;
with Memory.Physical;
with Config;
with Main;
with Memory.Virtual;

package body Arch.Entrypoint with SPARK_Mode => Off is
   procedure Bootstrap_Main (Protocol : access Arch.Stivale2.Header) is
      package ST renames Arch.Stivale2;
      package C1 is new System.Address_To_Access_Conversions (ST.RSDP_Tag);
      package C2 is new System.Address_To_Access_Conversions (ST.Terminal_Tag);
      package C3 is new System.Address_To_Access_Conversions (ST.SMP_Tag);

      RSDP : constant access ST.RSDP_Tag :=
         C1.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.RSDP_ID)));
      Term : constant access ST.Terminal_Tag :=
         C2.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Terminal_ID)));
      SMP : constant access ST.SMP_Tag :=
         C3.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.SMP_ID)));

      Info : Boot_Information;
   begin
      Arch.Stivale2.Stivale_Tag := Protocol;
      if not Config.Is_Small then
         ST.Init_Terminal (Term);
      end if;
      Lib.Messages.Put ("Booted by " & Protocol.BootloaderBrand & " ");
      Lib.Messages.Put_Line (Protocol.BootloaderVersion);

      Arch.GDT.Init;
      Arch.IDT.Init;

      --  Print the memory map and initialize the allocators.
      Info := Get_Info;
      for E of Info.Memmap (1 .. Info.Memmap_Len) loop
         Lib.Messages.Put      ('[');
         Lib.Messages.Put      (E.Start, True);
         Lib.Messages.Put      (" -> ");
         Lib.Messages.Put      (E.Start + E.Length, True);
         Lib.Messages.Put      ("] ");
         Lib.Messages.Put_Line (Arch.Boot_Memory_Type'Image (E.MemType));
      end loop;
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      if not Memory.Virtual.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("The VMM could not be initialized");
      end if;

      --  Scan the system's ACPI tables, which are needed for devices like
      --  the HPET or IOAPIC.
      if not Arch.ACPI.ScanTables (RSDP.RSDP_Address) then
         Lib.Panic.Hard_Panic ("ACPI tables not found");
      end if;

      --  Initialize the core's LAPIC and system's IOAPIC, essential for
      --  handling interrupts and IPIs.
      Arch.APIC.Init_LAPIC;
      if not Arch.APIC.Init_IOAPIC then
         Lib.Panic.Hard_Panic ("Could not start IOAPIC");
      end if;

      --  Initialize some system timers for interval counting.
      if not Arch.PIT.Init then
         Lib.Panic.Hard_Panic ("Could not start PIT");
      end if;
      Arch.HPET.Init;

      --  Initialize other cores, and then jump to the freestanding main.
      Arch.CPU.Init_Cores (SMP);
      Lib.Panic.Enable_Panic_Propagation;
      Main;
   end Bootstrap_Main;
end Arch.Entrypoint;
