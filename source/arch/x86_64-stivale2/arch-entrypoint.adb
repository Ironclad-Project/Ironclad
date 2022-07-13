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
with Interfaces; use Interfaces;
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
with Scheduler; use Scheduler;
with Main;
with Memory.Virtual;

package body Arch.Entrypoint with SPARK_Mode => Off is
   procedure Bootstrap_Main (Protocol : access Arch.Stivale2.Header) is
      package ST renames Arch.Stivale2;

      package C1 is new System.Address_To_Access_Conversions (ST.RSDP_Tag);
      package C2 is new System.Address_To_Access_Conversions (ST.Terminal_Tag);
      package C3 is new System.Address_To_Access_Conversions (ST.Memmap_Tag);
      package C4 is new System.Address_To_Access_Conversions (ST.SMP_Tag);

      RSDP : constant access ST.RSDP_Tag :=
         C1.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.RSDP_ID)));
      Term : constant access ST.Terminal_Tag :=
         C2.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Terminal_ID)));
      Memmap : constant access ST.Memmap_Tag :=
         C3.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Memmap_ID)));
      SMP : constant access ST.SMP_Tag :=
         C4.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.SMP_ID)));

      Info           : Boot_Information;
      Mem_Statistics : Memory.Physical.Statistics;
   begin
      Arch.Stivale2.Stivale_Tag := Protocol;
      if not Config.Is_Small then
         ST.Init_Terminal (Term);
      end if;
      Lib.Messages.Put (Config.Name & " " & Config.Version & " booted by ");
      Lib.Messages.Put (Protocol.BootloaderBrand & " ");
      Lib.Messages.Put_Line (Protocol.BootloaderVersion);
      Lib.Messages.Put ("Please report errors and issues to ");
      Lib.Messages.Put_Line (Config.Bug_Site);

      Arch.GDT.Init;
      Arch.IDT.Init;

      Lib.Messages.Put_Line ("Initializing allocators");
      Info := Get_Info;
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      Mem_Statistics := Memory.Physical.Get_Statistics;
      Lib.Messages.Put      (Unsigned_64 (Mem_Statistics.Used_Memory));
      Lib.Messages.Put      (" used + ");
      Lib.Messages.Put      (Unsigned_64 (Mem_Statistics.Free_Memory));
      Lib.Messages.Put      (" free / ");
      Lib.Messages.Put      (Unsigned_64 (Mem_Statistics.Total_Memory));
      Lib.Messages.Put_Line (" memory used");
      for E of Memmap.Entries loop
         Lib.Messages.Put      ('[');
         Lib.Messages.Put      (To_Address (E.Base), True);
         Lib.Messages.Put      ('+');
         Lib.Messages.Put      (Unsigned_64 (E.Length), True, True);
         Lib.Messages.Put      ("] ");
         Lib.Messages.Put      (Integer (E.EntryType), False, True);
         Lib.Messages.Put_Line ("");
      end loop;
      if not Memory.Virtual.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("The VMM could not be initialized");
      end if;

      Lib.Messages.Put_Line ("Scanning ACPI tables");
      if not Arch.ACPI.ScanTables (RSDP.RSDP_Address) then
         Lib.Panic.Hard_Panic ("ACPI tables not found");
      end if;

      Lib.Messages.Put_Line ("Initializing APICs");
      Arch.APIC.Init_LAPIC;
      if not Arch.APIC.Init_IOAPIC then
         Lib.Panic.Hard_Panic ("Could not start IOAPIC");
      end if;

      Lib.Messages.Put_Line ("Initialize cores");
      Arch.CPU.Init_Cores (SMP);
      Lib.Panic.Enable_Panic_Propagation;

      Lib.Messages.Put_Line ("Initializing timers");
      if not Arch.PIT.Init then
         Lib.Panic.Hard_Panic ("Could not start PIT");
      end if;
      if Arch.HPET.Init then
         Lib.Messages.Put_Line ("HPET found");
      end if;

      Lib.Messages.Put ("Initializing scheduler for ");
      Lib.Messages.Put (Arch.CPU.Core_Count);
      Lib.Messages.Put_Line (" cores");
      if not Scheduler.Init then
         Lib.Panic.Hard_Panic ("Could not initialize the scheduler");
      end if;

      Lib.Messages.Put_Line ("Bootstrap done, making kernel thread and idle");
      if Scheduler.Create_Kernel_Thread (To_Integer (Main'Address), 0) = 0
      then
         Lib.Panic.Hard_Panic ("Could not create main thread");
      end if;
      Scheduler.Idle_Core;
   end Bootstrap_Main;
end Arch.Entrypoint;
