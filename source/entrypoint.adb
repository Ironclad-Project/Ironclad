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
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.ACPI;
with Arch.APIC;
with Arch.CPU;
with Arch.GDT;
with Arch.HPET;
with Arch.IDT;
with Arch.PIT;
with Arch.Syscall;
with Devices.Ramdev;
with Devices;
with VFS.File; use VFS.File;
with VFS;
with VFS.Device;
with Lib.Cmdline;
with Lib.Messages;
with Lib.Panic;
with Memory.Physical;
with Memory.Virtual;
with Config;
with Userland.Loader;
with Userland.Process; use Userland.Process;
with Userland;
with Scheduler; use Scheduler;

package body Entrypoint is
   procedure Bootstrap_Main (Protocol : access Arch.Stivale2.Header) is
      package ST renames Arch.Stivale2;

      package C1 is new System.Address_To_Access_Conversions (ST.RSDP_Tag);
      package C2 is new System.Address_To_Access_Conversions (ST.Terminal_Tag);
      package C3 is new System.Address_To_Access_Conversions (ST.Memmap_Tag);
      package C4 is new System.Address_To_Access_Conversions (ST.PMR_Tag);
      package C5 is new System.Address_To_Access_Conversions (ST.SMP_Tag);

      RSDP : constant access ST.RSDP_Tag :=
         C1.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.RSDP_ID)));
      Term : constant access ST.Terminal_Tag :=
         C2.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Terminal_ID)));
      Memmap : constant access ST.Memmap_Tag :=
         C3.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Memmap_ID)));
      PMRs : constant access ST.PMR_Tag :=
         C4.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.PMR_ID)));
      SMP : constant access ST.SMP_Tag :=
         C5.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.SMP_ID)));
   begin
      ST.Init_Terminal (Term);
      Lib.Messages.Put (Config.Name & " " & Config.Version & " booted by ");
      Lib.Messages.Put (Protocol.BootloaderBrand & " ");
      Lib.Messages.Put_Line (Protocol.BootloaderVersion);
      Lib.Messages.Put ("Please report errors and issues to ");
      Lib.Messages.Put_Line (Config.Bug_Site);

      Arch.GDT.Init;
      Arch.IDT.Init;

      Lib.Messages.Put_Line ("Initializing allocators");
      Memory.Physical.Init_Allocator (Memmap);
      Lib.Messages.Put      (Unsigned_64 (Memory.Physical.Used_Memory));
      Lib.Messages.Put      (" used + ");
      Lib.Messages.Put      (Unsigned_64 (Memory.Physical.Free_Memory));
      Lib.Messages.Put      (" free / ");
      Lib.Messages.Put      (Unsigned_64 (Memory.Physical.Total_Memory));
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
      Memory.Virtual.Init (Memmap, PMRs);

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
      if Scheduler.Create_Kernel_Thread
         (To_Integer (Main_Thread'Address),
          Unsigned_64 (To_Integer (Protocol.all'Address))) = 0
      then
         Lib.Panic.Hard_Panic ("Could not create main thread");
      end if;
      Scheduler.Idle_Core;
   end Bootstrap_Main;

   procedure Main_Thread (Protocol : access Arch.Stivale2.Header) is
      package ST renames Arch.Stivale2;

      package C1 is new System.Address_To_Access_Conversions (ST.Modules_Tag);
      package C2 is new System.Address_To_Access_Conversions (ST.Cmdline_Tag);
      package C3 is new System.Address_To_Access_Conversions (ST.Framebuffer_Tag);

      Modules : constant access ST.Modules_Tag :=
         C1.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Modules_ID)));
      Cmdline : constant access ST.Cmdline_Tag :=
         C2.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Cmdline_ID)));
      Framebuffer : constant access ST.Framebuffer_Tag :=
         C3.To_Pointer (To_Address (ST.Get_Tag (Protocol, ST.Framebuffer_ID)));

      Init_Arguments   : Userland.Argument_Arr (1 .. 1);
      Init_Environment : Userland.Environment_Arr (1 .. 0);

      type String_Acc is access all String;
      Root_Value : String_Acc;
      Init_Value : String_Acc;
      Init_File  : File_Acc;
      procedure Free_S is new Ada.Unchecked_Deallocation (String, String_Acc);
      procedure Free_F is new Ada.Unchecked_Deallocation (File,   File_Acc);
   begin
      Lib.Messages.Put_Line ("Initializing FS subsystem");
      VFS.Device.Init_Registry;

      Lib.Messages.Put_Line ("Initializing processes");
      Userland.Process.Init;

      Lib.Messages.Put_Line ("Initializing devices");
      Devices.Init (Framebuffer);

      Lib.Messages.Put_Line ("Mounting stivale2 modules as ramdevs");
      for I in Modules.Entries'First .. Modules.Entries'Last loop
         declare
            Name : String := "ramdev0";
         begin
            exit when I = 10;
            Name (7) := Character'Val (I + Character'Pos ('0'));
            if not VFS.Device.Register
               (Devices.Ramdev.Init_Module (Modules.Entries (I), Name))
            then
               Lib.Panic.Hard_Panic ("Could not load a stivale2 ramdev");
            end if;
         end;
      end loop;

      Lib.Messages.Put_Line ("Fetching kernel cmdline options");
      Root_Value := Lib.Cmdline.Get_Parameter (Cmdline.Inner, "root");
      Init_Value := Lib.Cmdline.Get_Parameter (Cmdline.Inner, "init");

      Arch.Syscall.Set_Tracing
         (Lib.Cmdline.Is_Key_Present (Cmdline.Inner, "syscalltracing"));

      if Root_Value /= null then
         if not VFS.Device.Mount (Root_Value.all, "/", VFS.Device.FS_USTAR)
         then
            Lib.Panic.Soft_Panic ("Could not mount " & Root_Value.all & " /");
         end if;
         Free_S (Root_Value);
      end if;

      if Init_Value /= null then
         Lib.Messages.Put_Line ("Booting init " & Init_Value.all);
         Init_Arguments (1) := new String'(Init_Value.all);
         Init_File := Open (Init_Value.all, Access_R);
         if Init_File = null or else Userland.Loader.Start_Program
            (Init_File, Init_Arguments, Init_Environment, "/dev/ttydev",
             "/dev/ttydev", "/dev/ttydev") = null
         then
            Lib.Panic.Soft_Panic ("Could not start init");
         end if;
         Free_S (Init_Value);
         Free_F (Init_File);
      end if;

      Lib.Messages.Put_Line ("Bailing main thread");
      Scheduler.Bail;
   end Main_Thread;
end Entrypoint;