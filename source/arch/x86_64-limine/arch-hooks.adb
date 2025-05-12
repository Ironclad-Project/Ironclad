--  arch-hooks.adb: Architecture-specific hooks for several utilities.
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

with Arch.Flanterm;
with System; use System;
with Devices.FB;
with Devices.PS2;
with Devices.PC_Speaker;
with Devices.Power_Buttons;
with Devices.Serial;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
with Arch.APIC;
with Arch.Interrupts;
with Lib.Messages;
with Devices.Ramdev;
with Arch.Limine;

package body Arch.Hooks is
   function Devices_Hook return Boolean is
   begin
      return Devices.FB.Init            and then
             Devices.PC_Speaker.Init    and then
             Devices.Power_Buttons.Init and then
             Devices.PS2.Init           and then
             Devices.Serial.Init;
   end Devices_Hook;

   procedure PRCTL_Hook
      (Code       : Natural;
       Arg        : in out Unsigned_64;
       Write_Back : out Boolean;
       Success    : out Boolean)
   is
   begin
      case Code is
         when 1 =>
            Snippets.Write_FS (Arg);
            Write_Back := False;
            Success    := True;
         when 2 =>
            Arg := Snippets.Read_FS;
            Write_Back := True;
            Success    := True;
         when 3 =>
            Snippets.Write_GS (Arg);
            Write_Back := False;
            Success    := True;
         when 4 =>
            Arg := Snippets.Read_GS;
            Write_Back := True;
            Success    := True;
         when others =>
            Arg        := 0;
            Write_Back := False;
            Success    := False;
      end case;
   exception
      when Constraint_Error =>
         Arg        := 0;
         Write_Back := False;
         Success    := False;
   end PRCTL_Hook;

   procedure Panic_SMP_Hook is
   begin
      if CPU.Core_Locals /= null then
         for I in CPU.Core_Locals.all'Range loop
            if I /= CPU.Get_Local.Number then
               APIC.LAPIC_Send_IPI
                  (CPU.Core_Locals (I).LAPIC_ID, Interrupts.Panic_Interrupt);
            end if;
         end loop;
      end if;

      for I in 1 .. 3 loop
         Devices.PC_Speaker.Beep (1000);
         Devices.PC_Speaker.Beep (500);
      end loop;

      Arch.Flanterm.Enable_For_Panic;
   exception
      when Constraint_Error =>
         null;
   end Panic_SMP_Hook;

   function Get_Configured_Cores return Positive is
   begin
      return Core_Count;
   end Get_Configured_Cores;

   function Get_Active_Core_Count return Positive is
   begin
      return Core_Count;
   end Get_Active_Core_Count;

   --  Response is a pointer to an Modules_Response.
   Modules_Request : Limine.Request :=
      (ID       => Limine.Modules_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   procedure Register_RAM_Files is
   begin
      if Modules_Request.Response /= System.Null_Address then
         declare
            ModPonse : Limine.Modules_Response
               with Import, Address => Modules_Request.Response;
            Inner : constant Limine.Limine_File_Arr (1 .. ModPonse.Mod_Count)
               with Import, Address => ModPonse.Modules;
            Translated : Boot_RAM_Files (1 .. Natural (ModPonse.Mod_Count));
            Idx : Natural := 0;
         begin
            for Ent of Inner loop
               Idx := Idx + 1;
               Translated (Idx) := (Ent.Address, Storage_Count (Ent.Size));
            end loop;

            if not Devices.Ramdev.Init (Translated) then
               Lib.Messages.Put_Line ("Could not load RAM files");
            end if;
         end;
      end if;
   exception
      when Constraint_Error =>
         Lib.Messages.Put_Line ("Errored while loading RAM files");
   end Register_RAM_Files;

   procedure Get_CPU_Model (Model : out String) is
      use Arch.Snippets;

      Discard : Boolean;
      Result  : String (1 .. 48);
      R       : array (1 .. 12) of Unsigned_32
         with Import, Address => Result'Address;
   begin
      Get_CPUID (16#80000002#, 0, R (01), R (02), R (03), R (04), Discard);
      Get_CPUID (16#80000003#, 0, R (05), R (06), R (07), R (08), Discard);
      Get_CPUID (16#80000004#, 0, R (09), R (10), R (11), R (12), Discard);
      if Model'Length >= Result'Length then
         Model (Model'First .. Model'First + Result'Length - 1) := Result;
      else
         Model := Result (Result'First .. Result'First + Model'Length - 1);
      end if;
   exception
      when Constraint_Error =>
         Model := [others => 'E'];
   end Get_CPU_Model;

   procedure Get_CPU_Vendor (Vendor : out String) is
      use Arch.Snippets;

      Discard  : Boolean;
      Discard2 : Unsigned_32;
      Result   : String (1 .. 12);
      R        : array (1 .. 3) of Unsigned_32
         with Import, Address => Result'Address;
   begin
      Get_CPUID (0, 0, Discard2, R (1), R (3), R (2), Discard);
      if Vendor'Length >= Result'Length then
         Vendor (Vendor'First .. Vendor'First + Result'Length - 1) := Result;
      else
         Vendor := Result (Result'First .. Result'First + Vendor'Length - 1);
      end if;
   exception
      when Constraint_Error =>
         Vendor := [others => 'E'];
   end Get_CPU_Vendor;

   procedure Get_CPU_Frequency (Base, Max, Reference : out Unsigned_32) is
      use Arch.Snippets;

      Discard  : Boolean;
      Discard2 : Unsigned_32;
   begin
      Get_CPUID (16#16#, 0, Base, Max, Reference, Discard2, Discard);
   end Get_CPU_Frequency;
end Arch.Hooks;
