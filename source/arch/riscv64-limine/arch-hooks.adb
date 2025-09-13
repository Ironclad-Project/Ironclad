--  arch-hooks.adb: Architecture-specific hooks for several utilities.
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

with Arch.Flanterm;
with Devices.FB;
with Arch.CPU;
with Devices.Ramdev;
with Arch.Limine;
with Messages;
with System; use System;
with Arch.SBI;

package body Arch.Hooks with SPARK_Mode => Off is
   procedure Devices_Hook (Success : out Boolean) is
   begin
      Devices.FB.Init (Success);
   end Devices_Hook;

   procedure PRCTL_Hook
      (Code       : Natural;
       Arg        : in out Unsigned_64;
       Write_Back : out Boolean;
       Success    : out Boolean)
   is
      pragma Unreferenced (Code, Arg);
   begin
      Write_Back := False;
      Success    := False;
   end PRCTL_Hook;

   procedure Panic_SMP_Hook is
   begin
      Arch.Flanterm.Enable_For_Panic;
   end Panic_SMP_Hook;

   function Get_Configured_Cores return Positive is
   begin
      return Arch.CPU.Core_Count;
   end Get_Configured_Cores;

   function Get_Active_Core_Count return Positive is
   begin
      return Arch.CPU.Core_Count;
   end Get_Active_Core_Count;

   --  Response is a pointer to an Modules_Response.
   Modules_Request : Limine.Request :=
      (ID       => Limine.Modules_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export;

   procedure Register_RAM_Files is
      Success : Boolean;
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

            Devices.Ramdev.Init (Translated, Success);
            if not Success then
               Messages.Put_Line ("Could not load RAM files");
            end if;
         end;
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Errored while loading RAM files");
   end Register_RAM_Files;

   procedure Get_CPU_Model (Model : out String) is
      ID, Real_ID : Unsigned_64;
      Discard     : Boolean;
      Proprietary : Boolean;
      Str         : String (1 .. 18);
      Len         : Natural;
   begin
      Arch.SBI.Get_Arch_ID (ID, Discard);
      Proprietary := (ID and Shift_Left (1, 63)) /= 0;
      Real_ID     := ID and not Shift_Left (1, 63);

      if Proprietary then
         Str (1 .. 17) := "Ewwww Proprietary";
         Len           := 17;
      else
         --  These IDs are standard and maintained by the RISC-V foundation.
         --  https://github.com/riscv/riscv-isa-manual/blob/main/marchid.md
         case Real_ID is
            when      1 => Str (1 ..  6) := "Rocket";             Len := 6;
            when      2 => Str (1 ..  4) := "BOOM";               Len := 4;
            when     31 => Str (1 ..  6) := "AIRISC";             Len := 6;
            when     32 => Str (1 ..  7) := "Proteus";            Len := 7;
            when     42 => Str (1 ..  4) := "QEMU";               Len := 4;
            when others => Str (1 .. 18) := "Unrecognized Model"; Len := 18;
         end case;
      end if;

      if Model'Length >= Len then
         Model (Model'First .. Model'First + Len - 1) := Str (1 .. Len);
      else
         Model := Str (1 .. Model'Length);
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while getting CPU model");
         Model := [others => ' '];
   end Get_CPU_Model;

   procedure Get_CPU_Vendor (Vendor : out String) is
      ID      : Unsigned_64;
      Discard : Boolean;
   begin
      Arch.SBI.Get_Vendor_ID (ID, Discard);
      declare
         Str : constant  String := "JEDEC " & ID'Image;
         Len : constant Natural := Str'Length;
      begin
         if Vendor'Length >= Len then
            Vendor (Vendor'First .. Vendor'First + Len - 1) := Str (1 .. Len);
         else
            Vendor := Str (1 .. Vendor'Length);
         end if;
      end;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Exception while getting CPU vendor");
         Vendor := [others => ' '];
   end Get_CPU_Vendor;

   procedure Get_CPU_Frequency (Base, Max, Reference : out Unsigned_32) is
   begin
      Base      := 0;
      Max       := 0;
      Reference := 0;
   end Get_CPU_Frequency;
end Arch.Hooks;
