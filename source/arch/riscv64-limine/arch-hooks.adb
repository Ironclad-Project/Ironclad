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

package body Arch.Hooks is
   function Devices_Hook return Boolean is
   begin
      return True;
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
      null;
   end Panic_SMP_Hook;

   function Get_Configured_Cores return Positive is
   begin
      return 1;
   end Get_Configured_Cores;

   function Get_Active_Core_Count return Positive is
   begin
      return 1;
   end Get_Active_Core_Count;

   procedure Register_RAM_Files is
   begin
      null;
   end Register_RAM_Files;

   procedure Get_CPU_Model (Model : out String) is
      Returned : constant String := "GenericRISCV64";
   begin
      if Model'Length >= Returned'Length then
         Model (Model'First .. Model'First + Returned'Length - 1) := Returned;
      else
         Model := Returned
            (Returned'First .. Returned'First + Model'Length - 1);
      end if;
   end Get_CPU_Model;

   procedure Get_CPU_Vendor (Vendor : out String) is
      Returned : constant String := "GenericVendor";
   begin
      if Vendor'Length >= Returned'Length then
         Vendor (Vendor'First .. Vendor'First + Returned'Length - 1) :=
            Returned;
      else
         Vendor := Returned
            (Returned'First .. Returned'First + Vendor'Length - 1);
      end if;
   end Get_CPU_Vendor;

   procedure Get_CPU_Frequency (Base, Max, Reference : out Unsigned_32) is
   begin
      Base      := 0;
      Max       := 0;
      Reference := 0;
   end Get_CPU_Frequency;
end Arch.Hooks;
