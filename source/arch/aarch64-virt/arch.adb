--  arch.adb: Architecture-specific package.
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

with Config;

package body Arch is
   function Get_Info return Boot_Information is
      Ret : Boot_Information;
   begin
      return Ret;
   end Get_Info;

   procedure HCF is
   begin
      Disable_Interrupts;
      loop
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsie if", Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsid if", Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm ("wfi", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      System.Machine_Code.Asm ("wfe", Volatile => True);
   end Pause;

   procedure Debug_Print (Message : Character) is
   begin
      return;
   end Debug_Print;

   procedure Debug_Print (Message : String) is
   begin
      for C of Message loop
         Debug_Print (C);
      end loop;
   end Debug_Print;

   procedure Devices_Hook is
   begin
      return;
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      pragma Unreferenced (Code);
      pragma Unreferenced (Arg);
   begin
      return False;
   end PRCTL_Hook;
end Arch;
