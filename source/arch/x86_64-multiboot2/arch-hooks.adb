--  arch-hooks.adb: Architecture-specific hooks for several utilities.
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

with Devices.FB;
with Devices.PS2Mouse;
with Devices.PS2Keyboard;
with Devices.ATA;
with Devices.Serial;
with Devices.RTC;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
with Arch.APIC;
with Lib.Panic;
with Arch.Interrupts;
with Config;
with Interfaces; use Interfaces;

package body Arch.Hooks with SPARK_Mode => Off is
   procedure Devices_Hook is
      type Option is record
         Is_Called : Boolean;
         Callback  : access function return Boolean;
      end record;
      Init_Calls : constant array (1 .. 6) of Option :=
         ((Config.Support_X86_64_BootFB, Devices.FB.Init'Access),
          (Config.Support_X86_64_PS2,    Devices.PS2Keyboard.Init'Access),
          (Config.Support_X86_64_PS2,    Devices.PS2Mouse.Init'Access),
          (Config.Support_X86_64_ATA,    Devices.ATA.Init'Access),
          (Config.Support_X86_64_Serial, Devices.Serial.Init'Access),
          (Config.Support_X86_64_RTC,    Devices.RTC.Init'Access));
   begin
      for Op of Init_Calls loop
         if Op.Is_Called and then not Op.Callback.all then
            Lib.Panic.Hard_Panic ("Architectural VFS hook failed");
         end if;
      end loop;
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      Int_Arg : constant Unsigned_64 := Unsigned_64 (To_Integer (Arg));
      Value   : Unsigned_64 with Import, Address => Arg;
   begin
      case Code is
         when 1 => Snippets.Write_FS (Int_Arg);
         when 2 => Value := Snippets.Read_FS;
         when 3 => Snippets.Write_GS (Int_Arg);
         when 4 => Value := Snippets.Read_GS;
         when others => return False;
      end case;
      return True;
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
   end Panic_SMP_Hook;

   function Get_Active_Core_Count return Positive is
   begin
      return Core_Count;
   end Get_Active_Core_Count;
end Arch.Hooks;
