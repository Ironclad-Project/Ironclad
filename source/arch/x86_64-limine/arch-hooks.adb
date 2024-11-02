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

with Devices.FB;
with Devices.PS2Mouse;
with Devices.PS2Keyboard;
with Devices.PC_Speaker;
with Devices.ATA;
with Devices.SATA;
with Devices.Serial;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
with Arch.APIC;
with Arch.Interrupts;
with Interfaces; use Interfaces;
with Devices.i6300ESB;
with Devices.LPT;
with Lib.Messages;
with Devices.Ramdev;
with Devices.HWRNG;
with Arch.Limine;

package body Arch.Hooks is
   function Devices_Hook return Boolean is
   begin
      return Devices.ATA.Init         and then
             Devices.FB.Init          and then
             Devices.HWRNG.Init       and then
             Devices.i6300ESB.Init    and then
             Devices.LPT.Init         and then
             Devices.PC_Speaker.Init  and then
             Devices.PS2Keyboard.Init and then
             Devices.PS2Mouse.Init    and then
             Devices.SATA.Init        and then
             Devices.Serial.Init;
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

      for I in 1 .. 3 loop
         Devices.PC_Speaker.Beep (1000);
         Devices.PC_Speaker.Beep (500);
      end loop;
   end Panic_SMP_Hook;

   function Get_Active_Core_Count return Positive is
   begin
      return Core_Count;
   end Get_Active_Core_Count;

   procedure Register_RAM_Files is
   begin
      if not Devices.Ramdev.Init
         (Limine.Global_Info.RAM_Files (1 .. Limine.Global_Info.RAM_Files_Len))
      then
         Lib.Messages.Put_Line ("Could not load RAM files");
      end if;
   end Register_RAM_Files;
end Arch.Hooks;
