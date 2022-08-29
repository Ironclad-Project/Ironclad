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

with System.Address_To_Access_Conversions;
with Devices.BootFB;
with Devices.PS2Mouse;
with Devices.PS2Keyboard;
with Devices.Serial;
with Devices.TTY;
with Arch.Wrappers;
with Arch.IDT;
with Arch.CPU;
with Arch.APIC;
with Lib.Panic;
with Arch.Stivale2;
with Config;
with Interfaces; use Interfaces;

package body Arch.Hooks with SPARK_Mode => Off is
   procedure Devices_Hook is
      package ST renames Stivale2;
      package C is new
         System.Address_To_Access_Conversions (ST.Framebuffer_Tag);

      Fb : constant access ST.Framebuffer_Tag := C.To_Pointer
         (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Framebuffer_ID)));
   begin
      if not Config.Is_Small then
         if not Devices.BootFB.Init (Fb) or not Devices.PS2Mouse.Init or
            not Devices.PS2Keyboard.Init or not Devices.TTY.Init
         then
            goto Error;
         end if;
      end if;

      if not Devices.Serial.Init then
         goto Error;
      end if;
      return;

   <<Error>>
      Lib.Panic.Soft_Panic ("Architectural VFS hook failed");
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      Int_Arg : constant Unsigned_64 := Unsigned_64 (To_Integer (Arg));
      A : Unsigned_64 with Address => Arg;
   begin
      case Code is
         when 1 => Wrappers.Write_FS (Int_Arg);
         when 2 => A := Wrappers.Read_FS;
         when 3 => Wrappers.Write_GS (Int_Arg);
         when 4 => A := Wrappers.Read_GS;
         when others => return False;
      end case;
      return True;
   end PRCTL_Hook;

   Panic_Vector : Arch.IDT.IRQ_Index;

   function Panic_Prepare_Hook (Addr : System.Address) return Boolean is
   begin
      return Arch.IDT.Load_ISR (Addr, Panic_Vector);
   end Panic_Prepare_Hook;

   procedure Panic_Hook is
      Current_Core : constant Positive := Arch.CPU.Get_Local.Number;
   begin
      for I in Arch.CPU.Core_Locals.all'Range loop
         if I /= Current_Core then
            Arch.APIC.LAPIC_Send_IPI
               (Arch.CPU.Core_Locals (I).LAPIC_ID, Panic_Vector);
         end if;
      end loop;
   end Panic_Hook;
end Arch.Hooks;
