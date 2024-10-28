--  devices-i6300esb.adb: i6300ESB watchdog driver.
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

with Memory;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

package body Devices.i6300ESB is
   --  TODO: This beautiful piece of hardware is a 2-stage  \ ______/ V`-,
   --  is a 2-stage watchdog, so far we only use the second  }        /~~
   --  instant death stage, maybe a software recoverable dog /_)^ --,r'
   --  would be nice to expose to userland somehow?         |b      |b

   package Con is new System.Address_To_Access_Conversions (Dog_Data);

   function Init return Boolean is
      Success : Boolean;
      Data    : Dog_Data_Acc;
      PCI_Dev : Arch.PCI.PCI_Device;
      PCI_BAR : Arch.PCI.Base_Address_Register;
   begin
      Arch.PCI.Search_Device (16#8#, 16#80#, 0, 1, PCI_Dev, Success);
      if not Success then
         return True;
      end if;
      Arch.PCI.Get_BAR (PCI_Dev, 0, PCI_BAR, Success);
      if not Success or else not PCI_BAR.Is_MMIO then
         return True;
      end if;

      Data := new Dog_Data'
         (PCI_Data  => PCI_Dev,
          Base_Addr => To_Address (Memory.Memory_Offset + PCI_BAR.Base));

      --  Initialize the device.
      Arch.PCI.Write16 (PCI_Dev, CONFIG, DOG_OUTPUT or DOG_INT_TYPE);
      Arch.PCI.Write8  (PCI_Dev, LOCK,   0);
      Unlock_Registers (Data.Base_Addr);
      declare
         Reg : Unsigned_16 with Import, Address => Data.Base_Addr + RELOAD;
      begin
         Reg := DOG_RELOAD or DOG_TIMEOUT;
      end;

      Register
         ((Data        => Con.To_Address (Con.Object_Pointer (Data)),
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => null,
           Write       => Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => IO_Control'Access,
           Mmap        => null,
           Poll        => null), "i6300esb", Success);
      return Success;
   end Init;
   ----------------------------------------------------------------------------
   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
      D : constant Dog_Data_Acc := Dog_Data_Acc (Con.To_Pointer (Key));
   begin
      Keep_Alive (D.Base_Addr);
      Ret_Count := Data'Length;
      Success   := True;
   end Write;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      WDOG_START     : constant := 1;
      WDOG_STOP      : constant := 2;
      WDOG_HEARTBEAT : constant := 3;
      D : constant Dog_Data_Acc := Dog_Data_Acc (Con.To_Pointer (Data));

      Timeout    : Unsigned_32 with Import, Address => Argument;
      TIMER1_Reg : Unsigned_32 with Import, Address => D.Base_Addr + TIMER1;
      TIMER2_Reg : Unsigned_32 with Import, Address => D.Base_Addr + TIMER2;
   begin
      Keep_Alive (D.Base_Addr);
      case Request is
         when WDOG_START =>
            Arch.PCI.Write8 (D.PCI_Data, LOCK, DOG_ENABLE);
            return True;
         when WDOG_STOP =>
            Arch.PCI.Write8 (D.PCI_Data, LOCK, 0);
            return Arch.PCI.Read8 (D.PCI_Data, LOCK) /= 0;
         when WDOG_HEARTBEAT =>
            Unlock_Registers (D.Base_Addr);
            TIMER1_Reg := Shift_Left (Timeout, 9);
            Unlock_Registers (D.Base_Addr);
            TIMER2_Reg := Shift_Left (Timeout, 9);
            Keep_Alive (D.Base_Addr);
            return True;
         when others =>
            return False;
      end case;
   end IO_Control;
   ----------------------------------------------------------------------------
   procedure Unlock_Registers (Base_Addr : System.Address) is
      Reg : Unsigned_16 with Import, Address => Base_Addr + RELOAD;
   begin
      Reg := UNLOCK1;
      Reg := UNLOCK2;
   end Unlock_Registers;

   procedure Keep_Alive (Base_Addr : System.Address) is
      Reg : Unsigned_16 with Import, Address => Base_Addr + RELOAD;
   begin
      Unlock_Registers (Base_Addr);
      Reg := DOG_RELOAD;
   end Keep_Alive;
end Devices.i6300ESB;
