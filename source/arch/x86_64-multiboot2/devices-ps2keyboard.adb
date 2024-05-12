--  devices-ps2keyboard.adb: PS2 keyboard driver.
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

with Arch.IDT;
with Arch.APIC;
with Arch.CPU;
with Arch.Snippets;
with Scheduler;

package body Devices.PS2Keyboard is
   --  Globals to communicate with the interrupt routine.
   Has_Data  : Boolean                      := False  with Volatile;
   Scancodes : array (1 .. 2) of Unsigned_8 := (0, 0) with Volatile;

   function Init return Boolean is
      BSP_LAPIC : constant Unsigned_32 := Arch.CPU.Core_Locals (1).LAPIC_ID;
      Index     : Arch.IDT.IRQ_Index;
      Data      : Unsigned_8;
      Success   : Boolean;
   begin
      --  Set the interrupt up, which is always the 34 (we are 1 based).
      Arch.IDT.Load_ISR (Keyboard_Handler'Address, Index, Success);
      if not Success then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect (BSP_LAPIC, 34, Index, True) then
         return False;
      end if;

      --  Take the chance for initializing the PS2 controller.
      --  Disable primary and secondary PS/2 ports
      Arch.Snippets.Port_Out (16#64#, 16#AD#);
      Arch.Snippets.Port_Out (16#64#, 16#A7#);

      --  Read from port 0x60 to flush the PS/2 controller buffer.
      while (Arch.Snippets.Port_In (16#64#) and 1) /= 0 loop
         Data := Arch.Snippets.Port_In (16#60#);
      end loop;

      --  Enable keyboard interrupt and keyboard scancode translation.
      Data := Read_PS2_Config;
      Data := Data or Shift_Left (1, 0) or Shift_Left (1, 6);

      --  Enable mouse interrupt if any
      if (Data and Shift_Left (1, 5)) /= 0 then
         Data := Data or Shift_Left (1, 1);
      end if;

      --  Write changes, enable keyboard port, and enable mouse port if any.
      Write_PS2_Config (Data);
      Write_PS2 (16#64#, 16#AE#);
      if (Data and Shift_Left (1, 5)) /= 0 then
         Write_PS2 (16#64#, 16#A8#);
      end if;

      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Sync        => null,
           Sync_Range  => null,
           Read        => Read'Access,
           Write       => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => Poll'Access), "ps2keyboard", Success);
      return Success;
   end Init;

   function Read_PS2 return Unsigned_8 is
   begin
      while (Arch.Snippets.Port_In (16#64#) and 1) = 0 loop
         Scheduler.Yield_If_Able;
      end loop;
      return Arch.Snippets.Port_In (16#60#);
   end Read_PS2;

   procedure Write_PS2 (Port : Unsigned_16; Value : Unsigned_8) is
   begin
      while (Arch.Snippets.Port_In (16#64#) and 2) /= 0 loop
         Scheduler.Yield_If_Able;
      end loop;
      Arch.Snippets.Port_Out (Port, Value);
   end Write_PS2;

   function Read_PS2_Config return Unsigned_8 is
   begin
      Write_PS2 (16#64#, 16#20#);
      return Read_PS2;
   end Read_PS2_Config;

   procedure Write_PS2_Config (Value : Unsigned_8) is
   begin
      Write_PS2 (16#64#, 16#60#);
      Write_PS2 (16#60#, Value);
   end Write_PS2_Config;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      Temp : Boolean := Has_Data;
      Code : Unsigned_8;
   begin
      if Is_Blocking then
         loop
            exit when Temp;
            Scheduler.Yield_If_Able;
            Temp := Has_Data;
         end loop;
      end if;

      if Temp then
         Code := Scancodes (2);
         if Code = 16#00# then
            Data (Data'First) := Scancodes (1);
            Ret_Count         := 1;
         else
            Data (Data'First)     := Scancodes (1);
            Data (Data'First + 1) := Scancodes (2);
            Ret_Count             := 2;
         end if;

         Success  := True;
         Has_Data := False;
      else
         Success   := False;
         Ret_Count := 0;
      end if;
   end Read;

   procedure Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Data);
   begin
      Can_Read  := Has_Data;
      Can_Write := False;
      Is_Error  := False;
   end Poll;

   procedure Keyboard_Handler is
      Input : constant Unsigned_8 := Arch.Snippets.Port_In (16#60#);
      C1    : constant Unsigned_8 := Scancodes (1);
      C2    : constant Unsigned_8 := Scancodes (2);
   begin
      if C1 = 16#E0# and C2 = 16#00# then
         Scancodes (2) := Input;
         Has_Data      := True;
      else
         Scancodes (1) := Input;
         Scancodes (2) := 16#00#;
         Has_Data      := Input /= 16#E0#;
      end if;

      Arch.APIC.LAPIC_EOI;
   end Keyboard_Handler;
end Devices.PS2Keyboard;
