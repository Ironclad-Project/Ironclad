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
with Lib.Synchronization;

package body Devices.PS2Keyboard is
   --  Globals to communicate with the interrupt routine.
   Data_Mutex : aliased Lib.Synchronization.Binary_Semaphore
      := Lib.Synchronization.Unlocked_Semaphore;
   Has_Data   : Boolean := False with Volatile;
   Scan_Count : Natural := 0     with Volatile;
   Scancodes  : array (1 .. 10) of Unsigned_8 := [others => 0] with Volatile;

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
           ID          => [others => 0],
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Sync        => null,
           Sync_Range  => null,
           Read        => Read'Access,
           Write       => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => Poll'Access,
           Remove      => null), "ps2keyboard", Success);
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
      pragma Unreferenced (Key, Offset);
      Temp : Boolean;
   begin
      if Is_Blocking then
         loop
            Lib.Synchronization.Seize (Data_Mutex);
            Temp := Has_Data;
            exit when Temp;
            Lib.Synchronization.Release (Data_Mutex);
            Scheduler.Yield_If_Able;
         end loop;
      else
         Lib.Synchronization.Seize (Data_Mutex);
         Temp := Has_Data;
      end if;

      if Temp and Data'Length /= 0 then
         if Data'Length > Scan_Count then
            Ret_Count := Scan_Count;
         else
            Ret_Count := Data'Length;
         end if;

         Data (Data'First .. Data'First + Ret_Count - 1) :=
            Operation_Data (Scancodes (1 .. Ret_Count));
         if Ret_Count < Scan_Count then
            Scancodes (1 .. Ret_Count) :=
               Scancodes (Ret_Count + 1 .. Ret_Count * 2);
            Scan_Count := Scan_Count - Ret_Count;
         else
            Scan_Count := 0;
            Has_Data   := False;
         end if;

         Success := True;
      else
         Success   := False;
         Ret_Count := 0;
      end if;

      Lib.Synchronization.Release (Data_Mutex);
   end Read;

   procedure Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Data);
   begin
      Lib.Synchronization.Seize (Data_Mutex);
      Can_Read  := Has_Data;
      Can_Write := False;
      Is_Error  := False;
      Lib.Synchronization.Release (Data_Mutex);
   end Poll;

   procedure Keyboard_Handler is
      Input : constant Unsigned_8 := Arch.Snippets.Port_In (16#60#);
   begin
      Lib.Synchronization.Seize (Data_Mutex);

      if Scan_Count < Scancodes'Length then
         if Scan_Count > 1 and then Scancodes (Scan_Count - 1) = 16#E0# then
            if Scan_Count + 1 = Scancodes'Length then
               goto Cleanup;
            end if;

            Scancodes (Scancodes'First + Scan_Count) := Input;
            Has_Data := True;
         else
            Scancodes (Scancodes'First + Scan_Count) := Input;
            Has_Data := Input /= 16#E0#;
         end if;

         Scan_Count := Scan_Count + 1;
      end if;

   <<Cleanup>>
      Lib.Synchronization.Release (Data_Mutex);
      Arch.APIC.LAPIC_EOI;
   end Keyboard_Handler;
end Devices.PS2Keyboard;
